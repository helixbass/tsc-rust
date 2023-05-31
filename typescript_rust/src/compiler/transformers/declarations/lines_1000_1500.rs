use std::{io, ptr};

use gc::{Finalize, Gc, GcCell, Trace};

use super::{
    declaration_emit_node_builder_flags, is_preserved_declaration_statement, TransformDeclarations,
};
use crate::{
    can_have_modifiers, can_produce_diagnostics, create_empty_exports,
    create_get_symbol_accessibility_diagnostic_for_node, create_symbol_table,
    get_effective_base_type_node, get_effective_modifier_flags, get_first_constructor_with_body,
    get_original_node_id, get_parse_node_factory, has_dynamic_name, has_effective_modifier,
    has_syntactic_modifier, is_binding_pattern, is_declaration, is_entity_name_expression,
    is_external_module_augmentation, is_function_like, is_global_scope_augmentation,
    is_import_equals_declaration, is_omitted_expression, is_private_identifier,
    is_property_access_expression, is_source_file, is_string_a_non_contextual_keyword,
    is_type_node, is_type_parameter_declaration, map, map_defined, maybe_concatenate,
    maybe_get_original_node_id, return_ok_default_if_none, set_original_node, set_parent, some,
    try_flat_map, try_map, try_map_defined, try_maybe_map, try_maybe_visit_node,
    try_maybe_visit_nodes, try_visit_node, try_visit_nodes, unescape_leading_underscores,
    visit_nodes, AsDoubleDeref, ClassLikeDeclarationInterface, Debug_, Diagnostics,
    GeneratedIdentifierFlags, GetSymbolAccessibilityDiagnostic,
    GetSymbolAccessibilityDiagnosticInterface, HasQuestionTokenInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface,
    InterfaceOrClassLikeDeclarationInterface, ModifierFlags, NamedDeclarationInterface, Node,
    NodeArray, NodeFlags, NodeInterface, OptionTry, SignatureDeclarationInterface,
    SingleNodeOrVecNode, StringOrNumber, Symbol, SymbolAccessibilityDiagnostic,
    SymbolAccessibilityResult, SymbolInterface, SyntaxKind, VisitResult, VisitResultInterface,
};

impl TransformDeclarations {
    pub(super) fn visit_declaration_subtree_cleanup(
        &self,
        input: &Node,
        can_produce_diagnostic: bool,
        previous_enclosing_declaration: Option<&Gc<Node>>,
        old_diag: &GetSymbolAccessibilityDiagnostic,
        should_enter_suppress_new_diagnostics_context_context: bool,
        old_within_object_literal_type: Option<bool>,
        return_value: Option<&Node>,
    ) -> io::Result<VisitResult> {
        if return_value.is_some() && can_produce_diagnostic && has_dynamic_name(input) {
            self.check_name(input)?;
        }
        if self.is_enclosing_declaration(input) {
            self.set_enclosing_declaration(previous_enclosing_declaration.cloned());
        }
        if can_produce_diagnostic && self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(old_diag.clone());
        }
        if should_enter_suppress_new_diagnostics_context_context {
            self.set_suppress_new_diagnostic_contexts(old_within_object_literal_type);
        }
        if matches!(
            return_value,
            Some(return_value) if ptr::eq(return_value, input)
        ) {
            return Ok(return_value.map(Node::node_wrapper).map(Into::into));
        }
        Ok(return_value
            .map(|return_value| {
                set_original_node(
                    self.preserve_js_doc(return_value, input),
                    Some(input.node_wrapper()),
                )
            })
            .map(Into::into))
    }

    pub(super) fn is_private_method_type_parameter(
        &self,
        node: &Node, /*TypeParameterDeclaration*/
    ) -> bool {
        node.parent().kind() == SyntaxKind::MethodDeclaration
            && has_effective_modifier(&node.parent(), ModifierFlags::Private)
    }

    pub(super) fn visit_declaration_statements(&self, input: &Node) -> io::Result<VisitResult> /*<Node>*/
    {
        if !is_preserved_declaration_statement(input) {
            return Ok(None);
        }
        if self.should_strip_internal(input) {
            return Ok(None);
        }

        match input.kind() {
            SyntaxKind::ExportDeclaration => {
                let input_as_export_declaration = input.as_export_declaration();
                if is_source_file(&input.parent()) {
                    self.set_result_has_external_module_indicator(true);
                }
                self.set_result_has_scope_marker(true);
                return Ok(Some(
                    self.factory
                        .update_export_declaration(
                            input,
                            Option::<Gc<NodeArray>>::None,
                            input.maybe_modifiers(),
                            input_as_export_declaration.is_type_only,
                            input_as_export_declaration.export_clause.clone(),
                            self.rewrite_module_specifier(
                                input,
                                input_as_export_declaration.module_specifier.as_deref(),
                            )?,
                            None,
                        )
                        .into(),
                ));
            }
            SyntaxKind::ExportAssignment => {
                let input_as_export_assignment = input.as_export_assignment();
                if is_source_file(&input.parent()) {
                    self.set_result_has_external_module_indicator(true);
                }
                self.set_result_has_scope_marker(true);
                if input_as_export_assignment.expression.kind() == SyntaxKind::Identifier {
                    return Ok(Some(input.node_wrapper().into()));
                } else {
                    let new_id = self
                        .factory
                        .create_unique_name("_default", Some(GeneratedIdentifierFlags::Optimistic));
                    self.set_get_symbol_accessibility_diagnostic(
                        VisitDeclarationStatementsGetSymbolAccessibilityDiagnostic::new(input),
                    );
                    self.set_error_fallback_node(Some(input.node_wrapper()));
                    let var_decl = self
                        .factory
                        .create_variable_declaration(
                            Some(new_id.clone()),
                            None,
                            self.resolver.create_type_of_expression(
                                &input_as_export_assignment.expression,
                                input,
                                declaration_emit_node_builder_flags(),
                                self.symbol_tracker(),
                            )?,
                            None,
                        )
                        .wrap();
                    self.set_error_fallback_node(None);
                    let statement = self
                        .factory
                        .create_variable_statement(
                            if self.needs_declare() {
                                Some(vec![self
                                    .factory
                                    .create_modifier(SyntaxKind::DeclareKeyword)
                                    .wrap()])
                            } else {
                                Some(vec![])
                            },
                            self.factory
                                .create_variable_declaration_list(
                                    vec![var_decl],
                                    Some(NodeFlags::Const),
                                )
                                .wrap(),
                        )
                        .wrap();
                    return Ok(Some(
                        vec![
                            statement,
                            self.factory.update_export_assignment(
                                input,
                                input.maybe_decorators(),
                                input.maybe_modifiers(),
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
            .insert(get_original_node_id(input), result);
        Ok(Some(input.node_wrapper().into()))
    }

    pub(super) fn strip_export_modifiers(
        &self,
        statement: &Node, /*Statement*/
    ) -> Gc<Node /*Statement*/> {
        if is_import_equals_declaration(statement)
            || has_effective_modifier(statement, ModifierFlags::Default)
            || !can_have_modifiers(statement)
        {
            return statement.node_wrapper();
        }

        let modifiers = self.factory.create_modifiers_from_modifier_flags(
            get_effective_modifier_flags(statement) & (ModifierFlags::All ^ ModifierFlags::Export),
        );
        self.factory.update_modifiers(statement, modifiers)
    }

    pub(super) fn transform_top_level_declaration(
        &self,
        input: &Node, /*LateVisibilityPaintedStatement*/
    ) -> io::Result<VisitResult> {
        if self.should_strip_internal(input) {
            return Ok(None);
        }
        match input.kind() {
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
        if is_declaration(input) && self.is_declaration_and_not_visible(input) {
            return Ok(None);
        }

        if is_function_like(Some(input))
            && self.resolver.is_implementation_of_overload(input)? == Some(true)
        {
            return Ok(None);
        }

        let mut previous_enclosing_declaration: Option<Gc<Node>> = None;
        if self.is_enclosing_declaration(input) {
            previous_enclosing_declaration = self.maybe_enclosing_declaration();
            self.set_enclosing_declaration(Some(input.node_wrapper()));
        }

        let can_prodice_diagnostic = can_produce_diagnostics(input);
        let old_diag = self.get_symbol_accessibility_diagnostic();
        if can_prodice_diagnostic {
            self.set_get_symbol_accessibility_diagnostic(
                create_get_symbol_accessibility_diagnostic_for_node(input),
            );
        }

        let previous_needs_declare = self.needs_declare();
        Ok(match input.kind() {
            SyntaxKind::TypeAliasDeclaration => {
                let input_as_type_alias_declaration = input.as_type_alias_declaration();
                self.transform_top_level_declaration_cleanup(
                    input,
                    previous_enclosing_declaration.as_ref(),
                    can_prodice_diagnostic,
                    &old_diag,
                    previous_needs_declare,
                    Some(
                        &self.factory.update_type_alias_declaration(
                            input,
                            Option::<Gc<NodeArray>>::None,
                            self.ensure_modifiers(input),
                            input_as_type_alias_declaration.name(),
                            try_maybe_visit_nodes(
                                input_as_type_alias_declaration
                                    .maybe_type_parameters()
                                    .as_deref(),
                                Some(|node: &Node| self.visit_declaration_subtree(node)),
                                Some(is_type_parameter_declaration),
                                None,
                                None,
                            )?,
                            try_visit_node(
                                input_as_type_alias_declaration
                                    .maybe_type()
                                    .as_deref()
                                    .unwrap(),
                                Some(|node: &Node| self.visit_declaration_subtree(node)),
                                Some(is_type_node),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            )?,
                        ),
                    ),
                )
            }
            SyntaxKind::InterfaceDeclaration => {
                let input_as_interface_declaration = input.as_interface_declaration();
                self.transform_top_level_declaration_cleanup(
                    input,
                    previous_enclosing_declaration.as_ref(),
                    can_prodice_diagnostic,
                    &old_diag,
                    previous_needs_declare,
                    Some(
                        &self.factory.update_interface_declaration(
                            input,
                            Option::<Gc<NodeArray>>::None,
                            self.ensure_modifiers(input),
                            input_as_interface_declaration.name(),
                            self.ensure_type_params(
                                input,
                                input_as_interface_declaration
                                    .maybe_type_parameters()
                                    .as_deref(),
                            )?,
                            Some(
                                self.transform_heritage_clauses(
                                    input_as_interface_declaration
                                        .maybe_heritage_clauses()
                                        .as_deref(),
                                )?,
                            ),
                            try_visit_nodes(
                                &input_as_interface_declaration.members,
                                Some(|node: &Node| self.visit_declaration_subtree(node)),
                                Option::<fn(&Node) -> bool>::None,
                                None,
                                None,
                            )?,
                        ),
                    ),
                )
            }
            SyntaxKind::FunctionDeclaration => {
                let input_as_function_declaration = input.as_function_declaration();
                let clean = self.transform_top_level_declaration_cleanup(
                    input,
                    previous_enclosing_declaration.as_ref(),
                    can_prodice_diagnostic,
                    &old_diag,
                    previous_needs_declare,
                    Some(
                        &self.factory.update_function_declaration(
                            input,
                            Option::<Gc<NodeArray>>::None,
                            self.ensure_modifiers(input),
                            None,
                            input_as_function_declaration.maybe_name(),
                            self.ensure_type_params(
                                input,
                                input_as_function_declaration
                                    .maybe_type_parameters()
                                    .as_deref(),
                            )?,
                            self.update_params_list(
                                input,
                                Some(&input_as_function_declaration.parameters()),
                                None,
                            )?
                            .unwrap(),
                            self.ensure_type(
                                input,
                                input_as_function_declaration.maybe_type().as_deref(),
                                None,
                            )?,
                            None,
                        ),
                    ),
                );
                if let Some(clean) = clean.as_ref().try_filter(|_| -> io::Result<_> {
                    Ok(self.resolver.is_expando_function_declaration(input)?
                        && self.should_emit_function_properties(input))
                })? {
                    let ref clean = clean.as_single_node();
                    let clean_as_function_declaration = clean.as_function_declaration();
                    let props = self.resolver.get_properties_of_container_function(input)?;
                    let fakespace = get_parse_node_factory()
                        .create_module_declaration(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            clean_as_function_declaration
                                .maybe_name()
                                .unwrap_or_else(|| {
                                    self.factory
                                        .create_identifier(
                                            "_default",
                                            Option::<Gc<NodeArray>>::None,
                                            None,
                                        )
                                        .wrap()
                                }),
                            Some(self.factory.create_module_block(Some(vec![])).wrap()),
                            Some(NodeFlags::Namespace),
                        )
                        .wrap();
                    set_parent(&fakespace, self.maybe_enclosing_declaration());
                    fakespace.set_locals(Some(Gc::new(GcCell::new(create_symbol_table(Some(
                        &props,
                    ))))));
                    fakespace.set_symbol(props[0].maybe_parent().unwrap());
                    let mut export_mappings: Vec<(Gc<Node /*Identifier*/>, String)> =
                        Default::default();
                    let mut declarations = try_map_defined(
                        Some(&props),
                        |p: &Gc<Symbol>, _| -> io::Result<Option<Gc<Node>>> {
                            let ref p_value_declaration = return_ok_default_if_none!(p
                                .maybe_value_declaration()
                                .filter(|p_value_declaration| {
                                    is_property_access_expression(p_value_declaration)
                                }));
                            self.set_get_symbol_accessibility_diagnostic(
                                create_get_symbol_accessibility_diagnostic_for_node(
                                    p_value_declaration,
                                ),
                            );
                            let type_ = self.resolver.create_type_of_declaration(
                                p_value_declaration,
                                &fakespace,
                                declaration_emit_node_builder_flags(),
                                self.symbol_tracker(),
                                None,
                            )?;
                            self.set_get_symbol_accessibility_diagnostic(old_diag.clone());
                            let name_str = unescape_leading_underscores(p.escaped_name());
                            let is_non_contextual_keyword_name =
                                is_string_a_non_contextual_keyword(name_str);
                            let name = if is_non_contextual_keyword_name {
                                self.factory
                                    .get_generated_name_for_node(Some(&**p_value_declaration), None)
                            } else {
                                self.factory
                                    .create_identifier(
                                        name_str,
                                        Option::<Gc<NodeArray>>::None,
                                        None,
                                    )
                                    .wrap()
                            };
                            if is_non_contextual_keyword_name {
                                export_mappings.push((name.clone(), name_str.to_owned()));
                            }
                            let var_decl = self
                                .factory
                                .create_variable_declaration(Some(name), None, type_, None)
                                .wrap();
                            Ok(Some(
                                self.factory
                                    .create_variable_statement(
                                        if is_non_contextual_keyword_name {
                                            None
                                        } else {
                                            Some(vec![self
                                                .factory
                                                .create_token(SyntaxKind::ExportKeyword)
                                                .wrap()])
                                        },
                                        self.factory
                                            .create_variable_declaration_list(vec![var_decl], None)
                                            .wrap(),
                                    )
                                    .wrap(),
                            ))
                        },
                    )?;
                    if export_mappings.is_empty() {
                        declarations =
                            map_defined(Some(&declarations), |declaration: &Gc<Node>, _| {
                                Some(
                                    self.factory
                                        .update_modifiers(declaration, ModifierFlags::None),
                                )
                            });
                    } else {
                        declarations.push(
                            self.factory
                                .create_export_declaration(
                                    Option::<Gc<NodeArray>>::None,
                                    Option::<Gc<NodeArray>>::None,
                                    false,
                                    Some(
                                        self.factory
                                            .create_named_exports(map(
                                                export_mappings,
                                                |(gen, exp), _| {
                                                    self.factory
                                                        .create_export_specifier(
                                                            false,
                                                            Some(gen),
                                                            &*exp,
                                                        )
                                                        .wrap()
                                                },
                                            ))
                                            .wrap(),
                                    ),
                                    None,
                                    None,
                                )
                                .wrap(),
                        );
                    }
                    let namespace_decl = self
                        .factory
                        .create_module_declaration(
                            Option::<Gc<NodeArray>>::None,
                            self.ensure_modifiers(input),
                            input_as_function_declaration.name(),
                            Some(self.factory.create_module_block(Some(declarations)).wrap()),
                            Some(NodeFlags::Namespace),
                        )
                        .wrap();
                    if !has_effective_modifier(clean, ModifierFlags::Default) {
                        return Ok(Some(vec![clean.clone(), namespace_decl].into()));
                    }

                    let modifiers = self.factory.create_modifiers_from_modifier_flags(
                        (get_effective_modifier_flags(clean) & !ModifierFlags::ExportDefault)
                            | ModifierFlags::Ambient,
                    );
                    let clean_declaration = self.factory.update_function_declaration(
                        clean,
                        Option::<Gc<NodeArray>>::None,
                        Some(modifiers.clone()),
                        None,
                        clean_as_function_declaration.maybe_name(),
                        clean_as_function_declaration.maybe_type_parameters(),
                        clean_as_function_declaration.parameters(),
                        clean_as_function_declaration.maybe_type(),
                        None,
                    );

                    let namespace_decl_as_module_declaration =
                        namespace_decl.as_module_declaration();
                    let namespace_declaration = self.factory.update_module_declaration(
                        &namespace_decl,
                        Option::<Gc<NodeArray>>::None,
                        Some(modifiers),
                        namespace_decl_as_module_declaration.name(),
                        namespace_decl_as_module_declaration.body.clone(),
                    );

                    let export_default_declaration = self
                        .factory
                        .create_export_assignment(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            Some(false),
                            namespace_decl_as_module_declaration.name(),
                        )
                        .wrap();

                    if is_source_file(&input.parent()) {
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
                let input_as_module_declaration = input.as_module_declaration();
                self.set_needs_declare(false);
                let inner = input_as_module_declaration.body.as_ref();
                if let Some(inner) = inner.filter(|inner| inner.kind() == SyntaxKind::ModuleBlock) {
                    let old_needs_scope_fix = self.needs_scope_fix_marker();
                    let old_has_scope_fix = self.result_has_scope_marker();
                    self.set_result_has_scope_marker(false);
                    self.set_needs_scope_fix_marker(false);
                    let statements = try_visit_nodes(
                        &inner.as_module_block().statements,
                        Some(|node: &Node| self.visit_declaration_statements(node)),
                        Option::<fn(&Node) -> bool>::None,
                        None,
                        None,
                    )?;
                    let mut late_statements =
                        self.transform_and_replace_late_painted_statements(&statements)?;
                    if input.flags().intersects(NodeFlags::Ambient) {
                        self.set_needs_scope_fix_marker(false);
                    }
                    if !is_global_scope_augmentation(input)
                        && !self.has_scope_marker(&late_statements)
                        && !self.result_has_scope_marker()
                    {
                        if self.needs_scope_fix_marker() {
                            late_statements = self.factory.create_node_array(
                                Some({
                                    let mut late_statements = late_statements.to_vec();
                                    late_statements.push(create_empty_exports(&self.factory));
                                    late_statements
                                }),
                                None,
                            );
                        } else {
                            late_statements = visit_nodes(
                                &late_statements,
                                Some(|node: &Node| Some(self.strip_export_modifiers(node).into())),
                                Option::<fn(&Node) -> bool>::None,
                                None,
                                None,
                            );
                        }
                    }
                    let body = self.factory.update_module_block(inner, late_statements);
                    self.set_needs_declare(previous_needs_declare);
                    self.set_needs_scope_fix_marker(old_needs_scope_fix);
                    self.set_result_has_scope_marker(old_has_scope_fix);
                    let mods = self.ensure_modifiers(input);
                    self.transform_top_level_declaration_cleanup(
                        input,
                        previous_enclosing_declaration.as_ref(),
                        can_prodice_diagnostic,
                        &old_diag,
                        previous_needs_declare,
                        Some(&self.factory.update_module_declaration(
                            input,
                            Option::<Gc<NodeArray>>::None,
                            mods,
                            if is_external_module_augmentation(input) {
                                self.rewrite_module_specifier(
                                    input,
                                    input_as_module_declaration.maybe_name().as_deref(),
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
                        inner.as_double_deref(),
                        Some(|node: &Node| self.visit_declaration_statements(node)),
                        Option::<fn(&Node) -> bool>::None,
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?;
                    let id = maybe_get_original_node_id(inner.as_double_deref());
                    let body = self
                        .late_statement_replacement_map_mut()
                        .remove(&id)
                        .flatten();
                    self.transform_top_level_declaration_cleanup(
                        input,
                        previous_enclosing_declaration.as_ref(),
                        can_prodice_diagnostic,
                        &old_diag,
                        previous_needs_declare,
                        Some(&self.factory.update_module_declaration(
                            input,
                            Option::<Gc<NodeArray>>::None,
                            mods,
                            input_as_module_declaration.name(),
                            body.as_ref().map(SingleNodeOrVecNode::as_single_node),
                        )),
                    )
                }
            }
            SyntaxKind::ClassDeclaration => {
                let input_as_class_declaration = input.as_class_declaration();
                self.set_error_name_node(input_as_class_declaration.maybe_name());
                self.set_error_fallback_node(Some(input.node_wrapper()));
                let modifiers = self
                    .factory
                    .create_node_array(self.ensure_modifiers(input), None);
                let type_parameters = self.ensure_type_params(
                    input,
                    input_as_class_declaration
                        .maybe_type_parameters()
                        .as_deref(),
                )?;
                let ctor = get_first_constructor_with_body(input);
                let mut parameter_properties: Option<Vec<Gc<Node /*PropertyDeclaration*/>>> =
                    Default::default();
                if let Some(ref ctor) = ctor {
                    let old_diag = self.get_symbol_accessibility_diagnostic();
                    let ctor_as_constructor_declaration = ctor.as_constructor_declaration();
                    parameter_properties = Some(/*compact(*/ try_flat_map(
                        Some(&ctor_as_constructor_declaration.parameters()),
                        |param: &Gc<Node>, _| -> io::Result<_> {
                            if !has_syntactic_modifier(
                                param,
                                ModifierFlags::ParameterPropertyModifier,
                            ) || self.should_strip_internal(param)
                            {
                                return Ok(vec![]);
                            }
                            self.set_get_symbol_accessibility_diagnostic(
                                create_get_symbol_accessibility_diagnostic_for_node(param),
                            );
                            let param_as_parameter_declaration = param.as_parameter_declaration();
                            Ok(
                                if param_as_parameter_declaration.name().kind()
                                    == SyntaxKind::Identifier
                                {
                                    vec![self.preserve_js_doc(
                                        &self.factory.create_property_declaration(
                                            Option::<Gc<NodeArray>>::None,
                                            self.ensure_modifiers(param),
                                            param_as_parameter_declaration.name(),
                                            param_as_parameter_declaration.maybe_question_token(),
                                            self.ensure_type(
                                                param,
                                                param_as_parameter_declaration
                                                    .maybe_type()
                                                    .as_deref(),
                                                None,
                                            )?,
                                            self.ensure_no_initializer(param)?,
                                        ),
                                        param,
                                    )]
                                } else {
                                    self.walk_binding_pattern(
                                        param,
                                        &param_as_parameter_declaration.name(),
                                    )?
                                    .unwrap_or_default()
                                },
                            )
                        },
                    )? /*)*/);
                    self.set_get_symbol_accessibility_diagnostic(old_diag);
                }

                let has_private_identifier = some(
                    Some(&input_as_class_declaration.members()),
                    Some(|member: &Gc<Node>| {
                        matches!(
                            member.as_named_declaration().maybe_name().as_ref(),
                            Some(member_name) if is_private_identifier(member_name)
                        )
                    }),
                );
                let private_identifier = if has_private_identifier {
                    Some(vec![self.factory.create_property_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        self.factory.create_private_identifier("#private").wrap(),
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
                        Some(&input_as_class_declaration.members()),
                        Some(|node: &Node| self.visit_declaration_subtree(node)),
                        Option::<fn(&Node) -> bool>::None,
                        None,
                        None,
                    )?
                    .map(|node_array| node_array.to_vec()),
                );
                let members = self.factory.create_node_array(member_nodes, None);

                let extends_clause = get_effective_base_type_node(input);
                if let Some(extends_clause) = extends_clause.filter(|extends_clause| {
                    let extends_clause_as_expression_with_type_arguments =
                        extends_clause.as_expression_with_type_arguments();
                    !is_entity_name_expression(
                        &extends_clause_as_expression_with_type_arguments.expression,
                    ) && extends_clause_as_expression_with_type_arguments
                        .expression
                        .kind()
                        != SyntaxKind::NullKeyword
                }) {
                    let old_id = if let Some(input_name) = input_as_class_declaration.maybe_name() {
                        unescape_leading_underscores(&input_name.as_identifier().escaped_text)
                            .to_owned()
                    } else {
                        "default".to_owned()
                    };
                    let new_id = self.factory.create_unique_name(
                        &format!("{old_id}_base"),
                        Some(GeneratedIdentifierFlags::Optimistic),
                    );
                    self.set_get_symbol_accessibility_diagnostic(
                        TransformTopLevelDeclarationGetSymbolAccessibilityDiagnostic::new(
                            &extends_clause,
                            input,
                        ),
                    );
                    let var_decl = self
                        .factory
                        .create_variable_declaration(
                            Some(new_id.clone()),
                            None,
                            self.resolver.create_type_of_expression(
                                &extends_clause
                                    .as_expression_with_type_arguments()
                                    .expression,
                                input,
                                declaration_emit_node_builder_flags(),
                                self.symbol_tracker(),
                            )?,
                            None,
                        )
                        .wrap();
                    let statement = self
                        .factory
                        .create_variable_statement(
                            if self.needs_declare() {
                                Some(vec![self
                                    .factory
                                    .create_modifier(SyntaxKind::DeclareKeyword)
                                    .wrap()])
                            } else {
                                Some(vec![])
                            },
                            self.factory
                                .create_variable_declaration_list(
                                    vec![var_decl],
                                    Some(NodeFlags::Const),
                                )
                                .wrap(),
                        )
                        .wrap();
                    let heritage_clauses = self.factory.create_node_array(
                        try_maybe_map(
                            input_as_class_declaration
                                .maybe_heritage_clauses()
                                .as_deref(),
                            |clause: &Gc<Node>, _| -> io::Result<_> {
                                let clause_as_heritage_clause = clause.as_heritage_clause();
                                if clause_as_heritage_clause.token == SyntaxKind::ExtendsKeyword {
                                    let old_diag = self.get_symbol_accessibility_diagnostic();
                                    self.set_get_symbol_accessibility_diagnostic(
                                        create_get_symbol_accessibility_diagnostic_for_node(
                                            &clause_as_heritage_clause.types[0],
                                        ),
                                    );
                                    let new_clause = self.factory.update_heritage_clause(
                                        clause,
                                        try_map(
                                            &clause_as_heritage_clause.types,
                                            |t: &Gc<Node>, _| -> io::Result<_> {
                                                Ok(self
                                                    .factory
                                                    .update_expression_with_type_arguments(
                                                        t,
                                                        new_id.clone(),
                                                        try_maybe_visit_nodes(
                                                            t.as_expression_with_type_arguments()
                                                                .maybe_type_arguments()
                                                                .as_deref(),
                                                            Some(|node: &Node| {
                                                                self.visit_declaration_subtree(node)
                                                            }),
                                                            Option::<fn(&Node) -> bool>::None,
                                                            None,
                                                            None,
                                                        )?,
                                                    ))
                                            },
                                        )?,
                                    );
                                    self.set_get_symbol_accessibility_diagnostic(old_diag);
                                    return Ok(new_clause);
                                }
                                Ok(self.factory.update_heritage_clause(
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
                                                            &t_as_expression_with_type_arguments
                                                                .expression,
                                                        ) || t_as_expression_with_type_arguments
                                                            .expression
                                                            .kind()
                                                            == SyntaxKind::NullKeyword
                                                    })
                                                    .cloned()
                                                    .collect::<Vec<_>>(),
                                            ),
                                            None,
                                        ),
                                        Some(|node: &Node| self.visit_declaration_subtree(node)),
                                        Option::<fn(&Node) -> bool>::None,
                                        None,
                                        None,
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
                                previous_enclosing_declaration.as_ref(),
                                can_prodice_diagnostic,
                                &old_diag,
                                previous_needs_declare,
                                Some(&self.factory.update_class_declaration(
                                    input,
                                    Option::<Gc<NodeArray>>::None,
                                    Some(modifiers),
                                    input_as_class_declaration.maybe_name(),
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
                    let heritage_clauses = self.transform_heritage_clauses(
                        input_as_class_declaration
                            .maybe_heritage_clauses()
                            .as_deref(),
                    )?;
                    self.transform_top_level_declaration_cleanup(
                        input,
                        previous_enclosing_declaration.as_ref(),
                        can_prodice_diagnostic,
                        &old_diag,
                        previous_needs_declare,
                        Some(&self.factory.update_class_declaration(
                            input,
                            Option::<Gc<NodeArray>>::None,
                            Some(modifiers),
                            input_as_class_declaration.maybe_name(),
                            type_parameters,
                            Some(heritage_clauses),
                            members,
                        )),
                    )
                }
            }
            SyntaxKind::VariableStatement => self.transform_top_level_declaration_cleanup(
                input,
                previous_enclosing_declaration.as_ref(),
                can_prodice_diagnostic,
                &old_diag,
                previous_needs_declare,
                self.transform_variable_statement(input)?.as_deref(),
            ),
            SyntaxKind::EnumDeclaration => {
                let input_as_enum_declaration = input.as_enum_declaration();
                self.transform_top_level_declaration_cleanup(
                    input,
                    previous_enclosing_declaration.as_ref(),
                    can_prodice_diagnostic,
                    &old_diag,
                    previous_needs_declare,
                    Some(
                        &self.factory.update_enum_declaration(
                            input,
                            Option::<Gc<NodeArray>>::None,
                            Some(
                                self.factory
                                    .create_node_array(self.ensure_modifiers(input), None),
                            ),
                            input_as_enum_declaration.name(),
                            Some(self.factory.create_node_array(
                                Some(try_map_defined(
                                    Some(&input_as_enum_declaration.members),
                                    |m: &Gc<Node>, _| -> io::Result<_> {
                                        if self.should_strip_internal(m) {
                                            return Ok(None);
                                        }
                                        let const_value = self.resolver.get_constant_value(m)?;
                                        Ok(Some(self.preserve_js_doc(
                                            &self.factory.update_enum_member(
                                                m,
                                                m.as_enum_member().name(),
                                                const_value.map(|const_value| {
                                                    match const_value {
                                                        StringOrNumber::String(const_value) => self
                                                            .factory
                                                            .create_string_literal(
                                                                const_value,
                                                                None,
                                                                None,
                                                            )
                                                            .wrap(),
                                                        StringOrNumber::Number(const_value) => self
                                                            .factory
                                                            .create_numeric_literal(
                                                                const_value,
                                                                None,
                                                            )
                                                            .wrap(),
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
                    input.kind()
                )),
            ),
        })
    }

    pub(super) fn walk_binding_pattern(
        &self,
        param: &Node,
        pattern: &Node, /*BindingPattern*/
    ) -> io::Result<Option<Vec<Gc<Node>>>> {
        let mut elems: Option<Vec<Gc<Node /*PropertyDeclaration*/>>> = Default::default();
        for elem in &pattern.as_has_elements().elements() {
            let elem_as_binding_element = elem.as_binding_element();
            if is_omitted_expression(elem) {
                continue;
            }
            if is_binding_pattern(elem_as_binding_element.maybe_name()) {
                elems = maybe_concatenate(
                    elems,
                    self.walk_binding_pattern(param, &elem_as_binding_element.name())?,
                );
            }
            elems
                .get_or_insert_with(|| vec![])
                .push(self.factory.create_property_declaration(
                    Option::<Gc<NodeArray>>::None,
                    self.ensure_modifiers(param),
                    elem_as_binding_element.name(),
                    None,
                    self.ensure_type(elem, None, None)?,
                    None,
                ));
        }
        Ok(elems)
    }

    pub(super) fn transform_top_level_declaration_cleanup(
        &self,
        input: &Node,
        previous_enclosing_declaration: Option<&Gc<Node>>,
        can_prodice_diagnostic: bool,
        old_diag: &GetSymbolAccessibilityDiagnostic,
        previous_needs_declare: bool,
        node: Option<&Node>,
    ) -> VisitResult {
        if self.is_enclosing_declaration(input) {
            self.set_enclosing_declaration(previous_enclosing_declaration.cloned());
        }
        if can_prodice_diagnostic {
            self.set_get_symbol_accessibility_diagnostic(old_diag.clone());
        }
        if input.kind() == SyntaxKind::ModuleDeclaration {
            self.set_needs_declare(previous_needs_declare);
        }
        if matches!(
            node,
            Some(node) if ptr::eq(node, input)
        ) {
            return node.map(Node::node_wrapper).map(Into::into);
        }
        self.set_error_fallback_node(None);
        self.set_error_name_node(None);
        node.map(|node| {
            set_original_node(
                self.preserve_js_doc(node, input),
                Some(input.node_wrapper()),
            )
            .into()
        })
    }
}

#[derive(Trace, Finalize)]
struct VisitDeclarationStatementsGetSymbolAccessibilityDiagnostic {
    input: Gc<Node>,
}

impl VisitDeclarationStatementsGetSymbolAccessibilityDiagnostic {
    fn new(input: &Node) -> GetSymbolAccessibilityDiagnostic {
        Gc::new(Box::new(Self {
            input: input.node_wrapper(),
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface
    for VisitDeclarationStatementsGetSymbolAccessibilityDiagnostic
{
    fn call(
        &self,
        _symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        Some(Gc::new(SymbolAccessibilityDiagnostic {
            diagnostic_message:
                &Diagnostics::Default_export_of_the_module_has_or_is_using_private_name_0,
            error_node: self.input.clone(),
            type_name: None,
        }))
    }
}

#[derive(Trace, Finalize)]
struct TransformTopLevelDeclarationGetSymbolAccessibilityDiagnostic {
    extends_clause: Gc<Node>,
    input: Gc<Node>,
}

impl TransformTopLevelDeclarationGetSymbolAccessibilityDiagnostic {
    fn new(extends_clause: &Node, input: &Node) -> GetSymbolAccessibilityDiagnostic {
        Gc::new(Box::new(Self {
            extends_clause: extends_clause.node_wrapper(),
            input: input.node_wrapper(),
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface
    for TransformTopLevelDeclarationGetSymbolAccessibilityDiagnostic
{
    fn call(
        &self,
        _symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        Some(Gc::new(SymbolAccessibilityDiagnostic {
            diagnostic_message:
                &Diagnostics::extends_clause_of_exported_class_0_has_or_is_using_private_name_1,
            error_node: self.extends_clause.clone(),
            type_name: self.input.as_class_declaration().maybe_name(),
        }))
    }
}
