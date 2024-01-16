use std::{borrow::Borrow, io, ptr};

use gc::Gc;
use id_arena::Id;

use super::{declaration_emit_node_builder_flags, is_processed_component, TransformDeclarations};
use crate::{
    can_produce_diagnostics, create_get_symbol_accessibility_diagnostic_for_node,
    get_comment_range, get_external_module_import_equals_declaration_expression,
    get_external_module_name_from_declaration, get_line_and_character_of_position,
    get_original_node_id, get_parse_tree_node, get_set_accessor_value_parameter,
    get_this_parameter, has_dynamic_name, has_effective_modifier, has_jsdoc_nodes,
    is_binding_pattern, is_class_declaration, is_declaration, is_entity_name,
    is_entity_name_expression, is_external_module, is_external_module_indicator,
    is_function_declaration, is_function_like, is_index_signature_declaration,
    is_interface_declaration, is_late_visibility_painted_statement, is_literal_import_type_node,
    is_mapped_type_node, is_method_declaration, is_method_signature, is_module_declaration,
    is_omitted_expression, is_private_identifier, is_semicolon_class_element,
    is_set_accessor_declaration, is_source_file, is_string_literal_like, is_tuple_type_node,
    is_type_alias_declaration, is_type_node, is_type_query_node, length, map_defined,
    needs_scope_marker, return_ok_default_if_none, set_emit_flags, some,
    try_maybe_map, try_maybe_visit_each_child, try_maybe_visit_node, try_maybe_visit_nodes,
    try_visit_each_child, try_visit_node, visit_nodes, Debug_, EmitFlags,
    FunctionLikeDeclarationInterface, GetOrInsertDefault, GetSymbolAccessibilityDiagnostic,
    HasQuestionTokenInterface, HasTypeArgumentsInterface, HasTypeInterface,
    HasTypeParametersInterface, InArena, ModifierFlags, NamedDeclarationInterface, Node, NodeArray,
    NodeInterface, NonEmpty, OptionTry, ReadonlyTextRange, SignatureDeclarationInterface,
    SymbolInterface, SyntaxKind, VisitResult,
    set_comment_range,
};

impl TransformDeclarations {
    pub(super) fn ensure_type(
        &self,
        node: Id<Node>, /*HasInferredType*/
        type_: Option<Id<Node> /*TypeNode*/>,
        ignore_private: Option<bool>,
    ) -> io::Result<Option<Id<Node /*TypeNode*/>>> {
        if ignore_private != Some(true) && has_effective_modifier(node, ModifierFlags::Private, self) {
            return Ok(None);
        }
        if self.should_print_with_initializer(node)? {
            return Ok(None);
        }
        let should_use_resolver_type = node.kind() == SyntaxKind::Parameter
            && (self.resolver.is_required_initialized_parameter(node)?
                || self
                    .resolver
                    .is_optional_uninitialized_parameter_property(node)?);
        if let Some(type_) = type_ {
            if !should_use_resolver_type {
                return try_maybe_visit_node(
                    Some(type_),
                    Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                    Option::<fn(Id<Node>) -> bool>::None,
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                );
            }
        }
        if get_parse_tree_node(Some(node), Option::<fn(Id<Node>) -> bool>::None, self).is_none() {
            return Ok(if let Some(type_) = type_ {
                try_maybe_visit_node(
                    Some(type_),
                    Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                    Option::<fn(Id<Node>) -> bool>::None,
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?
            } else {
                Some(
                    self.factory
                        .create_keyword_type_node(SyntaxKind::AnyKeyword),
                )
            });
        }
        if node.kind() == SyntaxKind::SetAccessor {
            return Ok(Some(
                self.factory
                    .create_keyword_type_node(SyntaxKind::AnyKeyword),
            ));
        }
        self.set_error_name_node(node.as_named_declaration().maybe_name());
        let mut old_diag: Option<GetSymbolAccessibilityDiagnostic> = None;
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            old_diag = Some(self.get_symbol_accessibility_diagnostic());
            self.set_get_symbol_accessibility_diagnostic(
                create_get_symbol_accessibility_diagnostic_for_node(node),
            );
        }
        if matches!(
            node.kind(),
            SyntaxKind::VariableDeclaration | SyntaxKind::BindingElement
        ) {
            return Ok(Some(self.ensure_type_cleanup(
                old_diag,
                self.resolver.create_type_of_declaration(
                    node,
                    &self.enclosing_declaration(),
                    declaration_emit_node_builder_flags(),
                    self.symbol_tracker(),
                    None,
                )?,
            )));
        }
        if matches!(
            node.kind(),
            SyntaxKind::Parameter | SyntaxKind::PropertyDeclaration | SyntaxKind::PropertySignature
        ) {
            if node.as_has_initializer().maybe_initializer().is_none() {
                return Ok(Some(self.ensure_type_cleanup(
                    old_diag,
                    self.resolver.create_type_of_declaration(
                        node,
                        &self.enclosing_declaration(),
                        declaration_emit_node_builder_flags(),
                        self.symbol_tracker(),
                        Some(should_use_resolver_type),
                    )?,
                )));
            }
            return Ok(Some(
                self.ensure_type_cleanup(
                    old_diag,
                    self.resolver
                        .create_type_of_declaration(
                            node,
                            &self.enclosing_declaration(),
                            declaration_emit_node_builder_flags(),
                            self.symbol_tracker(),
                            None,
                        )?
                        .try_or_else(|| {
                            self.resolver.create_type_of_expression(
                                &node.as_has_initializer().maybe_initializer().unwrap(),
                                &self.enclosing_declaration(),
                                declaration_emit_node_builder_flags(),
                                self.symbol_tracker(),
                            )
                        })?,
                ),
            ));
        }
        Ok(Some(self.ensure_type_cleanup(
            old_diag,
            self.resolver.create_return_type_of_signature_declaration(
                node,
                &self.enclosing_declaration(),
                declaration_emit_node_builder_flags(),
                self.symbol_tracker(),
            )?,
        )))
    }

    pub(super) fn ensure_type_cleanup(
        &self,
        old_diag: Option<GetSymbolAccessibilityDiagnostic>,
        return_value: Option<Id<Node> /*TypeNode*/>,
    ) -> Id<Node> {
        self.set_error_name_node(None);
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(old_diag.unwrap())
        }
        return_value
            .map(|return_value| return_value.borrow().node_wrapper())
            .unwrap_or_else(|| {
                self.factory
                    .create_keyword_type_node(SyntaxKind::AnyKeyword)
            })
    }

    pub(super) fn is_declaration_and_not_visible(
        &self,
        node: Id<Node>, /*NamedDeclaration*/
    ) -> bool {
        let ref node =
            get_parse_tree_node(Some(node), Option::<fn(Id<Node>) -> bool>::None, self).unwrap();
        match node.kind() {
            SyntaxKind::FunctionDeclaration
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::EnumDeclaration => !self.resolver.is_declaration_visible(node),
            SyntaxKind::VariableDeclaration => !self.get_binding_name_visible(node),
            SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::ImportDeclaration
            | SyntaxKind::ExportDeclaration
            | SyntaxKind::ExportAssignment => false,
            SyntaxKind::ClassStaticBlockDeclaration => true,
            _ => false,
        }
    }

    pub(super) fn should_emit_function_properties(
        &self,
        input: Id<Node>, /*FunctionDeclaration*/
    ) -> bool {
        if input.as_function_declaration().maybe_body().is_some() {
            return true;
        }

        let overload_signatures: Option<Vec<_>> =
            input.symbol().ref_(self).maybe_declarations().as_ref().map(
                |input_symbol_declarations| {
                    input_symbol_declarations
                        .into_iter()
                        .filter(|decl: &&Id<Node>| {
                            is_function_declaration(decl)
                                && decl.as_function_declaration().maybe_body().is_none()
                        })
                        .cloned()
                        .collect()
                },
            );
        match overload_signatures {
            None => true,
            Some(overload_signatures) if overload_signatures.is_empty() => true,
            Some(overload_signatures) => {
                overload_signatures
                    .iter()
                    .position(|overload_signature: &Id<Node>| ptr::eq(&**overload_signature, input))
                    == Some(overload_signatures.len() - 1)
            }
        }
    }

    pub(super) fn get_binding_name_visible(
        &self,
        elem: Id<Node>, /*BindingElement | VariableDeclaration | OmittedExpression*/
    ) -> bool {
        if is_omitted_expression(elem) {
            return false;
        }
        if let Some(elem_name) = elem
            .as_named_declaration()
            .maybe_name()
            .filter(|elem_name| is_binding_pattern(Some(&**elem_name)))
            .as_ref()
        {
            some(
                Some(&elem_name.as_has_elements().elements()),
                Some(|element: &Id<Node>| self.get_binding_name_visible(element)),
            )
        } else {
            self.resolver.is_declaration_visible(elem)
        }
    }

    pub(super) fn update_params_list(
        &self,
        node: Id<Node>,
        params: Option<&NodeArray /*<ParameterDeclaration>*/>,
        modifier_mask: Option<ModifierFlags>,
    ) -> io::Result<Option<Gc<NodeArray> /*<ParameterDeclaration>*/>> {
        if has_effective_modifier(node, ModifierFlags::Private, self) {
            return Ok(None);
        }
        let new_params = return_ok_default_if_none!(try_maybe_map(params, |p: &Id<Node>, _| {
            self.ensure_parameter(p, modifier_mask, None)
        })
        .transpose()?);
        Ok(Some(self.factory.create_node_array(
            Some(new_params),
            Some(params.unwrap().has_trailing_comma),
        )))
    }

    pub(super) fn update_accessor_params_list(
        &self,
        input: Id<Node>, /*AccessorDeclaration*/
        is_private: bool,
    ) -> io::Result<Gc<NodeArray>> {
        let mut new_params: Vec<Id<Node /*ParameterDeclaration*/>> = Default::default();
        if !is_private {
            let this_parameter = get_this_parameter(input, self);
            if let Some(this_parameter) = this_parameter {
                new_params.push(self.ensure_parameter(&this_parameter, None, None)?);
            }
        }
        if is_set_accessor_declaration(input) {
            let mut new_value_parameter: Option<Id<Node /*ParameterDeclaration*/>> = None;
            if !is_private {
                let value_parameter = get_set_accessor_value_parameter(input, self);
                if let Some(value_parameter) = value_parameter {
                    let accessor_type = self.get_type_annotation_from_all_accessor_declarations(
                        input,
                        &self.resolver.get_all_accessor_declarations(input)?,
                    );
                    new_value_parameter = Some(self.ensure_parameter(
                        &value_parameter,
                        None,
                        accessor_type.as_deref(),
                    )?);
                }
            }
            if new_value_parameter.is_none() {
                new_value_parameter = Some(self.factory.create_parameter_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    None,
                    Some("value"),
                    None,
                    None,
                    None,
                ));
            }
            let new_value_parameter = new_value_parameter.unwrap();
            new_params.push(new_value_parameter);
        }
        Ok(self.factory.create_node_array(Some(new_params), None))
    }

    pub(super) fn ensure_type_params(
        &self,
        node: Id<Node>,
        params: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
    ) -> io::Result<Option<Gc<NodeArray>>> {
        Ok(if has_effective_modifier(node, ModifierFlags::Private, self) {
            None
        } else {
            try_maybe_visit_nodes(
                params,
                Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                Option::<fn(Id<Node>) -> bool>::None,
                None,
                None,
            )?
        })
    }

    pub(super) fn is_enclosing_declaration(&self, node: Id<Node>) -> bool {
        is_source_file(node)
            || is_type_alias_declaration(node)
            || is_module_declaration(node)
            || is_class_declaration(node)
            || is_interface_declaration(node)
            || is_function_like(Some(node))
            || is_index_signature_declaration(node)
            || is_mapped_type_node(node)
    }

    pub(super) fn check_entity_name_visibility(
        &self,
        entity_name: Id<Node>, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: Id<Node>,
    ) -> io::Result<()> {
        let visibility_result = self
            .resolver
            .is_entity_name_visible(entity_name, enclosing_declaration)?;
        self.handle_symbol_accessibility_error(
            &visibility_result.into_symbol_accessibility_result(),
        );
        self.record_type_reference_directives_if_necessary(
            self.resolver
                .get_type_reference_directives_for_entity_name(entity_name)?
                .as_deref(),
        );

        Ok(())
    }

    pub(super) fn preserve_js_doc(&self, updated: Id<Node>, original: Id<Node>) -> Id<Node> {
        if has_jsdoc_nodes(updated) && has_jsdoc_nodes(original) {
            updated.set_js_doc(original.maybe_js_doc());
        }
        set_comment_range(
            updated,
            &get_comment_range(original).into_readonly_text_range(),
            self,
        )
    }

    pub(super) fn rewrite_module_specifier(
        &self,
        parent: Id<Node>, /*ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration | ImportTypeNode*/
        input: Option<Id<Node>>,
    ) -> io::Result<Option<Id<Node>>> {
        let input = return_ok_default_if_none!(input);
        self.set_result_has_external_module_indicator(
            self.result_has_external_module_indicator()
                || !matches!(
                    parent.kind(),
                    SyntaxKind::ModuleDeclaration | SyntaxKind::ImportType
                ),
        );
        if is_string_literal_like(input) {
            if self.is_bundled_emit() {
                let new_name = get_external_module_name_from_declaration(
                    &**self.context.get_emit_host(),
                    &**self.resolver,
                    parent,
                    self,
                )?;
                if let Some(new_name) = new_name.non_empty() {
                    return Ok(Some(
                        self.factory.create_string_literal(new_name, None, None),
                    ));
                }
            } else {
                let symbol = self
                    .resolver
                    .get_symbol_of_external_module_specifier(input)?;
                if let Some(symbol) = symbol {
                    self.maybe_exported_modules_from_declaration_emit_mut()
                        .get_or_insert_default_()
                        .push(symbol);
                }
            }
        }
        Ok(Some(input.node_wrapper()))
    }

    pub(super) fn transform_import_equals_declaration(
        &self,
        decl: Id<Node>, /*ImportEqualsDeclaration*/
    ) -> io::Result<Option<Id<Node>>> {
        if !self.resolver.is_declaration_visible(decl) {
            return Ok(None);
        }
        let decl_as_import_equals_declaration = decl.as_import_equals_declaration();
        Ok(
            if decl_as_import_equals_declaration.module_reference.kind()
                == SyntaxKind::ExternalModuleReference
            {
                let specifier = get_external_module_import_equals_declaration_expression(decl);
                Some(
                    self.factory.update_import_equals_declaration(
                        decl,
                        Option::<Gc<NodeArray>>::None,
                        decl.maybe_modifiers().clone(),
                        decl_as_import_equals_declaration.is_type_only,
                        decl_as_import_equals_declaration.name(),
                        self.factory.update_external_module_reference(
                            &decl_as_import_equals_declaration.module_reference,
                            self.rewrite_module_specifier(decl, Some(&specifier))?
                                .unwrap(),
                        ),
                    ),
                )
            } else {
                let old_diag = self.get_symbol_accessibility_diagnostic();
                self.set_get_symbol_accessibility_diagnostic(
                    create_get_symbol_accessibility_diagnostic_for_node(decl),
                );
                self.check_entity_name_visibility(
                    &decl_as_import_equals_declaration.module_reference,
                    &self.enclosing_declaration(),
                )?;
                self.set_get_symbol_accessibility_diagnostic(old_diag);
                Some(decl.node_wrapper())
            },
        )
    }

    pub(super) fn transform_import_declaration(
        &self,
        decl: Id<Node>, /*ImportDeclaration*/
    ) -> io::Result<Option<Id<Node>>> {
        let decl_as_import_declaration = decl.as_import_declaration();
        if decl_as_import_declaration.import_clause.is_none() {
            return Ok(Some(
                self.factory.update_import_declaration(
                    decl,
                    Option::<Gc<NodeArray>>::None,
                    decl.maybe_modifiers().clone(),
                    decl_as_import_declaration.import_clause.clone(),
                    self.rewrite_module_specifier(
                        decl,
                        Some(&decl_as_import_declaration.module_specifier),
                    )?
                    .unwrap(),
                    None,
                ),
            ));
        }
        let decl_import_clause = decl_as_import_declaration.import_clause.as_ref().unwrap();
        let decl_import_clause_as_import_clause = decl_import_clause.as_import_clause();
        let visible_default_binding = /*decl.importClause &&*/
            decl_import_clause_as_import_clause.name.clone().filter(|_| {
                self.resolver.is_declaration_visible(decl_import_clause)
            });
        if decl_import_clause_as_import_clause.named_bindings.is_none() {
            return visible_default_binding.try_map(|visible_default_binding| -> io::Result<_> {
                Ok(self.factory.update_import_declaration(
                    decl,
                    Option::<Gc<NodeArray>>::None,
                    decl.maybe_modifiers().clone(),
                    Some(self.factory.update_import_clause(
                        decl_import_clause,
                        decl_import_clause_as_import_clause.is_type_only,
                        Some(visible_default_binding),
                        None,
                    )),
                    self.rewrite_module_specifier(
                        decl,
                        Some(&decl_as_import_declaration.module_specifier),
                    )?
                    .unwrap(),
                    None,
                ))
            });
        }
        let decl_import_clause_named_bindings = decl_import_clause_as_import_clause
            .named_bindings
            .as_ref()
            .unwrap();
        if decl_import_clause_named_bindings.kind() == SyntaxKind::NamespaceImport {
            let named_bindings = if self
                .resolver
                .is_declaration_visible(decl_import_clause_named_bindings)
            {
                Some(decl_import_clause_named_bindings)
            } else {
                None
            };
            return Ok(
                if visible_default_binding.is_some() || named_bindings.is_some() {
                    Some(
                        self.factory.update_import_declaration(
                            decl,
                            Option::<Gc<NodeArray>>::None,
                            decl.maybe_modifiers().clone(),
                            Some(self.factory.update_import_clause(
                                decl_import_clause,
                                decl_import_clause_as_import_clause.is_type_only,
                                visible_default_binding,
                                named_bindings.cloned(),
                            )),
                            self.rewrite_module_specifier(
                                decl,
                                Some(&decl_as_import_declaration.module_specifier),
                            )?
                            .unwrap(),
                            None,
                        ),
                    )
                } else {
                    None
                },
            );
        }
        let binding_list = map_defined(
            Some(
                &decl_import_clause_named_bindings
                    .as_named_imports()
                    .elements,
            ),
            |b: &Id<Node>, _| {
                if self.resolver.is_declaration_visible(b) {
                    Some(b.clone())
                } else {
                    None
                }
            },
        );
        if
        /*bindingList &&*/
        !binding_list.is_empty() || visible_default_binding.is_some() {
            return Ok(Some(
                self.factory.update_import_declaration(
                    decl,
                    Option::<Gc<NodeArray>>::None,
                    decl.maybe_modifiers().clone(),
                    Some(self.factory.update_import_clause(
                        decl_import_clause,
                        decl_import_clause_as_import_clause.is_type_only,
                        visible_default_binding,
                        if
                        /*bindingList &&*/
                        !binding_list.is_empty() {
                            Some(self.factory.update_named_imports(
                                decl_import_clause_named_bindings,
                                binding_list,
                            ))
                        } else {
                            None
                        },
                    )),
                    self.rewrite_module_specifier(
                        decl,
                        Some(&decl_as_import_declaration.module_specifier),
                    )?
                    .unwrap(),
                    None,
                ),
            ));
        }
        if self.resolver.is_import_required_by_augmentation(decl)? {
            return Ok(Some(
                self.factory.update_import_declaration(
                    decl,
                    Option::<Gc<NodeArray>>::None,
                    decl.maybe_modifiers().clone(),
                    None,
                    self.rewrite_module_specifier(
                        decl,
                        Some(&decl_as_import_declaration.module_specifier),
                    )?
                    .unwrap(),
                    None,
                ),
            ));
        }
        Ok(None)
    }

    pub(super) fn transform_and_replace_late_painted_statements(
        &self,
        statements: &NodeArray, /*<Statement>*/
    ) -> io::Result<Gc<NodeArray>> /*<Statement>*/ {
        while length(self.maybe_late_marked_statements().as_deref()) > 0 {
            let ref i = self.late_marked_statements_mut().remove(0);
            if !is_late_visibility_painted_statement(i) {
                /*return*/
                Debug_.fail(Some(
                    &format!(
                        "Late replaced statement was found which is not handled by the declaration transformer!: {:?}",
                        i.kind()
                    )
                ));
            }
            let prior_needs_declare = self.needs_declare();
            self.set_needs_declare(matches!(
                i.maybe_parent().as_ref(),
                Some(i_parent) if is_source_file(i_parent) && !(
                    is_external_module(i_parent) &&
                    self.is_bundled_emit()
                )
            ));
            let result = self.transform_top_level_declaration(i)?;
            self.set_needs_declare(prior_needs_declare);
            self.late_statement_replacement_map_mut()
                .insert(get_original_node_id(i), result);
        }

        Ok(visit_nodes(
            statements,
            Some(|node: Id<Node>| self.visit_late_visibility_marked_statements(node)),
            Option::<fn(Id<Node>) -> bool>::None,
            None,
            None,
        ))
    }

    pub(super) fn visit_late_visibility_marked_statements(
        &self,
        statement: Id<Node>, /*Statement*/
    ) -> VisitResult {
        if is_late_visibility_painted_statement(statement) {
            let key = get_original_node_id(statement);
            if self.late_statement_replacement_map().contains_key(&key) {
                let result = {
                    let value = self
                        .late_statement_replacement_map()
                        .get(&key)
                        .cloned()
                        .unwrap();
                    value
                };
                self.late_statement_replacement_map_mut().remove(&key);
                if let Some(result) = result.as_ref() {
                    if result
                        .into_iter()
                        .any(|&node: &Id<Node>| needs_scope_marker(node, self))
                    {
                        self.set_needs_scope_fix_marker(true);
                    }
                    if is_source_file(&statement.parent())
                        && result
                            .into_iter()
                            .any(|&node: &Id<Node>| is_external_module_indicator(node, self))
                    {
                        self.set_result_has_external_module_indicator(true);
                    }
                }
                return result;
            }
        }
        Some(statement.node_wrapper().into())
    }

    pub(super) fn visit_declaration_subtree(&self, input: Id<Node>) -> io::Result<VisitResult> /*<Node>*/
    {
        if self.should_strip_internal(input) {
            return Ok(None);
        }
        if is_declaration(input, self) {
            if self.is_declaration_and_not_visible(input) {
                return Ok(None);
            }
            if has_dynamic_name(input, self)
                && !self.resolver.is_late_bound(
                    &get_parse_tree_node(Some(input), Option::<fn(Id<Node>) -> bool>::None, self)
                        .unwrap(),
                )?
            {
                return Ok(None);
            }
        }

        if is_function_like(Some(input))
            && self.resolver.is_implementation_of_overload(input)? == Some(true)
        {
            return Ok(None);
        }

        if is_semicolon_class_element(input) {
            return Ok(None);
        }

        let mut previous_enclosing_declaration: Option<Id<Node>> = Default::default();
        if self.is_enclosing_declaration(input) {
            previous_enclosing_declaration = self.maybe_enclosing_declaration();
            self.set_enclosing_declaration(Some(input.node_wrapper()));
        }
        let old_diag = self.get_symbol_accessibility_diagnostic();

        let can_produce_diagnostic = can_produce_diagnostics(input);
        let old_within_object_literal_type = self.maybe_suppress_new_diagnostic_contexts();
        let mut should_enter_suppress_new_diagnostics_context_context = matches!(
            input.kind(),
            SyntaxKind::TypeLiteral | SyntaxKind::MappedType
        ) && input.parent().kind()
            != SyntaxKind::TypeAliasDeclaration;

        if is_method_declaration(input) || is_method_signature(input) {
            if has_effective_modifier(input, ModifierFlags::Private, self) {
                if matches!(
                    input.maybe_symbol(),
                    Some(input_symbol) if matches!(
                        input_symbol.ref_(self).maybe_declarations().as_ref(),
                        Some(input_symbol_declarations) if !ptr::eq(
                            &*input_symbol_declarations[0],
                            input
                        )
                    )
                ) {
                    return Ok(None);
                }
                return self.visit_declaration_subtree_cleanup(
                    input,
                    can_produce_diagnostic,
                    previous_enclosing_declaration.as_ref(),
                    &old_diag,
                    should_enter_suppress_new_diagnostics_context_context,
                    old_within_object_literal_type,
                    Some(&self.factory.create_property_declaration(
                        Option::<Gc<NodeArray>>::None,
                        self.ensure_modifiers(input),
                        input.as_named_declaration().name(),
                        None,
                        None,
                        None,
                    )),
                );
            }
        }

        if can_produce_diagnostic && self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(
                create_get_symbol_accessibility_diagnostic_for_node(input),
            );
        }

        if is_type_query_node(input) {
            self.check_entity_name_visibility(
                &input.as_type_query_node().expr_name,
                &self.enclosing_declaration(),
            )?;
        }

        if should_enter_suppress_new_diagnostics_context_context {
            self.set_suppress_new_diagnostic_contexts(Some(true));
        }

        if is_processed_component(input) {
            return Ok(match input.kind() {
                SyntaxKind::ExpressionWithTypeArguments => {
                    let input_as_expression_with_type_arguments =
                        input.as_expression_with_type_arguments();
                    if is_entity_name(&input_as_expression_with_type_arguments.expression)
                        || is_entity_name_expression(
                            input_as_expression_with_type_arguments.expression,
                            self,
                        )
                    {
                        self.check_entity_name_visibility(
                            &input_as_expression_with_type_arguments.expression,
                            &self.enclosing_declaration(),
                        )?;
                    }
                    let ref node = try_visit_each_child(
                        input,
                        |node: Id<Node>| self.visit_declaration_subtree(node),
                        &**self.context,
                    )?;
                    let node_as_expression_with_type_arguments =
                        node.as_expression_with_type_arguments();
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(
                            &self.factory.update_expression_with_type_arguments(
                                node,
                                node_as_expression_with_type_arguments.expression.clone(),
                                node_as_expression_with_type_arguments
                                    .maybe_type_arguments()
                                    .clone(),
                            ),
                        ),
                    )?
                }
                SyntaxKind::TypeReference => {
                    self.check_entity_name_visibility(
                        &input.as_type_reference_node().type_name,
                        &self.enclosing_declaration(),
                    )?;
                    let ref node = try_visit_each_child(
                        input,
                        |node: Id<Node>| self.visit_declaration_subtree(node),
                        &**self.context,
                    )?;
                    let node_as_type_reference_node = node.as_type_reference_node();
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(&self.factory.update_type_reference_node(
                            node,
                            node_as_type_reference_node.type_name.clone(),
                            node_as_type_reference_node.maybe_type_arguments(),
                        )),
                    )?
                }
                SyntaxKind::ConstructSignature => {
                    let input_as_construct_signature_declaration =
                        input.as_construct_signature_declaration();
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(
                            &self.factory.update_construct_signature(
                                input,
                                self.ensure_type_params(
                                    input,
                                    input_as_construct_signature_declaration
                                        .maybe_type_parameters()
                                        .as_deref(),
                                )?,
                                self.update_params_list(
                                    input,
                                    Some(&input_as_construct_signature_declaration.parameters()),
                                    None,
                                )?
                                .unwrap(),
                                self.ensure_type(
                                    input,
                                    input_as_construct_signature_declaration
                                        .maybe_type()
                                        .as_deref(),
                                    None,
                                )?,
                            ),
                        ),
                    )?
                }
                SyntaxKind::Constructor => {
                    let input_as_constructor_declaration = input.as_constructor_declaration();
                    let ctor = self.factory.create_constructor_declaration(
                        Option::<Gc<NodeArray>>::None,
                        self.ensure_modifiers(input),
                        self.update_params_list(
                            input,
                            Some(&input_as_constructor_declaration.parameters()),
                            Some(ModifierFlags::None),
                        )?,
                        None,
                    );
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(&ctor),
                    )?
                }
                SyntaxKind::MethodDeclaration => {
                    let input_as_method_declaration = input.as_method_declaration();
                    if is_private_identifier(&input_as_method_declaration.name()) {
                        return self.visit_declaration_subtree_cleanup(
                            input,
                            can_produce_diagnostic,
                            previous_enclosing_declaration.as_ref(),
                            &old_diag,
                            should_enter_suppress_new_diagnostics_context_context,
                            old_within_object_literal_type,
                            None,
                        );
                    }
                    let sig = self.factory.create_method_declaration(
                        Option::<Gc<NodeArray>>::None,
                        self.ensure_modifiers(input),
                        None,
                        input_as_method_declaration.name(),
                        input_as_method_declaration.maybe_question_token(),
                        self.ensure_type_params(
                            input,
                            input_as_method_declaration
                                .maybe_type_parameters()
                                .as_deref(),
                        )?,
                        self.update_params_list(
                            input,
                            Some(&input_as_method_declaration.parameters()),
                            None,
                        )?
                        .unwrap(),
                        self.ensure_type(
                            input,
                            input_as_method_declaration.maybe_type().as_deref(),
                            None,
                        )?,
                        None,
                    );
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(&sig),
                    )?
                }
                SyntaxKind::GetAccessor => {
                    let input_as_get_accessor_declaration = input.as_get_accessor_declaration();
                    if is_private_identifier(&input_as_get_accessor_declaration.name()) {
                        return self.visit_declaration_subtree_cleanup(
                            input,
                            can_produce_diagnostic,
                            previous_enclosing_declaration.as_ref(),
                            &old_diag,
                            should_enter_suppress_new_diagnostics_context_context,
                            old_within_object_literal_type,
                            None,
                        );
                    }
                    let accessor_type = self.get_type_annotation_from_all_accessor_declarations(
                        input,
                        &self.resolver.get_all_accessor_declarations(input)?,
                    );
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(&self.factory.update_get_accessor_declaration(
                            input,
                            Option::<Gc<NodeArray>>::None,
                            self.ensure_modifiers(input),
                            input_as_get_accessor_declaration.name(),
                            self.update_accessor_params_list(
                                input,
                                has_effective_modifier(input, ModifierFlags::Private, self),
                            )?,
                            self.ensure_type(input, accessor_type.as_deref(), None)?,
                            None,
                        )),
                    )?
                }
                SyntaxKind::SetAccessor => {
                    let input_as_set_accessor_declaration = input.as_set_accessor_declaration();
                    if is_private_identifier(&input_as_set_accessor_declaration.name()) {
                        return self.visit_declaration_subtree_cleanup(
                            input,
                            can_produce_diagnostic,
                            previous_enclosing_declaration.as_ref(),
                            &old_diag,
                            should_enter_suppress_new_diagnostics_context_context,
                            old_within_object_literal_type,
                            None,
                        );
                    }
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(&self.factory.update_set_accessor_declaration(
                            input,
                            Option::<Gc<NodeArray>>::None,
                            self.ensure_modifiers(input),
                            input_as_set_accessor_declaration.name(),
                            self.update_accessor_params_list(
                                input,
                                has_effective_modifier(input, ModifierFlags::Private, self),
                            )?,
                            None,
                        )),
                    )?
                }
                SyntaxKind::PropertyDeclaration => {
                    let input_as_property_declaration = input.as_property_declaration();
                    if is_private_identifier(&input_as_property_declaration.name()) {
                        return self.visit_declaration_subtree_cleanup(
                            input,
                            can_produce_diagnostic,
                            previous_enclosing_declaration.as_ref(),
                            &old_diag,
                            should_enter_suppress_new_diagnostics_context_context,
                            old_within_object_literal_type,
                            None,
                        );
                    }
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(&self.factory.update_property_declaration(
                            input,
                            Option::<Gc<NodeArray>>::None,
                            self.ensure_modifiers(input),
                            input_as_property_declaration.name(),
                            input_as_property_declaration.maybe_question_token(),
                            self.ensure_type(
                                input,
                                input_as_property_declaration.maybe_type().as_deref(),
                                None,
                            )?,
                            self.ensure_no_initializer(input)?,
                        )),
                    )?
                }
                SyntaxKind::PropertySignature => {
                    let input_as_property_signature = input.as_property_signature();
                    if is_private_identifier(&input_as_property_signature.name()) {
                        return self.visit_declaration_subtree_cleanup(
                            input,
                            can_produce_diagnostic,
                            previous_enclosing_declaration.as_ref(),
                            &old_diag,
                            should_enter_suppress_new_diagnostics_context_context,
                            old_within_object_literal_type,
                            None,
                        );
                    }
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(&self.factory.update_property_signature(
                            input,
                            self.ensure_modifiers(input),
                            input_as_property_signature.name(),
                            input_as_property_signature.maybe_question_token(),
                            self.ensure_type(
                                input,
                                input_as_property_signature.maybe_type().as_deref(),
                                None,
                            )?,
                        )),
                    )?
                }
                SyntaxKind::MethodSignature => {
                    let input_as_method_signature = input.as_method_signature();
                    if is_private_identifier(&input_as_method_signature.name()) {
                        return self.visit_declaration_subtree_cleanup(
                            input,
                            can_produce_diagnostic,
                            previous_enclosing_declaration.as_ref(),
                            &old_diag,
                            should_enter_suppress_new_diagnostics_context_context,
                            old_within_object_literal_type,
                            None,
                        );
                    }
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(
                            &self.factory.update_method_signature(
                                input,
                                self.ensure_modifiers(input),
                                input_as_method_signature.name(),
                                input_as_method_signature.maybe_question_token(),
                                self.ensure_type_params(
                                    input,
                                    input_as_method_signature.maybe_type_parameters().as_deref(),
                                )?,
                                self.update_params_list(
                                    input,
                                    Some(&input_as_method_signature.parameters()),
                                    None,
                                )?
                                .unwrap(),
                                self.ensure_type(
                                    input,
                                    input_as_method_signature.maybe_type().as_deref(),
                                    None,
                                )?,
                            ),
                        ),
                    )?
                }
                SyntaxKind::CallSignature => {
                    let input_as_call_signature_declaration = input.as_call_signature_declaration();
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(
                            &self.factory.update_call_signature(
                                input,
                                self.ensure_type_params(
                                    input,
                                    input_as_call_signature_declaration
                                        .maybe_type_parameters()
                                        .as_deref(),
                                )?,
                                self.update_params_list(
                                    input,
                                    Some(&input_as_call_signature_declaration.parameters()),
                                    None,
                                )?
                                .unwrap(),
                                self.ensure_type(
                                    input,
                                    input_as_call_signature_declaration.maybe_type().as_deref(),
                                    None,
                                )?,
                            ),
                        ),
                    )?
                }
                SyntaxKind::IndexSignature => {
                    let input_as_index_signature_declaration =
                        input.as_index_signature_declaration();
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(
                            &self.factory.update_index_signature(
                                input,
                                Option::<Gc<NodeArray>>::None,
                                self.ensure_modifiers(input),
                                self.update_params_list(
                                    input,
                                    Some(&input_as_index_signature_declaration.parameters()),
                                    None,
                                )?
                                .unwrap(),
                                try_maybe_visit_node(
                                    input_as_index_signature_declaration.maybe_type(),
                                    Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                                    Option::<fn(Id<Node>) -> bool>::None,
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                )?
                                .unwrap_or_else(|| {
                                    self.factory
                                        .create_keyword_type_node(SyntaxKind::AnyKeyword)
                                }),
                            ),
                        ),
                    )?
                }
                SyntaxKind::VariableDeclaration => {
                    let input_as_variable_declaration = input.as_variable_declaration();
                    if is_binding_pattern(input_as_variable_declaration.maybe_name()) {
                        return Ok(Some(
                            self.recreate_binding_pattern(&input_as_variable_declaration.name())?
                                .into(),
                        ));
                    }
                    should_enter_suppress_new_diagnostics_context_context = true;
                    self.set_suppress_new_diagnostic_contexts(Some(true));
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(&self.factory.update_variable_declaration(
                            input,
                            input_as_variable_declaration.maybe_name(),
                            None,
                            self.ensure_type(
                                input,
                                input_as_variable_declaration.maybe_type().as_deref(),
                                None,
                            )?,
                            self.ensure_no_initializer(input)?,
                        )),
                    )?
                }
                SyntaxKind::TypeParameter => {
                    let input_as_type_parameter_declaration = input.as_type_parameter_declaration();
                    if self.is_private_method_type_parameter(input)
                        && (input_as_type_parameter_declaration.default.is_some()
                            || input_as_type_parameter_declaration.constraint.is_some())
                    {
                        return self.visit_declaration_subtree_cleanup(
                            input,
                            can_produce_diagnostic,
                            previous_enclosing_declaration.as_ref(),
                            &old_diag,
                            should_enter_suppress_new_diagnostics_context_context,
                            old_within_object_literal_type,
                            Some(&self.factory.update_type_parameter_declaration(
                                input,
                                input_as_type_parameter_declaration.name(),
                                None,
                                None,
                            )),
                        );
                    }
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        try_maybe_visit_each_child(
                            Some(input),
                            |node: Id<Node>| self.visit_declaration_subtree(node),
                            &**self.context,
                        )?
                        .as_deref(),
                    )?
                }
                SyntaxKind::ConditionalType => {
                    let input_as_conditional_type_node = input.as_conditional_type_node();
                    let check_type = try_visit_node(
                        &input_as_conditional_type_node.check_type,
                        Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                        Option::<fn(Id<Node>) -> bool>::None,
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?;
                    let extends_type = try_visit_node(
                        &input_as_conditional_type_node.extends_type,
                        Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                        Option::<fn(Id<Node>) -> bool>::None,
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?;
                    let old_enclosing_decl = self.maybe_enclosing_declaration();
                    self.set_enclosing_declaration(Some(
                        input_as_conditional_type_node.true_type.clone(),
                    ));
                    let true_type = try_visit_node(
                        &input_as_conditional_type_node.true_type,
                        Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                        Option::<fn(Id<Node>) -> bool>::None,
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?;
                    self.set_enclosing_declaration(old_enclosing_decl);
                    let false_type = try_visit_node(
                        &input_as_conditional_type_node.false_type,
                        Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                        Option::<fn(Id<Node>) -> bool>::None,
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?;
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(&self.factory.update_conditional_type_node(
                            input,
                            check_type,
                            extends_type,
                            true_type,
                            false_type,
                        )),
                    )?
                }
                SyntaxKind::FunctionType => {
                    let input_as_function_type_node = input.as_function_type_node();
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(
                            &self.factory.update_function_type_node(
                                input,
                                try_maybe_visit_nodes(
                                    input_as_function_type_node
                                        .maybe_type_parameters()
                                        .as_deref(),
                                    Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                                    Option::<fn(Id<Node>) -> bool>::None,
                                    None,
                                    None,
                                )?,
                                self.update_params_list(
                                    input,
                                    Some(&input_as_function_type_node.parameters()),
                                    None,
                                )?
                                .unwrap(),
                                try_maybe_visit_node(
                                    input_as_function_type_node.maybe_type(),
                                    Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                                    Option::<fn(Id<Node>) -> bool>::None,
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                )?,
                            ),
                        ),
                    )?
                }
                SyntaxKind::ConstructorType => {
                    let input_as_constructor_type_node = input.as_constructor_type_node();
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(
                            &self.factory.update_constructor_type_node(
                                input,
                                self.ensure_modifiers(input),
                                try_maybe_visit_nodes(
                                    input_as_constructor_type_node
                                        .maybe_type_parameters()
                                        .as_deref(),
                                    Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                                    Option::<fn(Id<Node>) -> bool>::None,
                                    None,
                                    None,
                                )?,
                                self.update_params_list(
                                    input,
                                    Some(&input_as_constructor_type_node.parameters()),
                                    None,
                                )?
                                .unwrap(),
                                try_maybe_visit_node(
                                    input_as_constructor_type_node.maybe_type(),
                                    Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                                    Option::<fn(Id<Node>) -> bool>::None,
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                )?,
                            ),
                        ),
                    )?
                }
                SyntaxKind::ImportType => {
                    let input_as_import_type_node = input.as_import_type_node();
                    if !is_literal_import_type_node(input, self) {
                        return self.visit_declaration_subtree_cleanup(
                            input,
                            can_produce_diagnostic,
                            previous_enclosing_declaration.as_ref(),
                            &old_diag,
                            should_enter_suppress_new_diagnostics_context_context,
                            old_within_object_literal_type,
                            Some(input),
                        );
                    }
                    self.visit_declaration_subtree_cleanup(
                        input,
                        can_produce_diagnostic,
                        previous_enclosing_declaration.as_ref(),
                        &old_diag,
                        should_enter_suppress_new_diagnostics_context_context,
                        old_within_object_literal_type,
                        Some(
                            &self.factory.update_import_type_node(
                                input,
                                self.factory.update_literal_type_node(
                                    &input_as_import_type_node.argument,
                                    self.rewrite_module_specifier(
                                        input,
                                        Some(
                                            &input_as_import_type_node
                                                .argument
                                                .as_literal_type_node()
                                                .literal,
                                        ),
                                    )?
                                    .unwrap(),
                                ),
                                input_as_import_type_node.qualifier.clone(),
                                try_maybe_visit_nodes(
                                    input_as_import_type_node.maybe_type_arguments().as_deref(),
                                    Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                                    Some(is_type_node),
                                    None,
                                    None,
                                )?,
                                Some(input_as_import_type_node.is_type_of()),
                            ),
                        ),
                    )?
                }
                _ => Debug_.assert_never(
                    input,
                    Some(&format!(
                        "Attempted to process unhandled node kind: {:?}",
                        input.kind()
                    )),
                ),
            });
        }

        if is_tuple_type_node(input)
            && get_line_and_character_of_position(
                self.current_source_file().as_source_file(),
                input.pos().try_into().unwrap(),
            )
            .line
                == get_line_and_character_of_position(
                    self.current_source_file().as_source_file(),
                    input.end().try_into().unwrap(),
                )
                .line
        {
            set_emit_flags(input, EmitFlags::SingleLine, self);
        }

        self.visit_declaration_subtree_cleanup(
            input,
            can_produce_diagnostic,
            previous_enclosing_declaration.as_ref(),
            &old_diag,
            should_enter_suppress_new_diagnostics_context_context,
            old_within_object_literal_type,
            try_maybe_visit_each_child(
                Some(input),
                |node: Id<Node>| self.visit_declaration_subtree(node),
                &**self.context,
            )?
            .as_deref(),
        )
    }
}
