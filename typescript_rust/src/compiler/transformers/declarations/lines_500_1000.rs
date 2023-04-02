use std::{
    borrow::{Borrow, Cow},
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    mem, ptr,
};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use super::{declaration_emit_node_builder_flags, TransformDeclarations};
use crate::{
    add_related_info_rc, are_option_gcs_equal, can_produce_diagnostics, contains_comparer,
    create_diagnostic_for_node, create_empty_exports,
    create_get_symbol_accessibility_diagnostic_for_node, create_unparsed_source_file,
    declaration_name_to_string, filter, gc_cell_ref_mut_unwrapped, gc_cell_ref_unwrapped,
    get_comment_range, get_directory_path,
    get_external_module_import_equals_declaration_expression,
    get_external_module_name_from_declaration, get_factory, get_leading_comment_ranges,
    get_leading_comment_ranges_of_node, get_name_of_declaration, get_original_node_id,
    get_output_paths_for, get_parse_tree_node, get_relative_path_to_directory_or_url,
    get_resolved_external_module_name, get_set_accessor_value_parameter, get_source_file_of_node,
    get_text_of_node, get_this_parameter, get_trailing_comment_ranges, has_effective_modifier,
    has_extension, has_jsdoc_nodes, is_any_import_syntax, is_binding_pattern, is_class_declaration,
    is_export_assignment, is_external_module, is_external_module_indicator,
    is_external_module_reference, is_external_or_common_js_module, is_function_declaration,
    is_function_like, is_import_declaration, is_import_equals_declaration,
    is_index_signature_declaration, is_interface_declaration, is_json_source_file,
    is_late_visibility_painted_statement, is_mapped_type_node, is_module_declaration,
    is_omitted_expression, is_set_accessor_declaration, is_source_file, is_source_file_js,
    is_source_file_not_json, is_string_literal, is_string_literal_like, is_type_alias_declaration,
    is_unparsed_source, last, length, map, map_defined, maybe_concatenate, maybe_filter,
    maybe_for_each, maybe_for_each_bool, maybe_map, module_specifiers, needs_scope_marker,
    normalize_slashes, path_contains_node_modules, path_is_relative, push_if_unique_gc,
    set_comment_range_rc, set_text_range_node_array, skip_trivia, some, starts_with,
    string_contains, to_file_name_lower_case, to_path, transform_nodes, visit_node, visit_nodes,
    with_synthetic_factory, AllAccessorDeclarations, BaseNodeFactorySynthetic, CommentRange,
    CompilerOptions, Debug_, Diagnostic, Diagnostics, EmitHost, EmitResolver, FileReference,
    FunctionLikeDeclarationInterface, GetSymbolAccessibilityDiagnostic,
    GetSymbolAccessibilityDiagnosticInterface, HasInitializerInterface, HasStatementsInterface,
    HasTypeInterface, LiteralLikeNodeInterface, ModifierFlags,
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, NamedDeclarationInterface, Node,
    NodeArray, NodeBuilderFlags, NodeFactory, NodeId, NodeInterface, NonEmpty, ReadonlyTextRange,
    ScriptReferenceHost, SingleNodeOrVecNode, SourceFileLike, Symbol, SymbolAccessibility,
    SymbolAccessibilityDiagnostic, SymbolAccessibilityResult, SymbolFlags, SymbolInterface,
    SymbolTracker, SyntaxKind, TextRange, TransformationContext, TransformationResult, Transformer,
    TransformerFactory, TransformerFactoryInterface, TransformerInterface, VisitResult,
};

impl TransformDeclarations {
    pub(super) fn ensure_type(
        &self,
        node: &Node, /*HasInferredType*/
        type_: Option<&Node /*TypeNode*/>,
        ignore_private: Option<bool>,
    ) -> Option<Gc<Node /*TypeNode*/>> {
        if ignore_private != Some(true) && has_effective_modifier(node, ModifierFlags::Private) {
            return None;
        }
        if self.should_print_with_initializer(node) {
            return None;
        }
        let should_use_resolver_type = node.kind() == SyntaxKind::Parameter
            && (self.resolver.is_required_initialized_parameter(node)
                || self
                    .resolver
                    .is_optional_uninitialized_parameter_property(node));
        if let Some(type_) = type_ {
            if !should_use_resolver_type {
                return visit_node(
                    Some(type_),
                    Some(|node: &Node| self.visit_declaration_subtree(node)),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                );
            }
        }
        if get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None).is_none() {
            return if let Some(type_) = type_ {
                visit_node(
                    Some(type_),
                    Some(|node: &Node| self.visit_declaration_subtree(node)),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
            } else {
                Some(with_synthetic_factory(|synthetic_factory_| {
                    self.factory
                        .create_keyword_type_node(synthetic_factory_, SyntaxKind::AnyKeyword)
                        .into()
                }))
            };
        }
        if node.kind() == SyntaxKind::SetAccessor {
            return Some(with_synthetic_factory(|synthetic_factory_| {
                self.factory
                    .create_keyword_type_node(synthetic_factory_, SyntaxKind::AnyKeyword)
                    .into()
            }));
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
            return Some(self.cleanup(
                old_diag,
                self.resolver.create_type_of_declaration(
                    node,
                    &self.enclosing_declaration(),
                    declaration_emit_node_builder_flags(),
                    &**self.symbol_tracker(),
                    None,
                ),
            ));
        }
        if matches!(
            node.kind(),
            SyntaxKind::Parameter | SyntaxKind::PropertyDeclaration | SyntaxKind::PropertySignature
        ) {
            if node.as_has_initializer().maybe_initializer().is_none() {
                return Some(self.cleanup(
                    old_diag,
                    self.resolver.create_type_of_declaration(
                        node,
                        &self.enclosing_declaration(),
                        declaration_emit_node_builder_flags(),
                        &**self.symbol_tracker(),
                        Some(should_use_resolver_type),
                    ),
                ));
            }
            return Some(
                self.cleanup(
                    old_diag,
                    self.resolver
                        .create_type_of_declaration(
                            node,
                            &self.enclosing_declaration(),
                            declaration_emit_node_builder_flags(),
                            &**self.symbol_tracker(),
                            None,
                        )
                        .or_else(|| {
                            self.resolver.create_type_of_expression(
                                &node.as_has_initializer().maybe_initializer().unwrap(),
                                &self.enclosing_declaration(),
                                declaration_emit_node_builder_flags(),
                                &**self.symbol_tracker(),
                            )
                        }),
                ),
            );
        }
        Some(self.cleanup(
            old_diag,
            self.resolver.create_return_type_of_signature_declaration(
                node,
                &self.enclosing_declaration(),
                declaration_emit_node_builder_flags(),
                &**self.symbol_tracker(),
            ),
        ))
    }

    pub(super) fn cleanup(
        &self,
        old_diag: Option<GetSymbolAccessibilityDiagnostic>,
        return_value: Option<impl Borrow<Node> /*TypeNode*/>,
    ) -> Gc<Node> {
        self.set_error_name_node(None);
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(old_diag.unwrap())
        }
        return_value
            .map(|return_value| return_value.borrow().node_wrapper())
            .unwrap_or_else(|| {
                with_synthetic_factory(|synthetic_factory_| {
                    self.factory
                        .create_keyword_type_node(synthetic_factory_, SyntaxKind::AnyKeyword)
                        .into()
                })
            })
    }

    pub(super) fn is_declaration_and_not_visible(
        &self,
        node: &Node, /*NamedDeclaration*/
    ) -> bool {
        let ref node = get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None).unwrap();
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
        input: &Node, /*FunctionDeclaration*/
    ) -> bool {
        if input.as_function_declaration().maybe_body().is_some() {
            return true;
        }

        let overload_signatures: Option<Vec<_>> =
            input
                .symbol()
                .maybe_declarations()
                .as_ref()
                .map(|input_symbol_declarations| {
                    input_symbol_declarations
                        .into_iter()
                        .filter(|decl: &&Gc<Node>| {
                            is_function_declaration(decl)
                                && decl.as_function_declaration().maybe_body().is_none()
                        })
                        .cloned()
                        .collect()
                });
        match overload_signatures {
            None => true,
            Some(overload_signatures) if overload_signatures.is_empty() => true,
            Some(overload_signatures) => {
                overload_signatures
                    .iter()
                    .position(|overload_signature: &Gc<Node>| ptr::eq(&**overload_signature, input))
                    == Some(overload_signatures.len() - 1)
            }
        }
    }

    pub(super) fn get_binding_name_visible(
        &self,
        elem: &Node, /*BindingElement | VariableDeclaration | OmittedExpression*/
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
                Some(elem_name.as_has_elements().elements()),
                Some(|element: &Gc<Node>| self.get_binding_name_visible(element)),
            )
        } else {
            self.resolver.is_declaration_visible(elem)
        }
    }

    pub(super) fn update_params_list(
        &self,
        node: &Node,
        params: Option<&NodeArray /*<ParameterDeclaration>*/>,
        modifier_mask: Option<ModifierFlags>,
    ) -> Option<NodeArray /*<ParameterDeclaration>*/> {
        if has_effective_modifier(node, ModifierFlags::Private) {
            return None;
        }
        let new_params = maybe_map(params, |p: &Gc<Node>, _| {
            self.ensure_parameter(p, modifier_mask, None)
        })?;
        Some(
            self.factory
                .create_node_array(Some(new_params), Some(params.unwrap().has_trailing_comma)),
        )
    }

    pub(super) fn update_accessor_params_list(
        &self,
        input: &Node, /*AccessorDeclaration*/
        is_private: bool,
    ) -> NodeArray {
        let mut new_params: Vec<Gc<Node /*ParameterDeclaration*/>> = Default::default();
        if !is_private {
            let this_parameter = get_this_parameter(input);
            if let Some(this_parameter) = this_parameter {
                new_params.push(self.ensure_parameter(&this_parameter, None, None));
            }
        }
        if is_set_accessor_declaration(input) {
            let mut new_value_parameter: Option<Gc<Node /*ParameterDeclaration*/>> = None;
            if !is_private {
                let value_parameter = get_set_accessor_value_parameter(input);
                if let Some(value_parameter) = value_parameter {
                    let accessor_type = self.get_type_annotation_from_all_accessor_declarations(
                        input,
                        &self.resolver.get_all_accessor_declarations(input),
                    );
                    new_value_parameter = Some(self.ensure_parameter(
                        &value_parameter,
                        None,
                        accessor_type.as_deref(),
                    ));
                }
            }
            if new_value_parameter.is_none() {
                new_value_parameter = Some(with_synthetic_factory(|synthetic_factory_| {
                    self.factory
                        .create_parameter_declaration(
                            synthetic_factory_,
                            Option::<NodeArray>::None,
                            Option::<NodeArray>::None,
                            None,
                            Some("value"),
                            None,
                            None,
                            None,
                        )
                        .into()
                }));
            }
            let new_value_parameter = new_value_parameter.unwrap();
            new_params.push(new_value_parameter);
        }
        self.factory.create_node_array(Some(new_params), None)
    }

    pub(super) fn ensure_type_params(
        &self,
        node: &Node,
        params: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
    ) -> Option<NodeArray> {
        if has_effective_modifier(node, ModifierFlags::Private) {
            None
        } else {
            visit_nodes(
                params,
                Some(|node: &Node| self.visit_declaration_subtree(node)),
                Option::<fn(&Node) -> bool>::None,
                None,
                None,
            )
        }
    }

    pub(super) fn is_enclosing_declaration(&self, node: &Node) -> bool {
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
        entity_name: &Node, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: &Node,
    ) {
        let visibility_result = self
            .resolver
            .is_entity_name_visible(entity_name, enclosing_declaration);
        self.handle_symbol_accessibility_error(
            &visibility_result.into_symbol_accessibility_result(),
        );
        self.record_type_reference_directives_if_necessary(
            self.resolver
                .get_type_reference_directives_for_entity_name(entity_name)
                .as_deref(),
        );
    }

    pub(super) fn preserve_js_doc(&self, updated: &Node, original: &Node) -> Gc<Node> {
        if has_jsdoc_nodes(updated) && has_jsdoc_nodes(original) {
            updated.set_js_doc(original.maybe_js_doc());
        }
        set_comment_range_rc(
            updated.node_wrapper(),
            &get_comment_range(original).into_readonly_text_range(),
        )
    }

    pub(super) fn rewrite_module_specifier(
        &self,
        parent: &Node, /*ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration | ImportTypeNode*/
        input: Option<&Node>,
    ) -> Option<Gc<Node>> {
        let input = input?;
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
                );
                if let Some(new_name) = new_name.non_empty() {
                    return Some(with_synthetic_factory(|synthetic_factory_| {
                        self.factory
                            .create_string_literal(synthetic_factory_, new_name, None, None)
                            .into()
                    }));
                }
            } else {
                let symbol = self.resolver.get_symbol_of_external_module_specifier(input);
                if let Some(symbol) = symbol {
                    self.maybe_exported_modules_from_declaration_emit_mut()
                        .get_or_insert_with(|| vec![])
                        .push(symbol);
                }
            }
        }
        Some(input.node_wrapper())
    }

    pub(super) fn transform_import_equals_declaration(
        &self,
        decl: &Node, /*ImportEqualsDeclaration*/
    ) -> Option<Gc<Node>> {
        if !self.resolver.is_declaration_visible(decl) {
            return None;
        }
        let decl_as_import_equals_declaration = decl.as_import_equals_declaration();
        if decl_as_import_equals_declaration.module_reference.kind()
            == SyntaxKind::ExternalModuleReference
        {
            let specifier = get_external_module_import_equals_declaration_expression(decl);
            Some(with_synthetic_factory(|synthetic_factory_| {
                self.factory.update_import_equals_declaration(
                    synthetic_factory_,
                    decl,
                    Option::<NodeArray>::None,
                    decl.maybe_modifiers().clone(),
                    decl_as_import_equals_declaration.is_type_only,
                    decl_as_import_equals_declaration.name(),
                    self.factory.update_external_module_reference(
                        synthetic_factory_,
                        &decl_as_import_equals_declaration.module_reference,
                        self.rewrite_module_specifier(decl, Some(&specifier))
                            .unwrap(),
                    ),
                )
            }))
        } else {
            let old_diag = self.get_symbol_accessibility_diagnostic();
            self.set_get_symbol_accessibility_diagnostic(
                create_get_symbol_accessibility_diagnostic_for_node(decl),
            );
            self.check_entity_name_visibility(
                &decl_as_import_equals_declaration.module_reference,
                &self.enclosing_declaration(),
            );
            self.set_get_symbol_accessibility_diagnostic(old_diag);
            Some(decl.node_wrapper())
        }
    }

    pub(super) fn transform_import_declaration(
        &self,
        decl: &Node, /*ImportDeclaration*/
    ) -> Option<Gc<Node>> {
        let decl_as_import_declaration = decl.as_import_declaration();
        if decl_as_import_declaration.import_clause.is_none() {
            return Some(with_synthetic_factory(|synthetic_factory_| {
                self.factory.update_import_declaration(
                    synthetic_factory_,
                    decl,
                    Option::<NodeArray>::None,
                    decl.maybe_modifiers().clone(),
                    decl_as_import_declaration.import_clause.clone(),
                    self.rewrite_module_specifier(
                        decl,
                        Some(&decl_as_import_declaration.module_specifier),
                    )
                    .unwrap(),
                    None,
                )
            }));
        }
        let decl_import_clause = decl_as_import_declaration.import_clause.as_ref().unwrap();
        let decl_import_clause_as_import_clause = decl_import_clause.as_import_clause();
        let visible_default_binding = /*decl.importClause &&*/
            decl_import_clause_as_import_clause.name.clone().filter(|_| {
                self.resolver.is_declaration_visible(decl_import_clause)
            });
        if decl_import_clause_as_import_clause.named_bindings.is_none() {
            return visible_default_binding.map(|visible_default_binding| {
                with_synthetic_factory(|synthetic_factory_| {
                    self.factory.update_import_declaration(
                        synthetic_factory_,
                        decl,
                        Option::<NodeArray>::None,
                        decl.maybe_modifiers().clone(),
                        Some(self.factory.update_import_clause(
                            synthetic_factory_,
                            decl_import_clause,
                            decl_import_clause_as_import_clause.is_type_only,
                            Some(visible_default_binding),
                            None,
                        )),
                        self.rewrite_module_specifier(
                            decl,
                            Some(&decl_as_import_declaration.module_specifier),
                        )
                        .unwrap(),
                        None,
                    )
                })
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
            return if visible_default_binding.is_some() || named_bindings.is_some() {
                Some(with_synthetic_factory(|synthetic_factory_| {
                    self.factory.update_import_declaration(
                        synthetic_factory_,
                        decl,
                        Option::<NodeArray>::None,
                        decl.maybe_modifiers().clone(),
                        Some(self.factory.update_import_clause(
                            synthetic_factory_,
                            decl_import_clause,
                            decl_import_clause_as_import_clause.is_type_only,
                            visible_default_binding,
                            named_bindings.cloned(),
                        )),
                        self.rewrite_module_specifier(
                            decl,
                            Some(&decl_as_import_declaration.module_specifier),
                        )
                        .unwrap(),
                        None,
                    )
                }))
            } else {
                None
            };
        }
        let binding_list = map_defined(
            Some(
                &decl_import_clause_named_bindings
                    .as_named_imports()
                    .elements,
            ),
            |b: &Gc<Node>, _| {
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
            return Some(with_synthetic_factory(|synthetic_factory_| {
                self.factory.update_import_declaration(
                    synthetic_factory_,
                    decl,
                    Option::<NodeArray>::None,
                    decl.maybe_modifiers().clone(),
                    Some(self.factory.update_import_clause(
                        synthetic_factory_,
                        decl_import_clause,
                        decl_import_clause_as_import_clause.is_type_only,
                        visible_default_binding,
                        if
                        /*bindingList &&*/
                        !binding_list.is_empty() {
                            Some(self.factory.update_named_imports(
                                synthetic_factory_,
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
                    )
                    .unwrap(),
                    None,
                )
            }));
        }
        if self.resolver.is_import_required_by_augmentation(decl) {
            return Some(with_synthetic_factory(|synthetic_factory_| {
                self.factory.update_import_declaration(
                    synthetic_factory_,
                    decl,
                    Option::<NodeArray>::None,
                    decl.maybe_modifiers().clone(),
                    None,
                    self.rewrite_module_specifier(
                        decl,
                        Some(&decl_as_import_declaration.module_specifier),
                    )
                    .unwrap(),
                    None,
                )
            }));
        }
        None
    }

    pub(super) fn transform_and_replace_late_painted_statements(
        &self,
        statements: &NodeArray, /*<Statement>*/
    ) -> NodeArray /*<Statement>*/ {
        while length(self.maybe_late_marked_statements().as_deref()) > 0 {
            let ref i = self.late_marked_statements_mut().remove(0);
            if !is_late_visibility_painted_statement(i) {
                return Debug_.fail(Some(
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
            let result = self.transform_top_level_declaration(i);
            self.set_needs_declare(prior_needs_declare);
            self.late_statement_replacement_map_mut()
                .insert(get_original_node_id(i), result.map(Into::into));
        }

        visit_nodes(
            Some(statements),
            Some(|node: &Node| self.visit_late_visibility_marked_statements(node)),
            Option::<fn(&Node) -> bool>::None,
            None,
            None,
        )
        .unwrap()
    }

    pub(super) fn visit_late_visibility_marked_statements(
        &self,
        statement: &Node, /*Statement*/
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
                        .any(|node: &Gc<Node>| needs_scope_marker(node))
                    {
                        self.set_needs_scope_fix_marker(true);
                    }
                    if is_source_file(&statement.parent())
                        && result
                            .into_iter()
                            .any(|node: &Gc<Node>| is_external_module_indicator(node))
                    {
                        self.set_result_has_external_module_indicator(true);
                    }
                }
                return result;
            }
        }
        Some(statement.node_wrapper().into())
    }

    pub(super) fn visit_declaration_subtree(&self, input: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn visit_declaration_statements(&self, input: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn transform_top_level_declaration(
        &self,
        input: &Node, /*LateVisibilityPaintedStatement*/
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn get_type_annotation_from_all_accessor_declarations(
        &self,
        node: &Node, /*AccessorDeclaration*/
        accessors: &AllAccessorDeclarations,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }
}
