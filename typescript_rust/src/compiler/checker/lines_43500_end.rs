use gc::{Finalize, Gc, Trace};
use itertools::Itertools;
use std::{collections::HashMap, convert::TryInto, io};

use super::{ambient_module_symbol_regex, IterationTypeKind};
use crate::{
    are_option_gcs_equal, create_diagnostic_for_node, create_file_diagnostic, filter, find,
    first_or_undefined, for_each_bool, get_check_flags, get_declaration_of_kind,
    get_effective_return_type_node, get_jsdoc_type_parameter_declarations, get_object_flags,
    get_parse_tree_node, get_source_file_of_node, get_span_of_token_at_position,
    has_abstract_modifier, has_possible_external_module_reference, has_syntactic_modifier, id_text,
    is_accessor, is_binary_expression, is_binding_element, is_child_of_node_with_kind,
    is_computed_property_name, is_declaration, is_declaration_name, is_function_like,
    is_get_or_set_accessor_declaration, is_identifier, is_in_js_file, is_let, is_literal_type_node,
    is_omitted_expression, is_prefix_unary_expression, is_private_identifier,
    is_property_declaration, is_spread_element, is_static, is_string_literal, is_type_literal_node,
    is_var_const, is_variable_declaration, length, maybe_is_class_like,
    resolve_tripleslash_reference, return_ok_default_if_none, skip_trivia, text_span_end,
    token_to_string, try_find, AllAccessorDeclarations, CheckFlags, Debug_, DiagnosticMessage,
    Diagnostics, EmitResolver, HasInitializerInterface, HasStatementsInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, IterationTypesKey,
    LiteralLikeNodeInterface, ModifierFlags, ModuleKind, NamedDeclarationInterface, Node,
    NodeBuilderFlags, NodeCheckFlags, NodeFlags, NodeInterface, NonEmpty, ObjectFlags, OptionTry,
    ReadonlyTextRange, ReadonlyTextRangeConcrete, ScriptTarget, Signature, SignatureFlags,
    SignatureKind, SourceFileLike, StringOrNumber, Symbol, SymbolAccessibilityResult, SymbolFlags,
    SymbolInterface, SymbolTracker, SymbolVisibilityResult, SyntaxKind, Ternary, TokenFlags, Type,
    TypeChecker, TypeFlags, TypeInterface, TypeReferenceSerializationKind,
};

impl TypeChecker {
    pub(super) fn check_es_module_marker(
        &self,
        name: &Node, /*Identifier | BindingPattern*/
    ) -> bool {
        if name.kind() == SyntaxKind::Identifier {
            if id_text(name) == "__esModule" {
                return self.grammar_error_on_node_skipped_on(
                    "noEmit".to_owned(),
                    name,
                    &Diagnostics::Identifier_expected_esModule_is_reserved_as_an_exported_marker_when_transforming_ECMAScript_modules,
                    None,
                );
            }
        } else {
            let elements = name.as_has_elements().elements();
            for element in &elements {
                if !is_omitted_expression(element) {
                    return self.check_es_module_marker(&element.as_binding_element().name());
                }
            }
        }
        false
    }

    pub(super) fn check_grammar_name_in_let_or_const_declarations(
        &self,
        name: &Node, /*Identifier | BindingPattern*/
    ) -> bool {
        if name.kind() == SyntaxKind::Identifier {
            if name.as_identifier().original_keyword_kind == Some(SyntaxKind::LetKeyword) {
                return self.grammar_error_on_node(
                    name,
                    &Diagnostics::let_is_not_allowed_to_be_used_as_a_name_in_let_or_const_declarations,
                    None,
                );
            }
        } else {
            let elements = name.as_has_elements().elements();
            for element in &elements {
                if !is_omitted_expression(element) {
                    self.check_grammar_name_in_let_or_const_declarations(
                        &element.as_binding_element().name(),
                    );
                }
            }
        }
        false
    }

    pub(super) fn check_grammar_variable_declaration_list(
        &self,
        declaration_list: &Node, /*VariableDeclarationList*/
    ) -> bool {
        let declaration_list_as_variable_declaration_list =
            declaration_list.as_variable_declaration_list();
        let declarations = &declaration_list_as_variable_declaration_list.declarations;
        if self.check_grammar_for_disallowed_trailing_comma(
            Some(&declaration_list_as_variable_declaration_list.declarations),
            None,
        ) {
            return true;
        }

        if declaration_list_as_variable_declaration_list
            .declarations
            .is_empty()
        {
            return self.grammar_error_at_pos(
                declaration_list,
                declarations.pos(),
                declarations.end() - declarations.pos(),
                &Diagnostics::Variable_declaration_list_cannot_be_empty,
                None,
            );
        }
        false
    }

    pub(super) fn allow_let_and_const_declarations(&self, parent: &Node) -> bool {
        match parent.kind() {
            SyntaxKind::IfStatement
            | SyntaxKind::DoStatement
            | SyntaxKind::WhileStatement
            | SyntaxKind::WithStatement
            | SyntaxKind::ForStatement
            | SyntaxKind::ForInStatement
            | SyntaxKind::ForOfStatement => false,
            SyntaxKind::LabeledStatement => self.allow_let_and_const_declarations(&parent.parent()),
            _ => true,
        }
    }

    pub(super) fn check_grammar_for_disallowed_let_or_const_statement(
        &self,
        node: &Node, /*VariableStatement*/
    ) -> bool {
        if !self.allow_let_and_const_declarations(&node.parent()) {
            let node_as_variable_statement = node.as_variable_statement();
            if is_let(&node_as_variable_statement.declaration_list) {
                return self.grammar_error_on_node(
                    node,
                    &Diagnostics::let_declarations_can_only_be_declared_inside_a_block,
                    None,
                );
            } else if is_var_const(&node_as_variable_statement.declaration_list) {
                return self.grammar_error_on_node(
                    node,
                    &Diagnostics::const_declarations_can_only_be_declared_inside_a_block,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn check_grammar_meta_property(&self, node: &Node /*MetaProperty*/) -> bool {
        let node_as_meta_property = node.as_meta_property();
        let escaped_text = &node_as_meta_property.name.as_identifier().escaped_text;
        match node_as_meta_property.keyword_token {
            SyntaxKind::NewKeyword => {
                if escaped_text != "target" {
                    return self.grammar_error_on_node(
                        &node_as_meta_property.name,
                        &Diagnostics::_0_is_not_a_valid_meta_property_for_keyword_1_Did_you_mean_2,
                        Some(vec![
                            node_as_meta_property
                                .name
                                .as_identifier()
                                .escaped_text
                                .clone(),
                            token_to_string(node_as_meta_property.keyword_token)
                                .unwrap()
                                .to_owned(),
                            "target".to_owned(),
                        ]),
                    );
                }
            }
            SyntaxKind::ImportKeyword => {
                if escaped_text != "meta" {
                    return self.grammar_error_on_node(
                        &node_as_meta_property.name,
                        &Diagnostics::_0_is_not_a_valid_meta_property_for_keyword_1_Did_you_mean_2,
                        Some(vec![
                            node_as_meta_property
                                .name
                                .as_identifier()
                                .escaped_text
                                .clone(),
                            token_to_string(node_as_meta_property.keyword_token)
                                .unwrap()
                                .to_owned(),
                            "meta".to_owned(),
                        ]),
                    );
                }
            }
            _ => (),
        }
        false
    }

    pub(super) fn has_parse_diagnostics(&self, source_file: &Node /*SourceFile*/) -> bool {
        !(*source_file.as_source_file().parse_diagnostics())
            .borrow()
            .is_empty()
    }

    pub(super) fn grammar_error_on_first_token(
        &self,
        node: &Node,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> bool {
        let source_file = get_source_file_of_node(node);
        if !self.has_parse_diagnostics(&source_file) {
            let span = get_span_of_token_at_position(&source_file, node.pos().try_into().unwrap());
            self.diagnostics().add(Gc::new(
                create_file_diagnostic(&source_file, span.start, span.length, message, args).into(),
            ));
            return true;
        }
        false
    }

    pub(super) fn grammar_error_at_pos(
        &self,
        node_for_source_file: &Node,
        start: isize,
        length: isize,
        message: &'static DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> bool {
        let source_file = get_source_file_of_node(node_for_source_file);
        if !self.has_parse_diagnostics(&source_file) {
            self.diagnostics().add(Gc::new(
                create_file_diagnostic(&source_file, start, length, message, args).into(),
            ));
            return true;
        }
        false
    }

    pub(super) fn grammar_error_on_node_skipped_on(
        &self,
        key: String, /*keyof CompilerOptions*/
        node: &Node,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> bool {
        let source_file = get_source_file_of_node(node);
        if !self.has_parse_diagnostics(&source_file) {
            self.error_skipped_on(key, Some(node), message, args);
            return true;
        }
        false
    }

    pub(super) fn grammar_error_on_node(
        &self,
        node: &Node,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> bool {
        let source_file = get_source_file_of_node(node);
        if !self.has_parse_diagnostics(&source_file) {
            self.diagnostics()
                .add(create_diagnostic_for_node(node, message, args).into());
            return true;
        }
        false
    }

    pub(super) fn check_grammar_constructor_type_parameters(
        &self,
        node: &Node, /*ConstructorDeclaration*/
    ) -> bool {
        let jsdoc_type_parameters = if is_in_js_file(Some(node)) {
            Some(get_jsdoc_type_parameter_declarations(node))
        } else {
            None
        };
        let range: Option<Box<dyn ReadonlyTextRange>> = node
            .as_constructor_declaration()
            .maybe_type_parameters()
            .as_ref()
            // .map(|node_type_parameters| {
            //     Gc::new(node_type_parameters.clone()) as Gc<dyn ReadonlyTextRange>
            // })
            .map(|node_type_parameters| {
                Box::new(ReadonlyTextRangeConcrete::new(
                    node_type_parameters.pos(),
                    node_type_parameters.end(),
                )) as Box<dyn ReadonlyTextRange>
            })
            .or_else(|| {
                jsdoc_type_parameters
                    .as_ref()
                    .and_then(|jsdoc_type_parameters| {
                        first_or_undefined(jsdoc_type_parameters).cloned()
                    })
                    // .map(|jsdoc_type_parameter| jsdoc_type_parameter as Gc<dyn ReadonlyTextRange>)
                    .map(|jsdoc_type_parameter| {
                        Box::new(ReadonlyTextRangeConcrete::new(
                            jsdoc_type_parameter.pos(),
                            jsdoc_type_parameter.end(),
                        )) as Box<dyn ReadonlyTextRange>
                    })
            });
        if let Some(range) = range {
            let pos = if range.pos() == range.end() {
                range.pos()
            } else {
                skip_trivia(
                    &get_source_file_of_node(node)
                        .as_source_file()
                        .text_as_chars(),
                    range.pos(),
                    None,
                    None,
                    None,
                )
            };
            return self.grammar_error_at_pos(
                node,
                pos,
                range.end() - pos,
                &Diagnostics::Type_parameters_cannot_appear_on_a_constructor_declaration,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_constructor_type_annotation(
        &self,
        node: &Node, /*ConstructorDeclaration*/
    ) -> bool {
        let type_ = get_effective_return_type_node(node);
        if let Some(type_) = type_.as_ref() {
            return self.grammar_error_on_node(
                type_,
                &Diagnostics::Type_annotation_cannot_appear_on_a_constructor_declaration,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_property(
        &self,
        node: &Node, /*PropertyDeclaration | PropertySignature*/
    ) -> io::Result<bool> {
        let node_as_named_declaration = node.as_named_declaration();
        let node_name = node_as_named_declaration.name();
        if is_computed_property_name(&node_name) && {
            let node_name_as_computed_property_name = node_name.as_computed_property_name();
            is_binary_expression(&node_name_as_computed_property_name.expression)
                && node_name_as_computed_property_name
                    .expression
                    .as_binary_expression()
                    .operator_token
                    .kind()
                    == SyntaxKind::InKeyword
        } {
            return Ok(self.grammar_error_on_node(
                &node.parent().as_has_members().members()[0],
                &Diagnostics::A_mapped_type_may_not_declare_properties_or_methods,
                None,
            ));
        }
        if maybe_is_class_like(node.maybe_parent()) {
            if is_string_literal(&node_name)
                && &*node_name.as_string_literal().text() == "constructor"
            {
                return Ok(self.grammar_error_on_node(
                    &node_name,
                    &Diagnostics::Classes_may_not_have_a_field_named_constructor,
                    None,
                ));
            }
            if self.check_grammar_for_invalid_dynamic_name(
                &node_name,
                &Diagnostics::A_computed_property_name_in_a_class_property_declaration_must_have_a_simple_literal_type_or_a_unique_symbol_type,
            )? {
                return Ok(true);
            }
            if self.language_version < ScriptTarget::ES2015 && is_private_identifier(&node_name) {
                return Ok(self.grammar_error_on_node(
                    &node_name,
                    &Diagnostics::Private_identifiers_are_only_available_when_targeting_ECMAScript_2015_and_higher,
                    None,
                ));
            }
        } else if node.parent().kind() == SyntaxKind::InterfaceDeclaration {
            if self.check_grammar_for_invalid_dynamic_name(
                &node_name,
                &Diagnostics::A_computed_property_name_in_an_interface_must_refer_to_an_expression_whose_type_is_a_literal_type_or_a_unique_symbol_type,
            )? {
                return Ok(true);
            }
            if let Some(node_initializer) = node.as_has_initializer().maybe_initializer().as_ref() {
                return Ok(self.grammar_error_on_node(
                    node_initializer,
                    &Diagnostics::An_interface_property_cannot_have_an_initializer,
                    None,
                ));
            }
        } else if is_type_literal_node(&node.parent()) {
            if self.check_grammar_for_invalid_dynamic_name(
                &node_name,
                &Diagnostics::A_computed_property_name_in_a_type_literal_must_refer_to_an_expression_whose_type_is_a_literal_type_or_a_unique_symbol_type,
            )? {
                return Ok(true);
            }
            if let Some(node_initializer) = node.as_has_initializer().maybe_initializer().as_ref() {
                return Ok(self.grammar_error_on_node(
                    node_initializer,
                    &Diagnostics::A_type_literal_property_cannot_have_an_initializer,
                    None,
                ));
            }
        }

        if node.flags().intersects(NodeFlags::Ambient) {
            self.check_ambient_initializer(node)?;
        }

        if is_property_declaration(node) && {
            let node_as_property_declaration = node.as_property_declaration();
            node_as_property_declaration.exclamation_token.is_some()
                && (!maybe_is_class_like(node.maybe_parent())
                    || node_as_property_declaration.maybe_type().is_none()
                    || node_as_property_declaration.maybe_initializer().is_some()
                    || node.flags().intersects(NodeFlags::Ambient)
                    || is_static(node)
                    || has_abstract_modifier(node))
        } {
            let message = if node.as_has_initializer().maybe_initializer().is_some() {
                &*Diagnostics::Declarations_with_initializers_cannot_also_have_definite_assignment_assertions
            } else if node.as_has_type().maybe_type().is_none() {
                &*Diagnostics::Declarations_with_definite_assignment_assertions_must_also_have_type_annotations
            } else {
                &*Diagnostics::A_definite_assignment_assertion_is_not_permitted_in_this_context
            };
            return Ok(self.grammar_error_on_node(
                node.as_property_declaration()
                    .exclamation_token
                    .as_ref()
                    .unwrap(),
                message,
                None,
            ));
        }
        Ok(false)
    }

    pub(super) fn check_grammar_top_level_element_for_required_declare_modifier(
        &self,
        node: &Node,
    ) -> bool {
        if matches!(
            node.kind(),
            SyntaxKind::InterfaceDeclaration
                | SyntaxKind::TypeAliasDeclaration
                | SyntaxKind::ImportDeclaration
                | SyntaxKind::ImportEqualsDeclaration
                | SyntaxKind::ExportDeclaration
                | SyntaxKind::ExportAssignment
                | SyntaxKind::NamespaceExportDeclaration
        ) || has_syntactic_modifier(
            node,
            ModifierFlags::Ambient | ModifierFlags::Export | ModifierFlags::Default,
        ) {
            return false;
        }

        self.grammar_error_on_first_token(
            node,
            &Diagnostics::Top_level_declarations_in_d_ts_files_must_start_with_either_a_declare_or_export_modifier,
            None,
        )
    }

    pub(super) fn check_grammar_top_level_elements_for_required_declare_modifier(
        &self,
        file: &Node, /*SourceFile*/
    ) -> bool {
        for decl in &file.as_source_file().statements() {
            if is_declaration(decl) || decl.kind() == SyntaxKind::VariableStatement {
                if self.check_grammar_top_level_element_for_required_declare_modifier(decl) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn check_grammar_source_file(&self, node: &Node /*SourceFile*/) -> bool {
        node.flags().intersects(NodeFlags::Ambient)
            && self.check_grammar_top_level_elements_for_required_declare_modifier(node)
    }

    pub(super) fn check_grammar_statement_in_ambient_context(&self, node: &Node) -> bool {
        if node.flags().intersects(NodeFlags::Ambient) {
            let links = self.get_node_links(node);
            if (*links).borrow().has_reported_statement_in_ambient_context != Some(true)
                && (is_function_like(node.maybe_parent()) || is_accessor(&node.parent()))
            {
                let ret = self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::An_implementation_cannot_be_declared_in_ambient_contexts,
                    None,
                );
                links.borrow_mut().has_reported_statement_in_ambient_context = Some(ret);
                return ret;
            }

            if matches!(
                node.parent().kind(),
                SyntaxKind::Block | SyntaxKind::ModuleBlock | SyntaxKind::SourceFile
            ) {
                let links = self.get_node_links(&node.parent());
                if (*links).borrow().has_reported_statement_in_ambient_context != Some(true) {
                    let ret = self.grammar_error_on_first_token(
                        node,
                        &Diagnostics::Statements_are_not_allowed_in_ambient_contexts,
                        None,
                    );
                    links.borrow_mut().has_reported_statement_in_ambient_context = Some(ret);
                    return ret;
                }
            }
            // else {
            // }
        }
        false
    }

    pub(super) fn check_grammar_numeric_literal(
        &self,
        node: &Node, /*NumericLiteral*/
    ) -> bool {
        let node_as_numeric_literal = node.as_numeric_literal();
        if node_as_numeric_literal
            .numeric_literal_flags
            .intersects(TokenFlags::Octal)
        {
            let mut diagnostic_message: Option<&'static DiagnosticMessage> = None;
            if self.language_version >= ScriptTarget::ES5 {
                diagnostic_message = Some(&Diagnostics::Octal_literals_are_not_available_when_targeting_ECMAScript_5_and_higher_Use_the_syntax_0);
            } else if is_child_of_node_with_kind(node, SyntaxKind::LiteralType) {
                diagnostic_message =
                    Some(&Diagnostics::Octal_literal_types_must_use_ES2015_syntax_Use_the_syntax_0);
            } else if is_child_of_node_with_kind(node, SyntaxKind::EnumMember) {
                diagnostic_message = Some(&Diagnostics::Octal_literals_are_not_allowed_in_enums_members_initializer_Use_the_syntax_0);
            }
            if let Some(diagnostic_message) = diagnostic_message {
                let with_minus = is_prefix_unary_expression(&node.parent())
                    && node.parent().as_prefix_unary_expression().operator
                        == SyntaxKind::MinusToken;
                let literal = format!(
                    "{}0o{}",
                    if with_minus { "-" } else { "" },
                    &*node_as_numeric_literal.text()
                );
                return self.grammar_error_on_node(
                    &*if with_minus {
                        node.parent()
                    } else {
                        node.node_wrapper()
                    },
                    diagnostic_message,
                    Some(vec![literal]),
                );
            }
        }

        self.check_numeric_literal_value_size(node);

        false
    }

    pub(super) fn check_numeric_literal_value_size(&self, node: &Node /*NumericLiteral*/) {
        let node_as_numeric_literal = node.as_numeric_literal();
        if node_as_numeric_literal
            .numeric_literal_flags
            .intersects(TokenFlags::Scientific)
            || node_as_numeric_literal.text().len() <= 15
            || node_as_numeric_literal
                .text()
                .chars()
                .position(|ch| ch == '.')
                .is_some()
        {
            return;
        }

        // let apparent_value =
        unimplemented!()
    }

    pub(super) fn check_grammar_big_int_literal(&self, node: &Node /*BigIntLiteral*/) -> bool {
        let literal_type = is_literal_type_node(&node.parent())
            || is_prefix_unary_expression(&node.parent())
                && is_literal_type_node(&node.parent().parent());
        if !literal_type {
            if self.language_version < ScriptTarget::ES2020 {
                if self.grammar_error_on_node(
                    node,
                    &Diagnostics::BigInt_literals_are_not_available_when_targeting_lower_than_ES2020,
                    None,
                ) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn grammar_error_after_first_token(
        &self,
        node: &Node,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> bool {
        let source_file = get_source_file_of_node(node);
        if !self.has_parse_diagnostics(&source_file) {
            let span = get_span_of_token_at_position(&source_file, node.pos().try_into().unwrap());
            self.diagnostics().add(Gc::new(
                create_file_diagnostic(&source_file, text_span_end(&span), 0, message, args).into(),
            ));
            return true;
        }
        false
    }

    pub fn get_ambient_modules(&self) -> Vec<Gc<Symbol>> {
        if self.ambient_modules_cache.borrow().is_none() {
            let mut ambient_modules_cache = self.ambient_modules_cache.borrow_mut();
            *ambient_modules_cache = Some(vec![]);
            let ambient_modules_cache = ambient_modules_cache.as_mut().unwrap();
            for (sym, global) in &*(*self.globals).borrow() {
                if ambient_module_symbol_regex.is_match(&**sym) {
                    ambient_modules_cache.push(global.clone());
                }
            }
        }
        self.ambient_modules_cache.borrow().clone().unwrap()
    }

    pub(super) fn check_grammar_import_clause(&self, node: &Node /*ImportClause*/) -> bool {
        let node_as_import_clause = node.as_import_clause();
        if node_as_import_clause.is_type_only
            && node_as_import_clause.name.is_some()
            && node_as_import_clause.named_bindings.is_some()
        {
            return self.grammar_error_on_node(
                node,
                &Diagnostics::A_type_only_import_can_specify_a_default_import_or_named_bindings_but_not_both,
                None,
            );
        }
        if node_as_import_clause.is_type_only {
            if let Some(node_named_bindings) =
                node_as_import_clause
                    .named_bindings
                    .as_ref()
                    .filter(|node_named_bindings| {
                        node_named_bindings.kind() == SyntaxKind::NamedImports
                    })
            {
                return self.check_grammar_named_imports_or_exports(node_named_bindings);
            }
        }
        false
    }

    pub(super) fn check_grammar_named_imports_or_exports(
        &self,
        named_bindings: &Node, /*NamedImportsOrExports*/
    ) -> bool {
        for_each_bool(
            &named_bindings.as_has_elements().elements(),
            |specifier: &Gc<Node>, _| {
                if specifier.as_has_is_type_only().is_type_only() {
                    return self.grammar_error_on_first_token(
                        specifier,
                        if specifier.kind() == SyntaxKind::ImportSpecifier {
                            &Diagnostics::The_type_modifier_cannot_be_used_on_a_named_import_when_import_type_is_used_on_its_import_statement
                        } else {
                            &Diagnostics::The_type_modifier_cannot_be_used_on_a_named_export_when_export_type_is_used_on_its_export_statement
                        },
                        None
                    );
                }
                false
            },
        )
    }

    pub(super) fn check_grammar_import_call_expression(
        &self,
        node: &Node, /*ImportCall*/
    ) -> bool {
        if self.module_kind == ModuleKind::ES2015 {
            return self.grammar_error_on_node(
                node,
                &Diagnostics::Dynamic_imports_are_only_supported_when_the_module_flag_is_set_to_es2020_es2022_esnext_commonjs_amd_system_umd_node12_or_nodenext,
                None,
            );
        }

        let node_as_call_expression = node.as_call_expression();
        if node_as_call_expression.maybe_type_arguments().is_some() {
            return self.grammar_error_on_node(
                node,
                &Diagnostics::Dynamic_import_cannot_have_type_arguments,
                None,
            );
        }

        let node_arguments = &node_as_call_expression.arguments;
        if self.module_kind != ModuleKind::ESNext {
            self.check_grammar_for_disallowed_trailing_comma(Some(node_arguments), None);

            if node_arguments.len() > 1 {
                let assertion_argument = &node_arguments[1];
                return self.grammar_error_on_node(
                    assertion_argument,
                    &Diagnostics::Dynamic_imports_only_support_a_second_argument_when_the_module_option_is_set_to_esnext,
                    None,
                );
            }
        }

        if node_arguments.is_empty() || node_arguments.len() > 2 {
            return self.grammar_error_on_node(
                node,
                &Diagnostics::Dynamic_imports_can_only_accept_a_module_specifier_and_an_optional_assertion_as_arguments,
                None,
            );
        }

        let spread_element = find(node_arguments, |argument: &Gc<Node>, _| {
            is_spread_element(argument)
        });
        if let Some(spread_element) = spread_element {
            return self.grammar_error_on_node(
                spread_element,
                &Diagnostics::Argument_of_dynamic_import_cannot_be_spread_element,
                None,
            );
        }
        false
    }

    pub(super) fn find_matching_type_reference_or_type_alias_reference(
        &self,
        source: &Type,
        union_target: &Type, /*UnionOrIntersectionType*/
    ) -> Option<Gc<Type>> {
        let source_object_flags = get_object_flags(source);
        if source_object_flags.intersects(ObjectFlags::Reference | ObjectFlags::Anonymous)
            && union_target.flags().intersects(TypeFlags::Union)
        {
            return find(
                union_target
                    .as_union_or_intersection_type_interface()
                    .types(),
                |target: &Gc<Type>, _| {
                    if target.flags().intersects(TypeFlags::Object) {
                        let overlap_obj_flags = source_object_flags & get_object_flags(target);
                        if overlap_obj_flags.intersects(ObjectFlags::Reference) {
                            return Gc::ptr_eq(
                                &source.as_type_reference_interface().target(),
                                &target.as_type_reference_interface().target(),
                            );
                        }
                        if overlap_obj_flags.intersects(ObjectFlags::Anonymous) {
                            return source.maybe_alias_symbol().is_some()
                                && are_option_gcs_equal(
                                    source.maybe_alias_symbol().as_ref(),
                                    target.maybe_alias_symbol().as_ref(),
                                );
                        }
                    }
                    false
                },
            )
            .map(Clone::clone);
        }
        None
    }

    pub(super) fn find_best_type_for_object_literal(
        &self,
        source: &Type,
        union_target: &Type, /*UnionOrIntersectionType*/
    ) -> io::Result<Option<Gc<Type>>> {
        if get_object_flags(source).intersects(ObjectFlags::ObjectLiteral)
            && self.try_some_type(union_target, |type_: &Type| self.is_array_like_type(type_))?
        {
            return Ok(try_find(
                union_target
                    .as_union_or_intersection_type_interface()
                    .types(),
                |t: &Gc<Type>, _| -> io::Result<_> { Ok(!self.is_array_like_type(t)?) },
            )?
            .cloned());
        }
        Ok(None)
    }

    pub(super) fn find_best_type_for_invokable(
        &self,
        source: &Type,
        union_target: &Type, /*UnionOrIntersectionType*/
    ) -> io::Result<Option<Gc<Type>>> {
        let mut signature_kind = SignatureKind::Call;
        let has_signatures = !self
            .get_signatures_of_type(source, signature_kind)?
            .is_empty()
            || {
                signature_kind = SignatureKind::Construct;
                !self
                    .get_signatures_of_type(source, signature_kind)?
                    .is_empty()
            };
        if has_signatures {
            return Ok(try_find(
                union_target
                    .as_union_or_intersection_type_interface()
                    .types(),
                |t: &Gc<Type>, _| -> io::Result<_> {
                    Ok(!self.get_signatures_of_type(t, signature_kind)?.is_empty())
                },
            )?
            .cloned());
        }
        Ok(None)
    }

    pub(super) fn find_most_overlappy_type(
        &self,
        source: &Type,
        union_target: &Type, /*UnionOrIntersectionType*/
    ) -> io::Result<Option<Gc<Type>>> {
        let mut best_match: Option<Gc<Type>> = None;
        let mut matching_count = 0;
        for target in union_target
            .as_union_or_intersection_type_interface()
            .types()
        {
            let overlap = self.get_intersection_type(
                &vec![
                    self.get_index_type(source, None, None)?,
                    self.get_index_type(target, None, None)?,
                ],
                Option::<&Symbol>::None,
                None,
            )?;
            if overlap.flags().intersects(TypeFlags::Index) {
                best_match = Some(target.clone());
                matching_count = usize::MAX;
            } else if overlap.flags().intersects(TypeFlags::Union) {
                let len = length(Some(&filter(
                    overlap.as_union_or_intersection_type_interface().types(),
                    |type_: &Gc<Type>| self.is_unit_type(type_),
                )));
                if len >= matching_count {
                    best_match = Some(target.clone());
                    matching_count = len;
                }
            } else if self.is_unit_type(&overlap) && 1 >= matching_count {
                best_match = Some(target.clone());
                matching_count = 1;
            }
        }
        Ok(best_match)
    }

    pub(super) fn filter_primitives_if_contains_non_primitive(
        &self,
        type_: &Type, /*UnionType*/
    ) -> Gc<Type> {
        if self.maybe_type_of_kind(type_, TypeFlags::NonPrimitive) {
            let result = self.filter_type(type_, |t: &Type| {
                !t.flags().intersects(TypeFlags::Primitive)
            });
            if !result.flags().intersects(TypeFlags::Never) {
                return result;
            }
        }
        type_.type_wrapper()
    }

    pub(super) fn find_matching_discriminant_type(
        &self,
        source: &Type,
        target: &Type,
        mut is_related_to: impl FnMut(&Type, &Type) -> io::Result<Ternary>,
        skip_partial: Option<bool>,
    ) -> io::Result<Option<Gc<Type>>> {
        if target.flags().intersects(TypeFlags::Union)
            && source
                .flags()
                .intersects(TypeFlags::Intersection | TypeFlags::Object)
        {
            let match_ = self.get_matching_union_constituent_for_type(target, source)?;
            if match_.is_some() {
                return Ok(match_);
            }
            let source_properties = self.get_properties_of_type(source)?;
            // if (sourceProperties) {
            let source_properties_filtered =
                self.find_discriminant_properties(source_properties, target)?;
            if let Some(source_properties_filtered) = source_properties_filtered.as_ref() {
                return self.discriminate_type_by_discriminable_items(
                    target,
                    source_properties_filtered.into_iter().map(|p| {
                        let p_clone = p.clone();
                        let type_checker = self.rc_wrapper();
                        (
                            Box::new(move || type_checker.get_type_of_symbol(&p_clone))
                                as Box<dyn Fn() -> io::Result<Gc<Type>>>,
                            p.escaped_name().to_owned(),
                        )
                    }),
                    |source: &Type, target: &Type| {
                        Ok(is_related_to(source, target)? != Ternary::False)
                    },
                    Option::<&Type>::None,
                    skip_partial,
                );
            }
            // }
        }
        Ok(None)
    }
}

#[derive(Debug, Trace, Finalize)]
pub(super) struct EmitResolverCreateResolver {
    type_checker: Gc<TypeChecker>,
    file_to_directive: Option<HashMap<String, String>>,
}

impl EmitResolverCreateResolver {
    pub(super) fn new(type_checker: Gc<TypeChecker>) -> Self {
        let resolved_type_reference_directives =
            type_checker.host.get_resolved_type_reference_directives();
        let mut ret = Self {
            type_checker: type_checker.clone(),
            file_to_directive: Default::default(),
        };
        // if (resolvedTypeReferenceDirectives) {
        ret.file_to_directive = Some(Default::default());
        (*resolved_type_reference_directives)
            .borrow()
            .iter()
            .for_each(|(key, resolved_directive)| {
                if resolved_directive.is_none() {
                    return;
                }
                let resolved_directive = resolved_directive.clone().unwrap();
                let resolved_directive_resolved_file_name =
                    resolved_directive.resolved_file_name.as_ref();
                if resolved_directive_resolved_file_name.non_empty().is_none() {
                    return;
                }
                let resolved_directive_resolved_file_name =
                    resolved_directive_resolved_file_name.unwrap();
                let file = type_checker
                    .host
                    .get_source_file(resolved_directive_resolved_file_name);
                if let Some(file) = file {
                    ret.add_referenced_files_to_type_directive(&file, key);
                }
            });
        // }
        ret
    }

    pub(super) fn is_in_heritage_clause(
        &self,
        node: &Node, /*PropertyAccessEntityNameExpression*/
    ) -> bool {
        matches!(
            node.maybe_parent(),
            Some(node_parent) if node_parent.kind() == SyntaxKind::ExpressionWithTypeArguments &&
                matches!(
                    node_parent.maybe_parent(),
                    Some(node_parent_parent) if node_parent_parent.kind() == SyntaxKind::HeritageClause
                )
        )
    }

    pub(super) fn is_symbol_from_type_declaration_file(&self, symbol: &Symbol) -> io::Result<bool> {
        let symbol_declarations = symbol.maybe_declarations();
        if symbol_declarations.is_none() {
            return Ok(false);
        }
        let symbol_declarations = symbol_declarations.as_ref().unwrap();

        let mut current = symbol.symbol_wrapper();
        loop {
            let parent = self.type_checker.get_parent_of_symbol(&current)?;
            if let Some(parent) = parent {
                current = parent;
            } else {
                break;
            }
        }

        if matches!(
            current.maybe_value_declaration(),
            Some(current_value_declaration) if current_value_declaration.kind() == SyntaxKind::SourceFile
        ) && current.flags().intersects(SymbolFlags::ValueModule)
        {
            return Ok(false);
        }

        for decl in symbol_declarations {
            let file = get_source_file_of_node(decl);
            if self
                .file_to_directive
                .as_ref()
                .unwrap()
                .contains_key(&**file.as_source_file().path())
            {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn add_referenced_files_to_type_directive(
        &mut self,
        file: &Node, /*SourceFile*/
        key: &str,
    ) {
        let file_as_source_file = file.as_source_file();
        if self
            .file_to_directive
            .as_ref()
            .unwrap()
            .contains_key(&**file_as_source_file.path())
        {
            return;
        }
        self.file_to_directive
            .as_mut()
            .unwrap()
            .insert((&**file_as_source_file.path()).to_owned(), key.to_owned());
        for file_reference in &*(*file_as_source_file.referenced_files()).borrow() {
            let file_name = &file_reference.file_name;
            let resolved_file =
                resolve_tripleslash_reference(file_name, &file_as_source_file.file_name());
            let referenced_file = self.type_checker.host.get_source_file(&resolved_file);
            if let Some(ref referenced_file) = referenced_file {
                self.add_referenced_files_to_type_directive(referenced_file, key);
            }
        }
    }
}

impl EmitResolver for EmitResolverCreateResolver {
    fn has_global_name(&self, name: &str) -> bool {
        self.type_checker.has_global_name(name)
    }

    fn get_referenced_export_container(
        &self,
        node: &Node, /*Identifier*/
        prefix_locals: Option<bool>,
    ) -> io::Result<Option<Gc<Node /*SourceFile | ModuleDeclaration | EnumDeclaration*/>>> {
        self.type_checker
            .get_referenced_export_container(node, prefix_locals)
    }

    fn get_referenced_import_declaration(
        &self,
        node: &Node, /*Identifier*/
    ) -> io::Result<Option<Gc<Node /*Declaration*/>>> {
        self.type_checker.get_referenced_import_declaration(node)
    }

    fn get_referenced_declaration_with_colliding_name(
        &self,
        node: &Node, /*Identifier*/
    ) -> io::Result<Option<Gc<Node /*Declaration*/>>> {
        self.type_checker
            .get_referenced_declaration_with_colliding_name(node)
    }

    fn is_declaration_with_colliding_name(
        &self,
        node: &Node, /*Declaration*/
    ) -> io::Result<bool> {
        self.type_checker.is_declaration_with_colliding_name(node)
    }

    fn is_value_alias_declaration(&self, node_in: &Node) -> io::Result<bool> {
        let node = get_parse_tree_node(Some(node_in), Option::<fn(&Node) -> bool>::None);
        node.as_ref().try_map_or(false, |node| {
            self.type_checker.is_value_alias_declaration(node)
        })
    }

    fn is_referenced_alias_declaration(
        &self,
        node_in: &Node,
        check_children: Option<bool>,
    ) -> io::Result<bool> {
        let node = get_parse_tree_node(Some(node_in), Option::<fn(&Node) -> bool>::None);
        node.as_ref().try_map_or(false, |node| {
            self.type_checker
                .is_referenced_alias_declaration(node, check_children)
        })
    }

    fn is_top_level_value_import_equals_with_entity_name(
        &self,
        node: &Node, /*ImportEqualsDeclaration*/
    ) -> io::Result<bool> {
        self.type_checker
            .is_top_level_value_import_equals_with_entity_name(node)
    }

    fn get_node_check_flags(&self, node_in: &Node) -> NodeCheckFlags {
        let node = get_parse_tree_node(Some(node_in), Option::<fn(&Node) -> bool>::None);
        node.as_ref().map_or(NodeCheckFlags::None, |node| {
            self.type_checker.get_node_check_flags(node)
        })
    }

    fn is_declaration_visible(&self, node: &Node /*Declaration | AnyImportSyntax*/) -> bool {
        self.type_checker.is_declaration_visible(node)
    }

    fn is_late_bound(&self, node: &Node /*Declaration*/) -> io::Result<bool> {
        let node = get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None);
        let symbol = node
            .as_ref()
            .try_and_then(|node| self.type_checker.get_symbol_of_node(node))?;
        Ok(matches!(
            symbol.as_ref(),
            Some(symbol) if get_check_flags(symbol).intersects(CheckFlags::Late)
        ))
    }

    fn collect_linked_aliases(
        &self,
        node: &Node, /*Identifier*/
        set_visibility: Option<bool>,
    ) -> io::Result<Option<Vec<Gc<Node>>>> {
        self.type_checker
            .collect_linked_aliases(node, set_visibility)
    }

    fn is_implementation_of_overload(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> io::Result<Option<bool>> {
        self.type_checker.is_implementation_of_overload(node)
    }

    fn is_required_initialized_parameter(
        &self,
        node: &Node, /*ParameterDeclaration*/
    ) -> io::Result<bool> {
        self.type_checker.is_required_initialized_parameter(node)
    }

    fn is_optional_uninitialized_parameter_property(
        &self,
        node: &Node, /*ParameterDeclaration*/
    ) -> io::Result<bool> {
        self.type_checker
            .is_optional_uninitialized_parameter_property(node)
    }

    fn is_expando_function_declaration(
        &self,
        node: &Node, /*FunctionDeclaration*/
    ) -> io::Result<bool> {
        self.type_checker.is_expando_function_declaration(node)
    }

    fn get_properties_of_container_function(
        &self,
        node: &Node, /*Declaration*/
    ) -> io::Result<Vec<Gc<Symbol>>> {
        Ok(self
            .type_checker
            .get_properties_of_container_function(node)?
            .collect_vec())
    }

    fn create_type_of_declaration(
        &self,
        declaration: &Node, /*AccessorDeclaration | VariableLikeDeclaration | PropertyAccessExpression*/
        enclosing_declaration: &Node,
        flags: NodeBuilderFlags,
        tracker: Gc<Box<dyn SymbolTracker>>,
        add_undefined: Option<bool>,
    ) -> io::Result<Option<Gc<Node /*TypeNode*/>>> {
        self.type_checker.create_type_of_declaration(
            declaration,
            enclosing_declaration,
            flags,
            tracker,
            add_undefined,
        )
    }

    fn create_return_type_of_signature_declaration(
        &self,
        signature_declaration: &Node, /*SignatureDeclaration*/
        enclosing_declaration: &Node,
        flags: NodeBuilderFlags,
        tracker: Gc<Box<dyn SymbolTracker>>,
    ) -> io::Result<Option<Gc<Node /*TypeNode*/>>> {
        self.type_checker
            .create_return_type_of_signature_declaration(
                signature_declaration,
                enclosing_declaration,
                flags,
                tracker,
            )
    }

    fn create_type_of_expression(
        &self,
        expr: &Node, /*Expression*/
        enclosing_declaration: &Node,
        flags: NodeBuilderFlags,
        tracker: Gc<Box<dyn SymbolTracker>>,
    ) -> io::Result<Option<Gc<Node /*TypeNode*/>>> {
        self.type_checker
            .create_type_of_expression(expr, enclosing_declaration, flags, tracker)
    }

    fn create_literal_const_value(
        &self,
        node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
        tracker: Gc<Box<dyn SymbolTracker>>,
    ) -> io::Result<Gc<Node /*Expression*/>> {
        self.type_checker.create_literal_const_value(node, tracker)
    }

    fn is_symbol_accessible(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<&Node>,
        meaning: Option<SymbolFlags>,
        should_compute_alias_to_mark_visible: bool,
    ) -> io::Result<SymbolAccessibilityResult> {
        self.type_checker.is_symbol_accessible(
            Some(symbol),
            enclosing_declaration,
            meaning.unwrap(),
            should_compute_alias_to_mark_visible,
        )
    }

    fn is_entity_name_visible(
        &self,
        entity_name: &Node, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: &Node,
    ) -> io::Result<SymbolVisibilityResult> {
        self.type_checker
            .is_entity_name_visible(entity_name, enclosing_declaration)
    }

    fn get_constant_value(
        &self,
        node_in: &Node, /*EnumMember | PropertyAccessExpression | ElementAccessExpression*/
    ) -> io::Result<Option<StringOrNumber>> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| self.type_checker.can_have_constant_value(node)),
        );
        node.as_ref()
            .try_and_then(|node| self.type_checker.get_constant_value(node))
    }

    fn get_referenced_value_declaration(
        &self,
        reference: &Node, /*Identifier*/
    ) -> io::Result<Option<Gc<Node /*Declaration*/>>> {
        self.type_checker
            .get_referenced_value_declaration(reference)
    }

    fn get_type_reference_serialization_kind(
        &self,
        type_name: &Node, /*EntityName*/
        location: Option<&Node>,
    ) -> io::Result<TypeReferenceSerializationKind> {
        self.type_checker
            .get_type_reference_serialization_kind(type_name, location)
    }

    fn is_optional_parameter(&self, node: &Node /*ParameterDeclaration*/) -> io::Result<bool> {
        self.type_checker.is_optional_parameter(node)
    }

    fn module_exports_some_value(
        &self,
        module_reference_expression: &Node, /*Expression*/
    ) -> io::Result<bool> {
        self.type_checker
            .module_exports_some_value(module_reference_expression)
    }

    fn is_arguments_local_binding(&self, node: &Node /*Identifier*/) -> io::Result<bool> {
        self.type_checker.is_arguments_local_binding(node)
    }

    fn get_external_module_file_from_declaration(
        &self,
        node_in: &Node, /*ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration | ImportTypeNode | ImportCall*/
    ) -> io::Result<Option<Gc<Node /*SourceFile*/>>> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| has_possible_external_module_reference(node)),
        );
        node.as_ref().try_and_then(|node| {
            self.type_checker
                .get_external_module_file_from_declaration(node)
        })
    }

    fn get_type_reference_directives_for_entity_name(
        &self,
        node: &Node, /*EntityNameOrEntityNameExpression*/
    ) -> io::Result<Option<Vec<String>>> {
        // if (!fileToDirective) {
        //     return undefined;
        // }
        let mut meaning = SymbolFlags::Type | SymbolFlags::Namespace;
        if node.kind() == SyntaxKind::Identifier && self.type_checker.is_in_type_query(node)
            || node.kind() == SyntaxKind::PropertyAccessExpression
                && !self.is_in_heritage_clause(node)
        {
            meaning = SymbolFlags::Value | SymbolFlags::ExportValue;
        }

        let symbol = self.type_checker.resolve_entity_name(
            node,
            meaning,
            Some(true),
            None,
            Option::<&Node>::None,
        )?;
        symbol
            .filter(|symbol| !Gc::ptr_eq(symbol, &self.type_checker.unknown_symbol()))
            .try_and_then(|ref symbol| -> io::Result<_> {
                self.get_type_reference_directives_for_symbol(symbol, Some(meaning))
            })
    }

    fn get_type_reference_directives_for_symbol(
        &self,
        symbol: &Symbol,
        meaning: Option<SymbolFlags>,
    ) -> io::Result<Option<Vec<String>>> {
        let file_to_directive = return_ok_default_if_none!(self.file_to_directive.as_ref());
        if !self.is_symbol_from_type_declaration_file(symbol)? {
            return Ok(None);
        }
        let mut type_reference_directives: Option<Vec<String>> = Default::default();
        for decl in symbol.maybe_declarations().as_ref().unwrap() {
            if decl
                .maybe_symbol()
                .filter(|decl_symbol| {
                    matches!(
                        meaning,
                        Some(meaning) if decl_symbol.flags().intersects(meaning)
                    )
                })
                .is_some()
            {
                let file = get_source_file_of_node(decl);
                let type_reference_directive =
                    file_to_directive.get(&**file.as_source_file().path());
                if let Some(type_reference_directive) = type_reference_directive.non_empty() {
                    type_reference_directives
                        .get_or_insert_with(|| vec![])
                        .push(type_reference_directive.clone());
                } else {
                    return Ok(None);
                }
            }
        }
        Ok(type_reference_directives)
    }

    fn is_literal_const_declaration(
        &self,
        node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
    ) -> io::Result<bool> {
        self.type_checker.is_literal_const_declaration(node)
    }

    fn get_jsx_factory_entity(&self, location: Option<&Node>) -> Option<Gc<Node /*EntityName*/>> {
        self.type_checker.get_jsx_factory_entity(location.unwrap())
    }

    fn get_jsx_fragment_factory_entity(
        &self,
        location: Option<&Node>,
    ) -> Option<Gc<Node /*EntityName*/>> {
        self.type_checker
            .get_jsx_fragment_factory_entity(location.unwrap())
    }

    fn get_all_accessor_declarations(
        &self,
        accessor: &Node, /*AccessorDeclaration*/
    ) -> io::Result<AllAccessorDeclarations> {
        let ref accessor =
            get_parse_tree_node(Some(accessor), Some(is_get_or_set_accessor_declaration)).unwrap();
        let other_kind = if accessor.kind() == SyntaxKind::SetAccessor {
            SyntaxKind::GetAccessor
        } else {
            SyntaxKind::SetAccessor
        };
        let other_accessor = get_declaration_of_kind(
            &self.type_checker.get_symbol_of_node(accessor)?.unwrap(),
            other_kind,
        );
        let first_accessor = other_accessor
            .clone()
            .filter(|other_accessor| other_accessor.pos() < accessor.pos())
            .unwrap_or_else(|| accessor.clone());
        let second_accessor = if matches!(
            other_accessor.as_ref(),
            Some(other_accessor) if other_accessor.pos() < accessor.pos()
        ) {
            Some(accessor.clone())
        } else {
            other_accessor.clone()
        };
        let set_accessor = if accessor.kind() == SyntaxKind::SetAccessor {
            Some(accessor.clone())
        } else {
            other_accessor.clone()
        };
        let get_accessor = if accessor.kind() == SyntaxKind::GetAccessor {
            Some(accessor.clone())
        } else {
            other_accessor.clone()
        };
        Ok(AllAccessorDeclarations {
            first_accessor,
            second_accessor,
            get_accessor,
            set_accessor,
        })
    }

    fn get_symbol_of_external_module_specifier(
        &self,
        module_name: &Node, /*StringLiteralLike*/
    ) -> io::Result<Option<Gc<Symbol>>> {
        self.type_checker
            .resolve_external_module_name_worker(module_name, module_name, None, None)
    }

    fn is_binding_captured_by_node(
        &self,
        node: &Node,
        decl: &Node, /*VariableDeclaration | BindingElement*/
    ) -> io::Result<bool> {
        let parse_node = get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None);
        let parse_decl = get_parse_tree_node(Some(decl), Option::<fn(&Node) -> bool>::None);
        Ok(matches!(
            (parse_node, parse_decl),
            (Some(ref parse_node), Some(ref parse_decl)) if (
                is_variable_declaration(parse_decl) ||
                is_binding_element(parse_decl)
            ) && self.type_checker.is_binding_captured_by_node(parse_node, parse_decl)?
        ))
    }

    fn get_declaration_statements_for_source_file(
        &self,
        node: &Node, /*SourceFile*/
        flags: NodeBuilderFlags,
        tracker: Gc<Box<dyn SymbolTracker>>,
        bundled: Option<bool>,
    ) -> io::Result<Option<Vec<Gc<Node /*Statement*/>>>> {
        let n = get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None);
        Debug_.assert(
            matches!(
                n.as_ref(),
                Some(n) if n.kind() == SyntaxKind::SourceFile
            ),
            Some("Non-sourcefile node passed into getDeclarationsForSourceFile"),
        );
        let sym = self.type_checker.get_symbol_of_node(node)?;
        if sym.is_none() {
            return Ok(match node.maybe_locals().as_ref() {
                None => Some(vec![]),
                Some(node_locals) => self
                    .type_checker
                    .node_builder()
                    .symbol_table_to_declaration_statements(
                        node_locals.clone(),
                        Some(node),
                        Some(flags),
                        Some(tracker.clone()),
                        bundled,
                    )?,
            });
        }
        let sym = sym.unwrap();
        let ret = match sym.maybe_exports().as_ref() {
            None => Some(vec![]),
            Some(sym_exports) => self
                .type_checker
                .node_builder()
                .symbol_table_to_declaration_statements(
                    sym_exports.clone(),
                    Some(node),
                    Some(flags),
                    Some(tracker.clone()),
                    bundled,
                )?,
        };
        Ok(ret)
    }

    fn is_import_required_by_augmentation(
        &self,
        node: &Node, /*ImportDeclaration*/
    ) -> io::Result<bool> {
        let ref file = get_source_file_of_node(node);
        let file_symbol = file.maybe_symbol();
        if file_symbol.is_none() {
            return Ok(false);
        }
        let ref file_symbol = file_symbol.unwrap();
        let ref import_target = return_ok_default_if_none!(self
            .type_checker
            .get_external_module_file_from_declaration(node)?);
        if Gc::ptr_eq(import_target, file) {
            return Ok(false);
        }
        let exports = self.type_checker.get_exports_of_module_(file_symbol)?;
        for s in (*exports).borrow().values() {
            if s.maybe_merge_id().is_some() {
                let merged = self.type_checker.get_merged_symbol(Some(&**s)).unwrap();
                if let Some(merged_declarations) = merged.maybe_declarations().as_ref() {
                    for d in merged_declarations {
                        let ref decl_file = get_source_file_of_node(d);
                        if Gc::ptr_eq(decl_file, import_target) {
                            return Ok(true);
                        }
                    }
                };
            }
        }
        Ok(false)
    }
}

pub(super) fn is_not_accessor(declaration: &Node /*Declaration*/) -> bool {
    !is_accessor(declaration)
}

pub(super) fn is_not_overload(declaration: &Node /*Declaration*/) -> bool {
    !matches!(
        declaration.kind(),
        SyntaxKind::FunctionDeclaration | SyntaxKind::MethodDeclaration
    ) || declaration
        .as_function_like_declaration()
        .maybe_body()
        .is_some()
}

pub(super) fn is_declaration_name_or_import_property_name(name: &Node) -> bool {
    match name.parent().kind() {
        SyntaxKind::ImportSpecifier | SyntaxKind::ExportSpecifier => is_identifier(name),
        _ => is_declaration_name(name),
    }
}

pub(super) mod JsxNames {
    pub const JSX: &'static str /*__String*/ = "JSX";
    pub const IntrinsicElements: &'static str /*__String*/ = "IntrinsicElements";
    pub const ElementClass: &'static str /*__String*/ = "ElementClass";
    pub const ElementAttributesPropertyNameContainer: &'static str /*__String*/ = "ElementAttributesProperty";
    pub const ElementChildrenAttributeNameContainer: &'static str /*__String*/ = "ElementChildrenAttribute";
    pub const Element: &'static str /*__String*/ = "Element";
    pub const IntrinsicAttributes: &'static str /*__String*/ = "IntrinsicAttributes";
    pub const IntrinsicClassAttributes: &'static str /*__String*/ = "IntrinsicClassAttributes";
    pub const LibraryManagedAttributes: &'static str /*__String*/ = "LibraryManagedAttributes";
}

pub(super) fn get_iteration_types_key_from_iteration_type_kind(
    type_kind: IterationTypeKind,
) -> IterationTypesKey {
    match type_kind {
        IterationTypeKind::Yield => IterationTypesKey::YieldType,
        IterationTypeKind::Return => IterationTypesKey::ReturnType,
        IterationTypeKind::Next => IterationTypesKey::NextType,
    }
}

pub(super) fn signature_has_rest_parameter(s: &Signature) -> bool {
    s.flags.intersects(SignatureFlags::HasRestParameter)
}

pub(super) fn signature_has_literal_types(s: &Signature) -> bool {
    s.flags.intersects(SignatureFlags::HasLiteralTypes)
}
