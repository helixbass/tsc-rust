use std::{
    cell::Ref,
    collections::{HashMap, HashSet},
    convert::TryInto,
    io, ptr,
};

use gc::Gc;
use id_arena::Id;

use super::PipelinePhase;
use crate::{
    cast_present, create_text_writer, get_emit_flags, get_line_starts, get_snippet_element,
    get_trailing_semicolon_deferring_writer, is_bundle_file_text_like, is_declaration,
    is_empty_statement, is_expression, is_identifier, is_in_json_file, is_internal_declaration,
    is_keyword, is_source_file, is_string_literal, is_token_kind, is_type_parameter_declaration,
    is_unparsed_prepend, is_unparsed_source, is_variable_statement, BundleFileSection,
    BundleFileSectionKind, CurrentParenthesizerRule, Debug_, EmitFlags, EmitHint, EmitTextWriter,
    GetOrInsertDefault, Node, NodeInterface, Printer, SourceMapGenerator, SyntaxKind, TempFlags,
    InArena,
};

impl Printer {
    pub(super) fn record_bundle_file_internal_section_start(
        &self,
        node: Id<Node>,
    ) -> Option<BundleFileSectionKind> {
        if self.record_internal_section == Some(true)
            && self.maybe_bundle_file_info().is_some()
            && matches!(
                self.maybe_current_source_file(),
                Some(current_source_file) if (
                    is_declaration(node, self) ||
                    is_variable_statement(&node.ref_(self))
                ) && is_internal_declaration(
                    node,
                    current_source_file,
                    self,
                )
            )
            && self.source_file_text_kind() != BundleFileSectionKind::Internal
        {
            let prev_source_file_text_kind = self.source_file_text_kind();
            self.record_bundle_file_text_like_section(self.writer().get_text_pos());
            self.set_source_file_text_pos(self.get_text_pos_with_write_line());
            self.set_source_file_text_kind(BundleFileSectionKind::Internal);
            return Some(prev_source_file_text_kind);
        }
        None
    }

    pub(super) fn record_bundle_file_internal_section_end(
        &self,
        prev_source_file_text_kind: Option<BundleFileSectionKind>,
    ) {
        if let Some(prev_source_file_text_kind) = prev_source_file_text_kind {
            self.record_bundle_file_text_like_section(self.writer().get_text_pos());
            self.set_source_file_text_pos(self.get_text_pos_with_write_line());
            self.set_source_file_text_kind(prev_source_file_text_kind);
        }
    }

    pub(super) fn record_bundle_file_text_like_section(&self, end: usize) -> bool {
        if self.source_file_text_pos() < end {
            self.update_or_push_bundle_file_text_like(
                self.source_file_text_pos().try_into().unwrap(),
                end.try_into().unwrap(),
                self.source_file_text_kind(),
            );
            return true;
        }
        false
    }

    pub fn write_bundle(
        &self,
        bundle: Id<Node>, /*Bundle*/
        output: Gc<Box<dyn EmitTextWriter>>,
        source_map_generator: Option<Gc<Box<dyn SourceMapGenerator>>>,
    ) -> io::Result<()> {
        self.set_is_own_file_emit(false);
        let previous_writer = self.maybe_writer();
        self.set_writer(Some(output), source_map_generator);
        self.emit_shebang_if_needed(bundle);
        self.emit_prologue_directives_if_needed(bundle)?;
        self.emit_helpers(bundle);
        self.emit_synthetic_triple_slash_references_if_needed(bundle)?;

        let bundle_ref = bundle.ref_(self);
        let bundle_as_bundle = bundle_ref.as_bundle();
        for &prepend in &bundle_as_bundle.prepends {
            self.write_line(None);
            let pos = self.writer().get_text_pos();
            let bundle_file_info = self.maybe_bundle_file_info();
            let saved_sections = bundle_file_info
                .as_ref()
                .map(|bundle_file_info| (**bundle_file_info).borrow().sections.clone());
            if saved_sections.is_some() {
                bundle_file_info.as_ref().unwrap().borrow_mut().sections = vec![];
            }
            self.print(EmitHint::Unspecified, prepend, None)?;
            if let Some(bundle_file_info) = bundle_file_info {
                let mut bundle_file_info = bundle_file_info.borrow_mut();
                let mut new_sections = bundle_file_info.sections.clone();
                bundle_file_info.sections = saved_sections.unwrap();
                if prepend
                    .ref_(self).as_has_old_file_of_current_emit()
                    .maybe_old_file_of_current_emit()
                    == Some(true)
                {
                    bundle_file_info.sections.append(&mut new_sections);
                } else {
                    for section in &new_sections {
                        Debug_.assert(is_bundle_file_text_like(section), None);
                    }
                    bundle_file_info
                        .sections
                        .push(Gc::new(BundleFileSection::new_prepend(
                            self.relative_to_build_info(&prepend.ref_(self).as_unparsed_source().file_name),
                            new_sections,
                            pos.try_into().unwrap(),
                            self.writer().get_text_pos().try_into().unwrap(),
                        )))
                }
            }
        }

        self.set_source_file_text_pos(self.get_text_pos_with_write_line());
        for source_file in &bundle_as_bundle.source_files {
            let source_file = source_file.unwrap();
            self.print(EmitHint::SourceFile, source_file, Some(source_file))?;
        }
        let bundle_file_info = self.maybe_bundle_file_info();
        if let Some(bundle_file_info) = bundle_file_info {
            if !bundle_as_bundle.source_files.is_empty() {
                let end = self.writer().get_text_pos();
                if self.record_bundle_file_text_like_section(end) {
                    let prologues = self.get_prologue_directives_from_bundled_source_files(bundle);
                    if let Some(prologues) = prologues {
                        bundle_file_info
                            .borrow_mut()
                            .sources
                            .get_or_insert_default_()
                            .prologues = Some(prologues);
                    }

                    let helpers = self.get_helpers_from_bundled_source_files(bundle);
                    if let Some(helpers) = helpers {
                        bundle_file_info
                            .borrow_mut()
                            .sources
                            .get_or_insert_default_()
                            .helpers = Some(helpers);
                    }
                }
            }
        }

        self.reset();
        *self.writer.borrow_mut() = previous_writer;

        Ok(())
    }

    pub fn write_unparsed_source(
        &self,
        unparsed: Id<Node>, /*UnparsedSource*/
        output: Gc<Box<dyn EmitTextWriter>>,
    ) -> io::Result<()> {
        let previous_writer = self.maybe_writer();
        self.set_writer(Some(output), None);
        self.print(EmitHint::Unspecified, unparsed, None)?;
        self.reset();
        *self.writer.borrow_mut() = previous_writer;

        Ok(())
    }

    pub fn write_file(
        &self,
        source_file: Id<Node>, /*SourceFile*/
        output: Gc<Box<dyn EmitTextWriter>>,
        source_map_generator: Option<Gc<Box<dyn SourceMapGenerator>>>,
    ) -> io::Result<()> {
        self.set_is_own_file_emit(true);
        let previous_writer = self.maybe_writer();
        self.set_writer(Some(output), source_map_generator);
        self.emit_shebang_if_needed(source_file);
        self.emit_prologue_directives_if_needed(source_file)?;
        self.print(EmitHint::SourceFile, source_file, Some(source_file))?;
        self.reset();
        *self.writer.borrow_mut() = previous_writer;

        Ok(())
    }

    pub(super) fn begin_print(&self) -> Gc<Box<dyn EmitTextWriter>> {
        self.own_writer
            .borrow_mut()
            .get_or_insert_with(|| create_text_writer(&self.new_line).as_dyn_emit_text_writer())
            .clone()
    }

    pub(super) fn end_print(&self) -> String {
        let own_writer = self.own_writer.borrow();
        let own_writer = own_writer.as_ref().unwrap();
        let text = own_writer.get_text();
        own_writer.clear();
        text
    }

    pub(super) fn print(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        source_file: Option<Id<Node> /*SourceFile*/>,
    ) -> io::Result<()> {
        if let Some(source_file) = source_file {
            self.set_source_file(Some(source_file));
        }

        self.pipeline_emit(hint, node, None)?;

        Ok(())
    }

    pub(super) fn set_source_file(&self, source_file: Option<Id<Node> /*SourceFile*/>) {
        *self.current_source_file.borrow_mut() = source_file;
        self.set_current_line_map(None);
        self.set_detached_comments_info(None);
        if let Some(source_file) = source_file {
            self.set_source_map_source(Gc::new(source_file.into()));
        }
    }

    pub(super) fn set_writer(
        &self,
        mut writer: Option<Gc<Box<dyn EmitTextWriter>>>,
        source_map_generator: Option<Gc<Box<dyn SourceMapGenerator>>>,
    ) {
        if let Some(writer_present) = writer.as_ref() {
            if self.printer_options.omit_trailing_semicolon == Some(true) {
                writer = Some(get_trailing_semicolon_deferring_writer(
                    writer_present.clone(),
                ));
            }
        }
        *self.writer.borrow_mut() = writer.clone();
        self.set_source_map_generator(source_map_generator.clone());
        self.set_source_maps_disabled(writer.is_none() || source_map_generator.is_none());
    }

    pub(super) fn reset(&self) {
        self.set_node_id_to_generated_name(HashMap::new());
        self.set_auto_generated_id_to_generated_name(HashMap::new());
        self.set_generated_names(HashSet::new());
        self.set_temp_flags_stack(vec![]);
        self.set_temp_flags(TempFlags::Auto);
        self.set_reserved_names_stack(vec![]);
        self.set_current_source_file(None);
        self.set_current_line_map(None);
        self.set_detached_comments_info(None);
        self.set_writer(None, None);
    }

    pub(super) fn get_current_line_map(&self) -> Ref<Vec<usize>> {
        if self.maybe_current_line_map().is_none() {
            self.set_current_line_map(Some(
                get_line_starts(self.current_source_file().ref_(self).as_source_file()).clone(),
            ));
        }
        self.current_line_map()
    }

    pub(super) fn emit(
        &self,
        node: Option<Id<Node>>,
        parenthesizer_rule: Option<Gc<Box<dyn CurrentParenthesizerRule>>>,
    ) -> io::Result<()> {
        if node.is_none() {
            return Ok(());
        }
        let node = node.unwrap();
        let prev_source_file_text_kind = self.record_bundle_file_internal_section_start(node);
        self.pipeline_emit(EmitHint::Unspecified, node, parenthesizer_rule)?;
        self.record_bundle_file_internal_section_end(prev_source_file_text_kind);

        Ok(())
    }

    pub(super) fn emit_identifier_name(
        &self,
        node: Option<Id<Node> /*Identifier*/>,
    ) -> io::Result<()> {
        if node.is_none() {
            return Ok(());
        }
        let node = node.unwrap();
        self.pipeline_emit(EmitHint::IdentifierName, node, None)?;

        Ok(())
    }

    pub(super) fn emit_expression(
        &self,
        node: Option<Id<Node> /*Expression*/>,
        parenthesizer_rule: Option<Gc<Box<dyn CurrentParenthesizerRule>>>,
    ) -> io::Result<()> {
        if node.is_none() {
            return Ok(());
        }
        let node = node.unwrap();
        self.pipeline_emit(EmitHint::Expression, node, parenthesizer_rule)?;

        Ok(())
    }

    pub(super) fn emit_jsx_attribute_value(
        &self,
        node: Id<Node>, /*StringLiteral | JsxExpression*/
    ) -> io::Result<()> {
        self.pipeline_emit(
            if is_string_literal(&node.ref_(self)) {
                EmitHint::JsxAttributeValue
            } else {
                EmitHint::Unspecified
            },
            node,
            None,
        )?;

        Ok(())
    }

    pub(super) fn before_emit_node(&self, node: Id<Node>) {
        if self.maybe_preserve_source_newlines() == Some(true)
            && get_emit_flags(&node.ref_(self)).intersects(EmitFlags::IgnoreSourceNewlines)
        {
            self.set_preserve_source_newlines(Some(false));
        }
    }

    pub(super) fn after_emit_node(&self, saved_preserve_source_newlines: Option<bool>) {
        self.set_preserve_source_newlines(saved_preserve_source_newlines);
    }

    pub(super) fn pipeline_emit(
        &self,
        emit_hint: EmitHint,
        node: Id<Node>,
        parenthesizer_rule: Option<Gc<Box<dyn CurrentParenthesizerRule>>>,
    ) -> io::Result<()> {
        self.set_current_parenthesizer_rule(parenthesizer_rule);
        let pipeline_phase =
            self.get_pipeline_phase(PipelinePhase::Notification, emit_hint, node)?;
        pipeline_phase(self, emit_hint, node)?;
        self.set_current_parenthesizer_rule(None);

        Ok(())
    }

    pub(super) fn should_emit_comments(&self, node: Id<Node>) -> bool {
        !self.comments_disabled() && !is_source_file(&node.ref_(self))
    }

    pub(super) fn should_emit_source_maps(&self, node: Id<Node>) -> bool {
        !self.source_maps_disabled()
            && !is_source_file(&node.ref_(self))
            && !is_in_json_file(Some(&node.ref_(self)))
            && !is_unparsed_source(&node.ref_(self))
            && !is_unparsed_prepend(&node.ref_(self))
    }

    pub(super) fn get_pipeline_phase(
        &self,
        phase: PipelinePhase,
        emit_hint: EmitHint,
        node: Id<Node>,
    ) -> io::Result<fn(&Printer, EmitHint, Id<Node>) -> io::Result<()>> {
        if phase == PipelinePhase::Notification {
            if !self.is_on_emit_node_no_emit_notification()
                && match self.is_emit_notification_enabled(node) {
                    None => true,
                    Some(is_emit_notification_enabled) => is_emit_notification_enabled,
                }
            {
                return Ok(Printer::pipeline_emit_with_notification);
            }
        }
        if matches!(
            phase,
            PipelinePhase::Notification | PipelinePhase::Substitution
        ) {
            if !self.is_substitute_node_no_emit_substitution() {
                let last_substitution = self
                    .substitute_node(emit_hint, node)?
                    .unwrap_or(node);
                self.set_last_substitution(Some(last_substitution.clone()));
                if last_substitution != node {
                    if let Some(current_parenthesizer_rule) =
                        self.maybe_current_parenthesizer_rule().as_ref()
                    {
                        self.set_last_substitution(Some(
                            current_parenthesizer_rule.call(last_substitution),
                        ));
                    }
                    return Ok(Printer::pipeline_emit_with_substitution);
                }
            }
        }
        if matches!(
            phase,
            PipelinePhase::Notification | PipelinePhase::Substitution | PipelinePhase::Comments
        ) {
            if self.should_emit_comments(node) {
                return Ok(Printer::pipeline_emit_with_comments);
            }
        }
        if matches!(
            phase,
            PipelinePhase::Notification
                | PipelinePhase::Substitution
                | PipelinePhase::Comments
                | PipelinePhase::SourceMaps
        ) {
            if self.should_emit_source_maps(node) {
                return Ok(Printer::pipeline_emit_with_source_maps);
            }
        }
        if matches!(
            phase,
            PipelinePhase::Notification
                | PipelinePhase::Substitution
                | PipelinePhase::Comments
                | PipelinePhase::SourceMaps
                | PipelinePhase::Emit
        ) {
            return Ok(Printer::pipeline_emit_with_hint);
        }
        Debug_.assert_never(phase, None);
    }

    pub(super) fn get_next_pipeline_phase(
        &self,
        current_phase: PipelinePhase,
        emit_hint: EmitHint,
        node: Id<Node>,
    ) -> io::Result<fn(&Printer, EmitHint, Id<Node>) -> io::Result<()>> {
        self.get_pipeline_phase(current_phase.incremented(), emit_hint, node)
    }

    pub(super) fn pipeline_emit_with_notification(
        &self,
        hint: EmitHint,
        node: Id<Node>,
    ) -> io::Result<()> {
        let pipeline_phase =
            self.get_next_pipeline_phase(PipelinePhase::Notification, hint, node)?;
        self.on_emit_node(hint, node, &|hint: EmitHint, node: Id<Node>| {
            pipeline_phase(self, hint, node)
        })?;

        Ok(())
    }

    pub(super) fn pipeline_emit_with_hint(&self, hint: EmitHint, node: Id<Node>) -> io::Result<()> {
        self.on_before_emit_node(Some(node));
        if self.maybe_preserve_source_newlines() == Some(true) {
            let saved_preserve_source_newlines = self.maybe_preserve_source_newlines();
            self.before_emit_node(node);
            self.pipeline_emit_with_hint_worker(hint, node, None)?;
            self.after_emit_node(saved_preserve_source_newlines);
        } else {
            self.pipeline_emit_with_hint_worker(hint, node, None)?;
        }
        self.on_after_emit_node(Some(node));
        self.set_current_parenthesizer_rule(None);

        Ok(())
    }

    pub(super) fn pipeline_emit_with_hint_worker(
        &self,
        mut hint: EmitHint,
        mut node: Id<Node>,
        allow_snippets: Option<bool>,
    ) -> io::Result<()> {
        let allow_snippets = allow_snippets.unwrap_or(true);
        if allow_snippets {
            let snippet = get_snippet_element(node);
            if let Some(snippet) = snippet {
                return self.emit_snippet_node(hint, node, snippet);
            }
        }
        if hint == EmitHint::SourceFile {
            return self
                .emit_source_file(cast_present(node, |node: &Id<Node>| is_source_file(&node.ref_(self))));
        }
        if hint == EmitHint::IdentifierName {
            return self.emit_identifier(cast_present(node, |node: &Id<Node>| is_identifier(&node.ref_(self))));
        }
        if hint == EmitHint::JsxAttributeValue {
            return Ok(self.emit_literal(
                cast_present(node, |node: &Id<Node>| is_string_literal(&node.ref_(self))),
                true,
            ));
        }
        if hint == EmitHint::MappedTypeParameter {
            return self.emit_mapped_type_parameter(cast_present(node, |node: &Id<Node>| {
                is_type_parameter_declaration(&node.ref_(self))
            }));
        }
        if hint == EmitHint::EmbeddedStatement {
            Debug_.assert_node(Some(node), Some(|node: Id<Node>| is_empty_statement(&node.ref_(self))), None);
            return Ok(self.emit_empty_statement(true));
        }
        if hint == EmitHint::Unspecified {
            match node.ref_(self).kind() {
                SyntaxKind::TemplateHead
                | SyntaxKind::TemplateMiddle
                | SyntaxKind::TemplateTail => return Ok(self.emit_literal(node, false)),

                SyntaxKind::Identifier => return self.emit_identifier(node),

                SyntaxKind::PrivateIdentifier => return Ok(self.emit_private_identifier(node)),

                SyntaxKind::QualifiedName => return self.emit_qualified_name(node),
                SyntaxKind::ComputedPropertyName => return self.emit_computed_property_name(node),

                SyntaxKind::TypeParameter => return self.emit_type_parameter(node),
                SyntaxKind::Parameter => return self.emit_parameter(node),
                SyntaxKind::Decorator => return self.emit_decorator(node),

                SyntaxKind::PropertySignature => return self.emit_property_signature(node),
                SyntaxKind::PropertyDeclaration => return self.emit_property_declaration(node),
                SyntaxKind::MethodSignature => return self.emit_method_signature(node),
                SyntaxKind::MethodDeclaration => return self.emit_method_declaration(node),
                SyntaxKind::ClassStaticBlockDeclaration => {
                    return self.emit_class_static_block_declaration(node)
                }
                SyntaxKind::Constructor => return self.emit_constructor(node),
                SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                    return self.emit_accessor_declaration(node)
                }
                SyntaxKind::CallSignature => return self.emit_call_signature(node),
                SyntaxKind::ConstructSignature => return self.emit_construct_signature(node),
                SyntaxKind::IndexSignature => return self.emit_index_signature(node),

                SyntaxKind::TypePredicate => return self.emit_type_predicate(node),
                SyntaxKind::TypeReference => return self.emit_type_reference(node),
                SyntaxKind::FunctionType => return self.emit_function_type(node),
                SyntaxKind::ConstructorType => return self.emit_constructor_type(node),
                SyntaxKind::TypeQuery => return self.emit_type_query(node),
                SyntaxKind::TypeLiteral => return self.emit_type_literal(node),
                SyntaxKind::ArrayType => return self.emit_array_type(node),
                SyntaxKind::TupleType => return self.emit_tuple_type(node),
                SyntaxKind::OptionalType => return self.emit_optional_type(node),
                SyntaxKind::UnionType => return self.emit_union_type(node),
                SyntaxKind::IntersectionType => return self.emit_intersection_type(node),
                SyntaxKind::ConditionalType => return self.emit_conditional_type(node),
                SyntaxKind::InferType => return self.emit_infer_type(node),
                SyntaxKind::ParenthesizedType => return self.emit_parenthesized_type(node),
                SyntaxKind::ExpressionWithTypeArguments => {
                    return self.emit_expression_with_type_arguments(node)
                }
                SyntaxKind::ThisType => return Ok(self.emit_this_type()),
                SyntaxKind::TypeOperator => return self.emit_type_operator(node),
                SyntaxKind::IndexedAccessType => return self.emit_indexed_access_type(node),
                SyntaxKind::MappedType => return self.emit_mapped_type(node),
                SyntaxKind::LiteralType => return self.emit_literal_type(node),
                SyntaxKind::NamedTupleMember => return self.emit_named_tuple_member(node),
                SyntaxKind::TemplateLiteralType => return self.emit_template_type(node),
                SyntaxKind::TemplateLiteralTypeSpan => return self.emit_template_type_span(node),
                SyntaxKind::ImportType => return self.emit_import_type_node(node),

                SyntaxKind::ObjectBindingPattern => return self.emit_object_binding_pattern(node),
                SyntaxKind::ArrayBindingPattern => return self.emit_array_binding_pattern(node),
                SyntaxKind::BindingElement => return self.emit_binding_element(node),

                SyntaxKind::TemplateSpan => return self.emit_template_span(node),
                SyntaxKind::SemicolonClassElement => return Ok(self.emit_semicolon_class_element()),

                SyntaxKind::Block => return self.emit_block(node),
                SyntaxKind::VariableStatement => return self.emit_variable_statement(node),
                SyntaxKind::EmptyStatement => return Ok(self.emit_empty_statement(false)),
                SyntaxKind::ExpressionStatement => return self.emit_expression_statement(node),
                SyntaxKind::IfStatement => return self.emit_if_statement(node),
                SyntaxKind::DoStatement => return self.emit_do_statement(node),
                SyntaxKind::WhileStatement => return self.emit_while_statement(node),
                SyntaxKind::ForStatement => return self.emit_for_statement(node),
                SyntaxKind::ForInStatement => return self.emit_for_in_statement(node),
                SyntaxKind::ForOfStatement => return self.emit_for_of_statement(node),
                SyntaxKind::ContinueStatement => return self.emit_continue_statement(node),
                SyntaxKind::BreakStatement => return self.emit_break_statement(node),
                SyntaxKind::ReturnStatement => return self.emit_return_statement(node),
                SyntaxKind::WithStatement => return self.emit_with_statement(node),
                SyntaxKind::SwitchStatement => return self.emit_switch_statement(node),
                SyntaxKind::LabeledStatement => return self.emit_labeled_statement(node),
                SyntaxKind::ThrowStatement => return self.emit_throw_statement(node),
                SyntaxKind::TryStatement => return self.emit_try_statement(node),
                SyntaxKind::DebuggerStatement => return Ok(self.emit_debugger_statement(node)),

                SyntaxKind::VariableDeclaration => return self.emit_variable_declaration(node),
                SyntaxKind::VariableDeclarationList => {
                    return self.emit_variable_declaration_list(node)
                }
                SyntaxKind::FunctionDeclaration => return self.emit_function_declaration(node),
                SyntaxKind::ClassDeclaration => return self.emit_class_declaration(node),
                SyntaxKind::InterfaceDeclaration => return self.emit_interface_declaration(node),
                SyntaxKind::TypeAliasDeclaration => return self.emit_type_alias_declaration(node),
                SyntaxKind::EnumDeclaration => return self.emit_enum_declaration(node),
                SyntaxKind::ModuleDeclaration => return self.emit_module_declaration(node),
                SyntaxKind::ModuleBlock => return self.emit_module_block(node),
                SyntaxKind::CaseBlock => return self.emit_case_block(node),
                SyntaxKind::NamespaceExportDeclaration => {
                    return self.emit_namespace_export_declaration(node)
                }
                SyntaxKind::ImportEqualsDeclaration => {
                    return self.emit_import_equals_declaration(node)
                }
                SyntaxKind::ImportDeclaration => return self.emit_import_declaration(node),
                SyntaxKind::ImportClause => return self.emit_import_clause(node),
                SyntaxKind::NamespaceImport => return self.emit_namespace_import(node),
                SyntaxKind::NamespaceExport => return self.emit_namespace_export(node),
                SyntaxKind::NamedImports => return self.emit_named_imports(node),
                SyntaxKind::ImportSpecifier => return self.emit_import_specifier(node),
                SyntaxKind::ExportAssignment => return self.emit_export_assignment(node),
                SyntaxKind::ExportDeclaration => return self.emit_export_declaration(node),
                SyntaxKind::NamedExports => return self.emit_named_exports(node),
                SyntaxKind::ExportSpecifier => return self.emit_export_specifier(node),
                SyntaxKind::AssertClause => return self.emit_assert_clause(node),
                SyntaxKind::AssertEntry => return self.emit_assert_entry(node),
                SyntaxKind::MissingDeclaration => return Ok(()),

                SyntaxKind::ExternalModuleReference => {
                    return self.emit_external_module_reference(node)
                }

                SyntaxKind::JsxText => return Ok(self.emit_jsx_text(node)),
                SyntaxKind::JsxOpeningElement | SyntaxKind::JsxOpeningFragment => {
                    return self.emit_jsx_opening_element_or_fragment(node)
                }
                SyntaxKind::JsxClosingElement | SyntaxKind::JsxClosingFragment => {
                    return self.emit_jsx_closing_element_or_fragment(node)
                }
                SyntaxKind::JsxAttribute => return self.emit_jsx_attribute(node),
                SyntaxKind::JsxAttributes => return self.emit_jsx_attributes(node),
                SyntaxKind::JsxSpreadAttribute => return self.emit_jsx_spread_attribute(node),
                SyntaxKind::JsxExpression => return self.emit_jsx_expression(node),

                SyntaxKind::CaseClause => return self.emit_case_clause(node),
                SyntaxKind::DefaultClause => return self.emit_default_clause(node),
                SyntaxKind::HeritageClause => return self.emit_heritage_clause(node),
                SyntaxKind::CatchClause => return self.emit_catch_clause(node),

                SyntaxKind::PropertyAssignment => return self.emit_property_assignment(node),
                SyntaxKind::ShorthandPropertyAssignment => {
                    return self.emit_shorthand_property_assignment(node)
                }
                SyntaxKind::SpreadAssignment => return self.emit_spread_assignment(node),

                SyntaxKind::EnumMember => return self.emit_enum_member(node),

                SyntaxKind::UnparsedPrologue => return Ok(self.write_unparsed_node(node)),
                SyntaxKind::UnparsedSource | SyntaxKind::UnparsedPrepend => {
                    return self.emit_unparsed_source_or_prepend(node)
                }
                SyntaxKind::UnparsedText | SyntaxKind::UnparsedInternalText => {
                    return Ok(self.emit_unparsed_text_like(node))
                }
                SyntaxKind::UnparsedSyntheticReference => {
                    return Ok(self.emit_unparsed_synthetic_reference(node))
                }

                SyntaxKind::SourceFile => return self.emit_source_file(node),
                SyntaxKind::Bundle => {
                    Debug_.fail(Some("Bundles should be printed using printBundle"))
                }
                SyntaxKind::InputFiles => Debug_.fail(Some("InputFiles should not be printed")),

                SyntaxKind::JSDocTypeExpression => {
                    return self.emit_jsdoc_type_expression(Some(node))
                }
                SyntaxKind::JSDocNameReference => return self.emit_jsdoc_name_reference(node),
                SyntaxKind::JSDocAllType => return Ok(self.write_punctuation("*")),
                SyntaxKind::JSDocUnknownType => return Ok(self.write_punctuation("?")),
                SyntaxKind::JSDocNullableType => return self.emit_jsdoc_nullable_type(node),
                SyntaxKind::JSDocNonNullableType => {
                    return self.emit_jsdoc_non_nullable_type(node)
                }
                SyntaxKind::JSDocOptionalType => return self.emit_jsdoc_optional_type(node),
                SyntaxKind::JSDocFunctionType => return self.emit_jsdoc_function_type(node),
                SyntaxKind::RestType | SyntaxKind::JSDocVariadicType => {
                    return self.emit_rest_or_jsdoc_variadic_type(node)
                }
                SyntaxKind::JSDocNamepathType => return Ok(()),
                SyntaxKind::JSDocComment => return self.emit_jsdoc(node),
                SyntaxKind::JSDocTypeLiteral => return self.emit_jsdoc_type_literal(node),
                SyntaxKind::JSDocSignature => return self.emit_jsdoc_signature(node),
                SyntaxKind::JSDocTag | SyntaxKind::JSDocClassTag => {
                    return self.emit_jsdoc_simple_tag(node)
                }
                SyntaxKind::JSDocAugmentsTag | SyntaxKind::JSDocImplementsTag => {
                    return self.emit_jsdoc_heritage_tag(node)
                }
                SyntaxKind::JSDocAuthorTag | SyntaxKind::JSDocDeprecatedTag => return Ok(()),
                SyntaxKind::JSDocPublicTag
                | SyntaxKind::JSDocPrivateTag
                | SyntaxKind::JSDocProtectedTag
                | SyntaxKind::JSDocReadonlyTag
                | SyntaxKind::JSDocOverrideTag => return Ok(()),
                SyntaxKind::JSDocCallbackTag => return self.emit_jsdoc_callback_tag(node),
                SyntaxKind::JSDocParameterTag | SyntaxKind::JSDocPropertyTag => {
                    return self.emit_jsdoc_property_like_tag(node)
                }
                SyntaxKind::JSDocEnumTag
                | SyntaxKind::JSDocReturnTag
                | SyntaxKind::JSDocThisTag
                | SyntaxKind::JSDocTypeTag => return self.emit_jsdoc_simple_typed_tag(node),
                SyntaxKind::JSDocTemplateTag => return self.emit_jsdoc_template_tag(node),
                SyntaxKind::JSDocTypedefTag => return self.emit_jsdoc_typedef_tag(node),
                SyntaxKind::JSDocSeeTag => return self.emit_jsdoc_see_tag(node),

                SyntaxKind::NotEmittedStatement
                | SyntaxKind::EndOfDeclarationMarker
                | SyntaxKind::MergeDeclarationMarker => return Ok(()),
                _ => (),
            }
            if is_expression(node, self) {
                hint = EmitHint::Expression;
                if !self.is_substitute_node_no_emit_substitution() {
                    let substitute = self
                        .substitute_node(hint, node)?
                        .unwrap_or(node);
                    if substitute != node {
                        node = substitute;
                        if let Some(current_parenthesizer_rule) =
                            self.maybe_current_parenthesizer_rule()
                        {
                            node = current_parenthesizer_rule.call(node);
                        }
                    }
                }
            }
        }
        if hint == EmitHint::Expression {
            match node.ref_(self).kind() {
                SyntaxKind::NumericLiteral | SyntaxKind::BigIntLiteral => {
                    return Ok(self.emit_numeric_or_big_int_literal(node))
                }

                SyntaxKind::StringLiteral
                | SyntaxKind::RegularExpressionLiteral
                | SyntaxKind::NoSubstitutionTemplateLiteral => {
                    return Ok(self.emit_literal(node, false))
                }

                SyntaxKind::Identifier => return self.emit_identifier(node),
                SyntaxKind::PrivateIdentifier => return Ok(self.emit_private_identifier(node)),

                SyntaxKind::ArrayLiteralExpression => {
                    return self.emit_array_literal_expression(node)
                }
                SyntaxKind::ObjectLiteralExpression => {
                    return self.emit_object_literal_expression(node)
                }
                SyntaxKind::PropertyAccessExpression => {
                    return self.emit_property_access_expression(node)
                }
                SyntaxKind::ElementAccessExpression => {
                    return self.emit_element_access_expression(node)
                }
                SyntaxKind::CallExpression => return self.emit_call_expression(node),
                SyntaxKind::NewExpression => return self.emit_new_expression(node),
                SyntaxKind::TaggedTemplateExpression => {
                    return self.emit_tagged_template_expression(node)
                }
                SyntaxKind::TypeAssertionExpression => {
                    return self.emit_type_assertion_expression(node)
                }
                SyntaxKind::ParenthesizedExpression => {
                    return self.emit_parenthesized_expression(node)
                }
                SyntaxKind::FunctionExpression => return self.emit_function_expression(node),
                SyntaxKind::ArrowFunction => return self.emit_arrow_function(node),
                SyntaxKind::DeleteExpression => return self.emit_delete_expression(node),
                SyntaxKind::TypeOfExpression => return self.emit_type_of_expression(node),
                SyntaxKind::VoidExpression => return self.emit_void_expression(node),
                SyntaxKind::AwaitExpression => return self.emit_await_expression(node),
                SyntaxKind::PrefixUnaryExpression => {
                    return self.emit_prefix_unary_expression(node)
                }
                SyntaxKind::PostfixUnaryExpression => {
                    return self.emit_postfix_unary_expression(node)
                }
                SyntaxKind::BinaryExpression => return Ok(self.emit_binary_expression(node)),
                SyntaxKind::ConditionalExpression => {
                    return self.emit_conditional_expression(node)
                }
                SyntaxKind::TemplateExpression => return self.emit_template_expression(node),
                SyntaxKind::YieldExpression => return self.emit_yield_expression(node),
                SyntaxKind::SpreadElement => return self.emit_spread_element(node),
                SyntaxKind::ClassExpression => return self.emit_class_expression(node),
                SyntaxKind::OmittedExpression => return Ok(()),
                SyntaxKind::AsExpression => return self.emit_as_expression(node),
                SyntaxKind::NonNullExpression => return self.emit_non_null_expression(node),
                SyntaxKind::MetaProperty => return self.emit_meta_property(node),
                SyntaxKind::SyntheticExpression => {
                    Debug_.fail(Some("SyntheticExpression should never be printed."))
                }

                SyntaxKind::JsxElement => return self.emit_jsx_element(node),
                SyntaxKind::JsxSelfClosingElement => {
                    return self.emit_jsx_self_closing_element(node)
                }
                SyntaxKind::JsxFragment => return self.emit_jsx_fragment(node),

                SyntaxKind::SyntaxList => Debug_.fail(Some("SyntaxList should not be printed")),

                SyntaxKind::NotEmittedStatement => return Ok(()),
                SyntaxKind::PartiallyEmittedExpression => {
                    return self.emit_partially_emitted_expression(node)
                }
                SyntaxKind::CommaListExpression => return self.emit_comma_list(node),
                SyntaxKind::MergeDeclarationMarker | SyntaxKind::EndOfDeclarationMarker => {
                    return Ok(())
                }
                SyntaxKind::SyntheticReferenceExpression => {
                    Debug_.fail(Some("SyntheticReferenceExpression should not be printed"))
                }
                _ => (),
            }
        }
        if is_keyword(node.ref_(self).kind()) {
            return Ok(self.write_token_node(node, Printer::write_keyword));
        }
        if is_token_kind(node.ref_(self).kind()) {
            return Ok(self.write_token_node(node, Printer::write_punctuation));
        }
        Debug_.fail(Some(&format!("Unhandled SyntaxKind: {:?}", node.ref_(self).kind())));
    }
}
