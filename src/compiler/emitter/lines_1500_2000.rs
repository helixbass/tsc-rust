use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::{brackets, PipelinePhase};
use crate::{
    compare_emit_helpers, get_emit_flags, get_emit_helpers, get_external_helpers_module_name,
    get_literal_text, get_parse_tree_node, has_recorded_external_helpers, id_text, is_identifier,
    is_source_file, is_template_literal_kind, is_unparsed_source, positions_are_on_same_line,
    skip_trivia, stable_sort, token_to_string, with_synthetic_factory, BundleFileSection,
    BundleFileSectionKind, Debug_, EmitFlags, EmitHelper, EmitHelperBase, EmitHelperText, EmitHint,
    GetLiteralTextFlags, HasTypeArgumentsInterface, HasTypeInterface, ListFormat, ModuleKind,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, Printer, ReadonlyTextRange,
    SnippetElement, SnippetKind, SortedArray, SourceFileLike, SourceFilePrologueInfo,
    SourceMapSource, Symbol, SyntaxKind, TextRange,
};

impl Printer {
    pub(super) fn emit_mapped_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        let node_as_type_parameter_declaration = node.as_type_parameter_declaration();
        self.emit(
            node_as_type_parameter_declaration.maybe_name().as_deref(),
            None,
        );
        self.write_space();
        self.write_keyword("in");
        self.write_space();
        self.emit(
            node_as_type_parameter_declaration.constraint.as_deref(),
            None,
        );
    }

    pub(super) fn pipeline_emit_with_substitution(&self, hint: EmitHint, node: &Node) {
        let pipeline_phase = self.get_next_pipeline_phase(PipelinePhase::Substitution, hint, node);
        Debug_.assert_is_defined(&self.maybe_last_substitution(), None);
        let ref node = self.maybe_last_substitution().unwrap();
        self.set_last_substitution(None);
        pipeline_phase(self, hint, node);
    }

    pub(super) fn get_helpers_from_bundled_source_files(
        &self,
        bundle: &Node, /*Bundle*/
    ) -> Option<Vec<String>> {
        let mut result: Option<Vec<String>> = None;
        if self.module_kind == ModuleKind::None
            || self.printer_options.no_emit_helpers == Some(true)
        {
            return None;
        }
        let mut bundled_helpers: HashMap<String, bool> = HashMap::new();
        for source_file in &bundle.as_bundle().source_files {
            let should_skip = get_external_helpers_module_name(source_file).is_some();
            let helpers = self.get_sorted_emit_helpers(source_file);
            if helpers.is_none() {
                continue;
            }
            let helpers = helpers.unwrap();
            for helper in &*helpers {
                if !helper.scoped()
                    && !should_skip
                    && bundled_helpers.get(helper.name()).copied() != Some(true)
                {
                    bundled_helpers.insert(helper.name().to_owned(), true);
                    result
                        .get_or_insert_with(|| vec![])
                        .push(helper.name().to_owned());
                }
            }
        }

        result
    }

    pub(super) fn emit_helpers(&self, node: &Node) -> bool {
        let mut helpers_emitted = false;
        let bundle: Option<Rc<Node>> = if node.kind() == SyntaxKind::Bundle {
            Some(node.node_wrapper())
        } else {
            None
        };
        if bundle.is_some() && self.module_kind == ModuleKind::None {
            return false;
        }
        let num_prepends = bundle
            .as_ref()
            .map_or(0, |bundle| bundle.as_bundle().prepends.len());
        let num_nodes = bundle.as_ref().map_or(1, |bundle| {
            bundle.as_bundle().source_files.len() + num_prepends
        });
        for i in 0..num_nodes {
            let ref current_node = if let Some(bundle) = bundle.as_ref() {
                if i < num_prepends {
                    bundle.as_bundle().prepends[i].clone()
                } else {
                    bundle.as_bundle().source_files[i - num_prepends].clone()
                }
            } else {
                node.node_wrapper()
            };
            let source_file = if is_source_file(current_node) {
                Some(current_node.clone())
            } else if is_unparsed_source(current_node) {
                None
            } else {
                self.maybe_current_source_file()
            };
            let should_skip = self.printer_options.no_emit_helpers == Some(true)
                || matches!(
                    source_file.as_ref(),
                    Some(source_file) if has_recorded_external_helpers(source_file)
                );
            let should_bundle = (is_source_file(current_node) || is_unparsed_source(current_node))
                && !self.is_own_file_emit();
            let helpers = if is_unparsed_source(current_node) {
                current_node.as_unparsed_source().helpers.clone()
            } else {
                self.get_sorted_emit_helpers(current_node).map(Into::into)
            };
            if let Some(helpers) = helpers {
                for helper in &helpers {
                    if !helper.scoped() {
                        if should_skip {
                            continue;
                        }

                        if should_bundle {
                            if self.bundled_helpers().get(helper.name()).copied() == Some(true) {
                                continue;
                            }

                            self.bundled_helpers_mut()
                                .insert(helper.name().to_owned(), true);
                        }
                    } else if bundle.is_some() {
                        continue;
                    }
                    let pos = self.get_text_pos_with_write_line();
                    match helper.text() {
                        EmitHelperText::String(ref helper_text) => {
                            self.write_lines(helper_text);
                        }
                        EmitHelperText::Callback(helper_text) => {
                            self.write_lines(&helper_text(&|name: &str| {
                                self.make_file_level_optimistic_unique_name(name)
                            }));
                        }
                    }
                    if let Some(bundle_file_info) = self.maybe_bundle_file_info_mut().as_mut() {
                        bundle_file_info.sections.push(Rc::new(
                            BundleFileSection::new_emit_helpers(
                                helper.name().to_owned(),
                                pos.try_into().unwrap(),
                                self.writer().get_text_pos().try_into().unwrap(),
                            ),
                        ));
                    }
                    helpers_emitted = true;
                }
            }
        }

        helpers_emitted
    }

    pub(super) fn get_sorted_emit_helpers(
        &self,
        node: &Node,
    ) -> Option<SortedArray<Rc<EmitHelper>>> {
        let helpers = get_emit_helpers(node);
        helpers.map(|helpers| {
            stable_sort(&helpers, |a: &Rc<EmitHelper>, b: &Rc<EmitHelper>| {
                compare_emit_helpers(a, b)
            })
        })
    }

    pub(super) fn emit_numeric_or_big_int_literal(
        &self,
        node: &Node, /*NumericLiteral | BigIntLiteral*/
    ) {
        self.emit_literal(node, false);
    }

    pub(super) fn emit_literal(
        &self,
        node: &Node, /*LiteralLikeNode*/
        jsx_attribute_escape: bool,
    ) {
        let ref text = self.get_literal_text_of_node(
            node,
            self.printer_options.never_ascii_escape,
            jsx_attribute_escape,
        );
        if (self.printer_options.source_map == Some(true)
            || self.printer_options.inline_source_map == Some(true))
            && (node.kind() == SyntaxKind::StringLiteral || is_template_literal_kind(node.kind()))
        {
            self.write_literal(text);
        } else {
            self.write_string_literal(text);
        }
    }

    pub(super) fn emit_unparsed_source_or_prepend(
        &self,
        unparsed: &Node, /*UnparsedSource | UnparsedPrepend*/
    ) {
        for text in unparsed.as_has_texts().texts() {
            self.write_line(None);
            self.emit(Some(&**text), None);
        }
    }

    pub(super) fn write_unparsed_node(&self, unparsed: &Node /*UnparsedNode*/) {
        self.writer().raw_write(
            &unparsed.parent().as_unparsed_source().text[TryInto::<usize>::try_into(unparsed.pos())
                .unwrap()
                ..TryInto::<usize>::try_into(unparsed.end()).unwrap()],
        );
    }

    pub(super) fn emit_unparsed_text_like(&self, unparsed: &Node /*UnparsedTextLike*/) {
        let pos = self.get_text_pos_with_write_line();
        self.write_unparsed_node(unparsed);
        if self.maybe_bundle_file_info().is_some() {
            self.update_or_push_bundle_file_text_like(
                pos.try_into().unwrap(),
                self.writer().get_text_pos().try_into().unwrap(),
                if unparsed.kind() == SyntaxKind::UnparsedText {
                    BundleFileSectionKind::Text
                } else {
                    BundleFileSectionKind::Internal
                },
            );
        }
    }

    pub(super) fn emit_unparsed_synthetic_reference(
        &self,
        unparsed: &Node, /*UnparsedSyntheticReference*/
    ) {
        let pos = self.get_text_pos_with_write_line();
        self.write_unparsed_node(unparsed);
        if let Some(bundle_file_info) = self.maybe_bundle_file_info_mut().as_mut() {
            let section = (*unparsed.as_unparsed_synthetic_reference().section).clone();
            section.set_pos(pos.try_into().unwrap());
            section.set_end(self.writer().get_text_pos().try_into().unwrap());
            bundle_file_info.sections.push(Rc::new(section));
        }
    }

    pub(super) fn emit_snippet_node(&self, hint: EmitHint, node: &Node, snippet: SnippetElement) {
        match snippet.kind {
            SnippetKind::Placeholder => {
                self.emit_placeholder(hint, node, snippet);
            }
            SnippetKind::TabStop => {
                self.emit_tab_stop(snippet);
            }
            _ => unreachable!(),
        }
    }

    pub(super) fn emit_placeholder(
        &self,
        hint: EmitHint,
        node: &Node,
        snippet: SnippetElement, /*Placeholder*/
    ) {
        self.non_escaping_write(&format!("${{{}:", snippet.order));
        self.pipeline_emit_with_hint_worker(hint, node, Some(false));
        self.non_escaping_write("}");
    }

    pub(super) fn emit_tab_stop(&self, snippet: SnippetElement /*TabStop*/) {
        self.non_escaping_write(&format!("${}", snippet.order));
    }

    pub(super) fn emit_identifier(&self, node: &Node /*Identifier*/) {
        let text_of_node = self.get_text_of_node(node, Some(false));
        if let Some(symbol) = node.maybe_symbol() {
            self.write_symbol(&text_of_node, &symbol);
        } else {
            self.write(&text_of_node);
        }
        self.emit_list(
            Some(node),
            node.as_identifier().maybe_type_arguments().as_ref(),
            ListFormat::TypeParameters,
            None,
            None,
            None,
        );
    }

    pub(super) fn emit_private_identifier(&self, node: &Node /*PrivateIdentifier*/) {
        let text_of_node = self.get_text_of_node(node, Some(false));
        if let Some(symbol) = node.maybe_symbol() {
            self.write_symbol(&text_of_node, &symbol);
        } else {
            self.write(&text_of_node);
        }
    }

    pub(super) fn emit_qualified_name(&self, node: &Node /*QualifiedName*/) {
        let node_as_qualified_name = node.as_qualified_name();
        self.emit_entity_name(&node_as_qualified_name.left);
        self.write_punctuation(".");
        self.emit(Some(&*node_as_qualified_name.right), None);
    }

    pub(super) fn emit_entity_name(&self, node: &Node /*EntityName*/) {
        if node.kind() == SyntaxKind::Identifier {
            self.emit_expression(Some(node), None);
        } else {
            self.emit(Some(node), None);
        }
    }

    pub(super) fn emit_computed_property_name(&self, node: &Node /*ComputedPropertyName*/) {
        self.write_punctuation("[");
        self.emit_expression(
            Some(&*node.as_computed_property_name().expression),
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_expression_of_computed_property_name(
                            synthetic_factory,
                            node,
                        )
                    })
                }
            })),
        );
        self.write_punctuation("]");
    }
}
