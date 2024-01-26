use std::{collections::HashMap, convert::TryInto, io};

use gc::Gc;
use id_arena::Id;

use super::{ParenthesizeExpressionOfComputedPropertyNameCurrentParenthesizerRule, PipelinePhase};
use crate::{
    compare_emit_helpers, get_emit_helpers, get_external_helpers_module_name,
    has_recorded_external_helpers, is_source_file, is_template_literal_kind, is_unparsed_source,
    stable_sort, BundleFileSection, BundleFileSectionKind, Debug_, EmitHelper, EmitHelperBase,
    EmitHelperText, EmitHint, GetOrInsertDefault, HasTypeArgumentsInterface, ListFormat,
    ModuleKind, NamedDeclarationInterface, Node, NodeInterface, Printer, ReadonlyTextRange,
    SnippetElement, SnippetKind, SortedArray, SourceFileLike, SyntaxKind, TextRange,
    InArena,
};

impl Printer {
    pub(super) fn emit_mapped_type_parameter(
        &self,
        node: Id<Node>, /*TypeParameterDeclaration*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_type_parameter_declaration = node_ref.as_type_parameter_declaration();
        self.emit(
            node_as_type_parameter_declaration.maybe_name(),
            None,
        )?;
        self.write_space();
        self.write_keyword("in");
        self.write_space();
        self.emit(
            node_as_type_parameter_declaration.constraint,
            None,
        )?;

        Ok(())
    }

    pub(super) fn pipeline_emit_with_substitution(
        &self,
        hint: EmitHint,
        node: Id<Node>,
    ) -> io::Result<()> {
        let pipeline_phase =
            self.get_next_pipeline_phase(PipelinePhase::Substitution, hint, node)?;
        Debug_.assert_is_defined(&self.maybe_last_substitution(), None);
        let node = self.maybe_last_substitution().unwrap();
        self.set_last_substitution(None);
        pipeline_phase(self, hint, node)?;

        Ok(())
    }

    pub(super) fn get_helpers_from_bundled_source_files(
        &self,
        bundle: Id<Node>, /*Bundle*/
    ) -> Option<Vec<String>> {
        let mut result: Option<Vec<String>> = None;
        if self.module_kind == ModuleKind::None
            || self.printer_options.no_emit_helpers == Some(true)
        {
            return None;
        }
        let mut bundled_helpers: HashMap<String, bool> = HashMap::new();
        for source_file in &bundle.ref_(self).as_bundle().source_files {
            let source_file = source_file.unwrap();
            let should_skip = get_external_helpers_module_name(source_file, self).is_some();
            let Some(helpers) = self.get_sorted_emit_helpers(source_file) else {
                continue;
            };
            for helper in &*helpers {
                if !helper.ref_(self).scoped()
                    && !should_skip
                    && bundled_helpers.get(helper.ref_(self).name()).copied() != Some(true)
                {
                    bundled_helpers.insert(helper.ref_(self).name().to_owned(), true);
                    result
                        .get_or_insert_default_()
                        .push(helper.ref_(self).name().to_owned());
                }
            }
        }

        result
    }

    pub(super) fn emit_helpers(&self, node: Id<Node>) -> bool {
        let mut helpers_emitted = false;
        let bundle: Option<Id<Node>> = if node.ref_(self).kind() == SyntaxKind::Bundle {
            Some(node)
        } else {
            None
        };
        if bundle.is_some() && self.module_kind == ModuleKind::None {
            return false;
        }
        let num_prepends = bundle
            .as_ref()
            .map_or(0, |bundle| bundle.ref_(self).as_bundle().prepends.len());
        let num_nodes = bundle.as_ref().map_or(1, |bundle| {
            bundle.ref_(self).as_bundle().source_files.len() + num_prepends
        });
        for i in 0..num_nodes {
            let current_node = if let Some(bundle) = bundle {
                if i < num_prepends {
                    bundle.ref_(self).as_bundle().prepends[i].clone()
                } else {
                    bundle.ref_(self).as_bundle().source_files[i - num_prepends]
                        .clone()
                        .unwrap()
                }
            } else {
                node
            };
            let source_file = if is_source_file(&current_node.ref_(self)) {
                Some(current_node)
            } else if is_unparsed_source(&current_node.ref_(self)) {
                None
            } else {
                self.maybe_current_source_file()
            };
            let should_skip = self.printer_options.no_emit_helpers == Some(true)
                || matches!(
                    source_file,
                    Some(source_file) if has_recorded_external_helpers(source_file, self)
                );
            let should_bundle = (is_source_file(&current_node.ref_(self)) || is_unparsed_source(&current_node.ref_(self)))
                && !self.is_own_file_emit();
            let helpers = if is_unparsed_source(&current_node.ref_(self)) {
                current_node.ref_(self).as_unparsed_source().helpers.clone()
            } else {
                self.get_sorted_emit_helpers(current_node).map(Into::into)
            };
            if let Some(helpers) = helpers {
                for helper in &helpers {
                    if !helper.ref_(self).scoped() {
                        if should_skip {
                            continue;
                        }

                        if should_bundle {
                            if self.bundled_helpers().get(helper.ref_(self).name()).copied() == Some(true) {
                                continue;
                            }

                            self.bundled_helpers_mut()
                                .insert(helper.ref_(self).name().to_owned(), true);
                        }
                    } else if bundle.is_some() {
                        continue;
                    }
                    let pos = self.get_text_pos_with_write_line();
                    match helper.ref_(self).text() {
                        EmitHelperText::String(ref helper_text) => {
                            self.write_lines(helper_text);
                        }
                        EmitHelperText::Callback(helper_text) => {
                            self.write_lines(&helper_text.call(&|name: &str| {
                                self.make_file_level_optimistic_unique_name(name)
                            }));
                        }
                    }
                    if let Some(bundle_file_info) = self.maybe_bundle_file_info() {
                        bundle_file_info.borrow_mut().sections.push(Gc::new(
                            BundleFileSection::new_emit_helpers(
                                helper.ref_(self).name().to_owned(),
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
        node: Id<Node>,
    ) -> Option<SortedArray<Id<EmitHelper>>> {
        let helpers = get_emit_helpers(&node.ref_(self));
        helpers.map(|helpers| {
            stable_sort(&helpers, |&a: &Id<EmitHelper>, &b: &Id<EmitHelper>| {
                compare_emit_helpers(a, b, self)
            })
        })
    }

    pub(super) fn emit_numeric_or_big_int_literal(
        &self,
        node: Id<Node>, /*NumericLiteral | BigIntLiteral*/
    ) {
        self.emit_literal(node, false);
    }

    pub(super) fn emit_literal(
        &self,
        node: Id<Node>, /*LiteralLikeNode*/
        jsx_attribute_escape: bool,
    ) {
        let ref text = self.get_literal_text_of_node(
            node,
            self.printer_options.never_ascii_escape,
            jsx_attribute_escape,
        );
        if (self.printer_options.source_map == Some(true)
            || self.printer_options.inline_source_map == Some(true))
            && (node.ref_(self).kind() == SyntaxKind::StringLiteral || is_template_literal_kind(node.ref_(self).kind()))
        {
            self.write_literal(text);
        } else {
            self.write_string_literal(text);
        }
    }

    pub(super) fn emit_unparsed_source_or_prepend(
        &self,
        unparsed: Id<Node>, /*UnparsedSource | UnparsedPrepend*/
    ) -> io::Result<()> {
        for &text in unparsed.ref_(self).as_has_texts().texts() {
            self.write_line(None);
            self.emit(Some(text), None)?;
        }

        Ok(())
    }

    pub(super) fn write_unparsed_node(&self, unparsed: Id<Node> /*UnparsedNode*/) {
        self.writer().raw_write(
            &(*unparsed.ref_(self).parent().ref_(self).as_unparsed_source().text())[TryInto::<usize>::try_into(
                unparsed.ref_(self).pos(),
            )
            .unwrap()
                ..TryInto::<usize>::try_into(unparsed.ref_(self).end()).unwrap()],
        );
    }

    pub(super) fn emit_unparsed_text_like(&self, unparsed: Id<Node> /*UnparsedTextLike*/) {
        let pos = self.get_text_pos_with_write_line();
        self.write_unparsed_node(unparsed);
        if self.maybe_bundle_file_info().is_some() {
            self.update_or_push_bundle_file_text_like(
                pos.try_into().unwrap(),
                self.writer().get_text_pos().try_into().unwrap(),
                if unparsed.ref_(self).kind() == SyntaxKind::UnparsedText {
                    BundleFileSectionKind::Text
                } else {
                    BundleFileSectionKind::Internal
                },
            );
        }
    }

    pub(super) fn emit_unparsed_synthetic_reference(
        &self,
        unparsed: Id<Node>, /*UnparsedSyntheticReference*/
    ) {
        let pos = self.get_text_pos_with_write_line();
        self.write_unparsed_node(unparsed);
        if let Some(bundle_file_info) = self.maybe_bundle_file_info() {
            let section = (*unparsed.ref_(self).as_unparsed_synthetic_reference().section).clone();
            section.set_pos(pos.try_into().unwrap());
            section.set_end(self.writer().get_text_pos().try_into().unwrap());
            bundle_file_info
                .borrow_mut()
                .sections
                .push(Gc::new(section));
        }
    }

    pub(super) fn emit_snippet_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        snippet: SnippetElement,
    ) -> io::Result<()> {
        Ok(match snippet.kind {
            SnippetKind::Placeholder => {
                self.emit_placeholder(hint, node, snippet)?;
            }
            SnippetKind::TabStop => {
                self.emit_tab_stop(snippet);
            }
            _ => unreachable!(),
        })
    }

    pub(super) fn emit_placeholder(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        snippet: SnippetElement, /*Placeholder*/
    ) -> io::Result<()> {
        self.non_escaping_write(&format!("${{{}:", snippet.order));
        self.pipeline_emit_with_hint_worker(hint, node, Some(false))?;
        self.non_escaping_write("}");

        Ok(())
    }

    pub(super) fn emit_tab_stop(&self, snippet: SnippetElement /*TabStop*/) {
        self.non_escaping_write(&format!("${}", snippet.order));
    }

    pub(super) fn emit_identifier(&self, node: Id<Node> /*Identifier*/) -> io::Result<()> {
        let text_of_node = self.get_text_of_node(node, Some(false));
        if let Some(symbol) = node.ref_(self).maybe_symbol() {
            self.write_symbol(&text_of_node, symbol);
        } else {
            self.write(&text_of_node);
        }
        self.emit_list(
            Some(node),
            node.ref_(self).as_identifier().maybe_type_arguments().as_deref(),
            ListFormat::TypeParameters,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_private_identifier(&self, node: Id<Node> /*PrivateIdentifier*/) {
        let text_of_node = self.get_text_of_node(node, Some(false));
        if let Some(symbol) = node.ref_(self).maybe_symbol() {
            self.write_symbol(&text_of_node, symbol);
        } else {
            self.write(&text_of_node);
        }
    }

    pub(super) fn emit_qualified_name(
        &self,
        node: Id<Node>, /*QualifiedName*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_qualified_name = node_ref.as_qualified_name();
        self.emit_entity_name(node_as_qualified_name.left)?;
        self.write_punctuation(".");
        self.emit(Some(node_as_qualified_name.right), None)?;

        Ok(())
    }

    pub(super) fn emit_entity_name(&self, node: Id<Node> /*EntityName*/) -> io::Result<()> {
        Ok(if node.ref_(self).kind() == SyntaxKind::Identifier {
            self.emit_expression(Some(node), None)?;
        } else {
            self.emit(Some(node), None)?;
        })
    }

    pub(super) fn emit_computed_property_name(
        &self,
        node: Id<Node>, /*ComputedPropertyName*/
    ) -> io::Result<()> {
        self.write_punctuation("[");
        self.emit_expression(
            Some(node.ref_(self).as_computed_property_name().expression),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionOfComputedPropertyNameCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        )?;
        self.write_punctuation("]");

        Ok(())
    }
}
