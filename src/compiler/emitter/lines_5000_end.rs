use bitflags::bitflags;
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::{brackets, PipelinePhase};
use crate::{
    escape_leading_underscores, get_comment_range, get_emit_flags, get_external_module_name,
    get_node_id, get_synthetic_leading_comments, get_synthetic_trailing_comments, id_text,
    is_file_level_unique_name, is_identifier, is_node_descendant_of, is_string_literal,
    make_identifier_from_module_name, maybe_for_each, Debug_, EmitFlags, EmitHint,
    GeneratedIdentifierFlags, ListFormat, LiteralLikeNodeInterface, Node, NodeInterface, Printer,
    ReadonlyTextRange, SourceMapSource, SymbolFlags, SymbolInterface, SyntaxKind,
    SynthesizedComment, TextRange,
};

impl Printer {
    pub(super) fn generate_name_cached(
        &self,
        node: &Node,
        flags: Option<GeneratedIdentifierFlags>,
    ) -> String {
        let node_id = get_node_id(node);
        self.node_id_to_generated_name_mut()
            .entry(node_id)
            .or_insert_with(|| self.generate_name_for_node(node, flags))
            .clone()
    }

    pub(super) fn is_unique_name(&self, name: &str) -> bool {
        self.is_file_level_unique_name(name)
            && !self.generated_names().contains(name)
            && !matches!(
                self.maybe_reserved_names(),
                Some(reserved_names) if (*reserved_names).borrow().contains(name)
            )
    }

    pub(super) fn is_file_level_unique_name(&self, name: &str) -> bool {
        self.maybe_current_source_file()
            .as_ref()
            .map_or(true, |current_source_file| {
                is_file_level_unique_name(
                    current_source_file,
                    name,
                    Some(|name: &str| self.has_global_name(name) == Some(true)),
                )
            })
    }

    pub(super) fn is_unique_local_name(&self, name: &str, container: &Node) -> bool {
        let mut node = container.node_wrapper();
        while is_node_descendant_of(&node, Some(container)) {
            if let Some(node_locals) = node.maybe_locals().as_ref() {
                let local = (**node_locals)
                    .borrow()
                    .get(&escape_leading_underscores(name))
                    .cloned();
                if matches!(
                    local,
                    Some(local) if local.flags().intersects(
                        SymbolFlags::Value | SymbolFlags::ExportValue | SymbolFlags::Alias
                    )
                ) {
                    return false;
                }
            }
            node = node.maybe_next_container().unwrap();
        }
        true
    }

    pub(super) fn make_temp_variable_name(
        &self,
        flags: TempFlags,
        reserved_in_nested_scopes: Option<bool>,
    ) -> String {
        if flags != TempFlags::Auto && !self.temp_flags().intersects(flags) {
            let name = if flags == TempFlags::_I { "_i" } else { "_n" };
            if self.is_unique_name(name) {
                self.set_temp_flags(self.temp_flags() | flags);
                if reserved_in_nested_scopes == Some(true) {
                    self.reserve_name_in_nested_scopes(name);
                }
                return name.to_owned();
            }
        }
        loop {
            let count: usize = (self.temp_flags() & TempFlags::CountMask)
                .bits()
                .try_into()
                .unwrap();
            self.set_temp_flags(
                // TODO: could probably avoid using unsafe here by not using a bitflags for
                // TempFlags?
                unsafe { TempFlags::from_bits_unchecked(self.temp_flags().bits() + 1) },
            );
            if count != 8 && count != 13 {
                let name = if count < 26 {
                    format!("_{}", &"abcdefghijklmnopqrstuvwxyz"[count..count + 1])
                } else {
                    format!("_{}", count - 26)
                };
                if self.is_unique_name(&name) {
                    if reserved_in_nested_scopes == Some(true) {
                        self.reserve_name_in_nested_scopes(&name);
                        return name;
                    }
                }
            }
        }
    }

    pub(super) fn make_unique_name<TCheckFn: Fn(&str) -> bool>(
        &self,
        base_name: &str,
        check_fn: Option<TCheckFn>,
        optimistic: Option<bool>,
        scoped: Option<bool>,
    ) -> String {
        let check_fn_present = |name: &str| match check_fn.as_ref() {
            Some(check_fn) => check_fn(name),
            None => self.is_unique_name(name),
        };
        if optimistic == Some(true) {
            if check_fn_present(base_name) {
                if scoped == Some(true) {
                    self.reserve_name_in_nested_scopes(base_name);
                } else {
                    self.generated_names_mut().insert(base_name.to_owned());
                }
                return base_name.to_owned();
            }
        }
        let mut base_name = base_name.to_owned();
        if &base_name[base_name.len() - 1..] != "_" {
            base_name.push_str("_");
        }
        let mut i = 1;
        loop {
            let generated_name = format!("{}{}", base_name, i,);
            if check_fn_present(&generated_name) {
                if scoped == Some(true) {
                    self.reserve_name_in_nested_scopes(&generated_name);
                } else {
                    self.generated_names_mut().insert(generated_name.clone());
                }
                return generated_name;
            }
            i += 1;
        }
    }

    pub(super) fn make_file_level_optimistic_unique_name(&self, name: &str) -> String {
        self.make_unique_name(
            name,
            Some(|name: &str| self.is_file_level_unique_name(name)),
            Some(true),
            None,
        )
    }

    pub(super) fn generate_name_for_module_or_enum(
        &self,
        node: &Node, /*ModuleDeclaration | EnumDeclaration*/
    ) -> String {
        let name = self.get_text_of_node(&node.as_named_declaration().name(), None);
        if self.is_unique_local_name(&name, node) {
            name
        } else {
            self.make_unique_name(&name, Option::<fn(&str) -> bool>::None, None, None)
        }
    }

    pub(super) fn generate_name_for_import_or_export_declaration(
        &self,
        node: &Node, /*ImportDeclaration | ExportDeclaration*/
    ) -> String {
        let ref expr = get_external_module_name(node).unwrap();
        let base_name = if is_string_literal(expr) {
            make_identifier_from_module_name(&expr.as_string_literal().text())
        } else {
            "module".to_owned()
        };
        self.make_unique_name(&base_name, Option::<fn(&str) -> bool>::None, None, None)
    }

    pub(super) fn generate_name_for_export_default(&self) -> String {
        self.make_unique_name("default", Option::<fn(&str) -> bool>::None, None, None)
    }

    pub(super) fn generate_name_for_class_expression(&self) -> String {
        self.make_unique_name("class", Option::<fn(&str) -> bool>::None, None, None)
    }

    pub(super) fn generate_name_for_method_or_accessor(
        &self,
        node: &Node, /*MethodDeclaration | AccessorDeclaration*/
    ) -> String {
        let ref node_name = node.as_named_declaration().name();
        if is_identifier(node_name) {
            return self.generate_name_cached(node_name, None);
        }
        self.make_temp_variable_name(TempFlags::Auto, None)
    }

    pub(super) fn generate_name_for_node(
        &self,
        node: &Node,
        flags: Option<GeneratedIdentifierFlags>,
    ) -> String {
        match node.kind() {
            SyntaxKind::Identifier => {
                self.make_unique_name(
                    &self.get_text_of_node(
                        node,
                        None,
                    ),
                    Some(|name: &str| self.is_unique_name(name)),
                    Some(matches!(
                        flags,
                        Some(flags) if flags.intersects(GeneratedIdentifierFlags::Optimistic)
                    )),
                    Some(matches!(
                        flags,
                        Some(flags) if flags.intersects(GeneratedIdentifierFlags::ReservedInNestedScopes)
                    )),
                )
            }
            SyntaxKind::ModuleDeclaration |
            SyntaxKind::EnumDeclaration => {
                self.generate_name_for_module_or_enum(
                    node
                )
            }
            SyntaxKind::ImportDeclaration |
            SyntaxKind::ExportDeclaration => {
                self.generate_name_for_import_or_export_declaration(
                    node
                )
            }
            SyntaxKind::FunctionDeclaration |
            SyntaxKind::ClassDeclaration |
            SyntaxKind::ExportAssignment => {
                self.generate_name_for_export_default()
            }
            SyntaxKind::ClassExpression => {
                self.generate_name_for_class_expression()
            }
            SyntaxKind::MethodDeclaration |
            SyntaxKind::GetAccessor |
            SyntaxKind::SetAccessor => {
                self.generate_name_for_method_or_accessor(node)
            }
            SyntaxKind::ComputedPropertyName => {
                self.make_temp_variable_name(
                    TempFlags::Auto,
                    Some(true)
                )
            }
            _ => {
                self.make_temp_variable_name(
                    TempFlags::Auto,
                    None,
                )
            }
        }
    }

    pub(super) fn make_name(&self, name: &Node /*GeneratedIdentifier*/) -> String {
        let name_auto_generate_flags = name.as_identifier().auto_generate_flags.unwrap();
        match name_auto_generate_flags & GeneratedIdentifierFlags::KindMask {
            GeneratedIdentifierFlags::Auto => self.make_temp_variable_name(
                TempFlags::Auto,
                Some(
                    name_auto_generate_flags
                        .intersects(GeneratedIdentifierFlags::ReservedInNestedScopes),
                ),
            ),
            GeneratedIdentifierFlags::Loop => self.make_temp_variable_name(
                TempFlags::_I,
                Some(
                    name_auto_generate_flags
                        .intersects(GeneratedIdentifierFlags::ReservedInNestedScopes),
                ),
            ),
            GeneratedIdentifierFlags::Unique => self.make_unique_name(
                &id_text(name),
                Some(|name: &str| {
                    if name_auto_generate_flags.intersects(GeneratedIdentifierFlags::FileLevel) {
                        self.is_file_level_unique_name(name)
                    } else {
                        self.is_unique_name(name)
                    }
                }),
                Some(name_auto_generate_flags.intersects(GeneratedIdentifierFlags::Optimistic)),
                Some(
                    name_auto_generate_flags
                        .intersects(GeneratedIdentifierFlags::ReservedInNestedScopes),
                ),
            ),
            _ => Debug_.fail(Some("Unsupported GeneratedIdentifierKind.")),
        }
    }

    pub(super) fn get_node_for_generated_name(
        &self,
        name: &Node, /*GeneratedIdentifier*/
    ) -> Rc<Node> {
        let auto_generate_id = name.as_identifier().auto_generate_id;
        let mut node: Rc<Node> = name.node_wrapper();
        let mut original: Option<Rc<Node>> = node.maybe_original();
        while let Some(ref original_present) = original {
            node = original_present.clone();

            if is_identifier(&node) && {
                let node_as_identifier = node.as_identifier();
                node_as_identifier
                    .auto_generate_flags
                    .unwrap()
                    .intersects(GeneratedIdentifierFlags::Node)
                    && node_as_identifier.auto_generate_id != auto_generate_id
            } {
                break;
            }

            original = node.maybe_original();
        }

        node
    }

    pub(super) fn pipeline_emit_with_comments(&self, hint: EmitHint, node: &Node) {
        let pipeline_phase = self.get_next_pipeline_phase(PipelinePhase::Comments, hint, node);
        let saved_container_pos = self.container_pos();
        let saved_container_end = self.container_end();
        let saved_declaration_list_container_end = self.declaration_list_container_end();
        self.emit_comments_before_node(node);
        pipeline_phase(self, hint, node);
        self.emit_comments_after_node(
            node,
            saved_container_pos,
            saved_container_end,
            saved_declaration_list_container_end,
        );
    }

    pub(super) fn emit_comments_before_node(&self, node: &Node) {
        let emit_flags = get_emit_flags(node);
        let comment_range = get_comment_range(node);

        self.emit_leading_comments_of_node(
            node,
            emit_flags,
            comment_range.pos(),
            comment_range.end(),
        );
        if emit_flags.intersects(EmitFlags::NoNestedComments) {
            self.set_comments_disabled(true);
        }
    }

    pub(super) fn emit_comments_after_node(
        &self,
        node: &Node,
        saved_container_pos: isize,
        saved_container_end: isize,
        saved_declaration_list_container_end: isize,
    ) {
        let emit_flags = get_emit_flags(node);
        let comment_range = get_comment_range(node);

        if emit_flags.intersects(EmitFlags::NoNestedComments) {
            self.set_comments_disabled(false);
        }
        self.emit_trailing_comments_of_node(
            node,
            emit_flags,
            comment_range.pos(),
            comment_range.end(),
            saved_container_pos,
            saved_container_end,
            saved_declaration_list_container_end,
        );
    }

    pub(super) fn emit_leading_comments_of_node(
        &self,
        node: &Node,
        emit_flags: EmitFlags,
        pos: isize,
        end: isize,
    ) {
        self.enter_comment();
        self.set_has_written_comment(false);

        let skip_leading_comments = pos < 0
            || emit_flags.intersects(EmitFlags::NoLeadingComments)
            || node.kind() == SyntaxKind::JsxText;
        let skip_trailing_comments = end < 0
            || emit_flags.intersects(EmitFlags::NoTrailingComments)
            || node.kind() == SyntaxKind::JsxText;

        if (pos > 0 || end > 0) && pos != end {
            if !skip_leading_comments {
                self.emit_leading_comments(pos, node.kind() != SyntaxKind::NotEmittedStatement);
            }

            if !skip_leading_comments
                || pos >= 0 && emit_flags.intersects(EmitFlags::NoLeadingComments)
            {
                self.set_container_pos(pos);
            }

            if !skip_trailing_comments
                || end >= 0 && emit_flags.intersects(EmitFlags::NoTrailingComments)
            {
                self.set_container_end(end);

                if node.kind() == SyntaxKind::VariableDeclarationList {
                    self.set_declaration_list_container_end(end);
                }
            }
        }
        maybe_for_each(
            get_synthetic_leading_comments(node).as_ref(),
            |comment: &Rc<SynthesizedComment>, _| -> Option<()> {
                self.emit_leading_synthesized_comment(comment);
                None
            },
        );
        self.exit_comment();
    }

    pub(super) fn emit_trailing_comments_of_node(
        &self,
        node: &Node,
        emit_flags: EmitFlags,
        pos: isize,
        end: isize,
        saved_container_pos: isize,
        saved_container_end: isize,
        saved_declaration_list_container_end: isize,
    ) {
        self.enter_comment();
        let skip_trailing_comments = end < 0
            || emit_flags.intersects(EmitFlags::NoTrailingComments)
            || node.kind() == SyntaxKind::JsxText;
        maybe_for_each(
            get_synthetic_trailing_comments(node).as_ref(),
            |comment: &Rc<SynthesizedComment>, _| -> Option<()> {
                self.emit_trailing_synthesized_comment(comment);
                None
            },
        );
        if (pos > 0 || end > 0) && pos != end {
            self.set_container_pos(saved_container_pos);
            self.set_container_end(saved_container_end);
            self.set_declaration_list_container_end(saved_declaration_list_container_end);

            if !skip_trailing_comments && node.kind() != SyntaxKind::NotEmittedStatement {
                self.emit_trailing_comments(end);
            }
        }
        self.exit_comment();
    }

    pub(super) fn emit_leading_synthesized_comment(&self, comment: &SynthesizedComment) {
        unimplemented!()
    }

    pub(super) fn emit_trailing_synthesized_comment(&self, comment: &SynthesizedComment) {
        unimplemented!()
    }

    pub(super) fn emit_body_with_detached_comments<
        TDetachedRange: ReadonlyTextRange,
        TEmitCallback: FnMut(&Node),
    >(
        &self,
        node: &Node,
        detached_range: &TDetachedRange,
        emit_callback: TEmitCallback,
    ) {
        // unimplemented!()
    }

    pub(super) fn original_nodes_have_same_parent(&self, nodeA: &Node, nodeB: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn sibling_node_positions_are_comparable(
        &self,
        previous_node: &Node,
        next_node: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn emit_leading_comments(&self, pos: isize, is_emitted_node: bool) {
        // unimplemented!()
    }

    pub(super) fn emit_leading_comments_of_position(&self, pos: isize) {
        // unimplemented!()
    }

    pub(super) fn emit_trailing_comments(&self, pos: isize) {
        // unimplemented!()
    }

    pub(super) fn emit_trailing_comments_of_position(
        &self,
        pos: isize,
        prefix_space: Option<bool>,
        force_no_newline: Option<bool>,
    ) {
        // unimplemented!()
    }

    pub(super) fn pipeline_emit_with_source_maps(&self, hint: EmitHint, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_source_maps_before_node(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_source_maps_after_node(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_token_with_source_map<
        TWriter: FnMut(&str),
        TEmitCallback: FnMut(SyntaxKind, TWriter, isize),
    >(
        &self,
        node: Option<&Node>,
        token: SyntaxKind,
        writer: TWriter,
        token_pos: isize,
        emit_callback: TEmitCallback,
    ) -> isize {
        unimplemented!()
    }

    pub(super) fn set_source_map_source(&self, source: SourceMapSource) {
        unimplemented!()
    }
}

pub(super) fn create_brackets_map() -> HashMap<ListFormat, (&'static str, &'static str)> {
    HashMap::from_iter(IntoIterator::into_iter([
        (ListFormat::Braces, ("{", "}")),
        (ListFormat::Parenthesis, ("(", ")")),
        (ListFormat::AngleBrackets, ("<", ">")),
        (ListFormat::SquareBrackets, ("[", "]")),
    ]))
}

pub(super) fn get_opening_bracket(format: ListFormat) -> &'static str {
    brackets
        .get(&(format & ListFormat::BracketsMask))
        .unwrap()
        .0
}

pub(super) fn get_closing_bracket(format: ListFormat) -> &'static str {
    brackets
        .get(&(format & ListFormat::BracketsMask))
        .unwrap()
        .1
}

bitflags! {
    pub struct TempFlags: u32 {
        const Auto = 0x00000000;
        const CountMask = 0x0FFFFFFF;
        const _I = 0x10000000;
    }
}
