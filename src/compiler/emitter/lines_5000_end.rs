use bitflags::bitflags;
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::brackets;
use crate::{
    escape_leading_underscores, get_node_id, is_file_level_unique_name, is_node_descendant_of,
    EmitHint, GeneratedIdentifierFlags, ListFormat, Node, NodeInterface, Printer,
    ReadonlyTextRange, SourceMapSource, SymbolFlags, SymbolInterface, SyntaxKind,
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

    pub(super) fn make_file_level_optimistic_unique_name(&self, name: &str) -> String {
        unimplemented!()
    }

    pub(super) fn get_node_for_generated_name(
        &self,
        name: &Node, /*GeneratedIdentifier*/
    ) -> Rc<Node> {
        unimplemented!()
    }

    pub(super) fn generate_name_for_node(
        &self,
        name: &Node,
        flags: Option<GeneratedIdentifierFlags>,
    ) -> String {
        unimplemented!()
    }

    pub(super) fn make_name(&self, name: &Node /*GeneratedIdentifier*/) -> String {
        unimplemented!()
    }

    pub(super) fn pipeline_emit_with_comments(&self, hint: EmitHint, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_comments_before_node(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_comments_after_node(
        &self,
        node: &Node,
        saved_container_pos: isize,
        saved_container_end: isize,
        saved_declaration_list_container_end: isize,
    ) {
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

    pub(super) fn emit_leading_comments_of_position(&self, pos: isize) {
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
