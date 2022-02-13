#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::hash::Hash;
use std::ptr;
use std::rc::Rc;

use super::escape_template_substitution;
use crate::{
    create_mode_aware_cache, escape_non_ascii_string, for_each_child_bool, get_combined_node_flags,
    get_compiler_option_value, get_line_and_character_of_position, get_line_starts,
    get_mode_for_resolution_at_index, get_root_declaration, id_text, is_big_int_literal,
    is_export_declaration, is_identifier, is_line_break, is_module_declaration,
    is_private_identifier, is_source_file, is_white_space_like,
    module_resolution_option_declarations, node_is_synthesized,
    options_affecting_program_structure, skip_trivia, text_char_at_index, text_substring,
    CharacterCodes, CommandLineOption, CompilerOptions, Debug_, EmitFlags, EmitTextWriter,
    LiteralLikeNodeInterface, ModeAwareCache, ModuleKind, Node, NodeArray, NodeFlags,
    NodeInterface, PackageId, ProjectReference, ReadonlyCollection, ReadonlyTextRange,
    ResolvedModuleFull, ResolvedTypeReferenceDirective, SourceFileLike, SourceTextAsChars, Symbol,
    SymbolFlags, SymbolInterface, SymbolTable, SymbolTracker, SymbolWriter, SyntaxKind,
    UnderscoreEscapedMap,
};

// resolvingEmptyArray: never[] = [];

pub const external_helpers_module_name_text: &str = "tslib";

pub const default_maximum_truncation_length: usize = 160;
pub const no_truncation_maximum_truncation_length: usize = 1_000_000;

pub fn get_declaration_of_kind(
    symbol: &Symbol,
    kind: SyntaxKind,
) -> Option<Rc<Node /*T extends Declaration*/>> {
    let maybe_declarations = symbol.maybe_declarations();
    let declarations = maybe_declarations.as_ref();
    if let Some(declarations) = declarations {
        for declaration in declarations {
            if declaration.kind() == kind {
                return Some(declaration.clone());
            }
        }
    }

    None
}

pub fn create_underscore_escaped_map<TValue>() -> UnderscoreEscapedMap<TValue> {
    UnderscoreEscapedMap::new()
}

// function hasEntries

pub fn create_symbol_table(symbols: Option<&[Rc<Symbol>]>) -> SymbolTable {
    let mut result = SymbolTable::new();
    if let Some(symbols) = symbols {
        for symbol in symbols {
            result.insert(symbol.escaped_name().clone(), symbol.clone());
        }
    }
    result
}

pub fn is_transient_symbol(symbol: &Symbol) -> bool {
    symbol.flags().intersects(SymbolFlags::Transient)
}

// lazy_static! {
//     static ref string_writer: Rc<RefCell<dyn EmitTextWriter>> = create_single_line_string_writer();
// }

fn string_writer() -> Rc<RefCell<dyn EmitTextWriter>> {
    create_single_line_string_writer()
}

fn create_single_line_string_writer() -> Rc<RefCell<dyn EmitTextWriter>> {
    Rc::new(RefCell::new(SingleLineStringWriter::new()))
}

struct SingleLineStringWriter {
    str: String,
}

impl SingleLineStringWriter {
    pub fn new() -> Self {
        Self {
            str: "".to_string(),
        }
    }

    fn write_text(&mut self, text: &str) {
        self.str.push_str(text);
    }
}

impl EmitTextWriter for SingleLineStringWriter {
    fn get_text(&self) -> String {
        self.str.clone()
    }

    fn write(&mut self, text: &str) {
        self.write_text(text);
    }

    fn raw_write(&mut self, text: &str) {
        self.write_text(text);
    }

    fn write_literal(&mut self, text: &str) {
        self.write_text(text);
    }

    fn write_trailing_semicolon(&mut self, text: &str) {
        self.write_text(text);
    }

    fn write_comment(&mut self, text: &str) {
        self.write_text(text);
    }

    fn get_text_pos(&self) -> usize {
        self.str.len()
    }

    fn get_line(&self) -> usize {
        0
    }

    fn get_column(&self) -> usize {
        0
    }

    fn get_indent(&self) -> usize {
        0
    }

    fn is_at_start_of_line(&self) -> bool {
        false
    }

    fn has_trailing_comment(&self) -> bool {
        false
    }

    fn has_trailing_whitespace(&self) -> bool {
        !self.str.is_empty() && is_white_space_like(self.str.chars().last().unwrap())
    }
}

impl SymbolWriter for SingleLineStringWriter {
    fn write_keyword(&mut self, text: &str) {
        self.write_text(text);
    }

    fn write_operator(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_punctuation(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_space(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_string_literal(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_parameter(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_property(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_symbol(&mut self, s: &str, _: &Symbol) {
        self.write_text(s);
    }

    fn write_line(&mut self, _: Option<bool>) {
        self.str.push_str(" ");
    }

    fn increase_indent(&mut self) {}

    fn decrease_indent(&mut self) {}

    fn clear(&mut self) {
        self.str = "".to_string();
    }
}

impl SymbolTracker for SingleLineStringWriter {
    fn track_symbol(
        &mut self,
        symbol: &Symbol,
        enclosing_declaration: Option<Rc<Node>>,
        meaning: SymbolFlags,
    ) -> Option<bool> {
        Some(false)
    }

    fn report_inaccessible_this_error(&mut self) {}

    fn report_inaccessible_unique_symbol_error(&mut self) {}

    fn report_private_in_base_of_class_expression(&mut self, _property_name: &str) {}
}

pub fn changes_affect_module_resolution(
    old_options: &CompilerOptions,
    new_options: &CompilerOptions,
) -> bool {
    old_options.config_file_path != new_options.config_file_path
        || options_have_module_resolution_changes(old_options, new_options)
}

pub fn options_have_module_resolution_changes(
    old_options: &CompilerOptions,
    new_options: &CompilerOptions,
) -> bool {
    module_resolution_option_declarations.with(|module_resolution_option_declarations_| {
        options_have_changes(
            old_options,
            new_options,
            &*module_resolution_option_declarations_,
        )
    })
}

pub fn changes_affecting_program_structure(
    old_options: &CompilerOptions,
    new_options: &CompilerOptions,
) -> bool {
    options_affecting_program_structure.with(|options_affecting_program_structure_| {
        options_have_changes(
            old_options,
            new_options,
            &*options_affecting_program_structure_,
        )
    })
}

pub fn options_have_changes(
    old_options: &CompilerOptions,
    new_options: &CompilerOptions,
    option_declarations: &[Rc<CommandLineOption>],
) -> bool {
    !ptr::eq(old_options, new_options)
        && option_declarations.iter().any(|o| {
            /* !is_json_equal(*/
            get_compiler_option_value(old_options, o) != get_compiler_option_value(new_options, o)
            /*)*/
        })
}

pub enum ForEachAncestorReturn<TReturn> {
    Option(Option<TReturn>),
    Quit,
}

impl<TReturn> From<Option<TReturn>> for ForEachAncestorReturn<TReturn> {
    fn from(value: Option<TReturn>) -> Self {
        Self::Option(value)
    }
}

pub fn for_each_ancestor<
    TReturn,
    TCallbackReturn: Into<ForEachAncestorReturn<TReturn>>,
    TCallback: FnMut(&Node) -> TCallbackReturn,
>(
    node: &Node,
    mut callback: TCallback,
) -> Option<TReturn> {
    let mut node = node.node_wrapper();
    loop {
        let res = callback(&node).into();
        match res {
            ForEachAncestorReturn::Quit => {
                return None;
            }
            ForEachAncestorReturn::Option(option) => {
                if option.is_some() {
                    return option;
                }
            }
        }
        if is_source_file(&node) {
            return None;
        }
        node = node.parent();
    }
}

pub fn for_each_entry<
    TKey,
    TValue,
    TReturn,
    TCallback: FnMut(&TValue, &TKey) -> Option<TReturn>,
>(
    map: &HashMap<TKey, TValue>, /*ReadonlyESMap*/
    mut callback: TCallback,
) -> Option<TReturn> {
    for (key, value) in map {
        let result = callback(value, key);
        if result.is_some() {
            return result;
        }
    }
    None
}

pub fn for_each_key<
    TKey,
    TReturn,
    TMap: ReadonlyCollection<TKey>,
    TCallback: FnMut(TKey) -> Option<TReturn>,
>(
    map: TMap,
    mut callback: TCallback,
) -> Option<TReturn> {
    let iterator = map.keys();
    for key in iterator {
        let result = callback(key);
        if result.is_some() {
            return result;
        }
    }
    None
}

pub fn copy_entries<TKey: Clone + Eq + Hash, TValue: Clone>(
    source: &HashMap<TKey, TValue>,
    target: &mut HashMap<TKey, TValue>,
) {
    for (key, value) in source {
        target.insert(key.clone(), value.clone());
    }
}

pub fn using_single_line_string_writer<TAction: FnOnce(Rc<RefCell<dyn EmitTextWriter>>)>(
    action: TAction,
) -> String {
    let string_writer = string_writer();
    let old_string = (*string_writer).borrow().get_text();
    action(string_writer.clone());
    let mut string_writer = string_writer.borrow_mut();
    let ret = string_writer.get_text();
    string_writer.clear();
    string_writer.write_keyword(&old_string);
    ret
}

pub fn get_full_width(node: &Node) -> isize {
    node.end() - node.pos()
}

pub fn get_resolved_module<TSourceFile: Borrow<Node>>(
    source_file: Option<TSourceFile>, /*SourceFile*/
    module_name_text: &str,
    mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) -> Option<Rc<ResolvedModuleFull>> {
    if let Some(source_file) = source_file {
        let source_file = source_file.borrow();
        if let Some(source_file_resolved_modules) = source_file
            .as_source_file()
            .maybe_resolved_modules()
            .as_ref()
        {
            return source_file_resolved_modules
                .get(module_name_text, mode)
                .map(Clone::clone);
        }
    }
    None
}

pub fn set_resolved_module(
    source_file: &Node, /*SourceFile*/
    module_name_text: String,
    resolved_module: Rc<ResolvedModuleFull>,
    mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) {
    let mut source_file_resolved_modules = source_file.as_source_file().maybe_resolved_modules();
    if source_file_resolved_modules.is_none() {
        *source_file_resolved_modules = Some(create_mode_aware_cache());
    }

    source_file_resolved_modules
        .as_ref()
        .unwrap()
        .set(module_name_text, mode, resolved_module);
}

pub fn set_resolved_type_reference_directive(
    source_file: &Node, /*SourceFile*/
    type_reference_directive_name: String,
    resolved_type_reference_directive /*?*/: Rc<ResolvedTypeReferenceDirective>,
) {
    let mut source_file_resolved_type_reference_directive_names = source_file
        .as_source_file()
        .maybe_resolved_type_reference_directive_names();
    if source_file_resolved_type_reference_directive_names.is_none() {
        *source_file_resolved_type_reference_directive_names = Some(create_mode_aware_cache());
    }

    source_file_resolved_type_reference_directive_names
        .as_ref()
        .unwrap()
        .set(
            type_reference_directive_name,
            None,
            resolved_type_reference_directive,
        );
}

pub fn project_reference_is_equal_to(
    old_ref: &ProjectReference,
    new_ref: &ProjectReference,
) -> bool {
    old_ref.path == new_ref.path
        && old_ref.prepend.unwrap_or(false) == new_ref.prepend.unwrap_or(false)
        && old_ref.circular.unwrap_or(false) == new_ref.circular.unwrap_or(false)
}

pub fn module_resolution_is_equal_to(
    old_resolution: &ResolvedModuleFull,
    new_resolution: &ResolvedModuleFull,
) -> bool {
    old_resolution.is_external_library_import == new_resolution.is_external_library_import
        && old_resolution.extension == new_resolution.extension
        && old_resolution.resolved_file_name == new_resolution.resolved_file_name
        && old_resolution.original_path == new_resolution.original_path
        && package_id_is_equal(
            old_resolution.package_id.as_ref(),
            new_resolution.package_id.as_ref(),
        )
}

fn package_id_is_equal(a: Option<&PackageId>, b: Option<&PackageId>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) =>
        /*a === b ||*/
        {
            a.name == b.name && a.sub_module_name == b.sub_module_name && a.version == b.version
        }
        _ => false,
    }
}

pub fn package_id_to_string(package_id: &PackageId) -> String {
    let full_name: Cow<str> = if !package_id.sub_module_name.is_empty() {
        format!("{}/{}", package_id.name, package_id.sub_module_name).into()
    } else {
        (&package_id.name).into()
    };
    format!("{}@{}", full_name, package_id.version)
}

pub fn type_directive_is_equal_to(
    old_resolution: &ResolvedTypeReferenceDirective,
    new_resolution: &ResolvedTypeReferenceDirective,
) -> bool {
    old_resolution.resolved_file_name == new_resolution.resolved_file_name
        && old_resolution.primary == new_resolution.primary
        && old_resolution.original_path == new_resolution.original_path
}

pub fn has_changes_in_resolutions<
    TValue,
    TName: AsRef<str>,
    TOldSourceFile: Borrow<Node>,
    TComparer: FnMut(&TValue, &TValue) -> bool,
>(
    names: &[TName],
    new_resolutions: &[TValue],
    old_resolutions: Option<ModeAwareCache<TValue>>,
    old_source_file: Option<TOldSourceFile /*SourceFile*/>,
    mut comparer: TComparer,
) -> bool {
    Debug_.assert(names.len() == new_resolutions.len(), None);

    for (i, name) in names.iter().enumerate() {
        let name = name.as_ref();
        let new_resolution = &new_resolutions[i];
        let old_resolution = old_resolutions.as_ref().and_then(|old_resolutions| {
            old_resolutions.get(
                name,
                old_source_file.as_ref().and_then(|old_source_file| {
                    let old_source_file = old_source_file.borrow();
                    get_mode_for_resolution_at_index(old_source_file.as_source_file(), i)
                }),
            )
        });
        let changed = match old_resolution {
            Some(old_resolution) =>
            /* !newResolution ||*/
            {
                !comparer(old_resolution, new_resolution)
            }
            None =>
            /*newResolution*/
            {
                true
            }
        };
        if changed {
            return true;
        }
    }
    false
}

pub fn contains_parse_error(node: &Node) -> bool {
    aggregate_child_data(node);
    node.flags()
        .intersects(NodeFlags::ThisNodeOrAnySubNodesHasError)
}

fn aggregate_child_data(node: &Node) {
    if !node.flags().intersects(NodeFlags::HasAggregatedChildData) {
        let this_node_or_any_sub_nodes_has_error =
            node.flags().intersects(NodeFlags::ThisNodeHasError)
                || for_each_child_bool(
                    node,
                    |child| contains_parse_error(child),
                    Option::<fn(&NodeArray) -> bool>::None,
                );

        if this_node_or_any_sub_nodes_has_error {
            node.set_flags(node.flags() | NodeFlags::ThisNodeOrAnySubNodesHasError);
        }

        node.set_flags(node.flags() | NodeFlags::HasAggregatedChildData);
    }
}

pub fn get_source_file_of_node<TNode: Borrow<Node>>(
    node: Option<TNode>,
) -> Option<Rc<Node /*SourceFile*/>> {
    let mut node = node.map(|node| {
        let node = node.borrow();
        node.node_wrapper()
    });
    while matches!(node.as_ref(), Some(node) if node.kind() != SyntaxKind::SourceFile) {
        node = node.unwrap().maybe_parent();
    }
    node
}

pub fn get_source_file_of_module(module: &Symbol) -> Option<Rc<Node /*SourceFile*/>> {
    get_source_file_of_node(
        module
            .maybe_value_declaration()
            .as_ref()
            .map(|weak| weak.upgrade().unwrap())
            .or_else(|| get_non_augmentation_declaration(module)),
    )
}

pub fn is_statement_with_locals(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Block
            | SyntaxKind::CaseBlock
            | SyntaxKind::ForStatement
            | SyntaxKind::ForInStatement
            | SyntaxKind::ForOfStatement
    )
}

pub fn get_start_position_of_line<TSourceFile: SourceFileLike>(
    line: usize,
    source_file: &TSourceFile,
) -> usize {
    // Debug.assert(line >= 0);
    get_line_starts(source_file)[line]
}

pub fn node_pos_to_string(node: &Node) -> String {
    let file = get_source_file_of_node(Some(node)).unwrap();
    let file_as_source_file = file.as_source_file();
    let loc =
        get_line_and_character_of_position(file_as_source_file, node.pos().try_into().unwrap());
    format!(
        "{}({},{})",
        file_as_source_file.file_name(),
        loc.line + 1,
        loc.character + 1
    )
}

pub fn get_end_line_position<TSourceFile: SourceFileLike>(
    line: usize,
    source_file: &TSourceFile,
) -> usize {
    // Debug.assert(line >= 0);
    let line_starts = get_line_starts(source_file);

    let line_index = line;
    let source_text = source_file.text_as_chars();
    if line_index + 1 == line_starts.len() {
        source_text.len() - 1
    } else {
        let start = line_starts[line_index];
        let mut pos = line_starts[line_index + 1] - 1;
        Debug_.assert(is_line_break(text_char_at_index(&source_text, pos)), None);
        while start <= pos && is_line_break(text_char_at_index(&source_text, pos)) {
            pos -= 1;
        }
        pos
    }
}

pub fn is_file_level_unique_name<THasGlobalName: FnOnce(&str) -> bool>(
    source_file: &Node, /*SourceFile*/
    name: &str,
    mut has_global_name: Option<THasGlobalName>,
) -> bool {
    (match has_global_name {
        None => true,
        Some(has_global_name) => has_global_name(name),
    }) && !(*source_file.as_source_file().identifiers())
        .borrow()
        .contains_key(name)
}

pub fn node_is_missing<TNode: Borrow<Node>>(node: Option<TNode>) -> bool {
    if node.is_none() {
        return true;
    }
    let node = node.unwrap();
    let node = node.borrow();

    node.pos() == node.end() && node.pos() >= 0 && node.kind() != SyntaxKind::EndOfFileToken
}

pub fn node_is_present<TNode: Borrow<Node>>(node: Option<TNode>) -> bool {
    !node_is_missing(node)
}

fn get_source_text_of_node_from_source_file(
    source_file: &Node, /*SourceFile*/
    node: &Node,
    include_trivia: Option<bool>,
) -> String {
    let include_trivia = include_trivia.unwrap_or(false);
    get_text_of_node_from_source_text(
        &*source_file.as_source_file().text_as_chars(),
        node,
        Some(include_trivia),
    )
}

pub fn get_text_of_node_from_source_text(
    source_text: &SourceTextAsChars,
    node: &Node,
    include_trivia: Option<bool>,
) -> String {
    let include_trivia = include_trivia.unwrap_or(false);
    if node_is_missing(Some(node)) {
        return "".to_string();
    }

    let start = if include_trivia {
        node.pos()
    } else {
        skip_trivia(source_text, node.pos(), None, None, None)
    };
    let end = node.end();
    if !(start >= 0 && end >= 0 && end - start >= 0) {
        return "".to_string();
    }
    let start = start as usize;
    let end = end as usize;
    let text = text_substring(source_text, start, end);

    text
}

fn get_text_of_node(node: &Node, include_trivia: Option<bool>) -> String {
    let include_trivia = include_trivia.unwrap_or(false);
    get_source_text_of_node_from_source_file(
        &*get_source_file_of_node(Some(node)).unwrap(),
        node,
        Some(include_trivia),
    )
}

pub fn get_emit_flags(node: &Node) -> EmitFlags {
    EmitFlags::None
}

bitflags! {
    pub struct GetLiteralTextFlags: u32 {
        const None = 0;
        const NeverAsciiEscape = 1 << 0;
        const JsxAttributeEscape = 1 << 1;
        const TerminateUnterminatedLiterals = 1 << 2;
        const AllowNumericSeparator = 1 << 3;
    }
}

pub fn get_literal_text<TNodeRef: Borrow<Node>>(
    node: &Node, /*LiteralLikeNode*/
    source_file: Option<TNodeRef /*SourceFile*/>,
    flags: GetLiteralTextFlags,
) -> String {
    if can_use_original_text(node, flags) {
        return get_source_text_of_node_from_source_file(source_file.unwrap().borrow(), node, None);
    }

    match node {
        Node::StringLiteral(node_as_string_literal) => {
            let escape_text = if flags.intersects(GetLiteralTextFlags::JsxAttributeEscape) {
                unimplemented!()
            } else if flags.intersects(GetLiteralTextFlags::NeverAsciiEscape)
                || get_emit_flags(node).intersects(EmitFlags::NoAsciiEscaping)
            {
                unimplemented!()
            } else {
                escape_non_ascii_string
            };
            if matches!(node_as_string_literal.single_quote, Some(true)) {
                format!(
                    "'{}'",
                    escape_text(
                        &*node_as_string_literal.text(),
                        Some(CharacterCodes::single_quote)
                    )
                )
            } else {
                format!(
                    "\"{}\"",
                    escape_text(
                        &*node_as_string_literal.text(),
                        Some(CharacterCodes::double_quote)
                    )
                )
            }
        }
        Node::TemplateLiteralLikeNode(node_as_template_literal_like_node) => {
            let escape_text = if flags.intersects(GetLiteralTextFlags::NeverAsciiEscape)
                || get_emit_flags(node).intersects(EmitFlags::NoAsciiEscaping)
            {
                unimplemented!()
            } else {
                escape_non_ascii_string
            };

            let raw_text = node_as_template_literal_like_node
                .raw_text
                .clone()
                .unwrap_or_else(|| {
                    escape_template_substitution(&escape_text(
                        &*node_as_template_literal_like_node.text(),
                        Some(CharacterCodes::backtick),
                    ))
                });

            match node.kind() {
                SyntaxKind::NoSubstitutionTemplateLiteral => format!("`{}`", raw_text),
                SyntaxKind::TemplateHead => format!("`{}${{", raw_text),
                SyntaxKind::TemplateMiddle => format!("}}{}${{", raw_text),
                SyntaxKind::TemplateTail => format!("}}{}`", raw_text),
                _ => panic!("Unexpected TemplateLiteralLikeNode kind"),
            }
        }
        Node::NumericLiteral(_) | Node::BigIntLiteral(_) => {
            node.as_literal_like_node().text().to_string()
        }
        _ => Debug_.fail(Some(&format!(
            "Literal kind '{:?}' not accounted for.",
            node.kind()
        ))),
    }
}

fn can_use_original_text(node: &Node /*LiteralLikeNode*/, flags: GetLiteralTextFlags) -> bool {
    if node_is_synthesized(node)
        || node.maybe_parent().is_none()
        || flags.intersects(GetLiteralTextFlags::TerminateUnterminatedLiterals)
            && matches!(node.as_literal_like_node().is_unterminated(), Some(true))
    {
        return false;
    }

    !is_big_int_literal(node)
}

pub fn is_block_or_catch_scoped(declaration: &Node /*Declaration*/) -> bool {
    get_combined_node_flags(declaration).intersects(NodeFlags::BlockScoped)
        || is_catch_clause_variable_declaration_or_binding_element(declaration)
}

fn is_catch_clause_variable_declaration_or_binding_element(
    declaration: &Node, /*Declaration*/
) -> bool {
    let node = get_root_declaration(declaration);
    node.kind() == SyntaxKind::VariableDeclaration
        && node.parent().kind() == SyntaxKind::CatchClause
}

pub fn is_ambient_module(node: &Node) -> bool {
    is_module_declaration(node)
        && (node.as_module_declaration().name.kind() == SyntaxKind::StringLiteral
            || is_global_scope_augmentation(node))
}

pub fn is_global_scope_augmentation(module: &Node /*ModuleDeclaration*/) -> bool {
    module.flags().intersects(NodeFlags::GlobalAugmentation)
}

pub fn is_external_module_augmentation(node: &Node) -> bool {
    unimplemented!()
}

pub fn get_non_augmentation_declaration(symbol: &Symbol) -> Option<Rc<Node /*Declaration*/>> {
    symbol
        .maybe_declarations()
        .as_ref()
        .and_then(|declarations| {
            declarations
                .iter()
                .find(|d| {
                    !is_external_module_augmentation(d)
                        && !(is_module_declaration(d) && is_global_scope_augmentation(d))
                })
                .map(Clone::clone)
        })
}

pub fn is_any_import_syntax(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ImportDeclaration | SyntaxKind::ImportEqualsDeclaration
    )
}

pub fn is_any_import_or_re_export(node: &Node) -> bool {
    is_any_import_syntax(node) || is_export_declaration(node)
}

pub fn declaration_name_to_string<TNodeRef: Borrow<Node>>(name: Option<TNodeRef>) -> String {
    match name {
        None => "(Missing)".to_string(),
        Some(name) => {
            let name = name.borrow();
            if get_full_width(name) == 0 {
                "(Missing)".to_string()
            } else {
                get_text_of_node(name, None)
            }
        }
    }
}

pub fn entity_name_to_string(
    name: &Node, /*EntityNameOrEntityNameExpression | JSDocMemberName | JsxTagNameExpression | PrivateIdentifier*/
) -> Cow<'static, str> {
    match name.kind() {
        SyntaxKind::ThisKeyword => "this".into(),
        SyntaxKind::Identifier | SyntaxKind::PrivateIdentifier => {
            if get_full_width(name) == 0 {
                id_text(name).into()
            } else {
                get_text_of_node(name, None).into()
            }
        }
        SyntaxKind::QualifiedName => {
            let name_as_qualified_name = name.as_qualified_name();
            format!(
                "{}.{}",
                entity_name_to_string(&name_as_qualified_name.left),
                entity_name_to_string(&name_as_qualified_name.right)
            )
            .into()
        }
        SyntaxKind::PropertyAccessExpression => {
            let name_as_property_access_expression = name.as_property_access_expression();
            if is_identifier(&*name_as_property_access_expression.name)
                || is_private_identifier(&*name_as_property_access_expression.name)
            {
                format!(
                    "{}.{}",
                    entity_name_to_string(&name_as_property_access_expression.expression),
                    entity_name_to_string(&name_as_property_access_expression.name)
                )
                .into()
            } else {
                Debug_.assert_never(&name_as_property_access_expression.name, None)
            }
        }
        SyntaxKind::JSDocMemberName => {
            let name_as_jsdoc_member_name = name.as_jsdoc_member_name();
            format!(
                "{}{}",
                entity_name_to_string(&name_as_jsdoc_member_name.left),
                entity_name_to_string(&name_as_jsdoc_member_name.right)
            )
            .into()
        }
        _ => Debug_.assert_never(name, None),
    }
}
