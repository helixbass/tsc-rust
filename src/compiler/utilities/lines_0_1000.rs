#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use regex::Regex;
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::hash::Hash;
use std::iter::FromIterator;
use std::ptr;
use std::rc::Rc;

use super::{
    default_lib_reference_reg_ex, escape_template_substitution,
    full_triple_slash_reference_type_reference_directive_reg_ex,
};
use crate::{
    Type, __String, binary_search_copy_key, compare_values, create_mode_aware_cache,
    escape_jsx_attribute_string, escape_leading_underscores, escape_non_ascii_string,
    escape_string, find_ancestor, for_each_child_bool, full_triple_slash_amd_reference_path_reg_ex,
    full_triple_slash_reference_path_reg_ex, get_base_file_name, get_combined_node_flags,
    get_compiler_option_value, get_emit_module_kind, get_line_and_character_of_position,
    get_line_starts, get_mode_for_resolution_at_index, get_root_declaration,
    get_strict_option_value, has_jsdoc_nodes, id_text, is_big_int_literal, is_export_declaration,
    is_external_module, is_function_like_or_class_static_block_declaration, is_identifier,
    is_import_call, is_import_type_node, is_in_jsdoc, is_jsdoc_node, is_jsdoc_type_expression,
    is_line_break, is_module_declaration, is_namespace_export, is_numeric_literal,
    is_private_identifier, is_prologue_directive, is_source_file, is_string_literal,
    is_string_or_numeric_literal_like, is_white_space_like, maybe_text_char_at_index,
    module_resolution_option_declarations, node_is_synthesized,
    options_affecting_program_structure, skip_trivia, starts_with_use_strict, text_char_at_index,
    text_substring, trim_string_start, CharacterCodes, CommandLineOption, CommentDirective,
    CommentDirectiveType, CommentDirectivesMap, CompilerOptions, Debug_, EmitFlags, EmitTextWriter,
    IndexInfo, LiteralLikeNodeInterface, ModeAwareCache, ModuleKind, NamedDeclarationInterface,
    Node, NodeArray, NodeFlags, NodeInterface, PackageId, ProjectReference, ReadonlyCollection,
    ReadonlyTextRange, ResolvedModuleFull, ResolvedTypeReferenceDirective, ScriptKind,
    SignatureDeclarationInterface, SourceFileLike, SourceTextAsChars, StringOrNumber, Symbol,
    SymbolFlags, SymbolInterface, SymbolTable, SymbolTracker, SymbolWriter, SyntaxKind, TextRange,
    TokenFlags, UnderscoreEscapedMap,
};

thread_local! {
    static resolving_empty_array_: Rc<Vec<Rc<Type>>> = Rc::new(vec![]);
}
pub fn resolving_empty_array() -> Rc<Vec<Rc<Type>>> {
    resolving_empty_array_.with(|resolving_empty_array| resolving_empty_array.clone())
}

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

    fn as_symbol_tracker(&self) -> &dyn SymbolTracker {
        self
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

    fn report_inaccessible_unique_symbol_error(&self) {}

    fn report_private_in_base_of_class_expression(&self, _property_name: &str) {}
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

pub fn for_each_entry_bool<TKey, TValue, TCallback: FnMut(&TValue, &TKey) -> bool>(
    map: &HashMap<TKey, TValue>, /*ReadonlyESMap*/
    mut callback: TCallback,
) -> bool {
    for (key, value) in map {
        let result = callback(value, key);
        if result {
            return result;
        }
    }
    false
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
                .flatten();
        }
    }
    None
}

pub fn set_resolved_module(
    source_file: &Node, /*SourceFile*/
    module_name_text: &str,
    resolved_module: Rc<ResolvedModuleFull>,
    mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) {
    let mut source_file_resolved_modules = source_file.as_source_file().maybe_resolved_modules();
    if source_file_resolved_modules.is_none() {
        *source_file_resolved_modules = Some(create_mode_aware_cache());
    }

    source_file_resolved_modules.as_ref().unwrap().set(
        module_name_text,
        mode,
        Some(resolved_module),
    );
}

pub fn set_resolved_type_reference_directive(
    source_file: &Node, /*SourceFile*/
    type_reference_directive_name: &str,
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
    TValue: Clone,
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
        let changed = match old_resolution.as_ref() {
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
            .map(Clone::clone)
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

fn insert_statements_after_prologue<TIsPrologueDirective: FnMut(&Node) -> bool>(
    to: &mut Vec<Rc<Node>>,
    from: Option<&[Rc<Node>]>,
    mut is_prologue_directive: TIsPrologueDirective,
) {
    if from.is_none() {
        return /*to*/;
    }
    let from = from.unwrap();
    if from.is_empty() {
        return /*to*/;
    }
    let mut statement_index = 0;
    while statement_index < to.len() {
        if !is_prologue_directive(&to[statement_index]) {
            break;
        }
        statement_index += 1;
    }
    to.splice(
        statement_index..statement_index,
        from.into_iter().map(Clone::clone),
    );
}

fn insert_statement_after_prologue<TIsPrologueDirective: FnMut(&Node) -> bool>(
    to: &mut Vec<Rc<Node>>,
    statement: Option<Rc<Node>>,
    mut is_prologue_directive: TIsPrologueDirective,
) {
    if statement.is_none() {
        return /*to*/;
    }
    let statement = statement.unwrap();
    let mut statement_index = 0;
    while statement_index < to.len() {
        if !is_prologue_directive(&to[statement_index]) {
            break;
        }
        statement_index += 1;
    }
    to.insert(statement_index, statement);
}

fn is_any_prologue_directive(node: &Node) -> bool {
    is_prologue_directive(node) || get_emit_flags(node).intersects(EmitFlags::CustomPrologue)
}

pub fn insert_statements_after_standard_prologue(
    to: &mut Vec<Rc<Node>>,
    from: Option<&[Rc<Node>]>,
) {
    insert_statements_after_prologue(to, from, is_prologue_directive)
}

pub fn insert_statements_after_custom_prologue(to: &mut Vec<Rc<Node>>, from: Option<&[Rc<Node>]>) {
    insert_statements_after_prologue(to, from, is_any_prologue_directive)
}

pub fn insert_statement_after_standard_prologue(
    to: &mut Vec<Rc<Node>>,
    statement: Option<Rc<Node>>,
) {
    insert_statement_after_prologue(to, statement, is_prologue_directive)
}

pub fn insert_statement_after_custom_prologue(to: &mut Vec<Rc<Node>>, statement: Option<Rc<Node>>) {
    insert_statement_after_prologue(to, statement, is_any_prologue_directive)
}

pub fn is_recognized_triple_slash_comment(
    text: &SourceTextAsChars,
    comment_pos: usize,
    comment_end: usize,
) -> bool {
    if matches!(maybe_text_char_at_index(text, comment_pos + 1), Some(ch) if ch == CharacterCodes::slash)
        && comment_pos + 2 < comment_end
        && text_char_at_index(text, comment_pos + 2) == CharacterCodes::slash
    {
        let text_sub_str = text_substring(text, comment_pos, comment_end);
        return full_triple_slash_reference_path_reg_ex.is_match(&text_sub_str)
            || full_triple_slash_amd_reference_path_reg_ex.is_match(&text_sub_str)
            || full_triple_slash_reference_type_reference_directive_reg_ex.is_match(&text_sub_str)
            || default_lib_reference_reg_ex.is_match(&text_sub_str);
    }
    false
}

pub fn is_pinned_comment(text: &SourceTextAsChars, start: usize) -> bool {
    matches!(maybe_text_char_at_index(text, start + 1), Some(ch) if ch == CharacterCodes::asterisk)
        && matches!(maybe_text_char_at_index(text, start + 2), Some(ch) if ch == CharacterCodes::exclamation)
}

pub fn create_comment_directives_map(
    source_file: &Node, /*SourceFile*/
    comment_directives: &[Rc<CommentDirective>],
) -> CommentDirectivesMap {
    let directives_by_line: HashMap<String, Rc<CommentDirective>> =
        HashMap::from_iter(comment_directives.iter().map(|comment_directive| {
            (
                format!(
                    "{}",
                    get_line_and_character_of_position(
                        source_file.as_source_file(),
                        comment_directive.range.end().try_into().unwrap()
                    )
                    .line
                ),
                comment_directive.clone(),
            )
        }));

    let used_lines = HashMap::<String, bool>::new();

    CommentDirectivesMap::new(directives_by_line, used_lines)
}

impl CommentDirectivesMap {
    pub fn get_unused_expectations(&self) -> Vec<Rc<CommentDirective>> {
        /*arrayFrom(*/
        self.directives_by_line
            .iter() /*)*/
            .filter(|(line, directive)| {
                directive.type_ == CommentDirectiveType::ExpectError
                    && !matches!(self.used_lines.get(*line), Some(true))
            })
            .map(|(_, directive)| directive.clone())
            .collect()
    }

    pub fn mark_used(&mut self, line: usize) -> bool {
        let line_string = line.to_string();
        if !self.directives_by_line.contains_key(&line_string) {
            return false;
        }

        self.used_lines.insert(line_string, true);
        true
    }
}

// TODO: if source_file needs to be able to be some other SourceFileLike besides a SourceFile, I
// guess accept eg a &dyn SourceFileLike so that falling back to get_source_file_of_node() can be
// type-compatible?
// pub fn get_token_pos_of_node<TSourceFile: SourceFileLike>(
//     node: &Node,
//     source_file: Option<TSourceFile>,
pub fn get_token_pos_of_node<TSourceFile: Borrow<Node>>(
    node: &Node,
    source_file: Option<TSourceFile>,
    include_js_doc: Option<bool>,
) -> isize {
    if node_is_missing(Some(node)) {
        return node.pos();
    }

    if is_jsdoc_node(node) || node.kind() == SyntaxKind::JsxText {
        return skip_trivia(
            &source_file
                .as_ref()
                .map_or_else(
                    || get_source_file_of_node(Some(node)).unwrap(),
                    |source_file| source_file.borrow().node_wrapper(),
                )
                .as_source_file()
                .text_as_chars(),
            node.pos(),
            Some(false),
            Some(true),
            None,
        );
    }

    if include_js_doc.unwrap_or(false) && has_jsdoc_nodes(node) {
        return get_token_pos_of_node(&node.maybe_js_doc().unwrap()[0], source_file, None);
    }

    if node.kind() == SyntaxKind::SyntaxList {
        let node_as_syntax_list = node.as_syntax_list();
        if !node_as_syntax_list._children.is_empty() {
            return get_token_pos_of_node(
                &node_as_syntax_list._children[0],
                source_file,
                include_js_doc,
            );
        }
    }

    skip_trivia(
        &source_file
            .as_ref()
            .map_or_else(
                || get_source_file_of_node(Some(node)).unwrap(),
                |source_file| source_file.borrow().node_wrapper(),
            )
            .as_source_file()
            .text_as_chars(),
        node.pos(),
        Some(false),
        Some(false),
        Some(is_in_jsdoc(Some(node))),
    )
}

pub fn get_non_decorator_token_pos_of_node<TSourceFile: Borrow<Node>>(
    node: &Node,
    source_file: Option<TSourceFile>,
) -> isize {
    if node_is_missing(Some(node)) || node.maybe_decorators().is_none() {
        return get_token_pos_of_node(node, source_file, None);
    }

    skip_trivia(
        &source_file
            .as_ref()
            .map_or_else(
                || get_source_file_of_node(Some(node)).unwrap(),
                |source_file| source_file.borrow().node_wrapper(),
            )
            .as_source_file()
            .text_as_chars(),
        node.maybe_decorators().as_ref().unwrap().end(),
        None,
        None,
        None,
    )
}

pub fn get_source_text_of_node_from_source_file(
    source_file: &Node, /*SourceFile*/
    node: &Node,
    include_trivia: Option<bool>,
) -> Cow<'static, str> {
    let include_trivia = include_trivia.unwrap_or(false);
    get_text_of_node_from_source_text(
        &source_file.as_source_file().text_as_chars(),
        node,
        Some(include_trivia),
    )
}

fn is_jsdoc_type_expression_or_child(node: &Node) -> bool {
    find_ancestor(Some(node), |node| is_jsdoc_type_expression(node)).is_some()
}

pub fn is_export_namespace_as_default_declaration(node: &Node) -> bool {
    if !is_export_declaration(node) {
        return false;
    }
    let node_as_export_declaration = node.as_export_declaration();
    if node_as_export_declaration.export_clause.is_none() {
        return false;
    }
    let node_export_clause = node_as_export_declaration.export_clause.as_ref().unwrap();
    if !is_namespace_export(node_export_clause) {
        return false;
    }
    let node_export_clause_as_namespace_export = node_export_clause.as_namespace_export();
    node_export_clause_as_namespace_export
        .name
        .as_identifier()
        .escaped_text
        .eq_str("default")
}

pub fn get_text_of_node_from_source_text(
    source_text: &SourceTextAsChars,
    node: &Node,
    include_trivia: Option<bool>,
) -> Cow<'static, str> {
    let include_trivia = include_trivia.unwrap_or(false);
    if node_is_missing(Some(node)) {
        return "".into();
    }

    let start = if include_trivia {
        node.pos()
    } else {
        skip_trivia(source_text, node.pos(), None, None, None)
    };
    let end = node.end();
    if !(start >= 0 && end >= 0 && end - start >= 0) {
        return "".into();
    }
    let mut text = text_substring(
        source_text,
        start.try_into().unwrap(),
        end.try_into().unwrap(),
    );

    if is_jsdoc_type_expression_or_child(node) {
        lazy_static! {
            static ref line_ending_regex: Regex = Regex::new(r#"\r\n|\n|\r"#).unwrap();
        }
        text = line_ending_regex
            .split(&text)
            .map(|line| {
                lazy_static! {
                    static ref leading_star_regex: Regex = Regex::new(r#"^\s*\*"#).unwrap();
                }
                trim_string_start(&leading_star_regex.replace(line, "")).to_owned()
            })
            .collect::<Vec<_>>()
            .join("\n");
    }

    text.into()
}

pub fn get_text_of_node(node: &Node, include_trivia: Option<bool>) -> Cow<'static, str> {
    let include_trivia = include_trivia.unwrap_or(false);
    get_source_text_of_node_from_source_file(
        &get_source_file_of_node(Some(node)).unwrap(),
        node,
        Some(include_trivia),
    )
}

fn get_pos(range: &Node) -> isize {
    range.pos()
}

pub fn index_of_node(node_array: &[Rc<Node>], node: &Node) -> isize {
    binary_search_copy_key(
        node_array,
        &node.node_wrapper(),
        |node, _| get_pos(node),
        |a, b| compare_values(Some(a), Some(b)),
        None,
    )
}

pub fn get_emit_flags(node: &Node) -> EmitFlags {
    node.maybe_emit_node()
        .as_ref()
        .and_then(|emit_node| emit_node.flags)
        .unwrap_or(EmitFlags::None)
}

pub type ScriptTargetFeatures = HashMap<&'static str, HashMap<&'static str, Vec<&'static str>>>;

pub fn get_script_target_features() -> ScriptTargetFeatures {
    HashMap::from_iter(IntoIterator::into_iter([
        (
            "es2015",
            HashMap::from_iter(IntoIterator::into_iter([
                (
                    "Array",
                    vec![
                        "find",
                        "findIndex",
                        "fill",
                        "copyWithin",
                        "entries",
                        "keys",
                        "values",
                    ],
                ),
                ("RegExp", vec!["flags", "sticky", "unicode"]),
                (
                    "Reflect",
                    vec![
                        "apply",
                        "construct",
                        "defineProperty",
                        "deleteProperty",
                        "get",
                        " getOwnPropertyDescriptor",
                        "getPrototypeOf",
                        "has",
                        "isExtensible",
                        "ownKeys",
                        "preventExtensions",
                        "set",
                        "setPrototypeOf",
                    ],
                ),
                ("ArrayConstructor", vec!["from", "of"]),
                (
                    "ObjectConstructor",
                    vec![
                        "assign",
                        "getOwnPropertySymbols",
                        "keys",
                        "is",
                        "setPrototypeOf",
                    ],
                ),
                (
                    "NumberConstructor",
                    vec![
                        "isFinite",
                        "isInteger",
                        "isNaN",
                        "isSafeInteger",
                        "parseFloat",
                        "parseInt",
                    ],
                ),
                (
                    "Math",
                    vec![
                        "clz32", "imul", "sign", "log10", "log2", "log1p", "expm1", "cosh", "sinh",
                        "tanh", "acosh", "asinh", "atanh", "hypot", "trunc", "fround", "cbrt",
                    ],
                ),
                ("Map", vec!["entries", "keys", "values"]),
                ("Set", vec!["entries", "keys", "values"]),
                ("Promise", vec![]),
                (
                    "PromiseConstructor",
                    vec!["all", "race", "reject", "resolve"],
                ),
                ("Symbol", vec!["for", "keyFor"]),
                ("WeakMap", vec!["entries", "keys", "values"]),
                ("WeakSet", vec!["entries", "keys", "values"]),
                ("Iterator", vec![]),
                ("AsyncIterator", vec![]),
                (
                    "String",
                    vec![
                        "codePointAt",
                        "includes",
                        "endsWith",
                        "normalize",
                        "repeat",
                        "startsWith",
                        "anchor",
                        "big",
                        "blink",
                        "bold",
                        "fixed",
                        "fontcolor",
                        "fontsize",
                        "italics",
                        "link",
                        "small",
                        "strike",
                        "sub",
                        "sup",
                    ],
                ),
                ("StringConstructor", vec!["fromCodePoint", "raw"]),
            ])),
        ),
        (
            "es2016",
            HashMap::from_iter(IntoIterator::into_iter([("Array", vec!["includes"])])),
        ),
        (
            "es2017",
            HashMap::from_iter(IntoIterator::into_iter([
                ("Atomics", vec![]),
                ("SharedArrayBuffer", vec![]),
                ("String", vec!["padStart", "padEnd"]),
                (
                    "ObjectConstructor",
                    vec!["values", "entries", "getOwnPropertyDescriptors"],
                ),
                ("DateTimeFormat", vec!["formatToParts"]),
            ])),
        ),
        (
            "es2018",
            HashMap::from_iter(IntoIterator::into_iter([
                ("Promise", vec!["finally"]),
                ("RegExpMatchArray", vec!["groups"]),
                ("RegExpExecArray", vec!["groups"]),
                ("RegExp", vec!["dotAll"]),
                ("Intl", vec!["PluralRules"]),
                ("AsyncIterable", vec![]),
                ("AsyncIterableIterator", vec![]),
                ("AsyncGenerator", vec![]),
                ("AsyncGeneratorFunction", vec![]),
            ])),
        ),
        (
            "es2019",
            HashMap::from_iter(IntoIterator::into_iter([
                ("Array", vec!["flat", "flatMap"]),
                ("ObjectConstructor", vec!["fromEntries"]),
                (
                    "String",
                    vec!["trimStart", "trimEnd", "trimLeft", "trimRight"],
                ),
                ("Symbol", vec!["description"]),
            ])),
        ),
        (
            "es2020",
            HashMap::from_iter(IntoIterator::into_iter([
                ("BigInt", vec![]),
                ("BigInt64Array", vec![]),
                ("BigUint64Array", vec![]),
                ("PromiseConstructor", vec!["allSettled"]),
                ("SymbolConstructor", vec!["matchAll"]),
                ("String", vec!["matchAll"]),
                (
                    "DataView",
                    vec!["setBigInt64", "setBigUint64", "getBigInt64", "getBigUint64"],
                ),
                (
                    "RelativeTimeFormat",
                    vec!["format", "formatToParts", "resolvedOptions"],
                ),
            ])),
        ),
        (
            "es2021",
            HashMap::from_iter(IntoIterator::into_iter([
                ("PromiseConstructor", vec!["any"]),
                ("String", vec!["replaceAll"]),
            ])),
        ),
        (
            "esnext",
            HashMap::from_iter(IntoIterator::into_iter([(
                "NumberFormat",
                vec!["formatToParts"],
            )])),
        ),
    ]))
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

pub fn get_literal_text<TSourceFile: Borrow<Node>>(
    node: &Node, /*LiteralLikeNode*/
    source_file: Option<TSourceFile /*SourceFile*/>,
    flags: GetLiteralTextFlags,
) -> Cow<'static, str> {
    if can_use_original_text(node, flags) {
        return get_source_text_of_node_from_source_file(source_file.unwrap().borrow(), node, None);
    }

    match node {
        Node::StringLiteral(node_as_string_literal) => {
            let escape_text = if flags.intersects(GetLiteralTextFlags::JsxAttributeEscape) {
                escape_jsx_attribute_string
            } else if flags.intersects(GetLiteralTextFlags::NeverAsciiEscape)
                || get_emit_flags(node).intersects(EmitFlags::NoAsciiEscaping)
            {
                escape_string
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
                .into()
            } else {
                format!(
                    "\"{}\"",
                    escape_text(
                        &*node_as_string_literal.text(),
                        Some(CharacterCodes::double_quote)
                    )
                )
                .into()
            }
        }
        Node::TemplateLiteralLikeNode(node_as_template_literal_like_node) => {
            let escape_text = if flags.intersects(GetLiteralTextFlags::NeverAsciiEscape)
                || get_emit_flags(node).intersects(EmitFlags::NoAsciiEscaping)
            {
                escape_string
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
                SyntaxKind::NoSubstitutionTemplateLiteral => format!("`{}`", raw_text).into(),
                SyntaxKind::TemplateHead => format!("`{}${{", raw_text).into(),
                SyntaxKind::TemplateMiddle => format!("}}{}${{", raw_text).into(),
                SyntaxKind::TemplateTail => format!("}}{}`", raw_text).into(),
                _ => panic!("Unexpected TemplateLiteralLikeNode kind"),
            }
        }
        Node::NumericLiteral(_) | Node::BigIntLiteral(_) => {
            node.as_literal_like_node().text().to_owned().into()
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

    if is_numeric_literal(node)
        && node
            .as_numeric_literal()
            .numeric_literal_flags
            .intersects(TokenFlags::ContainsSeparator)
    {
        return flags.intersects(GetLiteralTextFlags::AllowNumericSeparator);
    }

    !is_big_int_literal(node)
}

pub fn get_text_of_constant_value<TValue: Into<StringOrNumber>>(value: TValue) -> String {
    match value.into() {
        StringOrNumber::String(value) => format!("\"{}\"", escape_non_ascii_string(&value, None)),
        StringOrNumber::Number(value) => format!("{}", value),
    }
}

pub fn make_identifier_from_module_name(module_name: &str) -> String {
    lazy_static! {
        static ref leading_digit_regex: Regex = Regex::new(r#"^(\d)"#).unwrap();
    }
    lazy_static! {
        static ref nonword_regex: Regex = Regex::new(r#"\W"#/*/g*/).unwrap();
    }
    nonword_regex
        .replace_all(
            &leading_digit_regex.replace(&get_base_file_name(module_name, None, None), "_$1"),
            "_",
        )
        .into_owned()
}

pub fn is_block_or_catch_scoped(declaration: &Node /*Declaration*/) -> bool {
    get_combined_node_flags(declaration).intersects(NodeFlags::BlockScoped)
        || is_catch_clause_variable_declaration_or_binding_element(declaration)
}

pub fn is_catch_clause_variable_declaration_or_binding_element(
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

pub fn is_module_with_string_literal_name(node: &Node) -> bool {
    is_module_declaration(node)
        && node.as_module_declaration().name.kind() == SyntaxKind::StringLiteral
}

pub fn is_non_global_ambient_module(node: &Node) -> bool {
    is_module_declaration(node) && is_string_literal(&node.as_module_declaration().name)
}

pub fn is_effective_module_declaration(node: &Node) -> bool {
    is_module_declaration(node) || is_identifier(node)
}

pub fn is_shorthand_ambient_module_symbol(module_symbol: &Symbol) -> bool {
    is_shorthand_ambient_module(module_symbol.maybe_value_declaration())
}

fn is_shorthand_ambient_module<TNode: Borrow<Node>>(node: Option<TNode>) -> bool {
    match node {
        None => false,
        Some(node) => {
            let node = node.borrow();
            node.kind() == SyntaxKind::ModuleDeclaration
                && node.as_module_declaration().body.is_none()
        }
    }
}

pub fn is_block_scoped_container_top_level(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::SourceFile | SyntaxKind::ModuleDeclaration
    ) || is_function_like_or_class_static_block_declaration(Some(node))
}

pub fn is_global_scope_augmentation(module: &Node /*ModuleDeclaration*/) -> bool {
    module.flags().intersects(NodeFlags::GlobalAugmentation)
}

pub fn is_external_module_augmentation(node: &Node) -> bool {
    is_ambient_module(node) && is_module_augmentation_external(node)
}

pub fn is_module_augmentation_external(node: &Node /*AmbientModuleDeclaration*/) -> bool {
    match node.parent().kind() {
        SyntaxKind::SourceFile => is_external_module(&node.parent()),
        SyntaxKind::ModuleBlock => {
            is_ambient_module(&node.parent().parent())
                && is_source_file(&node.parent().parent().parent())
                && !is_external_module(&node.parent().parent().parent())
        }
        _ => false,
    }
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

fn is_common_js_containing_module_kind(kind: ModuleKind) -> bool {
    matches!(
        kind,
        ModuleKind::CommonJS | ModuleKind::Node12 | ModuleKind::NodeNext
    )
}

pub fn is_effective_external_module(
    node: &Node, /*SourceFile*/
    compiler_options: &CompilerOptions,
) -> bool {
    is_external_module(node)
        || matches!(compiler_options.isolated_modules, Some(true))
        || (is_common_js_containing_module_kind(get_emit_module_kind(compiler_options))
            && node
                .as_source_file()
                .maybe_common_js_module_indicator()
                .is_some())
}

pub fn is_effective_strict_mode_source_file(
    node: &Node, /*SourceFile*/
    compiler_options: &CompilerOptions,
) -> bool {
    let node_as_source_file = node.as_source_file();
    match node_as_source_file.script_kind() {
        ScriptKind::JS | ScriptKind::TS | ScriptKind::JSX | ScriptKind::TSX => (),
        _ => {
            return false;
        }
    }
    if node_as_source_file.is_declaration_file() {
        return false;
    }
    if get_strict_option_value(compiler_options, "alwaysStrict") {
        return true;
    }
    if starts_with_use_strict(&node_as_source_file.statements) {
        return true;
    }
    if is_external_module(node) || compiler_options.isolated_modules.unwrap_or(false) {
        if get_emit_module_kind(compiler_options) >= ModuleKind::ES2015 {
            return true;
        }
        return !compiler_options.no_implicit_use_strict.unwrap_or(false);
    }
    false
}

pub fn is_block_scope<TParentNode: Borrow<Node>>(
    node: &Node,
    parent_node: Option<TParentNode>,
) -> bool {
    match node.kind() {
        SyntaxKind::SourceFile
        | SyntaxKind::CaseBlock
        | SyntaxKind::CatchClause
        | SyntaxKind::ModuleDeclaration
        | SyntaxKind::ForStatement
        | SyntaxKind::ForInStatement
        | SyntaxKind::ForOfStatement
        | SyntaxKind::Constructor
        | SyntaxKind::MethodDeclaration
        | SyntaxKind::GetAccessor
        | SyntaxKind::SetAccessor
        | SyntaxKind::FunctionDeclaration
        | SyntaxKind::FunctionExpression
        | SyntaxKind::ArrowFunction
        | SyntaxKind::PropertyDeclaration
        | SyntaxKind::ClassStaticBlockDeclaration => true,
        SyntaxKind::Block => !is_function_like_or_class_static_block_declaration(parent_node),
        _ => false,
    }
}

pub fn is_declaration_with_type_parameters(node: &Node) -> bool {
    match node.kind() {
        SyntaxKind::JSDocCallbackTag | SyntaxKind::JSDocTypedefTag | SyntaxKind::JSDocSignature => {
            true
        }
        _ => {
            // assertType<DeclarationWithTypeParameterChildren>(node);
            is_declaration_with_type_parameter_children(node)
        }
    }
}

pub fn is_declaration_with_type_parameter_children(node: &Node) -> bool {
    match node.kind() {
        SyntaxKind::CallSignature
        | SyntaxKind::ConstructSignature
        | SyntaxKind::MethodSignature
        | SyntaxKind::IndexSignature
        | SyntaxKind::FunctionType
        | SyntaxKind::ConstructorType
        | SyntaxKind::JSDocFunctionType
        | SyntaxKind::ClassDeclaration
        | SyntaxKind::ClassExpression
        | SyntaxKind::InterfaceDeclaration
        | SyntaxKind::TypeAliasDeclaration
        | SyntaxKind::JSDocTemplateTag
        | SyntaxKind::FunctionDeclaration
        | SyntaxKind::MethodDeclaration
        | SyntaxKind::Constructor
        | SyntaxKind::GetAccessor
        | SyntaxKind::SetAccessor
        | SyntaxKind::FunctionExpression
        | SyntaxKind::ArrowFunction => true,
        _ => {
            // assertType<never>(node);
            false
        }
    }
}

pub fn is_any_import_syntax(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ImportDeclaration | SyntaxKind::ImportEqualsDeclaration
    )
}

pub fn is_late_visibility_painted_statement(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ImportDeclaration
            | SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::VariableStatement
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::EnumDeclaration
    )
}

pub fn has_possible_external_module_reference(node: &Node) -> bool {
    is_any_import_or_re_export(node)
        || is_module_declaration(node)
        || is_import_type_node(node)
        || is_import_call(node)
}

pub fn is_any_import_or_re_export(node: &Node) -> bool {
    is_any_import_syntax(node) || is_export_declaration(node)
}

pub fn get_enclosing_block_scope_container(node: &Node) -> Option<Rc<Node>> {
    find_ancestor(node.maybe_parent(), |current| {
        is_block_scope(current, current.maybe_parent())
    })
}

pub fn for_each_enclosing_block_scope_container<TCallback: FnMut(&Node)>(
    node: &Node,
    mut cb: TCallback,
) {
    let mut container = get_enclosing_block_scope_container(node);
    while let Some(container_present) = container {
        cb(&container_present);
        container = get_enclosing_block_scope_container(&container_present);
    }
}

pub fn declaration_name_to_string<TName: Borrow<Node>>(name: Option<TName>) -> Cow<'static, str> {
    match name {
        None => "(Missing)".into(),
        Some(name) => {
            let name = name.borrow();
            if get_full_width(name) == 0 {
                "(Missing)".into()
            } else {
                get_text_of_node(name, None)
            }
        }
    }
}

pub fn get_name_from_index_info(info: &IndexInfo) -> Option<Cow<'static, str>> {
    info.declaration.as_ref().map(|info_declaration| {
        declaration_name_to_string(
            info_declaration
                .as_index_signature_declaration()
                .parameters()[0]
                .as_parameter_declaration()
                .maybe_name(),
        )
    })
}

pub fn is_computed_non_literal_name(name: &Node /*PropertyName*/) -> bool {
    name.kind() == SyntaxKind::ComputedPropertyName
        && !is_string_or_numeric_literal_like(&name.as_computed_property_name().expression)
}

pub fn get_text_of_property_name(
    name: &Node, /*PropertyName | NoSubstitutionTemplateLiteral*/
) -> __String {
    match name.kind() {
        SyntaxKind::Identifier => name.as_identifier().escaped_text.clone(),
        SyntaxKind::PrivateIdentifier => name.as_private_identifier().escaped_text.clone(),
        SyntaxKind::StringLiteral => escape_leading_underscores(&name.as_string_literal().text()),
        SyntaxKind::NumericLiteral => escape_leading_underscores(&name.as_numeric_literal().text()),
        SyntaxKind::NoSubstitutionTemplateLiteral => {
            escape_leading_underscores(&name.as_template_literal_like_node().text())
        }
        SyntaxKind::ComputedPropertyName => {
            let name_as_computed_property_name = name.as_computed_property_name();
            if is_string_or_numeric_literal_like(&name_as_computed_property_name.expression) {
                return escape_leading_underscores(
                    &name_as_computed_property_name
                        .expression
                        .as_literal_like_node()
                        .text(),
                );
            }
            Debug_.fail(Some(
                "Text of property name cannot be read from non-literal-valued ComputedPropertyName",
            ))
        }
        _ => Debug_.assert_never(name, None),
    }
}

pub fn entity_name_to_string(
    name: &Node, /*EntityNameOrEntityNameExpression | JSDocMemberName | JsxTagNameExpression | PrivateIdentifier*/
) -> Cow<'static, str> {
    match name.kind() {
        SyntaxKind::ThisKeyword => "this".into(),
        SyntaxKind::PrivateIdentifier | SyntaxKind::Identifier => {
            if get_full_width(name) == 0 {
                id_text(name).into()
            } else {
                get_text_of_node(name, None)
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
            if is_identifier(&name_as_property_access_expression.name)
                || is_private_identifier(&name_as_property_access_expression.name)
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
