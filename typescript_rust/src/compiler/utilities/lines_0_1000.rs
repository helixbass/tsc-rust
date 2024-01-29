use std::{
    borrow::{Borrow, Cow},
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    convert::TryInto,
    hash::Hash,
    io,
    iter::FromIterator,
    mem, ptr,
    rc::Rc,
};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;
use indexmap::IndexMap;
use regex::Regex;

use super::{
    default_lib_reference_reg_ex, escape_template_substitution,
    full_triple_slash_reference_type_reference_directive_reg_ex,
};
use crate::{
    binary_search_copy_key, compare_values, create_mode_aware_cache, escape_jsx_attribute_string,
    escape_leading_underscores, escape_non_ascii_string, escape_string, find_ancestor,
    for_each_child_bool, full_triple_slash_amd_reference_path_reg_ex,
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
    text_substring, trim_string_start, AllArenas, CharacterCodes, CommandLineOption,
    CommentDirective, CommentDirectiveType, CommentDirectivesMap, CompilerOptions, Debug_,
    EmitFlags, EmitTextWriter, HasArena, HasStatementsInterface, InArena, IndexInfo,
    LiteralLikeNodeInterface, ModeAwareCache, ModuleKind, NamedDeclarationInterface, Node,
    NodeArray, NodeFlags, NodeInterface, PackageId, ProjectReference, ReadonlyCollection,
    ReadonlyTextRange, ResolvedModuleFull, ResolvedTypeReferenceDirective, ScriptKind,
    SignatureDeclarationInterface, SourceFileLike, SourceTextAsChars, StringOrNumber, Symbol,
    SymbolFlags, SymbolInterface, SymbolTable, SymbolTracker, SymbolWriter, SyntaxKind, TextRange,
    TokenFlags, Type, UnderscoreEscapedMap, OptionInArena,
    per_arena,
};

pub fn resolving_empty_array(arena: &impl HasArena) -> Id<Vec<Id<Type>>> {
    per_arena!(
        Vec<Id<Type>>,
        arena,
        arena.alloc_vec_type(Default::default())
    )
}

pub const external_helpers_module_name_text: &str = "tslib";

pub const default_maximum_truncation_length: usize = 160;
pub const no_truncation_maximum_truncation_length: usize = 1_000_000;

pub fn get_declaration_of_kind(
    symbol: Id<Symbol>,
    kind: SyntaxKind,
    arena: &impl HasArena,
) -> Option<Id<Node /*T extends Declaration*/>> {
    let symbol_ref = symbol.ref_(arena);
    let maybe_declarations = symbol_ref.maybe_declarations();
    let declarations = maybe_declarations.as_ref();
    if let Some(declarations) = declarations {
        for &declaration in declarations {
            if declaration.ref_(arena).kind() == kind {
                return Some(declaration);
            }
        }
    }

    None
}

pub fn create_underscore_escaped_map<TValue>() -> UnderscoreEscapedMap<TValue> {
    UnderscoreEscapedMap::new()
}

// function hasEntries

pub fn create_symbol_table(
    arena: &AllArenas,
    symbols: Option<impl IntoIterator<Item = impl Borrow<Id<Symbol>>>>,
) -> SymbolTable {
    let mut result = SymbolTable::new();
    if let Some(symbols) = symbols {
        for symbol in symbols {
            let &symbol: &Id<Symbol> = symbol.borrow();
            result.insert(
                arena.symbol(symbol).escaped_name().to_owned(),
                symbol.clone(),
            );
        }
    }
    result
}

pub fn is_transient_symbol(symbol: &Symbol) -> bool {
    symbol.flags().intersects(SymbolFlags::Transient)
}

// lazy_static! {
//     static ref string_writer: Id<Box<dyn EmitTextWriter>> = create_single_line_string_writer();
// }

fn string_writer(arena: &impl HasArena) -> Id<Box<dyn EmitTextWriter>> {
    create_single_line_string_writer(arena)
}

fn create_single_line_string_writer(arena: &impl HasArena) -> Id<Box<dyn EmitTextWriter>> {
    SingleLineStringWriter::new(arena)
}

#[derive(Trace, Finalize)]
struct SingleLineStringWriter {
    _dyn_symbol_tracker_wrapper: Id<Box<dyn SymbolTracker>>,
    #[unsafe_ignore_trace]
    str: RefCell<String>,
}

impl SingleLineStringWriter {
    pub fn new(arena: &impl HasArena) -> Id<Box<dyn EmitTextWriter>> {
        arena.alloc_emit_text_writer(Box::new(Self {
            _dyn_symbol_tracker_wrapper: arena.alloc_symbol_tracker(Box::new(SingleLineStringWriterSymbolTracker)),
            str: Default::default(),
        }))
    }

    fn as_dyn_symbol_tracker(&self) -> Id<Box<dyn SymbolTracker>> {
        self._dyn_symbol_tracker_wrapper
    }

    fn str(&self) -> Ref<String> {
        self.str.borrow()
    }

    fn str_mut(&self) -> RefMut<String> {
        self.str.borrow_mut()
    }

    fn set_str(&self, str: String) {
        *self.str.borrow_mut() = str;
    }

    fn write_text(&self, text: &str) {
        self.str_mut().push_str(text);
    }
}

impl EmitTextWriter for SingleLineStringWriter {
    fn get_text(&self) -> String {
        self.str().clone()
    }

    fn write(&self, text: &str) {
        self.write_text(text);
    }

    fn raw_write(&self, text: &str) {
        self.write_text(text);
    }

    fn write_literal(&self, text: &str) {
        self.write_text(text);
    }

    fn write_trailing_semicolon(&self, text: &str) {
        self.write_text(text);
    }

    fn write_comment(&self, text: &str) {
        self.write_text(text);
    }

    fn get_text_pos(&self) -> usize {
        self.str().len()
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
        !self.str().is_empty() && is_white_space_like(self.str().chars().last().unwrap())
    }

    fn is_non_escaping_write_supported(&self) -> bool {
        false
    }
}

impl SymbolWriter for SingleLineStringWriter {
    fn write_keyword(&self, text: &str) {
        self.write_text(text);
    }

    fn write_operator(&self, s: &str) {
        self.write_text(s);
    }

    fn write_punctuation(&self, s: &str) {
        self.write_text(s);
    }

    fn write_space(&self, s: &str) {
        self.write_text(s);
    }

    fn write_string_literal(&self, s: &str) {
        self.write_text(s);
    }

    fn write_parameter(&self, s: &str) {
        self.write_text(s);
    }

    fn write_property(&self, s: &str) {
        self.write_text(s);
    }

    fn write_symbol(&self, s: &str, _: Id<Symbol>) {
        self.write_text(s);
    }

    fn write_line(&self, _: Option<bool>) {
        self.str_mut().push_str(" ");
    }

    fn increase_indent(&self) {}

    fn decrease_indent(&self) {}

    fn clear(&self) {
        self.set_str("".to_owned());
    }

    fn as_symbol_tracker(&self) -> Id<Box<dyn SymbolTracker>> {
        self.as_dyn_symbol_tracker()
    }
}

impl SymbolTracker for SingleLineStringWriter {
    fn track_symbol(
        &self,
        symbol: Id<Symbol>,
        enclosing_declaration: Option<Id<Node>>,
        meaning: SymbolFlags,
    ) -> Option<io::Result<bool>> {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).track_symbol(symbol, enclosing_declaration, meaning)
    }

    fn is_track_symbol_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper.ref_(self).is_track_symbol_supported()
    }

    fn disable_track_symbol(&self) {
        self._dyn_symbol_tracker_wrapper.ref_(self).disable_track_symbol();
    }

    fn reenable_track_symbol(&self) {
        self._dyn_symbol_tracker_wrapper.ref_(self).reenable_track_symbol();
    }

    fn report_inaccessible_this_error(&self) {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).report_inaccessible_this_error()
    }

    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_inaccessible_this_error_supported()
    }

    fn report_inaccessible_unique_symbol_error(&self) {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).report_inaccessible_unique_symbol_error()
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_inaccessible_unique_symbol_error_supported()
    }

    fn report_private_in_base_of_class_expression(&self, _property_name: &str) {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).report_private_in_base_of_class_expression(_property_name)
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_private_in_base_of_class_expression_supported()
    }

    fn is_report_cyclic_structure_error_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_cyclic_structure_error_supported()
    }

    fn is_report_likely_unsafe_import_required_error_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_likely_unsafe_import_required_error_supported()
    }

    fn is_report_nonlocal_augmentation_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_nonlocal_augmentation_supported()
    }

    fn is_report_non_serializable_property_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_non_serializable_property_supported()
    }

    fn is_module_resolver_host_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_module_resolver_host_supported()
    }

    fn is_track_referenced_ambient_module_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_track_referenced_ambient_module_supported()
    }
}

impl HasArena for SingleLineStringWriter {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct SingleLineStringWriterSymbolTracker;

impl SymbolTracker for SingleLineStringWriterSymbolTracker {
    fn track_symbol(
        &self,
        _symbol: Id<Symbol>,
        _enclosing_declaration: Option<Id<Node>>,
        _meaning: SymbolFlags,
    ) -> Option<io::Result<bool>> {
        Some(Ok(false))
    }

    fn is_track_symbol_supported(&self) -> bool {
        true
    }

    fn disable_track_symbol(&self) {}

    fn reenable_track_symbol(&self) {}

    fn report_inaccessible_this_error(&self) {}

    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        true
    }

    fn report_inaccessible_unique_symbol_error(&self) {}

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        true
    }

    fn report_private_in_base_of_class_expression(&self, _property_name: &str) {}

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        true
    }

    fn is_report_cyclic_structure_error_supported(&self) -> bool {
        false
    }

    fn is_report_likely_unsafe_import_required_error_supported(&self) -> bool {
        false
    }

    fn is_report_nonlocal_augmentation_supported(&self) -> bool {
        false
    }

    fn is_report_non_serializable_property_supported(&self) -> bool {
        false
    }

    fn is_module_resolver_host_supported(&self) -> bool {
        false
    }

    fn is_track_referenced_ambient_module_supported(&self) -> bool {
        false
    }
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
    option_declarations: &[Gc<CommandLineOption>],
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

pub fn for_each_ancestor<TReturn, TCallbackReturn: Into<ForEachAncestorReturn<TReturn>>>(
    mut node: Id<Node>,
    mut callback: impl FnMut(Id<Node>) -> TCallbackReturn,
    arena: &impl HasArena,
) -> Option<TReturn> {
    loop {
        let res = callback(node).into();
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
        if is_source_file(&node.ref_(arena)) {
            return None;
        }
        node = node.ref_(arena).parent();
    }
}

pub fn for_each_entry<TKey, TValue, TReturn>(
    map: impl IntoIterator<Item = (TKey, TValue)>, /*ReadonlyESMap*/
    mut callback: impl FnMut(TValue, TKey) -> Option<TReturn>,
) -> Option<TReturn> {
    for (key, value) in map {
        let result = callback(value, key);
        if result.is_some() {
            return result;
        }
    }
    None
}

pub fn try_for_each_entry<TKey, TValue, TReturn, TError>(
    map: impl IntoIterator<Item = (TKey, TValue)>, /*ReadonlyESMap*/
    mut callback: impl FnMut(TValue, TKey) -> Result<Option<TReturn>, TError>,
) -> Result<Option<TReturn>, TError> {
    for (key, value) in map {
        let result = callback(value, key)?;
        if result.is_some() {
            return Ok(result);
        }
    }
    Ok(None)
}

pub fn for_each_entry_bool<TKey, TValue>(
    map: impl IntoIterator<Item = (TKey, TValue)>, /*ReadonlyESMap*/
    mut callback: impl FnMut(TValue, TKey) -> bool,
) -> bool {
    for (key, value) in map {
        let result = callback(value, key);
        if result {
            return result;
        }
    }
    false
}

pub fn try_for_each_entry_bool<TKey, TValue, TError>(
    map: impl IntoIterator<Item = (TKey, TValue)>, /*ReadonlyESMap*/
    mut callback: impl FnMut(TValue, TKey) -> Result<bool, TError>,
) -> Result<bool, TError> {
    for (key, value) in map {
        let result = callback(value, key)?;
        if result {
            return Ok(result);
        }
    }
    Ok(false)
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
    source: &IndexMap<TKey, TValue>,
    target: &mut IndexMap<TKey, TValue>,
) {
    for (key, value) in source {
        target.insert(key.clone(), value.clone());
    }
}

pub fn using_single_line_string_writer(
    action: impl FnOnce(Id<Box<dyn EmitTextWriter>>) -> io::Result<()>,
    arena: &impl HasArena,
) -> io::Result<String> {
    let string_writer = string_writer(arena);
    let old_string = string_writer.ref_(arena).get_text();
    action(string_writer.clone())?;
    let ret = string_writer.ref_(arena).get_text();
    string_writer.ref_(arena).clear();
    string_writer.ref_(arena).write_keyword(&old_string);
    Ok(ret)
}

pub fn try_using_single_line_string_writer<TError>(
    action: impl FnOnce(Id<Box<dyn EmitTextWriter>>) -> Result<(), TError>,
    arena: &impl HasArena,
) -> Result<String, TError> {
    let string_writer = string_writer(arena);
    let old_string = string_writer.ref_(arena).get_text();
    action(string_writer.clone())?;
    let ret = string_writer.ref_(arena).get_text();
    string_writer.ref_(arena).clear();
    string_writer.ref_(arena).write_keyword(&old_string);
    Ok(ret)
}

pub fn get_full_width(node: &Node) -> isize {
    node.end() - node.pos()
}

pub fn get_resolved_module(
    source_file: Option<&Node>, /*SourceFile*/
    module_name_text: &str,
    mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) -> Option<Id<ResolvedModuleFull>> {
    if let Some(source_file) = source_file {
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
    resolved_module: Option<Id<ResolvedModuleFull>>,
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
    type_reference_directive_name: &str,
    resolved_type_reference_directive: Option<Id<ResolvedTypeReferenceDirective>>,
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

pub fn has_changes_in_resolutions<TValue: Clone + Trace + Finalize>(
    names: &[impl AsRef<str>],
    new_resolutions: &[TValue],
    old_resolutions: Option<&ModeAwareCache<TValue>>,
    old_source_file: Option<Id<Node /*SourceFile*/>>,
    mut comparer: impl FnMut(&TValue, &TValue) -> bool,
    arena: &impl HasArena,
) -> bool {
    Debug_.assert(names.len() == new_resolutions.len(), None);

    for (i, name) in names.iter().enumerate() {
        let name = name.as_ref();
        let new_resolution = &new_resolutions[i];
        let old_resolution = old_resolutions.and_then(|old_resolutions| {
            old_resolutions.get(
                name,
                old_source_file.and_then(|old_source_file| {
                    get_mode_for_resolution_at_index(old_source_file.ref_(arena).as_source_file(), i, arena)
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

pub fn contains_parse_error(node: Id<Node>, arena: &impl HasArena) -> bool {
    aggregate_child_data(node, arena);
    node.ref_(arena).flags()
        .intersects(NodeFlags::ThisNodeOrAnySubNodesHasError)
}

fn aggregate_child_data(node: Id<Node>, arena: &impl HasArena) {
    if !node.ref_(arena).flags().intersects(NodeFlags::HasAggregatedChildData) {
        let this_node_or_any_sub_nodes_has_error =
            node.ref_(arena).flags().intersects(NodeFlags::ThisNodeHasError)
                || for_each_child_bool(
                    node,
                    |child| contains_parse_error(child, arena),
                    Option::<fn(Id<NodeArray>) -> bool>::None,
                    arena,
                );

        if this_node_or_any_sub_nodes_has_error {
            node.ref_(arena).set_flags(node.ref_(arena).flags() | NodeFlags::ThisNodeOrAnySubNodesHasError);
        }

        node.ref_(arena).set_flags(node.ref_(arena).flags() | NodeFlags::HasAggregatedChildData);
    }
}

pub fn get_source_file_of_node(
    mut node: Id<Node>,
    arena: &impl HasArena,
) -> Id<Node /*SourceFile*/> {
    while node.ref_(arena).kind() != SyntaxKind::SourceFile {
        node = node.ref_(arena).parent();
    }
    node
}

pub fn maybe_get_source_file_of_node(mut node: Option<Id<Node>>, arena: &impl HasArena) -> Option<Id<Node /*SourceFile*/>> {
    // node.map(|node| get_source_file_of_node(node.borrow()))
    while matches!(
        node,
        Some(node) if node.ref_(arena).kind() != SyntaxKind::SourceFile
    ) {
        node = node.unwrap().ref_(arena).maybe_parent();
    }
    node
}

pub fn get_source_file_of_module(module: Id<Symbol>, arena: &impl HasArena) -> Option<Id<Node /*SourceFile*/>> {
    maybe_get_source_file_of_node(
        module
            .ref_(arena).maybe_value_declaration()
            .or_else(|| get_non_augmentation_declaration(module, arena)),
        arena,
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

pub fn get_start_position_of_line(line: usize, source_file: &impl SourceFileLike) -> usize {
    // Debug.assert(line >= 0);
    get_line_starts(source_file)[line]
}

pub fn node_pos_to_string(node: Id<Node>, arena: &impl HasArena) -> String {
    let file = get_source_file_of_node(node, arena);
    let file_ref = file.ref_(arena);
    let file_as_source_file = file_ref.as_source_file();
    let loc =
        get_line_and_character_of_position(file_as_source_file, node.ref_(arena).pos().try_into().unwrap());
    format!(
        "{}({},{})",
        file_as_source_file.file_name(),
        loc.line + 1,
        loc.character + 1
    )
}

pub fn get_end_line_position(line: usize, source_file: &impl SourceFileLike) -> usize {
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

pub fn is_file_level_unique_name(
    source_file: &Node, /*SourceFile*/
    name: &str,
    has_global_name: Option<impl FnOnce(&str) -> bool>,
) -> bool {
    (match has_global_name {
        None => true,
        Some(has_global_name) => has_global_name(name),
    }) && !(*source_file.as_source_file().identifiers())
        .borrow()
        .contains_key(name)
}

pub fn node_is_missing(node: Option<&Node>) -> bool {
    let Some(node) = node else {
        return true;
    };

    node.pos() == node.end() && node.pos() >= 0 && node.kind() != SyntaxKind::EndOfFileToken
}

pub fn node_is_present(node: Option<&Node>) -> bool {
    !node_is_missing(node)
}

fn insert_statements_after_prologue(
    to: &mut Vec<Id<Node>>,
    from: Option<&[Id<Node>]>,
    mut is_prologue_directive: impl FnMut(Id<Node>) -> bool,
) {
    let Some(from) = from else {
        return /*to*/;
    };
    if from.is_empty() {
        return /*to*/;
    }
    let mut statement_index = 0;
    while statement_index < to.len() {
        if !is_prologue_directive(to[statement_index]) {
            break;
        }
        statement_index += 1;
    }
    to.splice(
        statement_index..statement_index,
        from.into_iter().copied(),
    );
}

fn insert_statement_after_prologue(
    to: &mut Vec<Id<Node>>,
    statement: Option<Id<Node>>,
    mut is_prologue_directive: impl FnMut(Id<Node>) -> bool,
) {
    if statement.is_none() {
        return /*to*/;
    }
    let statement = statement.unwrap();
    let mut statement_index = 0;
    while statement_index < to.len() {
        if !is_prologue_directive(to[statement_index]) {
            break;
        }
        statement_index += 1;
    }
    to.insert(statement_index, statement);
}

fn is_any_prologue_directive(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_prologue_directive(node, arena) || get_emit_flags(node, arena).intersects(EmitFlags::CustomPrologue)
}

pub fn insert_statements_after_standard_prologue(
    to: &mut Vec<Id<Node>>,
    from: Option<&[Id<Node>]>,
    arena: &impl HasArena,
) {
    insert_statements_after_prologue(to, from, |node| is_prologue_directive(node, arena))
}

pub fn insert_statements_after_custom_prologue(
    to: &mut Vec<Id<Node>>,
    from: Option<&[Id<Node>]>,
    arena: &impl HasArena,
) {
    insert_statements_after_prologue(to, from, |node| is_any_prologue_directive(node, arena))
}

pub fn insert_statement_after_standard_prologue(
    to: &mut Vec<Id<Node>>,
    statement: Option<Id<Node>>,
    arena: &impl HasArena,
) {
    insert_statement_after_prologue(to, statement, |node| is_prologue_directive(node, arena))
}

pub fn insert_statement_after_custom_prologue(
    to: &mut Vec<Id<Node>>,
    statement: Option<Id<Node>>,
    arena: &impl HasArena,
) {
    insert_statement_after_prologue(to, statement, |node| is_any_prologue_directive(node, arena))
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
//     node: Id<Node>,
//     source_file: Option<TSourceFile>,
pub fn get_token_pos_of_node(
    node: Id<Node>,
    source_file: Option<Id<Node>>,
    include_js_doc: Option<bool>,
    arena: &impl HasArena,
) -> isize {
    if node_is_missing(Some(&node.ref_(arena))) {
        return node.ref_(arena).pos();
    }

    if is_jsdoc_node(&node.ref_(arena)) || node.ref_(arena).kind() == SyntaxKind::JsxText {
        return skip_trivia(
            &source_file
                .unwrap_or_else(
                    || get_source_file_of_node(node, arena),
                )
                .ref_(arena).as_source_file()
                .text_as_chars(),
            node.ref_(arena).pos(),
            Some(false),
            Some(true),
            None,
        );
    }

    if include_js_doc.unwrap_or(false) && has_jsdoc_nodes(&node.ref_(arena)) {
        return get_token_pos_of_node(node.ref_(arena).maybe_js_doc().unwrap()[0], source_file, None, arena);
    }

    if node.ref_(arena).kind() == SyntaxKind::SyntaxList {
        let node_ref = node.ref_(arena);
        let node_as_syntax_list = node_ref.as_syntax_list();
        if !node_as_syntax_list._children.is_empty() {
            return get_token_pos_of_node(
                node_as_syntax_list._children[0],
                source_file,
                include_js_doc,
                arena,
            );
        }
    }

    skip_trivia(
        &source_file
            .unwrap_or_else(
                || get_source_file_of_node(node, arena),
            )
            .ref_(arena).as_source_file()
            .text_as_chars(),
        node.ref_(arena).pos(),
        Some(false),
        Some(false),
        Some(is_in_jsdoc(Some(&node.ref_(arena)))),
    )
}

pub fn get_non_decorator_token_pos_of_node(node: Id<Node>, source_file: Option<Id<Node>>, arena: &impl HasArena) -> isize {
    if node_is_missing(Some(&node.ref_(arena))) || node.ref_(arena).maybe_decorators().is_none() {
        return get_token_pos_of_node(node, source_file, None, arena);
    }

    skip_trivia(
        &source_file
            .unwrap_or_else(|| get_source_file_of_node(node, arena))
            .ref_(arena).as_source_file()
            .text_as_chars(),
        node.ref_(arena).maybe_decorators().as_ref().unwrap().ref_(arena).end(),
        None,
        None,
        None,
    )
}

pub fn get_source_text_of_node_from_source_file(
    source_file: Id<Node>, /*SourceFile*/
    node: Id<Node>,
    include_trivia: Option<bool>,
    arena: &impl HasArena,
) -> Cow<'static, str> {
    let include_trivia = include_trivia.unwrap_or(false);
    get_text_of_node_from_source_text(
        &source_file.ref_(arena).as_source_file().text_as_chars(),
        node,
        Some(include_trivia),
        arena,
    )
}

fn is_jsdoc_type_expression_or_child(node: Id<Node>, arena: &impl HasArena) -> bool {
    find_ancestor(Some(node), |node| is_jsdoc_type_expression(&node.ref_(arena)), arena).is_some()
}

pub fn is_export_namespace_as_default_declaration(node: Id<Node>, arena: &impl HasArena) -> bool {
    if !is_export_declaration(&node.ref_(arena)) {
        return false;
    }
    let node_ref = node.ref_(arena);
    let node_as_export_declaration = node_ref.as_export_declaration();
    let Some(node_export_clause) = node_as_export_declaration.export_clause else {
        return false;
    };
    if !is_namespace_export(&node_export_clause.ref_(arena)) {
        return false;
    }
    let node_export_clause_ref = node_export_clause.ref_(arena);
    let node_export_clause_as_namespace_export = node_export_clause_ref.as_namespace_export();
    node_export_clause_as_namespace_export
        .name
        .ref_(arena).as_identifier()
        .escaped_text
        == "default"
}

pub fn get_text_of_node_from_source_text(
    source_text: &SourceTextAsChars,
    node: Id<Node>,
    include_trivia: Option<bool>,
    arena: &impl HasArena,
) -> Cow<'static, str> {
    let include_trivia = include_trivia.unwrap_or(false);
    if node_is_missing(Some(&node.ref_(arena))) {
        return "".into();
    }

    let start = if include_trivia {
        node.ref_(arena).pos()
    } else {
        skip_trivia(source_text, node.ref_(arena).pos(), None, None, None)
    };
    let end = node.ref_(arena).end();
    if !(start >= 0 && end >= 0 && end - start >= 0) {
        return "".into();
    }
    let mut text = text_substring(
        source_text,
        start.try_into().unwrap(),
        end.try_into().unwrap(),
    );

    if is_jsdoc_type_expression_or_child(node, arena) {
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

pub fn get_text_of_node(node: Id<Node>, include_trivia: Option<bool>, arena: &impl HasArena) -> Cow<'static, str> {
    let include_trivia = include_trivia.unwrap_or(false);
    get_source_text_of_node_from_source_file(
        get_source_file_of_node(node, arena),
        node,
        Some(include_trivia),
        arena,
    )
}

fn get_pos(range: &Node) -> isize {
    range.pos()
}

pub fn index_of_node(node_array: &[Id<Node>], node: Id<Node>, arena: &impl HasArena) -> isize {
    binary_search_copy_key(
        node_array,
        &node,
        |node, _| get_pos(&node.ref_(arena)),
        |a, b| compare_values(Some(a), Some(b)),
        None,
    )
}

pub fn get_emit_flags(node: Id<Node>, arena: &impl HasArena) -> EmitFlags {
    node.ref_(arena).maybe_emit_node()
        .and_then(|emit_node| emit_node.ref_(arena).flags)
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

pub fn get_literal_text(
    node: Id<Node>, /*LiteralLikeNode*/
    source_file: Option<Id<Node> /*SourceFile*/>,
    flags: GetLiteralTextFlags,
    arena: &impl HasArena,
) -> Cow<'static, str> {
    if can_use_original_text(&node.ref_(arena), flags) {
        return get_source_text_of_node_from_source_file(source_file.unwrap(), node, None, arena);
    }

    match &*node.ref_(arena) {
        Node::StringLiteral(node_as_string_literal) => {
            let escape_text = if flags.intersects(GetLiteralTextFlags::JsxAttributeEscape) {
                escape_jsx_attribute_string
            } else if flags.intersects(GetLiteralTextFlags::NeverAsciiEscape)
                || get_emit_flags(node, arena).intersects(EmitFlags::NoAsciiEscaping)
            {
                escape_string
            } else {
                escape_non_ascii_string
            };
            if node_as_string_literal.single_quote == Some(true) {
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
                || get_emit_flags(node, arena).intersects(EmitFlags::NoAsciiEscaping)
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

            match node.ref_(arena).kind() {
                SyntaxKind::NoSubstitutionTemplateLiteral => format!("`{}`", raw_text).into(),
                SyntaxKind::TemplateHead => format!("`{}${{", raw_text).into(),
                SyntaxKind::TemplateMiddle => format!("}}{}${{", raw_text).into(),
                SyntaxKind::TemplateTail => format!("}}{}`", raw_text).into(),
                _ => panic!("Unexpected TemplateLiteralLikeNode kind"),
            }
        }
        Node::NumericLiteral(_) | Node::BigIntLiteral(_) => {
            node.ref_(arena).as_literal_like_node().text().to_owned().into()
        }
        _ => Debug_.fail(Some(&format!(
            "Literal kind '{:?}' not accounted for.",
            node.ref_(arena).kind()
        ))),
    }
}

fn can_use_original_text(
    node: &Node, /*LiteralLikeNode*/
    flags: GetLiteralTextFlags,
) -> bool {
    if node_is_synthesized(node)
        || node.maybe_parent().is_none()
        || flags.intersects(GetLiteralTextFlags::TerminateUnterminatedLiterals)
            && node.as_literal_like_node().is_unterminated() == Some(true)
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

pub fn get_text_of_constant_value(value: impl Into<StringOrNumber>) -> String {
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

pub fn is_block_or_catch_scoped(declaration: Id<Node> /*Declaration*/, arena: &impl HasArena) -> bool {
    get_combined_node_flags(declaration, arena).intersects(NodeFlags::BlockScoped)
        || is_catch_clause_variable_declaration_or_binding_element(declaration, arena)
}

pub fn is_catch_clause_variable_declaration_or_binding_element(
    declaration: Id<Node>, /*Declaration*/
    arena: &impl HasArena,
) -> bool {
    let node = get_root_declaration(declaration, arena);
    node.ref_(arena).kind() == SyntaxKind::VariableDeclaration
        && node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::CatchClause
}

pub fn is_ambient_module(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_module_declaration(&node.ref_(arena))
        && (node.ref_(arena).as_module_declaration().name.ref_(arena).kind() == SyntaxKind::StringLiteral
            || is_global_scope_augmentation(&node.ref_(arena)))
}

pub fn is_module_with_string_literal_name(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_module_declaration(&node.ref_(arena))
        && node.ref_(arena).as_module_declaration().name.ref_(arena).kind() == SyntaxKind::StringLiteral
}

pub fn is_non_global_ambient_module(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_module_declaration(&node.ref_(arena)) && is_string_literal(&node.ref_(arena).as_module_declaration().name.ref_(arena))
}

pub fn is_effective_module_declaration(node: &Node) -> bool {
    is_module_declaration(node) || is_identifier(node)
}

pub fn is_shorthand_ambient_module_symbol(module_symbol: Id<Symbol>, arena: &impl HasArena) -> bool {
    is_shorthand_ambient_module(module_symbol.ref_(arena).maybe_value_declaration().refed(arena).as_deref())
}

fn is_shorthand_ambient_module(node: Option<&Node>) -> bool {
    match node {
        None => false,
        Some(node) => {
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

pub fn is_external_module_augmentation(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_ambient_module(node, arena) && is_module_augmentation_external(node, arena)
}

pub fn is_module_augmentation_external(node: Id<Node> /*AmbientModuleDeclaration*/, arena: &impl HasArena) -> bool {
    match node.ref_(arena).parent().ref_(arena).kind() {
        SyntaxKind::SourceFile => is_external_module(&node.ref_(arena).parent().ref_(arena)),
        SyntaxKind::ModuleBlock => {
            is_ambient_module(node.ref_(arena).parent().ref_(arena).parent(), arena)
                && is_source_file(&node.ref_(arena).parent().ref_(arena).parent().ref_(arena).parent().ref_(arena))
                && !is_external_module(&node.ref_(arena).parent().ref_(arena).parent().ref_(arena).parent().ref_(arena))
        }
        _ => false,
    }
}

pub fn get_non_augmentation_declaration(symbol: Id<Symbol>, arena: &impl HasArena) -> Option<Id<Node /*Declaration*/>> {
    symbol
        .ref_(arena).maybe_declarations()
        .as_ref()
        .and_then(|declarations| {
            declarations
                .into_iter()
                .find(|&&d| {
                    !is_external_module_augmentation(d, arena)
                        && !(is_module_declaration(&d.ref_(arena)) && is_global_scope_augmentation(&d.ref_(arena)))
                })
                .copied()
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
    node: Id<Node>, /*SourceFile*/
    compiler_options: &CompilerOptions,
    arena: &impl HasArena,
) -> bool {
    let node_ref = node.ref_(arena);
    let node_as_source_file = node_ref.as_source_file();
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
    if starts_with_use_strict(&node_as_source_file.statements().ref_(arena), arena) {
        return true;
    }
    if is_external_module(&node.ref_(arena)) || compiler_options.isolated_modules.unwrap_or(false) {
        if get_emit_module_kind(compiler_options) >= ModuleKind::ES2015 {
            return true;
        }
        return !compiler_options.no_implicit_use_strict.unwrap_or(false);
    }
    false
}

pub fn is_block_scope(node: &Node, parent_node: Option<&Node>) -> bool {
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

pub fn has_possible_external_module_reference(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_any_import_or_re_export(&node.ref_(arena))
        || is_module_declaration(&node.ref_(arena))
        || is_import_type_node(&node.ref_(arena))
        || is_import_call(node, arena)
}

pub fn is_any_import_or_re_export(node: &Node) -> bool {
    is_any_import_syntax(node) || is_export_declaration(node)
}

pub fn get_enclosing_block_scope_container(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node>> {
    find_ancestor(node.ref_(arena).maybe_parent(), |current| {
        is_block_scope(&current.ref_(arena), current.ref_(arena).maybe_parent().refed(arena).as_deref())
    }, arena)
}

pub fn for_each_enclosing_block_scope_container(
    node: Id<Node>,
    mut cb: impl FnMut(Id<Node>),
    arena: &impl HasArena,
) {
    let mut container = get_enclosing_block_scope_container(node, arena);
    while let Some(container_present) = container {
        cb(container_present);
        container = get_enclosing_block_scope_container(container_present, arena);
    }
}

pub fn declaration_name_to_string(name: Option<Id<Node>>, arena: &impl HasArena) -> Cow<'static, str> {
    match name {
        None => "(Missing)".into(),
        Some(name) => {
            if get_full_width(&name.ref_(arena)) == 0 {
                "(Missing)".into()
            } else {
                get_text_of_node(name, None, arena)
            }
        }
    }
}

pub fn get_name_from_index_info(info: Id<IndexInfo>, arena: &impl HasArena) -> Option<Cow<'static, str>> {
    info.ref_(arena).declaration.map(|info_declaration| {
        declaration_name_to_string(
            info_declaration
                .ref_(arena).as_index_signature_declaration()
                .parameters().ref_(arena)[0]
                .ref_(arena).as_parameter_declaration()
                .maybe_name(),
            arena,
        )
    })
}

pub fn is_computed_non_literal_name(name: Id<Node> /*PropertyName*/, arena: &impl HasArena) -> bool {
    name.ref_(arena).kind() == SyntaxKind::ComputedPropertyName
        && !is_string_or_numeric_literal_like(&name.ref_(arena).as_computed_property_name().expression.ref_(arena))
}

pub fn get_text_of_property_name(
    name: Id<Node>,
    /*PropertyName | NoSubstitutionTemplateLiteral*/ arena: &impl HasArena,
) -> String /*__String*/ {
    match name.ref_(arena).kind() {
        SyntaxKind::Identifier => name.ref_(arena).as_identifier().escaped_text.clone(),
        SyntaxKind::PrivateIdentifier => {
            name.ref_(arena).as_private_identifier().escaped_text.clone()
        }
        SyntaxKind::StringLiteral => {
            escape_leading_underscores(&name.ref_(arena).as_string_literal().text())
                .into_owned()
        }
        SyntaxKind::NumericLiteral => {
            escape_leading_underscores(&name.ref_(arena).as_numeric_literal().text())
                .into_owned()
        }
        SyntaxKind::NoSubstitutionTemplateLiteral => {
            escape_leading_underscores(&name.ref_(arena).as_template_literal_like_node().text())
                .into_owned()
        }
        SyntaxKind::ComputedPropertyName => {
            let name_ref = name.ref_(arena);
            let name_as_computed_property_name = name_ref.as_computed_property_name();
            if is_string_or_numeric_literal_like(
                &name_as_computed_property_name.expression.ref_(arena),
            ) {
                return escape_leading_underscores(
                    &name_as_computed_property_name
                        .expression
                        .ref_(arena)
                        .as_literal_like_node()
                        .text(),
                )
                .into_owned();
            }
            Debug_.fail(Some(
                "Text of property name cannot be read from non-literal-valued ComputedPropertyName",
            ))
        }
        _ => Debug_.assert_never(name, None),
    }
}

pub fn entity_name_to_string(
    name: Id<Node>, /*EntityNameOrEntityNameExpression | JSDocMemberName | JsxTagNameExpression | PrivateIdentifier*/
    arena: &impl HasArena,
) -> Cow<'static, str> {
    match name.ref_(arena).kind() {
        SyntaxKind::ThisKeyword => "this".into(),
        SyntaxKind::PrivateIdentifier | SyntaxKind::Identifier => {
            if get_full_width(&name.ref_(arena)) == 0 {
                id_text(&name.ref_(arena)).to_owned().into()
            } else {
                get_text_of_node(name, None, arena)
            }
        }
        SyntaxKind::QualifiedName => {
            let name_ref = name.ref_(arena);
            let name_as_qualified_name = name_ref.as_qualified_name();
            format!(
                "{}.{}",
                entity_name_to_string(name_as_qualified_name.left, arena),
                entity_name_to_string(name_as_qualified_name.right, arena)
            )
            .into()
        }
        SyntaxKind::PropertyAccessExpression => {
            let name_ref = name.ref_(arena);
            let name_as_property_access_expression = name_ref.as_property_access_expression();
            if is_identifier(&name_as_property_access_expression.name.ref_(arena))
                || is_private_identifier(&name_as_property_access_expression.name.ref_(arena))
            {
                format!(
                    "{}.{}",
                    entity_name_to_string(name_as_property_access_expression.expression, arena),
                    entity_name_to_string(name_as_property_access_expression.name, arena)
                )
                .into()
            } else {
                Debug_.assert_never(name_as_property_access_expression.name, None)
            }
        }
        SyntaxKind::JSDocMemberName => {
            let name_ref = name.ref_(arena);
            let name_as_jsdoc_member_name = name_ref.as_jsdoc_member_name();
            format!(
                "{}{}",
                entity_name_to_string(name_as_jsdoc_member_name.left, arena),
                entity_name_to_string(name_as_jsdoc_member_name.right, arena)
            )
            .into()
        }
        _ => Debug_.assert_never(name, None),
    }
}
