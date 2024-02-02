use std::{
    borrow::Borrow,
    cell::{Cell, Ref, RefCell, RefMut},
    io, mem,
};

use id_arena::Id;

use super::is_static;
use crate::{
    combine_paths, compute_line_of_position, compute_line_starts, create_compiler_diagnostic,
    create_get_canonical_file_name, ensure_path_is_non_module_name,
    ensure_trailing_directory_separator, file_extension_is_one_of, find, get_directory_path,
    get_emit_module_kind, get_external_module_name, get_line_starts, get_normalized_absolute_path,
    get_property_name_for_property_name_node, get_relative_path_to_directory_or_url,
    get_root_length, has_dynamic_name, is_accessor, is_constructor_declaration, is_external_module,
    is_jsdoc_signature, is_json_source_file, is_qualified_name, is_source_file_js,
    is_string_literal_like, is_white_space_like, last, node_is_present, normalize_path,
    path_is_relative, remove_file_extension, str_to_source_text_as_chars, string_contains, to_path,
    AllAccessorDeclarations, CharacterCodes, CompilerOptions, Debug_, DiagnosticCollection,
    Diagnostics, EmitHost, EmitResolver, EmitTextWriter, Extension,
    FunctionLikeDeclarationInterface, HasTypeInterface, ModuleKind,
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, NamedDeclarationInterface, Node,
    NodeInterface, OptionTry, ScriptReferenceHost, SignatureDeclarationInterface,
    SourceFileMayBeEmittedHost, Symbol, SymbolFlags, SymbolTracker, SymbolWriter, SyntaxKind,
    WriteFileCallback, HasArena, InArena, OptionInArena, AllArenas,
};

pub(super) fn is_quote_or_backtick(char_code: char) -> bool {
    matches!(
        char_code,
        CharacterCodes::single_quote | CharacterCodes::double_quote | CharacterCodes::backtick
    )
}

pub fn is_intrinsic_jsx_name(name: &str) -> bool {
    let ch = name.chars().next();
    matches!(ch, Some(ch) if ch >= CharacterCodes::a && ch <= CharacterCodes::z)
        || string_contains(name, "-")
        || string_contains(name, ":")
}

thread_local! {
    static indent_strings: RefCell<Vec<String>> = RefCell::new(vec!["".to_owned(), "    ".to_owned()]);
}
pub fn get_indent_string(level: usize) -> String {
    indent_strings.with(|indent_strings_| {
        let mut indent_strings_ = indent_strings_.borrow_mut();
        let single_level = indent_strings_[1].clone();
        for current in indent_strings_.len()..=level {
            let prev = indent_strings_[current - 1].clone();
            indent_strings_.push(format!("{}{}", prev, single_level));
        }
        indent_strings_[level].clone()
    })
}

pub fn get_indent_size() -> usize {
    indent_strings.with(|indent_strings_| indent_strings_.borrow()[1].len())
}

#[derive(Clone)]
pub struct TextWriter {
    _dyn_symbol_tracker_wrapper: Id<Box<dyn SymbolTracker>>,
    new_line: String,
    output: RefCell<String>,
    indent: Cell<usize>,
    line_start: Cell<bool>,
    line_count: Cell<usize>,
    line_pos: Cell<usize>,
    has_trailing_comment: Cell<bool>,
    output_as_chars: RefCell<Vec<char>>,
}

impl TextWriter {
    pub fn new(new_line: &str, arena: &impl HasArena) -> Id<Box<dyn EmitTextWriter>> {
        arena.alloc_emit_text_writer(Box::new(Self {
            _dyn_symbol_tracker_wrapper: arena.alloc_symbol_tracker(Box::new(TextWriterSymbolTracker)),
            new_line: new_line.to_owned(),
            output: Default::default(),
            indent: Default::default(),
            line_start: Cell::new(true),
            line_count: Default::default(),
            line_pos: Default::default(),
            has_trailing_comment: Default::default(),
            output_as_chars: Default::default(),
        }))
    }

    pub fn as_dyn_symbol_tracker(&self) -> Id<Box<dyn SymbolTracker>> {
        self._dyn_symbol_tracker_wrapper
    }

    fn output(&self) -> Ref<String> {
        self.output.borrow()
    }

    fn output_mut(&self) -> RefMut<String> {
        self.output.borrow_mut()
    }

    fn set_output(&self, output: String) {
        *self.output.borrow_mut() = output;
    }

    fn indent(&self) -> usize {
        self.indent.get()
    }

    fn set_indent(&self, indent: usize) {
        self.indent.set(indent);
    }

    fn line_start(&self) -> bool {
        self.line_start.get()
    }

    fn set_line_start(&self, line_start: bool) {
        self.line_start.set(line_start);
    }

    fn line_count(&self) -> usize {
        self.line_count.get()
    }

    fn set_line_count(&self, line_count: usize) {
        self.line_count.set(line_count);
    }

    fn line_pos(&self) -> usize {
        self.line_pos.get()
    }

    fn set_line_pos(&self, line_pos: usize) {
        self.line_pos.set(line_pos);
    }

    fn set_has_trailing_comment(&self, has_trailing_comment: bool) {
        self.has_trailing_comment.set(has_trailing_comment);
    }

    fn has_trailing_comment(&self) -> bool {
        self.has_trailing_comment.get()
    }

    fn output_as_chars(&self) -> Ref<Vec<char>> {
        self.output_as_chars.borrow()
    }

    fn output_as_chars_mut(&self) -> RefMut<Vec<char>> {
        self.output_as_chars.borrow_mut()
    }

    fn set_output_as_chars(&self, output_as_chars: Vec<char>) {
        *self.output_as_chars.borrow_mut() = output_as_chars;
    }

    fn push_output(&self, str: &str) {
        self.output_mut().push_str(str);
        self.output_as_chars_mut()
            .append(&mut str.chars().collect::<Vec<_>>());
    }

    fn update_line_count_and_pos_for(&self, s: &str) {
        let s_as_chars = str_to_source_text_as_chars(s);
        let line_starts_of_s = compute_line_starts(&s_as_chars);
        if line_starts_of_s.len() > 1 {
            self.set_line_count(self.line_count() + line_starts_of_s.len() - 1);
            self.set_line_pos(
                self.output_as_chars().len() - s_as_chars.len() + last(&line_starts_of_s),
            );
            self.set_line_start(
                (isize::try_from(self.line_pos()).unwrap()
                    - isize::try_from(self.output_as_chars().len()).unwrap())
                    == 0,
            );
        } else {
            self.set_line_start(false);
        }
    }

    fn write_text(&self, s: &str) {
        let mut s = s.to_owned();
        if !s.is_empty() {
            if self.line_start() {
                s = format!("{}{}", get_indent_string(self.indent()), s);
                self.set_line_start(false);
            }
            self.push_output(&s);
            self.update_line_count_and_pos_for(&s);
        }
    }

    fn reset(&self) {
        self.set_output(String::new());
        self.set_indent(0);
        self.set_line_start(true);
        self.set_line_count(0);
        self.set_line_pos(0);
        self.set_has_trailing_comment(false);
        self.set_output_as_chars(vec![]);
    }
}

impl EmitTextWriter for TextWriter {
    fn write(&self, s: &str) {
        if !s.is_empty() {
            self.set_has_trailing_comment(false);
        }
        self.write_text(s);
    }

    fn write_comment(&self, s: &str) {
        if !s.is_empty() {
            self.set_has_trailing_comment(true);
        }
        self.write_text(s);
    }

    fn raw_write(&self, s: &str) {
        // if (s!== undefined) {
        self.push_output(s);
        self.update_line_count_and_pos_for(s);
        self.set_has_trailing_comment(false);
        //}
    }

    fn write_literal(&self, s: &str) {
        if
        /*s && */
        !s.is_empty() {
            self.write(s);
        }
    }

    fn write_trailing_semicolon(&self, text: &str) {
        self.write(text);
    }

    fn get_text(&self) -> String {
        self.output().clone()
    }

    fn get_text_pos(&self) -> usize {
        self.output_as_chars().len()
    }

    fn get_line(&self) -> usize {
        self.line_count()
    }

    fn get_column(&self) -> usize {
        if self.line_start() {
            self.indent() * get_indent_size()
        } else {
            self.output_as_chars().len() - self.line_pos()
        }
    }

    fn get_indent(&self) -> usize {
        self.indent()
    }

    fn is_at_start_of_line(&self) -> bool {
        self.line_start()
    }

    fn has_trailing_comment(&self) -> bool {
        self.has_trailing_comment()
    }

    fn has_trailing_whitespace(&self) -> bool {
        !self.output().is_empty()
            && is_white_space_like(self.output_as_chars()[self.output_as_chars().len() - 1])
    }

    fn get_text_pos_with_write_line(&self) -> Option<usize> {
        if self.line_start() {
            Some(self.output_as_chars().len())
        } else {
            Some(self.output_as_chars().len() + self.new_line.len())
        }
    }

    fn is_non_escaping_write_supported(&self) -> bool {
        false
    }
}

impl SymbolWriter for TextWriter {
    fn write_line(&self, force: Option<bool>) {
        let force = force.unwrap_or(false);
        if !self.line_start() || force {
            self.push_output(&self.new_line.clone());
            self.set_line_count(self.line_count() + 1);
            self.set_line_pos(self.output_as_chars().len());
            self.set_line_start(true);
            self.set_has_trailing_comment(false);
        }
    }

    fn increase_indent(&self) {
        self.set_indent(self.indent() + 1);
    }

    fn decrease_indent(&self) {
        self.set_indent(self.indent() - 1); // TODO: should use isize to avoid this crashing if misused?
    }

    fn write_keyword(&self, text: &str) {
        self.write(text);
    }

    fn write_operator(&self, text: &str) {
        self.write(text);
    }

    fn write_parameter(&self, text: &str) {
        self.write(text);
    }

    fn write_punctuation(&self, s: &str) {
        self.write(s);
    }

    fn write_space(&self, s: &str) {
        self.write(s);
    }

    fn write_string_literal(&self, s: &str) {
        self.write(s);
    }

    fn write_property(&self, s: &str) {
        self.write(s);
    }

    fn write_symbol(&self, s: &str, _: Id<Symbol>) {
        self.write(s);
    }

    fn clear(&self) {
        self.reset();
    }

    fn as_symbol_tracker(&self) -> Id<Box<dyn SymbolTracker>> {
        self.as_dyn_symbol_tracker()
    }
}

impl SymbolTracker for TextWriter {
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

    // TODO: are these correct?
    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_inaccessible_this_error_supported()
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_private_in_base_of_class_expression_supported()
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_inaccessible_unique_symbol_error_supported()
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

impl HasArena for TextWriter {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

struct TextWriterSymbolTracker;

impl SymbolTracker for TextWriterSymbolTracker {
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

    // TODO: are these correct?
    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        false
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        false
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        false
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

pub fn create_text_writer(new_line: &str, arena: &impl HasArena) -> Id<Box<dyn EmitTextWriter>> {
    TextWriter::new(new_line, arena)
    // text_writer.reset()
}

pub fn get_trailing_semicolon_deferring_writer(
    writer: Id<Box<dyn EmitTextWriter>>,
    arena: *const AllArenas,
) -> Id<Box<dyn EmitTextWriter>> {
    TrailingSemicolonDeferringWriter::new(writer, arena)
}

pub struct TrailingSemicolonDeferringWriter {
    _arena: *const AllArenas,
    _dyn_symbol_tracker_wrapper: Id<Box<dyn SymbolTracker>>,
    writer: Id<Box<dyn EmitTextWriter>>,
    pending_trailing_semicolon: Cell<bool>,
}

impl TrailingSemicolonDeferringWriter {
    pub fn new(writer: Id<Box<dyn EmitTextWriter>>, arena: *const AllArenas) -> Id<Box<dyn EmitTextWriter>> {
        let arena_ref = unsafe { &*arena };
        arena_ref.alloc_emit_text_writer(Box::new(Self {
            _arena: arena,
            _dyn_symbol_tracker_wrapper: arena_ref.alloc_symbol_tracker(Box::new(
                TrailingSemicolonDeferringWriterSymbolTracker,
            )),
            writer,
            pending_trailing_semicolon: Default::default(),
        }))
    }

    pub fn as_dyn_symbol_tracker(&self) -> Id<Box<dyn SymbolTracker>> {
        self._dyn_symbol_tracker_wrapper
    }

    fn commit_pending_trailing_semicolon(&self) {
        if self.pending_trailing_semicolon.get() {
            self.writer.ref_(self).write_trailing_semicolon(";");
            self.pending_trailing_semicolon.set(false);
        }
    }
}

impl EmitTextWriter for TrailingSemicolonDeferringWriter {
    fn write(&self, s: &str) {
        self.writer.ref_(self).write(s)
    }

    fn write_comment(&self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).write_comment(s)
    }

    fn raw_write(&self, s: &str) {
        self.writer.ref_(self).raw_write(s)
    }

    fn write_literal(&self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).write_literal(s)
    }

    fn write_trailing_semicolon(&self, _text: &str) {
        self.pending_trailing_semicolon.set(true);
    }

    fn get_text(&self) -> String {
        self.writer.ref_(self).get_text()
    }

    fn get_text_pos(&self) -> usize {
        self.writer.ref_(self).get_text_pos()
    }

    fn get_line(&self) -> usize {
        self.writer.ref_(self).get_line()
    }

    fn get_column(&self) -> usize {
        self.writer.ref_(self).get_column()
    }

    fn get_indent(&self) -> usize {
        self.writer.ref_(self).get_indent()
    }

    fn is_at_start_of_line(&self) -> bool {
        self.writer.ref_(self).is_at_start_of_line()
    }

    fn has_trailing_comment(&self) -> bool {
        self.writer.ref_(self).has_trailing_comment()
    }

    fn has_trailing_whitespace(&self) -> bool {
        self.writer.ref_(self).has_trailing_whitespace()
    }

    fn get_text_pos_with_write_line(&self) -> Option<usize> {
        self.writer.ref_(self).get_text_pos_with_write_line()
    }

    fn non_escaping_write(&self, text: &str) {
        self.writer.ref_(self).non_escaping_write(text)
    }

    fn is_non_escaping_write_supported(&self) -> bool {
        self.writer.ref_(self).is_non_escaping_write_supported()
    }
}

impl SymbolWriter for TrailingSemicolonDeferringWriter {
    fn write_line(&self, _force: Option<bool>) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).write_line(None)
    }

    fn increase_indent(&self) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).increase_indent()
    }

    fn decrease_indent(&self) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).decrease_indent()
    }

    fn write_keyword(&self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).write_keyword(s)
    }

    fn write_operator(&self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).write_operator(s)
    }

    fn write_parameter(&self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).write_parameter(s)
    }

    fn write_punctuation(&self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).write_punctuation(s)
    }

    fn write_space(&self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).write_space(s)
    }

    fn write_string_literal(&self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).write_string_literal(s)
    }

    fn write_property(&self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).write_property(s)
    }

    fn write_symbol(&self, s: &str, sym: Id<Symbol>) {
        self.commit_pending_trailing_semicolon();
        self.writer.ref_(self).write_symbol(s, sym)
    }

    fn clear(&self) {
        self.writer.ref_(self).clear()
    }

    fn as_symbol_tracker(&self) -> Id<Box<dyn SymbolTracker>> {
        self.as_dyn_symbol_tracker()
    }
}

// TODO: should explicitly forward all SymbolTracker methods to self.writer too?
impl SymbolTracker for TrailingSemicolonDeferringWriter {
    // TODO: are these correct?
    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_inaccessible_this_error_supported()
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_private_in_base_of_class_expression_supported()
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper
            .ref_(self).is_report_inaccessible_unique_symbol_error_supported()
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

    fn is_track_symbol_supported(&self) -> bool {
        self._dyn_symbol_tracker_wrapper.ref_(self).is_track_symbol_supported()
    }

    fn disable_track_symbol(&self) {
        self._dyn_symbol_tracker_wrapper.ref_(self).disable_track_symbol();
    }

    fn reenable_track_symbol(&self) {
        self._dyn_symbol_tracker_wrapper.ref_(self).reenable_track_symbol();
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

impl HasArena for TrailingSemicolonDeferringWriter {
    fn arena(&self) -> &AllArenas {
        unsafe { &*self._arena }
    }
}

struct TrailingSemicolonDeferringWriterSymbolTracker;

impl SymbolTracker for TrailingSemicolonDeferringWriterSymbolTracker {
    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        false
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        false
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        false
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

    fn is_track_symbol_supported(&self) -> bool {
        false
    }

    fn disable_track_symbol(&self) {}

    fn reenable_track_symbol(&self) {}

    fn is_module_resolver_host_supported(&self) -> bool {
        false
    }

    fn is_track_referenced_ambient_module_supported(&self) -> bool {
        false
    }
}

pub fn host_uses_case_sensitive_file_names<TGetUseCaseSensitiveFileNames: Fn() -> Option<bool>>(
    get_use_case_sensitive_file_names: TGetUseCaseSensitiveFileNames,
) -> bool {
    get_use_case_sensitive_file_names().unwrap_or(false)
}

pub fn host_get_canonical_file_name(
    get_use_case_sensitive_file_names: impl Fn() -> Option<bool>,
) -> fn(&str) -> String {
    create_get_canonical_file_name(host_uses_case_sensitive_file_names(
        get_use_case_sensitive_file_names,
    ))
}

pub trait ResolveModuleNameResolutionHost {
    fn get_canonical_file_name(&self, p: &str) -> String;
    fn get_common_source_directory(&self) -> String;
    fn get_current_directory(&self) -> String;
}

pub fn get_resolved_external_module_name(
    host: &(impl ResolveModuleNameResolutionHost + ?Sized),
    file: &Node, /*SourceFile*/
    reference_file: Option<&Node /*SourceFile*/>,
) -> String {
    let file_as_source_file = file.as_source_file();
    file_as_source_file
        .maybe_module_name()
        .as_ref()
        .map_or_else(
            || {
                get_external_module_name_from_path(
                    host,
                    &file_as_source_file.file_name(),
                    reference_file
                        .map(|reference_file| {
                            reference_file.as_source_file().file_name().clone()
                        })
                        .as_deref(),
                )
            },
            |module_name| module_name.clone(),
        )
}

pub(super) fn get_canonical_absolute_path(
    host: &(impl ResolveModuleNameResolutionHost + ?Sized),
    path: &str,
) -> String {
    host.get_canonical_file_name(&get_normalized_absolute_path(
        path,
        Some(&host.get_current_directory()),
    ))
}

pub fn get_external_module_name_from_declaration(
    host: &(impl ResolveModuleNameResolutionHost + ?Sized),
    resolver: &(impl EmitResolver + ?Sized),
    declaration: Id<Node>, /*ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration | ImportTypeNode*/
    arena: &impl HasArena,
) -> io::Result<Option<String>> {
    let file = resolver.get_external_module_file_from_declaration(declaration)?;
    if match file {
        None => true,
        Some(file) => file.ref_(arena).as_source_file().is_declaration_file(),
    } {
        return Ok(None);
    }
    let file = file.unwrap();
    let specifier = get_external_module_name(declaration, arena);
    if let Some(specifier) = specifier {
        if is_string_literal_like(&specifier.ref_(arena))
            && !path_is_relative(&specifier.ref_(arena).as_literal_like_node().text())
            && !get_canonical_absolute_path(host, &file.ref_(arena).as_source_file().path()).contains(
                &*get_canonical_absolute_path(
                    host,
                    &ensure_trailing_directory_separator(&host.get_common_source_directory()),
                ),
            )
        {
            return Ok(None);
        }
    }
    Ok(Some(get_resolved_external_module_name(
        host,
        &file.ref_(arena),
        None,
    )))
}

pub fn get_external_module_name_from_path(
    host: &(impl ResolveModuleNameResolutionHost + ?Sized),
    file_name: &str,
    reference_path: Option<&str>,
) -> String {
    let get_canonical_file_name = |f: &str| host.get_canonical_file_name(f);
    let dir = to_path(
        &if let Some(reference_path) = reference_path {
            get_directory_path(reference_path)
        } else {
            host.get_common_source_directory()
        },
        Some(&host.get_current_directory()),
        get_canonical_file_name,
    );
    let file_path = get_normalized_absolute_path(file_name, Some(&host.get_current_directory()));
    let relative_path = get_relative_path_to_directory_or_url(
        &dir,
        &file_path,
        &dir,
        get_canonical_file_name,
        false,
    );
    let extensionless = remove_file_extension(&relative_path);
    if reference_path.is_some() {
        ensure_path_is_non_module_name(&extensionless)
    } else {
        extensionless.to_owned()
    }
}

pub fn get_own_emit_output_file_path(
    file_name: &str,
    host: &dyn EmitHost,
    extension: &str,
    arena: &impl HasArena,
) -> String {
    let compiler_options = ScriptReferenceHost::get_compiler_options(host);
    let emit_output_file_path_without_extension: String;
    if let Some(compiler_options_out_dir) = compiler_options.ref_(arena).out_dir.as_ref() {
        emit_output_file_path_without_extension = remove_file_extension(
            &get_source_file_path_in_new_dir(file_name, host, compiler_options_out_dir),
        )
        .to_owned();
    } else {
        emit_output_file_path_without_extension = remove_file_extension(file_name).to_owned();
    }

    format!("{}{}", emit_output_file_path_without_extension, extension)
}

pub fn get_declaration_emit_output_file_path(file_name: &str, host: &dyn EmitHost, arena: &impl HasArena) -> String {
    get_declaration_emit_output_file_path_worker(
        file_name,
        &ScriptReferenceHost::get_compiler_options(host).ref_(arena),
        &ScriptReferenceHost::get_current_directory(host),
        &ModuleSpecifierResolutionHostAndGetCommonSourceDirectory::get_common_source_directory(
            host,
        ),
        |f| host.get_canonical_file_name(f),
    )
}

pub fn get_declaration_emit_output_file_path_worker<TGetCanonicalFileName: Fn(&str) -> String>(
    file_name: &str,
    options: &CompilerOptions,
    current_directory: &str,
    common_source_directory: &str,
    get_canonical_file_name: TGetCanonicalFileName,
) -> String {
    let output_dir: Option<&str> = options
        .declaration_dir
        .as_deref()
        .or_else(|| options.out_dir.as_deref());

    let path = if let Some(output_dir) = output_dir {
        get_source_file_path_in_new_dir_worker(
            file_name,
            output_dir,
            current_directory,
            common_source_directory,
            get_canonical_file_name,
        )
    } else {
        file_name.to_owned()
    };
    let declaration_extension = get_declaration_emit_extension_for_path(&path);
    format!("{}{}", remove_file_extension(&path), declaration_extension)
}

pub fn get_declaration_emit_extension_for_path(path: &str) -> &str {
    if file_extension_is_one_of(
        path,
        &vec![Extension::Mjs.to_str(), Extension::Mts.to_str()],
    ) {
        Extension::Dmts.to_str()
    } else if file_extension_is_one_of(
        path,
        &vec![Extension::Cjs.to_str(), Extension::Cts.to_str()],
    ) {
        Extension::Dcts.to_str()
    } else if file_extension_is_one_of(path, &vec![Extension::Json.to_str()]) {
        ".json.d.ts"
    } else {
        Extension::Dts.to_str()
    }
}

pub fn out_file(options: &CompilerOptions) -> Option<&str> {
    options
        .out_file
        .as_deref()
        .or_else(|| options.out.as_deref())
}

pub fn get_paths_base_path(
    options: &CompilerOptions,
    get_current_directory: impl Fn() -> Option<io::Result<String>>,
) -> io::Result<Option<String>> {
    if options.paths.is_none() {
        return Ok(None);
    }
    options.base_url.clone().try_or_else(|| {
        Ok(Some(
            Debug_.check_defined(
                options.paths_base_path.clone().try_or_else(|| {
                    get_current_directory().transpose()
                })?,
                Some("Encounted 'paths' without a 'baseUrl', config file, or host 'getCurrentDirectory'.")
            )
        ))
    })
}

#[derive(Debug, Default)]
pub struct EmitFileNames {
    pub js_file_path: Option<String>,
    pub source_map_file_path: Option<String>,
    pub declaration_file_path: Option<String>,
    pub declaration_map_path: Option<String>,
    pub build_info_path: Option<String>,
}

pub fn get_source_files_to_emit(
    host: &dyn EmitHost,
    target_source_file: Option<Id<Node> /*SourceFile*/>,
    force_dts_emit: Option<bool>,
    arena: &impl HasArena,
) -> Vec<Id<Node /*SourceFile*/>> {
    let options = ScriptReferenceHost::get_compiler_options(host);
    if matches!(
        out_file(&options.ref_(arena)),
        Some(out_file) if !out_file.is_empty()
    ) {
        let module_kind = get_emit_module_kind(&options.ref_(arena));
        let module_emit_enabled = matches!(options.ref_(arena).emit_declaration_only, Some(true))
            || matches!(module_kind, ModuleKind::AMD | ModuleKind::System);
        host.get_source_files()
            .into_iter()
            .filter(|source_file| {
                (module_emit_enabled || !is_external_module(&source_file.ref_(arena)))
                    && source_file_may_be_emitted(
                        &source_file.ref_(arena),
                        host.as_source_file_may_be_emitted_host(),
                        force_dts_emit,
                        arena,
                    )
            })
            .collect()
    } else {
        let source_files = match target_source_file {
            None => host.get_source_files(),
            Some(target_source_file) => vec![target_source_file],
        };
        source_files
            .into_iter()
            .filter(|source_file| {
                source_file_may_be_emitted(
                    &source_file.ref_(arena),
                    host.as_source_file_may_be_emitted_host(),
                    force_dts_emit,
                    arena,
                )
            })
            .collect()
    }
}

pub fn source_file_may_be_emitted(
    source_file: &Node, /*SourceFile*/
    host: &dyn SourceFileMayBeEmittedHost,
    force_dts_emit: Option<bool>,
    arena: &impl HasArena,
) -> bool {
    let options = host.get_compiler_options();
    let source_file_as_source_file = source_file.as_source_file();
    !(matches!(options.ref_(arena).no_emit_for_js_files, Some(true)) && is_source_file_js(source_file))
        && !source_file_as_source_file.is_declaration_file()
        && !host.is_source_file_from_external_library(source_file.arena_id())
        && (matches!(force_dts_emit, Some(true))
            || (!(is_json_source_file(source_file)
                && host
                    .get_resolved_project_reference_to_redirect(
                        &source_file_as_source_file.file_name(),
                    )
                    .is_some())
                && !host.is_source_of_project_reference_redirect(
                    &source_file_as_source_file.file_name(),
                )))
}

pub fn get_source_file_path_in_new_dir(
    file_name: &str,
    host: &dyn EmitHost,
    new_dir_path: &str,
) -> String {
    get_source_file_path_in_new_dir_worker(
        file_name,
        new_dir_path,
        &ScriptReferenceHost::get_current_directory(host),
        &ModuleSpecifierResolutionHostAndGetCommonSourceDirectory::get_common_source_directory(
            host,
        ),
        |f| host.get_canonical_file_name(f),
    )
}

pub fn get_source_file_path_in_new_dir_worker(
    file_name: &str,
    new_dir_path: &str,
    current_directory: &str,
    common_source_directory: &str,
    get_canonical_file_name: impl Fn(&str) -> String,
) -> String {
    let mut source_file_path = get_normalized_absolute_path(file_name, Some(current_directory));
    let is_source_file_in_common_source_directory = get_canonical_file_name(&source_file_path)
        .starts_with(&get_canonical_file_name(common_source_directory));
    source_file_path = if is_source_file_in_common_source_directory {
        source_file_path[common_source_directory.len()..].to_owned()
    } else {
        source_file_path
    };
    combine_paths(new_dir_path, &vec![Some(&*source_file_path)])
}

pub fn write_file(
    write_file: &dyn WriteFileCallback,
    diagnostics: &mut DiagnosticCollection,
    file_name: &str,
    data: &str,
    write_byte_order_mark: bool,
    source_files: Option<&[Id<Node /*SourceFile*/>]>,
    arena: &impl HasArena,
) -> io::Result<()> {
    write_file.call(
        file_name,
        data,
        write_byte_order_mark,
        Some(&mut |host_error_message| {
            diagnostics.add(arena.alloc_diagnostic(
                create_compiler_diagnostic(
                    &Diagnostics::Could_not_write_file_0_Colon_1,
                    Some(vec![file_name.to_owned(), host_error_message.to_owned()]),
                )
                .into(),
            ))
        }),
        source_files,
    )?;

    Ok(())
}

fn ensure_directories_exist(
    directory_path: &str,
    create_directory: &impl Fn(&str) -> io::Result<()>,
    directory_exists: impl Fn(&str) -> bool,
) -> io::Result<()> {
    if directory_path.len() > get_root_length(directory_path) && !directory_exists(directory_path) {
        let parent_directory = get_directory_path(directory_path);
        ensure_directories_exist(&parent_directory, create_directory, directory_exists)?;
        create_directory(directory_path)?;
    }

    Ok(())
}

pub fn write_file_ensuring_directories(
    path: &str,
    data: &str,
    write_byte_order_mark: bool,
    write_file: impl Fn(&str, &str, bool) -> io::Result<()>,
    create_directory: impl Fn(&str) -> io::Result<()>,
    directory_exists: impl Fn(&str) -> bool,
) -> io::Result<()> {
    match write_file(path, data, write_byte_order_mark) {
        Err(_) => {
            ensure_directories_exist(
                &get_directory_path(&normalize_path(path)),
                &create_directory,
                directory_exists,
            )?;
            write_file(path, data, write_byte_order_mark)
        }
        Ok(_) => Ok(()),
    }
}

pub fn get_line_of_local_position(source_file: &Node /*SourceFile*/, pos: usize) -> usize {
    let line_starts = get_line_starts(source_file.as_source_file());
    compute_line_of_position(&line_starts, pos.try_into().unwrap(), None)
}

pub fn get_line_of_local_position_from_line_map(line_map: &[usize], pos: usize) -> usize {
    compute_line_of_position(line_map, pos.try_into().unwrap(), None)
}

pub fn get_first_constructor_with_body(
    node: Id<Node>, /*ClassLikeDeclaration*/
    arena: &impl HasArena,
) -> Option<Id<Node /*ConstructorDeclaration & { body: FunctionBody }*/>> {
    find(&node.ref_(arena).as_class_like_declaration().members().ref_(arena), |member, _| {
        is_constructor_declaration(&member.ref_(arena))
            && node_is_present(member.ref_(arena).as_constructor_declaration().maybe_body().refed(arena).as_deref())
    })
    .copied()
}

pub fn get_set_accessor_value_parameter(
    accessor: Id<Node>, /*SetAccessorDeclaration*/
    arena: &impl HasArena,
) -> Option<Id<Node /*ParameterDeclaration*/>> {
    let accessor_ref = accessor.ref_(arena);
    let accessor_as_set_accessor_declaration = accessor_ref.as_set_accessor_declaration();
    let accessor_parameters = accessor_as_set_accessor_declaration.parameters();
    if
    /*accessor &&*/
    !accessor_parameters.ref_(arena).is_empty() {
        let has_this =
            accessor_parameters.ref_(arena).len() == 2 && parameter_is_this_keyword(accessor_parameters.ref_(arena)[0], arena);
        return Some(accessor_parameters.ref_(arena)[if has_this { 1 } else { 0 }]);
    }
    None
}

pub fn get_set_accessor_type_annotation_node(
    accessor: Id<Node>, /*SetAccessorDeclaration*/
    arena: &impl HasArena,
) -> Option<Id<Node /*TypeNode*/>> {
    let parameter = get_set_accessor_value_parameter(accessor, arena);
    parameter.and_then(|parameter| parameter.ref_(arena).as_parameter_declaration().maybe_type())
}

pub fn get_this_parameter(
    signature: Id<Node>, /*SignatureDeclaration | JSDocSignature*/
    arena: &impl HasArena,
) -> Option<Id<Node /*ParameterDeclaration*/>> {
    let signature_ref = signature.ref_(arena);
    let signature_as_signature_declaration = signature_ref.as_signature_declaration();
    if !signature_as_signature_declaration.parameters().ref_(arena).is_empty() && !is_jsdoc_signature(&signature.ref_(arena))
    {
        let this_parameter = signature_as_signature_declaration.parameters().ref_(arena)[0];
        if parameter_is_this_keyword(this_parameter, arena) {
            return Some(this_parameter);
        }
    }
    None
}

pub fn parameter_is_this_keyword(parameter: Id<Node> /*ParameterDeclaration*/, arena: &impl HasArena) -> bool {
    is_this_identifier(Some(&parameter.ref_(arena).as_parameter_declaration().name().ref_(arena)))
}

pub fn is_this_identifier(node: Option<&Node>) -> bool {
    let Some(node) = node else {
        return false;
    };
    node.kind() == SyntaxKind::Identifier && identifier_is_this_keyword(node)
}

pub fn is_this_in_type_query(mut node: Id<Node>, arena: &impl HasArena) -> bool {
    if !is_this_identifier(Some(&node.ref_(arena))) {
        return false;
    }

    while is_qualified_name(&node.ref_(arena).parent().ref_(arena))
        && node.ref_(arena).parent().ref_(arena).as_qualified_name().left == node
    {
        node = node.ref_(arena).parent();
    }

    node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::TypeQuery
}

pub fn identifier_is_this_keyword(id: &Node /*Identifier*/) -> bool {
    matches!(
        id.as_identifier().original_keyword_kind,
        Some(SyntaxKind::ThisKeyword)
    )
}

pub fn get_all_accessor_declarations(
    declarations: &[Id<Node /*Declaration*/>],
    accessor: Id<Node>, /*AccessorDeclaration*/ arena: &impl HasArena
) -> AllAccessorDeclarations {
    let mut first_accessor: Option<Id<Node>> = None;
    let mut second_accessor: Option<Id<Node>> = None;
    let mut get_accessor: Option<Id<Node>> = None;
    let mut set_accessor: Option<Id<Node>> = None;
    if has_dynamic_name(accessor, arena) {
        first_accessor = Some(accessor);
        if accessor.ref_(arena).kind() == SyntaxKind::GetAccessor {
            get_accessor = Some(accessor);
        } else if accessor.ref_(arena).kind() == SyntaxKind::SetAccessor {
            set_accessor = Some(accessor);
        } else {
            Debug_.fail(Some("Accessor has wrong kind"));
        }
    } else {
        declarations.into_iter().for_each(|&member| {
            if is_accessor(&member.ref_(arena)) && is_static(member, arena) == is_static(accessor, arena) {
                let member_name = member.ref_(arena).as_named_declaration().name();
                let member_name = get_property_name_for_property_name_node(member_name, arena);
                let accessor_name = accessor.ref_(arena).as_named_declaration().name();
                let accessor_name = get_property_name_for_property_name_node(accessor_name, arena);
                if member_name == accessor_name {
                    if first_accessor.is_none() {
                        first_accessor = Some(member);
                    } else if second_accessor.is_none() {
                        second_accessor = Some(member);
                    }

                    if member.ref_(arena).kind() == SyntaxKind::GetAccessor && get_accessor.is_none() {
                        get_accessor = Some(member);
                    }

                    if member.ref_(arena).kind() == SyntaxKind::SetAccessor && set_accessor.is_none() {
                        set_accessor = Some(member);
                    }
                }
            }
        });
    }
    AllAccessorDeclarations {
        first_accessor: first_accessor.unwrap(),
        second_accessor,
        get_accessor,
        set_accessor,
    }
}
