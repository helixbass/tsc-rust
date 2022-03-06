#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp;
use std::convert::TryInto;
use std::io;
use std::ptr;
use std::rc::Rc;

use super::is_static;
use crate::{
    combine_paths, compute_line_and_character_of_position, compute_line_of_position,
    compute_line_starts, create_compiler_diagnostic, create_get_canonical_file_name,
    ensure_path_is_non_module_name, ensure_trailing_directory_separator, factory,
    file_extension_is_one_of, filter, find, flat_map, get_directory_path, get_emit_module_kind,
    get_external_module_name, get_jsdoc_deprecated_tag_no_cache, get_jsdoc_override_tag_no_cache,
    get_jsdoc_private_tag_no_cache, get_jsdoc_protected_tag_no_cache,
    get_jsdoc_public_tag_no_cache, get_jsdoc_readonly_tag_no_cache, get_jsdoc_return_type,
    get_jsdoc_tags, get_jsdoc_type, get_leading_comment_ranges, get_line_starts,
    get_normalized_absolute_path, get_property_name_for_property_name_node,
    get_relative_path_to_directory_or_url, get_root_length, has_dynamic_name, is_accessor,
    is_binary_expression, is_class_element, is_class_like, is_class_static_block_declaration,
    is_constructor_declaration, is_expression_with_type_arguments, is_external_module,
    is_function_declaration, is_heritage_clause, is_in_js_file, is_jsdoc_property_like_tag,
    is_jsdoc_signature, is_jsdoc_template_tag, is_jsdoc_type_alias, is_json_source_file,
    is_left_hand_side_expression, is_parameter, is_pinned_comment,
    is_property_access_entity_name_expression, is_qualified_name, is_source_file_js,
    is_string_literal_like, is_white_space_like, is_white_space_single_line, last,
    maybe_text_char_at_index, node_is_present, normalize_path, path_is_relative,
    remove_file_extension, skip_trivia, str_to_source_text_as_chars, string_contains,
    synthetic_factory, text_char_at_index, text_substring, to_path, trim_string,
    AllAccessorDeclarations, CharacterCodes, CommentRange, CompilerOptions, Debug_,
    DiagnosticCollection, Diagnostics, EmitHost, EmitResolver, EmitTextWriter, Extension,
    FunctionLikeDeclarationInterface, GetCanonicalFileName, HasTypeInterface, ModifierFlags,
    ModifiersArray, ModuleKind, NamedDeclarationInterface, Node, NodeArray, NodeFlags,
    NodeInterface, ScriptReferenceHost, SignatureDeclarationInterface, SourceFileMayBeEmittedHost,
    SourceTextAsChars, Symbol, SymbolFlags, SymbolTracker, SymbolWriter, SyntaxKind, TextRange,
    WriteFileCallback,
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
    new_line: String,
    output: String,
    indent: usize,
    line_start: bool,
    line_count: usize,
    line_pos: usize,
    has_trailing_comment: bool,
    output_as_chars: Vec<char>,
}

impl TextWriter {
    pub fn new(new_line: &str) -> Self {
        Self {
            new_line: new_line.to_string(),
            output: String::new(),
            indent: 0,
            line_start: true,
            line_count: 0,
            line_pos: 0,
            has_trailing_comment: false,
            output_as_chars: vec![],
        }
    }

    fn push_output(&mut self, str: &str) {
        self.output.push_str(str);
        self.output_as_chars
            .append(&mut str.chars().collect::<Vec<_>>());
    }

    fn update_line_count_and_pos_for(&mut self, s: &str) {
        let s_as_chars = str_to_source_text_as_chars(s);
        let line_starts_of_s = compute_line_starts(&s_as_chars);
        if line_starts_of_s.len() > 1 {
            self.line_count = self.line_count + line_starts_of_s.len() - 1;
            self.line_pos = self.output_as_chars.len() - s_as_chars.len() + last(&line_starts_of_s);
            self.line_start = (self.line_pos - self.output_as_chars.len()) == 0;
        } else {
            self.line_start = false;
        }
    }

    fn write_text(&mut self, s: &str) {
        let mut s = s.to_owned();
        if !s.is_empty() {
            if self.line_start {
                s = format!("{}{}", get_indent_string(self.indent), s);
                self.line_start = false;
            }
            self.push_output(&s);
            self.update_line_count_and_pos_for(&s);
        }
    }

    fn reset(&mut self) {
        self.output = String::new();
        self.indent = 0;
        self.line_start = true;
        self.line_count = 0;
        self.line_pos = 0;
        self.has_trailing_comment = false;
        self.output_as_chars = vec![];
    }

    fn get_text_pos_with_write_line(&self) -> Option<usize> {
        Some(if self.line_start {
            self.output_as_chars.len()
        } else {
            self.output_as_chars.len() + self.new_line.len()
        })
    }
}

impl EmitTextWriter for TextWriter {
    fn write(&mut self, s: &str) {
        if !s.is_empty() {
            self.has_trailing_comment = false;
        }
        self.write_text(s);
    }

    fn write_comment(&mut self, s: &str) {
        if !s.is_empty() {
            self.has_trailing_comment = true;
        }
        self.write_text(s);
    }

    fn raw_write(&mut self, s: &str) {
        // if (s!== undefined) {
        self.push_output(s);
        self.update_line_count_and_pos_for(s);
        self.has_trailing_comment = false;
        //}
    }

    fn write_literal(&mut self, s: &str) {
        if
        /*s && */
        !s.is_empty() {
            self.write(s);
        }
    }

    fn write_trailing_semicolon(&mut self, text: &str) {
        self.write(text);
    }

    fn get_text(&self) -> String {
        self.output.clone()
    }

    fn get_text_pos(&self) -> usize {
        self.output_as_chars.len()
    }

    fn get_line(&self) -> usize {
        self.line_count
    }

    fn get_column(&self) -> usize {
        if self.line_start {
            self.indent * get_indent_size()
        } else {
            self.output_as_chars.len() - self.line_pos
        }
    }

    fn get_indent(&self) -> usize {
        self.indent
    }

    fn is_at_start_of_line(&self) -> bool {
        self.line_start
    }

    fn has_trailing_comment(&self) -> bool {
        self.has_trailing_comment
    }

    fn has_trailing_whitespace(&self) -> bool {
        !self.output.is_empty()
            && is_white_space_like(self.output_as_chars[self.output_as_chars.len() - 1])
    }
}

impl SymbolWriter for TextWriter {
    fn write_line(&mut self, force: Option<bool>) {
        let force = force.unwrap_or(false);
        if !self.line_start || force {
            self.push_output(&self.new_line.clone());
            self.line_count += 1;
            self.line_pos = self.output_as_chars.len();
            self.line_start = true;
            self.has_trailing_comment = false;
        }
    }

    fn increase_indent(&mut self) {
        self.indent += 1;
    }

    fn decrease_indent(&mut self) {
        self.indent -= 1; // TODO: should use isize to avoid this crashing if misused?
    }

    fn write_keyword(&mut self, text: &str) {
        self.write(text);
    }

    fn write_operator(&mut self, text: &str) {
        self.write(text);
    }

    fn write_parameter(&mut self, text: &str) {
        self.write(text);
    }

    fn write_punctuation(&mut self, s: &str) {
        self.write(s);
    }

    fn write_space(&mut self, s: &str) {
        self.write(s);
    }

    fn write_string_literal(&mut self, s: &str) {
        self.write(s);
    }

    fn write_property(&mut self, s: &str) {
        self.write(s);
    }

    fn write_symbol(&mut self, s: &str, _: &Symbol) {
        self.write(s);
    }

    fn clear(&mut self) {
        self.reset();
    }
}

impl SymbolTracker for TextWriter {
    fn track_symbol(
        &mut self,
        _symbol: &Symbol,
        _enclosing_declaration: Option<Rc<Node>>,
        _meaning: SymbolFlags,
    ) -> Option<bool> {
        Some(false)
    }
}

pub fn create_text_writer(new_line: &str) -> TextWriter {
    TextWriter::new(new_line)
    // text_writer.reset()
}

pub fn get_trailing_semicolon_deferring_writer<TWriter: EmitTextWriter>(
    writer: TWriter,
) -> TrailingSemicolonDeferringWriter<TWriter> {
    TrailingSemicolonDeferringWriter::new(writer)
}

pub struct TrailingSemicolonDeferringWriter<TWriter: EmitTextWriter> {
    writer: TWriter,
    pending_trailing_semicolon: bool,
}

impl<TWriter: EmitTextWriter> TrailingSemicolonDeferringWriter<TWriter> {
    pub fn new(writer: TWriter) -> Self {
        Self {
            writer,
            pending_trailing_semicolon: false,
        }
    }

    fn commit_pending_trailing_semicolon(&mut self) {
        if self.pending_trailing_semicolon {
            self.writer.write_trailing_semicolon(";");
            self.pending_trailing_semicolon = false;
        }
    }
}

impl<TWriter: EmitTextWriter> EmitTextWriter for TrailingSemicolonDeferringWriter<TWriter> {
    fn write(&mut self, s: &str) {
        self.writer.write(s)
    }

    fn write_comment(&mut self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.write_comment(s)
    }

    fn raw_write(&mut self, s: &str) {
        self.writer.raw_write(s)
    }

    fn write_literal(&mut self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.write_literal(s)
    }

    fn write_trailing_semicolon(&mut self, text: &str) {
        self.pending_trailing_semicolon = true;
    }

    fn get_text(&self) -> String {
        self.writer.get_text()
    }

    fn get_text_pos(&self) -> usize {
        self.writer.get_text_pos()
    }

    fn get_line(&self) -> usize {
        self.writer.get_line()
    }

    fn get_column(&self) -> usize {
        self.writer.get_column()
    }

    fn get_indent(&self) -> usize {
        self.writer.get_indent()
    }

    fn is_at_start_of_line(&self) -> bool {
        self.writer.is_at_start_of_line()
    }

    fn has_trailing_comment(&self) -> bool {
        self.writer.has_trailing_comment()
    }

    fn has_trailing_whitespace(&self) -> bool {
        self.writer.has_trailing_whitespace()
    }
}

impl<TWriter: EmitTextWriter> SymbolWriter for TrailingSemicolonDeferringWriter<TWriter> {
    fn write_line(&mut self, _force: Option<bool>) {
        self.commit_pending_trailing_semicolon();
        self.writer.write_line(None)
    }

    fn increase_indent(&mut self) {
        self.commit_pending_trailing_semicolon();
        self.writer.increase_indent()
    }

    fn decrease_indent(&mut self) {
        self.commit_pending_trailing_semicolon();
        self.writer.decrease_indent()
    }

    fn write_keyword(&mut self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.write_keyword(s)
    }

    fn write_operator(&mut self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.write_operator(s)
    }

    fn write_parameter(&mut self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.write_parameter(s)
    }

    fn write_punctuation(&mut self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.write_punctuation(s)
    }

    fn write_space(&mut self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.write_space(s)
    }

    fn write_string_literal(&mut self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.write_string_literal(s)
    }

    fn write_property(&mut self, s: &str) {
        self.commit_pending_trailing_semicolon();
        self.writer.write_property(s)
    }

    fn write_symbol(&mut self, s: &str, sym: &Symbol) {
        self.commit_pending_trailing_semicolon();
        self.writer.write_symbol(s, sym)
    }

    fn clear(&mut self) {
        self.writer.clear()
    }
}

// TODO: should explicitly forward all SymbolTracker methods to self.writer too?
impl<TWriter: EmitTextWriter> SymbolTracker for TrailingSemicolonDeferringWriter<TWriter> {}

pub fn host_uses_case_sensitive_file_names<TGetUseCaseSensitiveFileNames: Fn() -> Option<bool>>(
    get_use_case_sensitive_file_names: TGetUseCaseSensitiveFileNames,
) -> bool {
    get_use_case_sensitive_file_names().unwrap_or(false)
}

pub fn host_get_canonical_file_name<TGetUseCaseSensitiveFileNames: Fn() -> Option<bool>>(
    get_use_case_sensitive_file_names: TGetUseCaseSensitiveFileNames,
) -> GetCanonicalFileName {
    create_get_canonical_file_name(host_uses_case_sensitive_file_names(
        get_use_case_sensitive_file_names,
    ))
}

pub trait ResolveModuleNameResolutionHost {
    fn get_canonical_file_name(&self, p: &str) -> String;
    fn get_common_source_directory(&self) -> String;
    fn get_current_directory(&self) -> String;
}

pub fn get_resolved_external_module_name<
    THost: ResolveModuleNameResolutionHost,
    TReferenceFile: Borrow<Node>,
>(
    host: &THost,
    file: &Node, /*SourceFile*/
    reference_file: Option<TReferenceFile /*SourceFile*/>,
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
                            reference_file.borrow().as_source_file().file_name().clone()
                        })
                        .as_deref(),
                )
            },
            |module_name| module_name.clone(),
        )
}

pub(super) fn get_canonical_absolute_path<THost: ResolveModuleNameResolutionHost>(
    host: &THost,
    path: &str,
) -> String {
    host.get_canonical_file_name(&get_normalized_absolute_path(
        path,
        Some(&host.get_current_directory()),
    ))
}

pub fn get_external_module_name_from_declaration<THost: ResolveModuleNameResolutionHost>(
    host: &THost,
    resolver: &dyn EmitResolver,
    declaration: &Node, /*ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration | ImportTypeNode*/
) -> Option<String> {
    let file = resolver.get_external_module_file_from_declaration(declaration);
    if match file.as_ref() {
        None => true,
        Some(file) => file.as_source_file().is_declaration_file(),
    } {
        return None;
    }
    let file = file.unwrap();
    let specifier = get_external_module_name(declaration);
    if let Some(specifier) = specifier {
        if is_string_literal_like(&specifier)
            && !path_is_relative(&specifier.as_literal_like_node().text())
            && !get_canonical_absolute_path(host, &file.as_source_file().path()).contains(
                &*get_canonical_absolute_path(
                    host,
                    &ensure_trailing_directory_separator(&host.get_common_source_directory()),
                ),
            )
        {
            return None;
        }
    }
    Some(get_resolved_external_module_name(
        host,
        &file,
        Option::<&Node>::None,
    ))
}

pub fn get_external_module_name_from_path<THost: ResolveModuleNameResolutionHost>(
    host: &THost,
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
        extensionless.into_owned()
    }
}

pub fn get_own_emit_output_file_path(
    file_name: &str,
    host: &dyn EmitHost,
    extension: &str,
) -> String {
    let compiler_options = ScriptReferenceHost::get_compiler_options(host);
    let emit_output_file_path_without_extension: String;
    if let Some(compiler_options_out_dir) = compiler_options.out_dir.as_ref() {
        emit_output_file_path_without_extension = remove_file_extension(
            &get_source_file_path_in_new_dir(file_name, host, compiler_options_out_dir),
        )
        .into_owned();
    } else {
        emit_output_file_path_without_extension = remove_file_extension(file_name).into_owned();
    }

    format!("{}{}", emit_output_file_path_without_extension, extension)
}

pub fn get_declaration_emit_output_file_path(file_name: &str, host: &dyn EmitHost) -> String {
    get_declaration_emit_output_file_path_worker(
        file_name,
        &ScriptReferenceHost::get_compiler_options(host),
        &ScriptReferenceHost::get_current_directory(host),
        &host.get_common_source_directory(),
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

pub fn get_paths_base_path<TGetCurrentDirectory: Fn() -> Option<String>>(
    options: &CompilerOptions,
    get_current_directory: TGetCurrentDirectory,
) -> Option<String> {
    if options.paths.is_none() {
        return None;
    }
    options.base_url.clone().or_else(|| Some(Debug_.check_defined(options.paths_base_path.clone().or_else(|| get_current_directory()), Some("Encounted 'paths' without a 'baseUrl', config file, or host 'getCurrentDirectory'."))))
}

pub struct EmitFileNames {
    pub js_file_path: Option<String>,
    pub source_map_file_path: Option<String>,
    pub declaration_file_path: Option<String>,
    pub declaration_map_path: Option<String>,
    pub build_info_path: Option<String>,
}

pub fn get_source_files_to_emit<TTargetSourceFile: Borrow<Node>>(
    host: &dyn EmitHost,
    target_source_file: Option<TTargetSourceFile /*SourceFile*/>,
    force_dts_emit: Option<bool>,
) -> Vec<Rc<Node /*SourceFile*/>> {
    let options = ScriptReferenceHost::get_compiler_options(host);
    if matches!(out_file(&options), Some(out_file) if !out_file.is_empty()) {
        let module_kind = get_emit_module_kind(&options);
        let module_emit_enabled = matches!(options.emit_declaration_only, Some(true))
            || matches!(module_kind, ModuleKind::AMD | ModuleKind::System);
        host.get_source_files()
            .into_iter()
            .filter(|source_file| {
                (module_emit_enabled || !is_external_module(source_file))
                    && source_file_may_be_emitted(
                        source_file,
                        host.as_source_file_may_be_emitted_host(),
                        force_dts_emit,
                    )
            })
            .map(Clone::clone)
            .collect()
    } else {
        let source_files = match target_source_file {
            None => host.get_source_files().to_owned(),
            Some(target_source_file) => vec![target_source_file.borrow().node_wrapper()],
        };
        source_files
            .into_iter()
            .filter(|source_file| {
                source_file_may_be_emitted(
                    source_file,
                    host.as_source_file_may_be_emitted_host(),
                    force_dts_emit,
                )
            })
            .collect()
    }
}

pub fn source_file_may_be_emitted(
    source_file: &Node, /*SourceFile*/
    host: &dyn SourceFileMayBeEmittedHost,
    force_dts_emit: Option<bool>,
) -> bool {
    let options = host.get_compiler_options();
    let source_file_as_source_file = source_file.as_source_file();
    !(matches!(options.no_emit_for_js_files, Some(true)) && is_source_file_js(source_file))
        && !source_file_as_source_file.is_declaration_file()
        && !host.is_source_file_from_external_library(source_file)
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
        &EmitHost::get_current_directory(host),
        &host.get_common_source_directory(),
        |f| host.get_canonical_file_name(f),
    )
}

pub fn get_source_file_path_in_new_dir_worker<TGetCanonicalFileName: Fn(&str) -> String>(
    file_name: &str,
    new_dir_path: &str,
    current_directory: &str,
    common_source_directory: &str,
    get_canonical_file_name: TGetCanonicalFileName,
) -> String {
    let mut source_file_path = get_normalized_absolute_path(file_name, Some(current_directory));
    let is_source_file_in_common_source_directory = get_canonical_file_name(&source_file_path)
        .starts_with(&get_canonical_file_name(common_source_directory));
    source_file_path = if is_source_file_in_common_source_directory {
        source_file_path[0..common_source_directory.len()].to_owned()
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
    source_files: Option<&[Rc<Node /*SourceFile*/>]>,
) {
    write_file.call(
        file_name,
        data,
        write_byte_order_mark,
        Some(&|host_error_message| {
            diagnostics.add(Rc::new(
                create_compiler_diagnostic(
                    &Diagnostics::Could_not_write_file_0_Colon_1,
                    Some(vec![file_name.to_owned(), host_error_message]),
                )
                .into(),
            ))
        }),
        source_files,
    );
}

fn ensure_directories_exist<TCreateDirectory: Fn(&str), TDirectoryExists: Fn(&str) -> bool>(
    directory_path: &str,
    create_directory: &TCreateDirectory,
    directory_exists: TDirectoryExists,
) {
    if directory_path.len() > get_root_length(directory_path) && !directory_exists(directory_path) {
        let parent_directory = get_directory_path(directory_path);
        ensure_directories_exist(&parent_directory, create_directory, directory_exists);
        create_directory(directory_path);
    }
}

fn write_file_ensuring_directories<
    TWriteFile: Fn(&str, &str, bool) -> io::Result<()>,
    TCreateDirectory: Fn(&str),
    TDirectoryExists: Fn(&str) -> bool,
>(
    path: &str,
    data: &str,
    write_byte_order_mark: bool,
    write_file: TWriteFile,
    create_directory: TCreateDirectory,
    directory_exists: TDirectoryExists,
) {
    if write_file(path, data, write_byte_order_mark).is_err() {
        ensure_directories_exist(
            &get_directory_path(&normalize_path(path)),
            &create_directory,
            directory_exists,
        );
        write_file(path, data, write_byte_order_mark).expect("Expected write to succeed");
    }
}

pub fn get_line_of_local_position(source_file: &Node /*SourceFile*/, pos: usize) -> usize {
    let line_starts = get_line_starts(source_file.as_source_file());
    compute_line_of_position(&line_starts, pos, None)
}

pub fn get_line_of_local_position_from_line_map(line_map: &[usize], pos: usize) -> usize {
    compute_line_of_position(line_map, pos, None)
}

pub fn get_first_constructor_with_body(
    node: &Node, /*ClassLikeDeclaration*/
) -> Option<Rc<Node /*ConstructorDeclaration & { body: FunctionBody }*/>> {
    find(node.as_class_like_declaration().members(), |member, _| {
        is_constructor_declaration(member)
            && node_is_present(member.as_constructor_declaration().maybe_body())
    })
    .map(Clone::clone)
}

pub fn get_set_accessor_value_parameter(
    accessor: &Node, /*SetAccessorDeclaration*/
) -> Option<Rc<Node /*ParameterDeclaration*/>> {
    let accessor_as_set_accessor_declaration = accessor.as_set_accessor_declaration();
    let accessor_parameters = accessor_as_set_accessor_declaration.parameters();
    if
    /*accessor &&*/
    !accessor_parameters.is_empty() {
        let has_this =
            accessor_parameters.len() == 2 && parameter_is_this_keyword(&accessor_parameters[0]);
        return Some(accessor_parameters[if has_this { 1 } else { 0 }].clone());
    }
    None
}

pub fn get_set_accessor_type_annotation_node(
    accessor: &Node, /*SetAccessorDeclaration*/
) -> Option<Rc<Node /*TypeNode*/>> {
    let parameter = get_set_accessor_value_parameter(accessor);
    parameter.and_then(|parameter| parameter.as_parameter_declaration().maybe_type())
}

pub fn get_this_parameter(
    signature: &Node, /*SignatureDeclaration | JSDocSignature*/
) -> Option<Rc<Node /*ParameterDeclaration*/>> {
    let signature_as_signature_declaration = signature.as_signature_declaration();
    if !signature_as_signature_declaration.parameters().is_empty() && !is_jsdoc_signature(signature)
    {
        let this_parameter = &signature_as_signature_declaration.parameters()[0];
        if parameter_is_this_keyword(this_parameter) {
            return Some(this_parameter.clone());
        }
    }
    None
}

pub fn parameter_is_this_keyword(parameter: &Node /*ParameterDeclaration*/) -> bool {
    is_this_identifier(Some(parameter.as_parameter_declaration().name()))
}

pub fn is_this_identifier<TNode: Borrow<Node>>(node: Option<TNode>) -> bool {
    if node.is_none() {
        return false;
    }
    let node = node.unwrap();
    let node = node.borrow();
    node.kind() == SyntaxKind::Identifier && identifier_is_this_keyword(node)
}

pub fn is_this_in_type_query(node: &Node) -> bool {
    if !is_this_identifier(Some(node)) {
        return false;
    }

    let mut node = node.node_wrapper();
    while is_qualified_name(&node.parent())
        && Rc::ptr_eq(&node.parent().as_qualified_name().left, &node)
    {
        node = node.parent();
    }

    node.parent().kind() == SyntaxKind::TypeQuery
}

pub fn identifier_is_this_keyword(id: &Node /*Identifier*/) -> bool {
    matches!(
        id.as_identifier().original_keyword_kind,
        Some(SyntaxKind::ThisKeyword)
    )
}

pub fn get_all_accessor_declarations(
    declarations: &[Rc<Node /*Declaration*/>],
    accessor: &Node, /*AccessorDeclaration*/
) -> AllAccessorDeclarations {
    let mut first_accessor: Option<Rc<Node>> = None;
    let mut second_accessor: Option<Rc<Node>> = None;
    let mut get_accessor: Option<Rc<Node>> = None;
    let mut set_accessor: Option<Rc<Node>> = None;
    if has_dynamic_name(accessor) {
        first_accessor = Some(accessor.node_wrapper());
        if accessor.kind() == SyntaxKind::GetAccessor {
            get_accessor = Some(accessor.node_wrapper());
        } else if accessor.kind() == SyntaxKind::SetAccessor {
            set_accessor = Some(accessor.node_wrapper());
        } else {
            Debug_.fail(Some("Accessor has wrong kind"));
        }
    } else {
        declarations.into_iter().for_each(|member| {
            if is_accessor(member) && is_static(member) == is_static(accessor) {
                let member_name =
                    get_property_name_for_property_name_node(&member.as_named_declaration().name());
                let accessor_name = get_property_name_for_property_name_node(
                    &accessor.as_named_declaration().name(),
                );
                if member_name == accessor_name {
                    if first_accessor.is_none() {
                        first_accessor = Some(member.clone());
                    } else if second_accessor.is_none() {
                        second_accessor = Some(member.clone());
                    }

                    if member.kind() == SyntaxKind::GetAccessor && get_accessor.is_none() {
                        get_accessor = Some(member.clone());
                    }

                    if member.kind() == SyntaxKind::SetAccessor && set_accessor.is_none() {
                        set_accessor = Some(member.clone());
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
