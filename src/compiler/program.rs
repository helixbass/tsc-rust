use regex::Regex;
use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::cmp;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::io;
use std::rc::Rc;
use std::time;
use std::time::SystemTime;

use crate::{
    append, change_extension, clone, combine_paths, compare_paths, concatenate, contains_path,
    convert_to_relative_path, create_diagnostic_collection,
    create_diagnostic_for_node_in_source_file, create_get_canonical_file_name,
    create_module_resolution_cache, create_multi_map, create_source_file, create_symlink_cache,
    create_type_checker, create_type_reference_directive_resolution_cache,
    diagnostic_category_name, extension_from_path, external_helpers_module_name_text,
    file_extension_is, file_extension_is_one_of, flatten, for_each,
    for_each_ancestor_directory_str, for_each_bool, for_each_child_returns, generate_djb2_hash,
    get_allow_js_compiler_option, get_automatic_type_directive_names,
    get_common_source_directory_of_config, get_default_lib_file_name, get_directory_path,
    get_emit_module_resolution_kind, get_emit_script_target, get_external_module_name,
    get_jsx_implicit_import_base, get_jsx_runtime_import, get_line_and_character_of_position,
    get_new_line_character, get_normalized_absolute_path,
    get_normalized_absolute_path_without_root, get_normalized_path_components,
    get_output_declaration_file_name, get_package_scope_for_path, get_path_from_path_components,
    get_property_assignment, get_spelling_suggestion, get_strict_option_value,
    get_supported_extensions, get_supported_extensions_with_json_if_resolve_json_module, get_sys,
    get_text_of_identifier_or_literal, has_extension, has_js_file_extension, has_jsdoc_nodes,
    has_syntactic_modifier, is_ambient_module, is_any_import_or_re_export,
    is_declaration_file_name, is_external_module, is_external_module_name_relative, is_import_call,
    is_import_equals_declaration, is_in_js_file, is_literal_import_type_node,
    is_module_declaration, is_require_call, is_rooted_disk_path, is_source_file_js,
    is_string_literal, is_string_literal_like, is_watch_set, lib_map, libs, map_defined,
    maybe_for_each, maybe_map, missing_file_modified_time, node_modules_path_part, normalize_path,
    options_have_changes, out_file, package_id_to_string, remove_file_extension, remove_prefix,
    remove_suffix, resolution_extension_is_ts_or_json, resolve_config_file_project_name,
    resolve_module_name, resolve_type_reference_directive, set_parent_recursive,
    set_resolved_module, set_resolved_type_reference_directive,
    source_file_affecting_compiler_options, stable_sort, starts_with, string_contains,
    supported_js_extensions_flat, to_file_name_lower_case, to_path as to_path_helper,
    walk_up_parenthesized_expressions, write_file_ensuring_directories, AutomaticTypeDirectiveFile,
    CancellationTokenDebuggable, Comparison, CompilerHost, CompilerOptions, CompilerOptionsBuilder,
    ConfigFileDiagnosticsReporter, CreateProgramOptions, CustomTransformers, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticMessageText,
    DiagnosticRelatedInformationInterface, Diagnostics, DirectoryStructureHost, EmitResult,
    Extension, FileIncludeKind, FileIncludeReason, FilePreprocessingDiagnostics,
    FilePreprocessingDiagnosticsKind, FilePreprocessingFileExplainingDiagnostic,
    FilePreprocessingReferencedDiagnostic, FileReference, LibFile, LineAndCharacter,
    LiteralLikeNodeInterface, ModifierFlags, ModuleKind, ModuleResolutionCache,
    ModuleResolutionHost, ModuleResolutionHostOverrider, ModuleResolutionKind,
    ModuleSpecifierResolutionHost, MultiMap, NamedDeclarationInterface, Node, NodeArray, NodeFlags,
    NodeInterface, PackageId, PackageJsonInfoCache, ParseConfigFileHost, ParseConfigHost,
    ParsedCommandLine, Path, Program, ProjectReference, ReadonlyTextRange, RedirectTargetsMap,
    ReferencedFile, ResolvedConfigFileName, ResolvedModuleFull, ResolvedProjectReference,
    ResolvedTypeReferenceDirective, RootFile, ScriptReferenceHost, ScriptTarget, SortedArray,
    SourceFile, SourceFileLike, SourceOfProjectReferenceRedirect, StringOrRcNode,
    StructureIsReused, SymlinkCache, SyntaxKind, System, TypeChecker, TypeCheckerHost,
    TypeCheckerHostDebuggable, TypeReferenceDirectiveResolutionCache, WriteFileCallback,
};

pub fn find_config_file<TFileExists: FnMut(&str) -> bool>(
    search_path: &str,
    mut file_exists: TFileExists,
    config_name: Option<&str>,
) -> Option<String> {
    let config_name = config_name.unwrap_or("tsconfig.json");
    for_each_ancestor_directory_str(search_path, |ancestor| {
        let file_name = combine_paths(ancestor, &[Some(config_name)]);
        if file_exists(&file_name) {
            Some(file_name)
        } else {
            None
        }
    })
}

pub fn resolve_tripleslash_reference(module_name: &str, containing_file: &str) -> String {
    let base_path = get_directory_path(containing_file);
    let referenced_file_name = if is_rooted_disk_path(module_name) {
        module_name.to_owned()
    } else {
        combine_paths(&base_path, &[Some(module_name)])
    };
    normalize_path(&referenced_file_name)
}

pub(crate) fn compute_common_source_directory_of_filenames<
    TFileName: AsRef<str>,
    TGetCanonicalFileName: FnMut(&str) -> String,
>(
    file_names: &[TFileName],
    current_directory: &str,
    mut get_canonical_file_name: TGetCanonicalFileName,
) -> String {
    let mut common_path_components: Option<Vec<String>> = None;
    let failed = for_each(file_names, |source_file, _| {
        let source_file = source_file.as_ref();
        let mut source_path_components =
            get_normalized_path_components(source_file, Some(current_directory));
        source_path_components.pop();

        if common_path_components.is_none() {
            common_path_components = Some(source_path_components);
            return None;
        }
        let mut common_path_components = common_path_components.as_mut().unwrap();

        let n = cmp::min(common_path_components.len(), source_path_components.len());
        for i in 0..n {
            if get_canonical_file_name(&common_path_components[i])
                != get_canonical_file_name(&source_path_components[i])
            {
                if i == 0 {
                    return Some(());
                }

                common_path_components.truncate(i);
                break;
            }
        }

        if source_path_components.len() < common_path_components.len() {
            common_path_components.truncate(source_path_components.len());
        }
        None
    });

    if failed.is_some() {
        return "".to_owned();
    }

    if common_path_components.is_none() {
        return current_directory.to_owned();
    }
    let common_path_components = common_path_components.unwrap();

    get_path_from_path_components(&common_path_components)
}

#[derive(Debug)]
struct OutputFingerprint {
    pub hash: String,
    pub byte_order_mark: bool,
    pub mtime: SystemTime,
}

fn create_compiler_host(
    options: Rc<CompilerOptions>,
    set_parent_nodes: Option<bool>,
) -> impl CompilerHost {
    create_compiler_host_worker(options, set_parent_nodes, None)
}

pub(crate) fn create_compiler_host_worker(
    options: Rc<CompilerOptions>,
    set_parent_nodes: Option<bool>,
    system: Option<Rc<dyn System>>,
) -> impl CompilerHost {
    let system = system.unwrap_or_else(|| get_sys());
    let existing_directories: HashMap<String, bool> = HashMap::new();
    let get_canonical_file_name =
        create_get_canonical_file_name(system.use_case_sensitive_file_names());
    let new_line = get_new_line_character(options.new_line, Some(|| system.new_line().to_owned()));

    CompilerHostConcrete {
        set_parent_nodes,
        system,
        existing_directories: RefCell::new(existing_directories),
        output_fingerprints: RefCell::new(None),
        options,
        new_line,
        current_directory: RefCell::new(None),
        get_canonical_file_name,
        file_exists_override: RefCell::new(None),
        directory_exists_override: RefCell::new(None),
        realpath_override: RefCell::new(None),
        get_directories_override: RefCell::new(None),
    }
}

struct CompilerHostConcrete {
    set_parent_nodes: Option<bool>,
    system: Rc<dyn System>,
    existing_directories: RefCell<HashMap<String, bool>>,
    output_fingerprints: RefCell<Option<HashMap<String, OutputFingerprint>>>,
    options: Rc<CompilerOptions>,
    new_line: String,
    current_directory: RefCell<Option<String>>,
    get_canonical_file_name: fn(&str) -> String,
    file_exists_override: RefCell<Option<Rc<dyn ModuleResolutionHostOverrider>>>,
    directory_exists_override: RefCell<Option<Rc<dyn ModuleResolutionHostOverrider>>>,
    realpath_override: RefCell<Option<Rc<dyn ModuleResolutionHostOverrider>>>,
    get_directories_override: RefCell<Option<Rc<dyn ModuleResolutionHostOverrider>>>,
}

impl CompilerHostConcrete {
    fn maybe_file_exists_override(&self) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
        self.file_exists_override.borrow().clone()
    }

    fn maybe_directory_exists_override(&self) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
        self.directory_exists_override.borrow().clone()
    }

    fn maybe_realpath_override(&self) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
        self.realpath_override.borrow().clone()
    }

    fn maybe_get_directories_override(&self) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
        self.get_directories_override.borrow().clone()
    }

    fn compute_hash(&self, data: &str) -> String {
        if self.system.is_create_hash_supported() {
            self.system.create_hash(data)
        } else {
            generate_djb2_hash(data)
        }
    }

    fn directory_exists_(&self, directory_path: &str) -> bool {
        let mut existing_directories = self.existing_directories.borrow_mut();
        if existing_directories.contains_key(directory_path) {
            return true;
        }
        if matches!(self.directory_exists(directory_path), Some(true)) {
            existing_directories.insert(directory_path.to_owned(), true);
            return true;
        }
        false
    }

    fn write_file_worker(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
    ) -> io::Result<()> {
        if !is_watch_set(&self.options) || !self.system.is_get_modified_time_supported() {
            return self
                .system
                .write_file(file_name, data, Some(write_byte_order_mark));
        }

        let mut output_fingerprints = self.output_fingerprints.borrow_mut();
        if output_fingerprints.is_none() {
            *output_fingerprints = Some(HashMap::new());
        }
        let output_fingerprints = output_fingerprints.as_mut().unwrap();

        let hash = self.compute_hash(data);
        let mtime_before = self.system.get_modified_time(file_name);

        if let Some(mtime_before) = mtime_before {
            let fingerprint = output_fingerprints.get(file_name);
            if matches!(
                fingerprint,
                Some(fingerprint) if fingerprint.byte_order_mark == write_byte_order_mark &&
                fingerprint.hash == hash &&
                fingerprint.mtime.duration_since(time::UNIX_EPOCH).unwrap().as_millis() ==
                    mtime_before.duration_since(time::UNIX_EPOCH).unwrap().as_millis()
            ) {
                return Ok(());
            }
        }

        self.system
            .write_file(file_name, data, Some(write_byte_order_mark))?;

        let mtime_after = self
            .system
            .get_modified_time(file_name)
            .unwrap_or_else(|| missing_file_modified_time());

        output_fingerprints.insert(
            file_name.to_owned(),
            OutputFingerprint {
                hash,
                byte_order_mark: write_byte_order_mark,
                mtime: mtime_after,
            },
        );

        Ok(())
    }
}

impl ModuleResolutionHost for CompilerHostConcrete {
    fn read_file(&self, file_name: &str) -> io::Result<String> {
        self.system.read_file(file_name)
    }

    fn file_exists(&self, file_name: &str) -> bool {
        if let Some(file_exists_override) = self.maybe_file_exists_override() {
            file_exists_override.file_exists(file_name)
        } else {
            self.file_exists_non_overridden(file_name)
        }
    }

    fn file_exists_non_overridden(&self, file_name: &str) -> bool {
        self.system.file_exists(file_name)
    }

    fn set_overriding_file_exists(
        &self,
        overriding_file_exists: Option<Rc<dyn ModuleResolutionHostOverrider>>,
    ) {
        *self.file_exists_override.borrow_mut() = overriding_file_exists;
    }

    fn trace(&self, s: &str) {
        self.system.write(&format!("{}{}", s, self.new_line));
    }

    fn is_trace_supported(&self) -> bool {
        true
    }

    fn directory_exists(&self, directory_name: &str) -> Option<bool> {
        if let Some(directory_exists_override) = self.maybe_directory_exists_override() {
            directory_exists_override.directory_exists(directory_name)
        } else {
            self.directory_exists_non_overridden(directory_name)
        }
    }

    fn is_directory_exists_supported(&self) -> bool {
        true
    }

    fn directory_exists_non_overridden(&self, directory_name: &str) -> Option<bool> {
        Some(self.system.directory_exists(directory_name))
    }

    fn set_overriding_directory_exists(
        &self,
        overriding_directory_exists: Option<Rc<dyn ModuleResolutionHostOverrider>>,
    ) {
        *self.directory_exists_override.borrow_mut() = overriding_directory_exists;
    }

    fn realpath(&self, path: &str) -> Option<String> {
        if let Some(realpath_override) = self.maybe_realpath_override() {
            realpath_override.realpath(path)
        } else {
            self.realpath_non_overridden(path)
        }
    }

    fn realpath_non_overridden(&self, path: &str) -> Option<String> {
        self.system.realpath(path)
    }

    fn is_realpath_supported(&self) -> bool {
        self.system.is_realpath_supported()
    }

    fn set_overriding_realpath(
        &self,
        overriding_realpath: Option<Rc<dyn ModuleResolutionHostOverrider>>,
    ) {
        *self.realpath_override.borrow_mut() = overriding_realpath;
    }

    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        if let Some(get_directories_override) = self.maybe_get_directories_override() {
            get_directories_override.get_directories(path)
        } else {
            self.get_directories_non_overridden(path)
        }
    }

    fn is_get_directories_supported(&self) -> bool {
        true
    }

    fn get_directories_non_overridden(&self, path: &str) -> Option<Vec<String>> {
        Some(self.system.get_directories(path))
    }

    fn set_overriding_get_directories(
        &self,
        overriding_get_directories: Option<Rc<dyn ModuleResolutionHostOverrider>>,
    ) {
        *self.get_directories_override.borrow_mut() = overriding_get_directories;
    }
}

impl CompilerHost for CompilerHostConcrete {
    fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
        self
    }

    fn get_source_file(
        &self,
        file_name: &str,
        language_version: ScriptTarget,
        on_error: Option<&mut dyn FnMut(&str)>,
        should_create_new_source_file: Option<bool>,
    ) -> Option<Rc<Node /*SourceFile*/>> {
        let mut text: Option<String> = None;
        // performance.mark("beforeIORead");
        match self.read_file(file_name) {
            Ok(value) => {
                text = Some(value);
                // performance.mark("afterIORead");
                // performance.measure("I/O Read", "beforeIORead", "afterIORead");
            }
            Err(e) => {
                if let Some(on_error) = on_error {
                    on_error(&format!("{}", e));
                }
                text = Some("".to_owned());
            }
        }
        text.map(|text| {
            create_source_file(
                file_name,
                text,
                language_version,
                self.set_parent_nodes,
                None,
            )
        })
    }

    fn get_default_lib_location(&self) -> Option<String> {
        Some(get_directory_path(&normalize_path(
            &self.system.get_executing_file_path(),
        )))
    }

    fn get_default_lib_file_name(&self, options: &CompilerOptions) -> String {
        combine_paths(
            &self.get_default_lib_location().unwrap(),
            &[Some(get_default_lib_file_name(options))],
        )
    }

    fn get_current_directory(&self) -> String {
        let mut current_directory = self.current_directory.borrow_mut();
        if current_directory.is_none() {
            *current_directory = Some(self.system.get_current_directory());
        }
        current_directory.clone().unwrap()
    }

    fn use_case_sensitive_file_names(&self) -> bool {
        self.system.use_case_sensitive_file_names()
    }

    fn get_canonical_file_name(&self, file_name: &str) -> String {
        (self.get_canonical_file_name)(file_name)
    }

    fn get_new_line(&self) -> String {
        self.new_line.clone()
    }

    fn is_resolve_module_names_supported(&self) -> bool {
        false
    }

    fn is_resolve_type_reference_directives_supported(&self) -> bool {
        false
    }

    fn write_file(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        _source_files: Option<&[Rc<Node /*SourceFile*/>]>,
    ) {
        // performance.mark("beforeIOWrite");
        match write_file_ensuring_directories(
            file_name,
            data,
            write_byte_order_mark,
            |path, data, write_byte_order_mark| {
                self.write_file_worker(path, data, write_byte_order_mark)
            },
            |path| self.create_directory(path),
            |path| self.directory_exists_(path),
        ) {
            Ok(_) => {
                // performance.mark("afterIOWrite");
                // performance.measure("I/O Write", "beforeIOWrite", "afterIOWrite");
            }
            Err(e) => {
                if let Some(on_error) = on_error {
                    on_error(&format!("{}", e));
                }
            }
        }
    }

    fn get_environment_variable(&self, name: &str) -> Option<String> {
        Some(self.system.get_environment_variable(name))
    }

    fn read_directory(
        &self,
        path: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> Option<Vec<String>> {
        Some(
            self.system
                .read_directory(path, Some(extensions), excludes, Some(includes), depth),
        )
    }
    fn is_read_directory_implemented(&self) -> bool {
        true
    }

    fn create_directory(&self, d: &str) {
        self.system.create_directory(d)
    }

    fn create_hash(&self, data: &str) -> Option<String> {
        if self.system.is_create_hash_supported() {
            Some(self.system.create_hash(data))
        } else {
            None
        }
    }
}

pub(crate) fn change_compiler_host_like_to_use_cache<
    THost: CompilerHost,
    TToPath: FnMut(&str) -> Path,
    TGetSourceFile: FnMut(&str, ScriptTarget, Option<&mut dyn FnMut(&str)>, Option<bool>) -> Option<Rc<Node>>,
>(
    host: &THost,
    to_path: TToPath,
    get_source_file: Option<TGetSourceFile>,
) /*-> */
{
    // unimplemented!()
}

pub trait FormatDiagnosticsHost {
    fn get_current_directory(&self) -> String;
    fn get_new_line(&self) -> &str;
    fn get_canonical_file_name(&self, file_name: &str) -> String;
}

pub fn format_diagnostic<THost: FormatDiagnosticsHost>(
    diagnostic: &Diagnostic,
    host: &THost,
) -> String {
    let error_message = format!(
        "{} TS{}: {}{}",
        diagnostic_category_name(diagnostic.category(), None),
        diagnostic.code(),
        flatten_diagnostic_message_text(Some(diagnostic.message_text()), host.get_new_line(), None),
        host.get_new_line()
    );

    if let Some(diagnostic_file) = diagnostic.maybe_file() {
        let LineAndCharacter { line, character } = get_line_and_character_of_position(
            diagnostic_file.as_source_file(),
            diagnostic.maybe_start().unwrap().try_into().unwrap(),
        );
        let file_name = diagnostic_file.as_source_file().file_name();
        let relative_file_name =
            convert_to_relative_path(&file_name, &host.get_current_directory(), |file_name| {
                host.get_canonical_file_name(file_name)
            });
        return format!(
            "{}({},{}): {}",
            relative_file_name,
            line + 1,
            character + 1,
            error_message
        );
    }

    error_message
}

pub(crate) struct ForegroundColorEscapeSequences;
impl ForegroundColorEscapeSequences {
    pub const Grey: &'static str = "\u{001b}[90m";
    pub const Red: &'static str = "\u{001b}[91m";
    pub const Yellow: &'static str = "\u{001b}[93m";
    pub const Blue: &'static str = "\u{001b}[94m";
    pub const Cyan: &'static str = "\u{001b}[96m";
}

pub(crate) fn format_color_and_reset(text: &str, format_style: &str) -> String {
    unimplemented!()
}

pub fn format_diagnostics_with_color_and_context<THost: FormatDiagnosticsHost>(
    diagnostics: &[Rc<Diagnostic>],
    host: &THost,
) -> String {
    unimplemented!()
}

pub fn flatten_diagnostic_message_text(
    diag: Option<&DiagnosticMessageText>,
    new_line: &str,
    indent: Option<usize>,
) -> String {
    let mut indent = indent.unwrap_or(0);
    match diag {
        Some(DiagnosticMessageText::String(diag)) => diag.clone(),
        None => "".to_owned(),
        Some(DiagnosticMessageText::DiagnosticMessageChain(diag)) => {
            let mut result = "".to_owned();
            if indent != 0 {
                result.push_str(new_line);

                for _i in 0..indent {
                    result.push_str("  ");
                }
            }
            result.push_str(&diag.message_text);
            indent += 1;
            if let Some(diag_next) = diag.next.as_ref() {
                for kid in diag_next {
                    result.push_str(&flatten_diagnostic_message_text(
                        // TODO: this .clone() seems non-ideal because we're cloning the entire vec (recursively)
                        Some(&DiagnosticMessageText::DiagnosticMessageChain(kid.clone())),
                        new_line,
                        Some(indent),
                    ));
                }
            }
            result
        }
    }
}

pub trait LoadWithLocalCacheLoader<TValue> {
    fn call(
        &self,
        name: &str,
        containing_file: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> TValue;
}

pub struct LoadWithLocalCacheLoaderResolveTypeReferenceDirective {
    options: Rc<CompilerOptions>,
    host: Rc<dyn CompilerHost>,
    type_reference_directive_resolution_cache: Option<Rc<TypeReferenceDirectiveResolutionCache>>,
}

impl LoadWithLocalCacheLoaderResolveTypeReferenceDirective {
    pub fn new(
        options: Rc<CompilerOptions>,
        host: Rc<dyn CompilerHost>,
        type_reference_directive_resolution_cache: Option<
            Rc<TypeReferenceDirectiveResolutionCache>,
        >,
    ) -> Self {
        Self {
            options,
            host,
            type_reference_directive_resolution_cache,
        }
    }
}

impl LoadWithLocalCacheLoader<Rc<ResolvedTypeReferenceDirective>>
    for LoadWithLocalCacheLoaderResolveTypeReferenceDirective
{
    fn call(
        &self,
        types_ref: &str,
        containing_file: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Rc<ResolvedTypeReferenceDirective> {
        resolve_type_reference_directive(
            types_ref,
            Some(containing_file),
            self.options.clone(),
            self.host.as_dyn_module_resolution_host(),
            redirected_reference,
            self.type_reference_directive_resolution_cache.clone(),
        )
        .resolved_type_reference_directive
        .clone()
        .unwrap()
    }
}

pub(crate) fn load_with_local_cache<TValue: Clone>(
    names: &[String],
    containing_file: &str,
    redirected_reference: Option<Rc<ResolvedProjectReference>>,
    loader: &dyn LoadWithLocalCacheLoader<TValue>,
) -> Vec<TValue> {
    if names.is_empty() {
        return vec![];
    }
    let mut resolutions: Vec<TValue> = vec![];
    let mut cache: HashMap<String, TValue> = HashMap::new();
    for name in names {
        let result: TValue;
        if cache.contains_key(name) {
            result = cache.get(name).cloned().unwrap();
        } else {
            result = loader.call(name, containing_file, redirected_reference.clone());
            cache.insert(name.clone(), result.clone());
        }
        resolutions.push(result);
    }
    resolutions
}

pub(crate) trait SourceFileImportsList {}

impl SourceFileImportsList for SourceFile {}

pub(crate) fn get_mode_for_resolution_at_index<TFile: SourceFileImportsList>(
    file: &TFile,
    index: usize,
) -> Option<ModuleKind> {
    unimplemented!()
}

pub(crate) fn get_mode_for_usage_location(
    implied_node_format: Option<ModuleKind>,
    usage: &Node, /*StringLiteralLike*/
) -> Option<ModuleKind> {
    let implied_node_format = implied_node_format?;
    if implied_node_format != ModuleKind::ESNext {
        return Some(
            if is_import_call(&walk_up_parenthesized_expressions(&usage.parent()).unwrap()) {
                ModuleKind::ESNext
            } else {
                ModuleKind::CommonJS
            },
        );
    }
    let expr_parent_parent =
        walk_up_parenthesized_expressions(&usage.parent()).and_then(|node| node.maybe_parent());
    Some(
        if matches!(
            expr_parent_parent.as_ref(),
            Some(expr_parent_parent) if is_import_equals_declaration(expr_parent_parent)
        ) {
            ModuleKind::CommonJS
        } else {
            ModuleKind::ESNext
        },
    )
}

pub trait LoadWithModeAwareCacheLoader<TValue> {
    fn call(
        &self,
        name: &str,
        resolver_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
        containing_file_name: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> TValue;
}

pub struct LoadWithModeAwareCacheLoaderResolveModuleName {
    options: Rc<CompilerOptions>,
    host: Rc<dyn CompilerHost>,
    module_resolution_cache: Option<Rc<ModuleResolutionCache>>,
}

impl LoadWithModeAwareCacheLoaderResolveModuleName {
    pub fn new(
        options: Rc<CompilerOptions>,
        host: Rc<dyn CompilerHost>,
        module_resolution_cache: Option<Rc<ModuleResolutionCache>>,
    ) -> Self {
        Self {
            options,
            host,
            module_resolution_cache,
        }
    }
}

impl LoadWithModeAwareCacheLoader<Rc<ResolvedModuleFull>>
    for LoadWithModeAwareCacheLoaderResolveModuleName
{
    fn call(
        &self,
        module_name: &str,
        resolver_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
        containing_file_name: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Rc<ResolvedModuleFull> {
        resolve_module_name(
            module_name,
            containing_file_name,
            self.options.clone(),
            self.host.as_dyn_module_resolution_host(),
            self.module_resolution_cache.clone(),
            redirected_reference,
            resolver_mode,
        )
        .resolved_module
        .clone()
        .unwrap()
    }
}

pub(crate) fn load_with_mode_aware_cache<TValue>(
    names: &[String],
    containing_file: &Node, /*SourceFile*/
    containing_file_name: &str,
    redirected_reference: Option<&ResolvedProjectReference>,
    loader: &dyn LoadWithModeAwareCacheLoader<TValue>,
) -> Vec<TValue> {
    unimplemented!()
}

pub fn for_each_resolved_project_reference<
    TReturn,
    TCallback: FnMut(&ResolvedProjectReference, Option<&ResolvedProjectReference>) -> Option<TReturn>,
>(
    resolved_project_references: Option<&[Option<Rc<ResolvedProjectReference>>]>,
    cb: TCallback,
) -> Option<TReturn> {
    unimplemented!()
}

pub(crate) const inferred_types_containing_file: &str = "__inferred type names__.ts";

pub(crate) struct DiagnosticCache {
    pub per_file: Option<HashMap<Path, Vec<Rc<Diagnostic>>>>,
    pub all_diagnostics: Option<Vec<Rc<Diagnostic>>>,
}

impl Default for DiagnosticCache {
    fn default() -> Self {
        Self {
            per_file: None,
            all_diagnostics: None,
        }
    }
}

pub(crate) fn is_referenced_file(reason: Option<&FileIncludeReason>) -> bool {
    matches!(reason, Some(FileIncludeReason::ReferencedFile(_)))
}

#[derive(Debug)]
pub(crate) struct ReferenceFileLocation {
    pub file: Rc<Node /*SourceFile*/>,
    pub pos: usize,
    pub end: usize,
    pub package_id: Option<PackageId>,
}

#[derive(Debug)]
pub(crate) struct SyntheticReferenceFileLocation {
    pub file: Rc<Node /*SourceFile*/>,
    pub package_id: Option<PackageId>,
    pub text: String,
}

pub(crate) fn is_reference_file_location(
    location: &ReferenceFileLocationOrSyntheticReferenceFileLocation,
) -> bool {
    matches!(
        location,
        ReferenceFileLocationOrSyntheticReferenceFileLocation::ReferenceFileLocation(_)
    )
}

#[derive(Debug)]
pub(crate) enum ReferenceFileLocationOrSyntheticReferenceFileLocation {
    ReferenceFileLocation(ReferenceFileLocation),
    SyntheticReferenceFileLocation(SyntheticReferenceFileLocation),
}

impl ReferenceFileLocationOrSyntheticReferenceFileLocation {
    pub fn maybe_package_id(&self) -> Option<&PackageId> {
        match self {
            Self::ReferenceFileLocation(location) => location.package_id.as_ref(),
            Self::SyntheticReferenceFileLocation(location) => location.package_id.as_ref(),
        }
    }

    pub fn file(&self) -> Rc<Node> {
        match self {
            Self::ReferenceFileLocation(location) => location.file.clone(),
            Self::SyntheticReferenceFileLocation(location) => location.file.clone(),
        }
    }
}

pub(crate) fn get_referenced_file_location<
    TGetSourceFileByPath: FnMut(&Path) -> Option<Rc<Node /*SourceFile*/>>,
>(
    get_source_file_by_path: TGetSourceFileByPath,
    ref_: &ReferencedFile,
) -> ReferenceFileLocationOrSyntheticReferenceFileLocation {
    unimplemented!()
}

pub fn get_config_file_parsing_diagnostics(
    config_file_parse_result: &ParsedCommandLine,
) -> Vec<Rc<Diagnostic>> {
    // unimplemented!()
    vec![]
}

pub fn get_implied_node_format_for_file(
    file_name: &Path,
    package_json_info_cache: Option<&dyn PackageJsonInfoCache>,
    host: &dyn ModuleResolutionHost,
    options: &CompilerOptions,
) -> Option<ModuleKind /*ModuleKind.ESNext | ModuleKind.CommonJS*/> {
    match get_emit_module_resolution_kind(options) {
        ModuleResolutionKind::Node12 | ModuleResolutionKind::NodeNext => {
            if file_extension_is_one_of(
                file_name,
                &[Extension::Dmts, Extension::Mts, Extension::Mjs],
            ) {
                Some(ModuleKind::ESNext)
            } else if file_extension_is_one_of(
                file_name,
                &[Extension::Dcts, Extension::Cts, Extension::Cjs],
            ) {
                Some(ModuleKind::CommonJS)
            } else if file_extension_is_one_of(
                file_name,
                &[
                    Extension::Dts,
                    Extension::Ts,
                    Extension::Tsx,
                    Extension::Js,
                    Extension::Jsx,
                ],
            ) {
                Some(lookup_from_package_json(
                    file_name,
                    package_json_info_cache,
                    host,
                    options,
                ))
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn lookup_from_package_json(
    file_name: &Path,
    package_json_info_cache: Option<&dyn PackageJsonInfoCache>,
    host: &dyn ModuleResolutionHost,
    options: &CompilerOptions,
) -> ModuleKind /*ModuleKind.ESNext | ModuleKind.CommonJS*/ {
    let scope = get_package_scope_for_path(file_name, package_json_info_cache, host, options);
    if matches!(
        scope.as_ref(),
        Some(scope) if matches!(
            scope.package_json_content.as_object().and_then(|scope_package_json_content| {
                scope_package_json_content.get("type_")
            }),
            Some(serde_json::Value::String(value)) if &**value == "module"
        )
    ) {
        ModuleKind::ESNext
    } else {
        ModuleKind::CommonJS
    }
}

fn should_program_create_new_source_files(
    program: Option<&Program>,
    new_options: &CompilerOptions,
) -> bool {
    if program.is_none() {
        return false;
    }
    let program = program.unwrap();
    source_file_affecting_compiler_options.with(|source_file_affecting_compiler_options_| {
        options_have_changes(
            &program.get_compiler_options(),
            new_options,
            source_file_affecting_compiler_options_,
        )
    })
}

impl Program {
    pub fn new(
        create_program_options: CreateProgramOptions,
        // options: Rc<CompilerOptions>,
        // files: Vec<Rc<Node>>,
        // current_directory: String,
        // host: Rc<dyn CompilerHost>,
    ) -> Rc<Self> {
        let options = create_program_options.options.clone();
        let max_node_module_js_depth = options.max_node_module_js_depth.unwrap_or(0);
        let rc = Rc::new(Program {
            _rc_wrapper: RefCell::new(None),
            create_program_options: RefCell::new(Some(create_program_options)),
            options,
            processing_default_lib_files: RefCell::new(None),
            processing_other_files: RefCell::new(None),
            files: RefCell::new(None),
            symlinks: RefCell::new(None),
            common_source_directory: RefCell::new(None),
            diagnostics_producing_type_checker: RefCell::new(None),
            no_diagnostics_type_checker: RefCell::new(None),
            classifiable_names: RefCell::new(None),
            ambient_module_name_to_unmodified_file_name: RefCell::new(HashMap::new()),
            file_reasons: RefCell::new(create_multi_map()),
            cached_bind_and_check_diagnostics_for_file: RefCell::new(Default::default()),
            cached_declaration_diagnostics_for_file: RefCell::new(Default::default()),

            resolved_type_reference_directives: RefCell::new(HashMap::new()),
            file_processing_diagnostics: RefCell::new(None),

            max_node_module_js_depth,
            current_node_modules_depth: Cell::new(0),

            modules_with_elided_imports: RefCell::new(HashMap::new()),

            source_files_found_searching_node_modules: RefCell::new(HashMap::new()),

            old_program: RefCell::new(None),
            host: RefCell::new(None),
            config_parsing_host: RefCell::new(None),

            skip_default_lib: Cell::new(None),
            get_default_library_file_name_memoized: RefCell::new(None),
            default_library_path: RefCell::new(None),
            program_diagnostics: RefCell::new(None),
            current_directory: RefCell::new(None),
            supported_extensions: RefCell::new(None),
            supported_extensions_with_json_if_resolve_json_module: RefCell::new(None),
            has_emit_blocking_diagnostics: RefCell::new(None),
            _compiler_options_object_literal_syntax: RefCell::new(None),
            module_resolution_cache: RefCell::new(None),
            type_reference_directive_resolution_cache: RefCell::new(None),
            actual_resolve_module_names_worker: RefCell::new(None),
            actual_resolve_type_reference_directive_names_worker: RefCell::new(None),
            package_id_to_source_file: RefCell::new(None),
            source_file_to_package_name: RefCell::new(None),
            redirect_targets_map: RefCell::new(None),
            uses_uri_style_node_core_modules: Cell::new(None),
            files_by_name: RefCell::new(None),
            missing_file_paths: RefCell::new(None),
            files_by_name_ignore_case: RefCell::new(None),
            resolved_project_references: RefCell::new(None),
            project_reference_redirects: RefCell::new(None),
            map_from_file_to_project_reference_redirects: RefCell::new(None),
            map_from_to_project_reference_redirect_source: RefCell::new(None),
            use_source_of_project_reference_redirect: Cell::new(None),
            file_exists_rc: RefCell::new(None),
            directory_exists_rc: RefCell::new(None),
            should_create_new_source_file: Cell::new(None),
        });
        rc.set_rc_wrapper(Some(rc.clone()));
        rc
    }

    pub fn create(&self) {
        let CreateProgramOptions {
            root_names,
            config_file_parsing_diagnostics,
            project_references,
            old_program,
            host,
            ..
        } = self.create_program_options.borrow_mut().take().unwrap();
        *self.old_program.borrow_mut() = old_program;

        // tracing?.push(tracing.Phase.Program, "createProgram", { configFilePath: options.configFilePath, rootDir: options.rootDir }, /*separateBeginAndEnd*/ true);
        // performance.mark("beforeProgram");

        *self.host.borrow_mut() =
            Some(host.unwrap_or_else(|| Rc::new(create_compiler_host(self.options.clone(), None))));
        *self.config_parsing_host.borrow_mut() =
            Some(Rc::new(parse_config_host_from_compiler_host_like(
                Rc::new(CompilerHostLikeRcDynCompilerHost::new(self.host())),
                None,
            )));

        self.skip_default_lib.set(self.options.no_lib);
        *self.default_library_path.borrow_mut() = Some(
            self.host()
                .get_default_lib_location()
                .unwrap_or_else(|| get_directory_path(&self.get_default_library_file_name())),
        );
        *self.program_diagnostics.borrow_mut() = Some(create_diagnostic_collection());
        *self.current_directory.borrow_mut() =
            Some(CompilerHost::get_current_directory(&*self.host()));
        *self.supported_extensions.borrow_mut() =
            Some(get_supported_extensions(Some(&self.options), None));
        *self
            .supported_extensions_with_json_if_resolve_json_module
            .borrow_mut() = Some(get_supported_extensions_with_json_if_resolve_json_module(
            Some(&self.options),
            &self.supported_extensions(),
        ));

        *self.has_emit_blocking_diagnostics.borrow_mut() = Some(HashMap::new());

        if self.host().is_resolve_module_names_supported() {
            *self.actual_resolve_module_names_worker.borrow_mut() = Some(Rc::new(
                ActualResolveModuleNamesWorkerHost::new(self.host(), self.options.clone()),
            ));
            *self.module_resolution_cache.borrow_mut() = self.host().get_module_resolution_cache();
        } else {
            *self.module_resolution_cache.borrow_mut() =
                Some(Rc::new(create_module_resolution_cache(
                    &self.current_directory(),
                    self.get_canonical_file_name_rc(),
                    Some(self.options.clone()),
                    None,
                    None,
                )));
            let loader = LoadWithModeAwareCacheLoaderResolveModuleName::new(
                self.options.clone(),
                self.host(),
                self.maybe_module_resolution_cache().clone(),
            );
            *self.actual_resolve_module_names_worker.borrow_mut() = Some(Rc::new(
                ActualResolveModuleNamesWorkerLoadWithModeAwareCache::new(Rc::new(loader)),
            ));
        }

        if self.host().is_resolve_type_reference_directives_supported() {
            *self
                .actual_resolve_type_reference_directive_names_worker
                .borrow_mut() = Some(Rc::new(
                ActualResolveTypeReferenceDirectiveNamesWorkerHost::new(
                    self.host(),
                    self.options.clone(),
                ),
            ));
        } else {
            *self.type_reference_directive_resolution_cache.borrow_mut() =
                Some(Rc::new(create_type_reference_directive_resolution_cache(
                    &self.current_directory(),
                    self.get_canonical_file_name_rc(),
                    None,
                    self.maybe_module_resolution_cache()
                        .as_ref()
                        .map(|module_resolution_cache| {
                            module_resolution_cache.get_package_json_info_cache()
                        }),
                    None,
                )));
            let loader = LoadWithLocalCacheLoaderResolveTypeReferenceDirective::new(
                self.options.clone(),
                self.host(),
                self.maybe_type_reference_directive_resolution_cache()
                    .clone(),
            );
            *self
                .actual_resolve_type_reference_directive_names_worker
                .borrow_mut() = Some(Rc::new(
                ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache::new(Rc::new(
                    loader,
                )),
            ));
        }

        *self.package_id_to_source_file.borrow_mut() = Some(HashMap::new());
        *self.source_file_to_package_name.borrow_mut() = Some(HashMap::new());
        *self.redirect_targets_map.borrow_mut() = Some(create_multi_map());
        self.uses_uri_style_node_core_modules.set(Some(false));

        *self.files_by_name.borrow_mut() = Some(HashMap::new());
        *self.files_by_name_ignore_case.borrow_mut() =
            if CompilerHost::use_case_sensitive_file_names(&*self.host()) {
                Some(HashMap::new())
            } else {
                None
            };

        self.use_source_of_project_reference_redirect.set(Some(
            self.host().use_source_of_project_reference_redirect() == Some(true)
                && self.options.disable_source_of_project_reference_redirect != Some(true),
        ));
        let UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
            on_program_create_complete,
            file_exists,
            directory_exists,
        } = update_host_for_use_source_of_project_reference_redirect(
            HostForUseSourceOfProjectReferenceRedirect {
                compiler_host: self.host(),
                get_symlink_cache: self.get_symlink_cache_rc(),
                use_source_of_project_reference_redirect: self
                    .use_source_of_project_reference_redirect(),
                to_path: self.to_path_rc(),
                get_resolved_project_references: {
                    let self_clone = self.rc_wrapper();
                    Rc::new(move || self_clone.get_resolved_project_references().clone())
                },
                for_each_resolved_project_reference: self.for_each_resolved_project_reference_rc(),
            },
        );
        *self.file_exists_rc.borrow_mut() = Some(file_exists);
        *self.directory_exists_rc.borrow_mut() = directory_exists;

        // tracing?.push(tracing.Phase.Program, "shouldProgramCreateNewSourceFiles", { hasOldProgram: !!oldProgram });
        self.should_create_new_source_file
            .set(Some(should_program_create_new_source_files(
                self.maybe_old_program().as_deref(),
                &self.options,
            )));
        // tracing?.pop();
        let structure_is_reused: StructureIsReused;
        // tracing?.push(tracing.Phase.Program, "tryReuseStructureFromOldProgram", {});
        structure_is_reused = self.try_reuse_structure_from_old_program();
        // tracing?.pop();
        if structure_is_reused != StructureIsReused::Completely {
            *self.processing_default_lib_files.borrow_mut() = Some(vec![]);
            *self.processing_other_files.borrow_mut() = Some(vec![]);

            if let Some(project_references) = project_references.as_ref() {
                if self.resolved_project_references.borrow().is_none() {
                    *self.resolved_project_references.borrow_mut() = Some(
                        project_references
                            .into_iter()
                            .map(|project_reference| {
                                self.parse_project_reference_config_file(project_reference)
                            })
                            .collect(),
                    );
                }
                unimplemented!()
            }

            // tracing?.push(tracing.Phase.Program, "processRootFiles", { count: rootNames.length });
            for_each(&root_names, |name, index| {
                self.process_root_file(
                    name,
                    false,
                    false,
                    &FileIncludeReason::RootFile(RootFile {
                        kind: FileIncludeKind::RootFile,
                        index,
                    }),
                );
                Option::<()>::None
            });
            // tracing?.pop();

            let type_references = if !root_names.is_empty() {
                get_automatic_type_directive_names(
                    &self.options,
                    self.host().as_dyn_module_resolution_host(),
                )
            } else {
                vec![]
            };

            if !type_references.is_empty() {
                // tracing?.push(tracing.Phase.Program, "processTypeReferences", { count: typeReferences.length });
                let containing_directory = if let Some(options_config_file_path) =
                    self.options.config_file_path.as_ref()
                {
                    get_directory_path(options_config_file_path)
                } else {
                    CompilerHost::get_current_directory(&*self.host())
                };
                let containing_filename = combine_paths(
                    &containing_directory,
                    &[Some(inferred_types_containing_file)],
                );
                let resolutions = self.resolve_type_reference_directive_names_worker(
                    &type_references,
                    containing_filename,
                );
                for i in 0..type_references.len() {
                    self.process_type_reference_directive(
                        &type_references[i],
                        resolutions.get(i).and_then(|resolution| resolution.clone()),
                        &FileIncludeReason::AutomaticTypeDirectiveFile(
                            AutomaticTypeDirectiveFile {
                                kind: FileIncludeKind::AutomaticTypeDirectiveFile,
                                type_reference: type_references[i].clone(),
                                package_id: resolutions
                                    .get(i)
                                    .cloned()
                                    .flatten()
                                    .and_then(|resolution| resolution.package_id.clone()),
                            },
                        ),
                    );
                }
                // tracing?.pop();
            }

            if !root_names.is_empty() && self.maybe_skip_default_lib() != Some(true) {
                let default_library_file_name = self.get_default_library_file_name();
                if self.options.lib.is_none() && *default_library_file_name != "" {
                    self.process_root_file(
                        &default_library_file_name,
                        true,
                        false,
                        &FileIncludeReason::LibFile(LibFile {
                            kind: FileIncludeKind::LibFile,
                            index: None,
                        }),
                    );
                } else {
                    maybe_for_each(
                        self.options.lib.as_ref(),
                        |lib_file_name: &String, index| -> Option<()> {
                            self.process_root_file(
                                &self.path_for_lib_file(lib_file_name),
                                true,
                                false,
                                &FileIncludeReason::LibFile(LibFile {
                                    kind: FileIncludeKind::LibFile,
                                    index: Some(index),
                                }),
                            );
                            None
                        },
                    );
                }
            }

            *self.missing_file_paths.borrow_mut() = Some(map_defined(
                Some(&*self.files_by_name()),
                |(path, file), _| {
                    if matches!(file, FilesByNameValue::Undefined,) {
                        Some(path.clone().into())
                    } else {
                        None
                    }
                },
            ));

            println!(
                "processing_default_lib_files: {:?}",
                self.processing_default_lib_files
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .into_iter()
                    .map(|file| file.as_source_file().file_name().clone())
                    .collect::<Vec<_>>()
            );
            println!(
                "processing_other_files: {:?}",
                self.processing_other_files
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .into_iter()
                    .map(|file| file.as_source_file().file_name().clone())
                    .collect::<Vec<_>>()
            );
            *self.files.borrow_mut() = Some({
                let mut files: Vec<Rc<Node>> = stable_sort(
                    self.processing_default_lib_files.borrow().as_ref().unwrap(),
                    |a, b| self.compare_default_lib_files(a, b),
                )
                .into();
                let mut processing_other_files =
                    self.processing_other_files.borrow().clone().unwrap();
                files.append(&mut processing_other_files);
                files
            });
            // println!("files: {:#?}", &*self.files());
            *self.processing_default_lib_files.borrow_mut() = None;
            *self.processing_other_files.borrow_mut() = None;
        }

        // performance.mark("afterProgram");
        // performance.measure("Program", "beforeProgram", "afterProgram");
        // tracing?.pop();
    }

    pub fn set_rc_wrapper(&self, rc_wrapper: Option<Rc<Program>>) {
        *self._rc_wrapper.borrow_mut() = rc_wrapper;
    }

    pub fn rc_wrapper(&self) -> Rc<Program> {
        self._rc_wrapper.borrow().clone().unwrap()
    }

    pub(super) fn files(&self) -> Ref<Vec<Rc<Node>>> {
        Ref::map(self.files.borrow(), |files| files.as_ref().unwrap())
    }

    pub(super) fn get_default_library_file_name(&self) -> Ref<String> {
        if self
            .get_default_library_file_name_memoized
            .borrow()
            .is_none()
        {
            *self.get_default_library_file_name_memoized.borrow_mut() =
                Some(self.host().get_default_lib_file_name(&self.options));
        }
        Ref::map(
            self.get_default_library_file_name_memoized.borrow(),
            |default_library_file_name_memoized| {
                default_library_file_name_memoized.as_ref().unwrap()
            },
        )
    }

    pub(super) fn maybe_skip_default_lib(&self) -> Option<bool> {
        self.skip_default_lib.get()
    }

    pub(super) fn set_skip_default_lib(&self, skip_default_lib: Option<bool>) {
        self.skip_default_lib.set(skip_default_lib);
    }

    pub(super) fn default_library_path(&self) -> Ref<String> {
        Ref::map(self.default_library_path.borrow(), |default_library_path| {
            default_library_path.as_ref().unwrap()
        })
    }

    pub(super) fn current_node_modules_depth(&self) -> usize {
        self.current_node_modules_depth.get()
    }

    pub(super) fn set_current_node_modules_depth(&self, current_node_modules_depth: usize) {
        self.current_node_modules_depth
            .set(current_node_modules_depth);
    }

    pub(super) fn modules_with_elided_imports(&self) -> RefMut<HashMap<String, bool>> {
        self.modules_with_elided_imports.borrow_mut()
    }

    pub(super) fn source_files_found_searching_node_modules(
        &self,
    ) -> RefMut<HashMap<String, bool>> {
        self.source_files_found_searching_node_modules.borrow_mut()
    }

    pub(super) fn maybe_old_program(&self) -> Option<Rc<Program>> {
        self.old_program.borrow().clone()
    }

    pub(super) fn host(&self) -> Rc<dyn CompilerHost> {
        self.host.borrow().clone().unwrap()
    }

    pub(super) fn symlinks(&self) -> RefMut<Option<Rc<SymlinkCache>>> {
        self.symlinks.borrow_mut()
    }

    pub(super) fn file_reasons(&self) -> RefMut<MultiMap<Path, FileIncludeReason>> {
        self.file_reasons.borrow_mut()
    }

    pub(super) fn maybe_file_processing_diagnostics(
        &self,
    ) -> RefMut<Option<Vec<FilePreprocessingDiagnostics>>> {
        self.file_processing_diagnostics.borrow_mut()
    }

    pub(super) fn resolved_type_reference_directives(
        &self,
    ) -> RefMut<HashMap<String, Option<Rc<ResolvedTypeReferenceDirective>>>> {
        self.resolved_type_reference_directives.borrow_mut()
    }

    pub(super) fn program_diagnostics(&self) -> RefMut<DiagnosticCollection> {
        RefMut::map(
            self.program_diagnostics.borrow_mut(),
            |program_diagnostics| program_diagnostics.as_mut().unwrap(),
        )
    }

    pub(super) fn current_directory(&self) -> Ref<String> {
        Ref::map(self.current_directory.borrow(), |current_directory| {
            current_directory.as_ref().unwrap()
        })
    }

    pub(super) fn supported_extensions(&self) -> Ref<Vec<Vec<Extension>>> {
        Ref::map(self.supported_extensions.borrow(), |supported_extensions| {
            supported_extensions.as_ref().unwrap()
        })
    }

    pub(super) fn supported_extensions_with_json_if_resolve_json_module(
        &self,
    ) -> Ref<Vec<Vec<Extension>>> {
        Ref::map(
            self.supported_extensions_with_json_if_resolve_json_module
                .borrow(),
            |supported_extensions_with_json_if_resolve_json_module| {
                supported_extensions_with_json_if_resolve_json_module
                    .as_ref()
                    .unwrap()
            },
        )
    }

    pub(super) fn has_emit_blocking_diagnostics(&self) -> RefMut<HashMap<Path, bool>> {
        RefMut::map(
            self.has_emit_blocking_diagnostics.borrow_mut(),
            |has_emit_blocking_diagnostics| has_emit_blocking_diagnostics.as_mut().unwrap(),
        )
    }

    pub(super) fn maybe_compiler_options_object_literal_syntax(
        &self,
    ) -> RefMut<Option<Option<Rc<Node>>>> {
        self._compiler_options_object_literal_syntax.borrow_mut()
    }

    pub(super) fn maybe_module_resolution_cache(
        &self,
    ) -> RefMut<Option<Rc<ModuleResolutionCache>>> {
        self.module_resolution_cache.borrow_mut()
    }

    pub(super) fn maybe_type_reference_directive_resolution_cache(
        &self,
    ) -> RefMut<Option<Rc<TypeReferenceDirectiveResolutionCache>>> {
        self.type_reference_directive_resolution_cache.borrow_mut()
    }

    pub(super) fn actual_resolve_module_names_worker(
        &self,
    ) -> Rc<dyn ActualResolveModuleNamesWorker> {
        self.actual_resolve_module_names_worker
            .borrow_mut()
            .clone()
            .unwrap()
    }

    pub(super) fn actual_resolve_type_reference_directive_names_worker(
        &self,
    ) -> Rc<dyn ActualResolveTypeReferenceDirectiveNamesWorker> {
        self.actual_resolve_type_reference_directive_names_worker
            .borrow_mut()
            .clone()
            .unwrap()
    }

    pub(super) fn package_id_to_source_file(
        &self,
    ) -> RefMut<HashMap<String, Rc<Node /*SourceFile*/>>> {
        RefMut::map(
            self.package_id_to_source_file.borrow_mut(),
            |package_id_to_source_file| package_id_to_source_file.as_mut().unwrap(),
        )
    }

    pub(super) fn source_file_to_package_name(&self) -> RefMut<HashMap<Path, String>> {
        RefMut::map(
            self.source_file_to_package_name.borrow_mut(),
            |source_file_to_package_name| source_file_to_package_name.as_mut().unwrap(),
        )
    }

    pub(super) fn redirect_targets_map(&self) -> RefMut<MultiMap<Path, String>> {
        RefMut::map(
            self.redirect_targets_map.borrow_mut(),
            |redirect_targets_map| redirect_targets_map.as_mut().unwrap(),
        )
    }

    pub(super) fn uses_uri_style_node_core_modules(&self) -> bool {
        self.uses_uri_style_node_core_modules.get().unwrap()
    }

    pub(super) fn set_uses_uri_style_node_core_modules(
        &self,
        uses_uri_style_node_core_modules: bool,
    ) {
        self.uses_uri_style_node_core_modules
            .set(Some(uses_uri_style_node_core_modules));
    }

    pub(super) fn files_by_name(&self) -> RefMut<HashMap<String, FilesByNameValue>> {
        RefMut::map(self.files_by_name.borrow_mut(), |files_by_name| {
            files_by_name.as_mut().unwrap()
        })
    }

    pub(super) fn maybe_missing_file_paths(&self) -> RefMut<Option<Vec<Path>>> {
        self.missing_file_paths.borrow_mut()
    }

    pub(super) fn files_by_name_ignore_case(&self) -> RefMut<HashMap<String, Rc<Node>>> {
        RefMut::map(
            self.files_by_name_ignore_case.borrow_mut(),
            |files_by_name_ignore_case| files_by_name_ignore_case.as_mut().unwrap(),
        )
    }

    pub(super) fn maybe_resolved_project_references(
        &self,
    ) -> RefMut<Option<Vec<Option<Rc<ResolvedProjectReference>>>>> {
        self.resolved_project_references.borrow_mut()
    }

    pub(super) fn maybe_project_reference_redirects(
        &self,
    ) -> RefMut<Option<HashMap<Path, Option<Rc<ResolvedProjectReference>>>>> {
        self.project_reference_redirects.borrow_mut()
    }

    pub(super) fn maybe_map_from_file_to_project_reference_redirects(
        &self,
    ) -> RefMut<Option<HashMap<Path, Path>>> {
        self.map_from_file_to_project_reference_redirects
            .borrow_mut()
    }

    pub(super) fn maybe_map_from_to_project_reference_redirect_source(
        &self,
    ) -> RefMut<Option<HashMap<Path, SourceOfProjectReferenceRedirect>>> {
        self.map_from_to_project_reference_redirect_source
            .borrow_mut()
    }

    pub(super) fn use_source_of_project_reference_redirect(&self) -> bool {
        self.use_source_of_project_reference_redirect.get().unwrap()
    }

    pub(super) fn file_exists_rc(&self) -> Rc<dyn ModuleResolutionHostOverrider> {
        self.file_exists_rc.borrow().clone().unwrap()
    }

    pub(super) fn maybe_directory_exists_rc(
        &self,
    ) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
        self.directory_exists_rc.borrow().clone()
    }

    pub(super) fn should_create_new_source_file(&self) -> bool {
        self.should_create_new_source_file.get().unwrap()
    }

    pub(super) fn resolve_type_reference_directive_names_worker<
        TContainingFile: Into<StringOrRcNode>,
    >(
        &self,
        type_directive_names: &[String],
        containing_file: TContainingFile,
    ) -> Vec<Option<Rc<ResolvedTypeReferenceDirective>>> {
        if type_directive_names.is_empty() {
            return vec![];
        }
        let containing_file: StringOrRcNode = containing_file.into();
        let containing_file_name = match &containing_file {
            StringOrRcNode::RcNode(containing_file) => get_normalized_absolute_path(
                &containing_file.as_source_file().original_file_name(),
                Some(&self.current_directory()),
            ),
            StringOrRcNode::String(containing_file) => containing_file.clone(),
        };
        let redirected_reference = match &containing_file {
            StringOrRcNode::RcNode(containing_file) => {
                self.get_redirect_reference_for_resolution(containing_file)
            }
            StringOrRcNode::String(_) => None,
        };
        // tracing?.push(tracing.Phase.Program, "resolveTypeReferenceDirectiveNamesWorker", { containingFileName });
        // performance.mark("beforeResolveTypeReference");
        let result = self
            .actual_resolve_type_reference_directive_names_worker()
            .call(
                type_directive_names,
                &containing_file_name,
                redirected_reference.clone(),
            );
        // performance.mark("afterResolveTypeReference");
        // performance.measure("ResolveTypeReference", "beforeResolveTypeReference", "afterResolveTypeReference");
        // tracing?.pop();
        result
    }

    pub(super) fn get_redirect_reference_for_resolution(
        &self,
        file: &Node, /*SourceFile*/
    ) -> Option<Rc<ResolvedProjectReference>> {
        unimplemented!()
    }

    pub(super) fn compare_default_lib_files(
        &self,
        a: &Node, /*SourceFile*/
        b: &Node, /*SourceFile*/
    ) -> Comparison {
        unimplemented!()
    }

    pub(super) fn has_invalidated_resolution(&self, source_file: &Path) -> bool {
        self.host()
            .has_invalidated_resolution(source_file)
            .unwrap_or(false)
    }

    pub fn get_root_file_names(&self) -> &[String] {
        unimplemented!()
    }

    pub fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        self.options.clone()
    }

    pub fn use_case_sensitive_file_names(&self) -> bool {
        unimplemented!()
    }

    pub fn get_file_include_reasons(&self) -> MultiMap<Path, FileIncludeReason> {
        unimplemented!()
    }

    pub fn get_source_file_(&self, file_name: &str) -> Option<Rc<Node /*SourceFile*/>> {
        unimplemented!()
    }

    pub fn get_syntactic_diagnostics(
        &self,
        source_file: Option<&Node /*SourceFile*/>,
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>> {
        self.get_diagnostics_helper(
            Program::get_syntactic_diagnostics_for_file,
            cancellation_token,
        )
    }

    pub fn get_semantic_diagnostics(
        &self,
        source_file: Option<&Node /*SourceFile*/>,
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_helper(
            Program::get_semantic_diagnostics_for_file,
            cancellation_token,
        )
    }

    pub fn emit(
        &self,
        target_source_file: Option<&Node /*SourceFile*/>,
        write_file: Option<&dyn WriteFileCallback>,
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
        emit_only_dts_files: Option<bool>,
        custom_transformers: Option<CustomTransformers>,
        force_dts_emit: Option<bool>,
    ) -> EmitResult {
        EmitResult {
            emit_skipped: true,
            diagnostics: vec![],
            emitted_files: None,
            source_maps: None,
            exported_modules_from_declaration_emit: None,
        }
    }

    pub fn get_current_directory(&self) -> String {
        self.current_directory().clone()
    }

    pub fn to_path(&self, file_name: &str) -> Path {
        to_path_helper(file_name, Some(&self.current_directory()), |file_name| {
            self.get_canonical_file_name(file_name)
        })
    }

    pub fn to_path_rc(&self) -> Rc<dyn Fn(&str) -> Path> {
        let self_clone = self.rc_wrapper();
        Rc::new(move |file_name| self_clone.to_path(file_name))
    }

    fn resolve_module_names_reusing_old_state(
        &self,
        module_names: &[String],
        file: &Node, /*SourceFile*/
    ) -> Vec<Rc<ResolvedModuleFull>> {
        unimplemented!()
    }

    pub fn try_reuse_structure_from_old_program(&self) -> StructureIsReused {
        if self.maybe_old_program().is_none() {
            return StructureIsReused::Not;
        }

        unimplemented!()
    }

    pub fn get_resolved_project_references(
        &self,
    ) -> Ref<Option<Vec<Option<Rc<ResolvedProjectReference>>>>> {
        self.resolved_project_references.borrow()
    }

    pub fn is_source_file_default_library(&self, file: &Node /*SourceFile*/) -> bool {
        unimplemented!()
    }

    fn get_diagnostics_producing_type_checker(&self) -> Rc<TypeChecker> {
        // self.diagnostics_producing_type_checker
        //     .get_or_insert_with(|| create_type_checker(self, true))

        // if let Some(type_checker) = self.diagnostics_producing_type_checker.as_ref() {
        //     return type_checker;
        // } else {
        //     self.diagnostics_producing_type_checker = Some(create_type_checker(self, true));
        //     self.diagnostics_producing_type_checker.as_ref().unwrap()
        // }
        let mut diagnostics_producing_type_checker =
            self.diagnostics_producing_type_checker.borrow_mut();
        if diagnostics_producing_type_checker.is_none() {
            *diagnostics_producing_type_checker =
                Some(create_type_checker(self.rc_wrapper(), true));
        }
        diagnostics_producing_type_checker.as_ref().unwrap().clone()
    }

    fn get_diagnostics_helper(
        &self,
        get_diagnostics: fn(
            &Program,
            &Node, /*SourceFile*/
            Option<Rc<dyn CancellationTokenDebuggable>>,
        ) -> Vec<Rc<Diagnostic>>,
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_source_files()
            .iter()
            .flat_map(|source_file| get_diagnostics(self, source_file, cancellation_token.clone()))
            .collect()
    }

    fn get_program_diagnostics(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Diagnostic>> {
        vec![]
    }

    fn run_with_cancellation_token<TReturn, TClosure: FnOnce() -> TReturn>(
        &self,
        func: TClosure,
    ) -> TReturn {
        func()
    }

    fn get_syntactic_diagnostics_for_file(
        &self,
        source_file: &Node, /*SourceFile*/
        // TODO: getSyntacticDiagnosticsForFile() doesn't actually take this argument, should
        // refactor eg get_diagnostics_helper() to use closures instead?
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        source_file.as_source_file().parse_diagnostics().clone()
    }

    fn get_semantic_diagnostics_for_file(
        &self,
        source_file: &Node, /*SourceFile*/
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        concatenate(
            filter_semantic_diagnostics(
                self.get_bind_and_check_diagnostics_for_file(source_file, cancellation_token),
            ),
            self.get_program_diagnostics(source_file),
        )
    }

    fn get_bind_and_check_diagnostics_for_file(
        &self,
        source_file: &Node, /*SourceFile*/
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_and_cache_diagnostics(
            source_file,
            cancellation_token,
            Program::get_bind_and_check_diagnostics_for_file_no_cache,
        )
    }

    fn get_bind_and_check_diagnostics_for_file_no_cache(
        &self,
        source_file: &Node, /*SourceFile*/
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        // self.run_with_cancellation_token(|| {
        let type_checker = self.get_diagnostics_producing_type_checker();

        let include_bind_and_check_diagnostics = true;
        let check_diagnostics = if include_bind_and_check_diagnostics {
            type_checker.get_diagnostics(Some(source_file), cancellation_token)
        } else {
            vec![]
        };

        check_diagnostics
        // })
    }

    fn get_and_cache_diagnostics(
        &self,
        source_file: &Node, /*SourceFile*/
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
        get_diagnostics: fn(
            &Program,
            &Node, /*SourceFile*/
            Option<Rc<dyn CancellationTokenDebuggable>>,
        ) -> Vec<Rc<Diagnostic>>,
    ) -> Vec<Rc<Diagnostic>> {
        let result = get_diagnostics(self, source_file, cancellation_token);
        result
    }

    pub fn get_options_diagnostics(
        &self,
        _cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> SortedArray<Rc<Diagnostic>> {
        SortedArray::new(vec![])
    }

    pub fn get_global_diagnostics(
        &self,
        _cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> SortedArray<Rc<Diagnostic>> {
        SortedArray::new(vec![])
    }

    pub fn get_config_file_parsing_diagnostics(&self) -> Vec<Rc<Diagnostic>> {
        vec![]
    }

    pub fn process_root_file(
        &self,
        file_name: &str,
        is_default_lib: bool,
        ignore_no_default_lib: bool,
        reason: &FileIncludeReason,
    ) {
        self.process_source_file(
            &normalize_path(file_name),
            is_default_lib,
            ignore_no_default_lib,
            None,
            reason,
        );
    }

    pub fn create_synthetic_import(&self, text: &str, file: &Node /*SourceFile*/) -> Rc<Node> {
        unimplemented!()
    }

    pub fn collect_external_module_references(&self, file: &Node /*SourceFile*/) {
        let file_as_source_file = file.as_source_file();
        if file_as_source_file.maybe_imports().is_some() {
            return;
        }

        let is_java_script_file = is_source_file_js(file);
        let is_external_module_file = is_external_module(file);

        let mut imports: Option<Vec<Rc<Node /*StringLiteralLike*/>>> = None;
        let mut module_augmentations: Option<Vec<Rc<Node /*StringLiteral | Identifier*/>>> = None;
        let mut ambient_modules: Option<Vec<String>> = None;

        if (self.options.isolated_modules == Some(true) || is_external_module_file)
            && !file_as_source_file.is_declaration_file()
        {
            if self.options.import_helpers == Some(true) {
                imports =
                    Some(vec![self.create_synthetic_import(
                        external_helpers_module_name_text,
                        file,
                    )]);
            }
            let jsx_import = get_jsx_runtime_import(
                get_jsx_implicit_import_base(&self.options, Some(file)).as_deref(),
                &self.options,
            );
            if let Some(jsx_import) = jsx_import
                .as_ref()
                .filter(|jsx_import| !jsx_import.is_empty())
            {
                imports
                    .get_or_insert_with(|| vec![])
                    .push(self.create_synthetic_import(jsx_import, file));
            }
        }

        for node in &file_as_source_file.statements {
            self.collect_module_references(
                &mut imports,
                file,
                is_external_module_file,
                &mut module_augmentations,
                &mut ambient_modules,
                node,
                false,
            );
        }
        if file
            .flags()
            .intersects(NodeFlags::PossiblyContainsDynamicImport)
            || is_java_script_file
        {
            self.collect_dynamic_import_or_require_calls(is_java_script_file, &mut imports, file);
        }

        *file_as_source_file.maybe_imports() = Some(imports.unwrap_or_else(|| vec![]));
        *file_as_source_file.maybe_module_augmentations() =
            Some(module_augmentations.unwrap_or_else(|| vec![]));
        *file_as_source_file.maybe_ambient_module_names() =
            Some(ambient_modules.unwrap_or_else(|| vec![]));
    }

    fn collect_module_references(
        &self,
        imports: &mut Option<Vec<Rc<Node>>>,
        file: &Node,
        is_external_module_file: bool,
        module_augmentations: &mut Option<Vec<Rc<Node>>>,
        ambient_modules: &mut Option<Vec<String>>,
        node: &Node, /*Statement*/
        in_ambient_module: bool,
    ) {
        if is_any_import_or_re_export(node) {
            let module_name_expr = get_external_module_name(node);
            if let Some(module_name_expr) = module_name_expr.as_ref().filter(|module_name_expr| {
                is_string_literal(module_name_expr) && {
                    let module_name_text = module_name_expr.as_string_literal().text();
                    !module_name_text.is_empty()
                        && (!in_ambient_module
                            || !is_external_module_name_relative(&module_name_text))
                }
            }) {
                set_parent_recursive(Some(node), false);
                append(
                    imports.get_or_insert_with(|| vec![]),
                    Some(module_name_expr.clone()),
                );
                if !self.uses_uri_style_node_core_modules()
                    && self.current_node_modules_depth() == 0
                    && !file.as_source_file().is_declaration_file()
                {
                    self.set_uses_uri_style_node_core_modules(starts_with(
                        &module_name_expr.as_string_literal().text(),
                        "node:",
                    ));
                }
            }
        } else if is_module_declaration(node) {
            if is_ambient_module(node)
                && (in_ambient_module
                    || has_syntactic_modifier(node, ModifierFlags::Ambient)
                    || file.as_source_file().is_declaration_file())
            {
                let node_name = node.as_named_declaration().name();
                node_name.set_parent(node.node_wrapper());
                let name_text = get_text_of_identifier_or_literal(&node_name);
                if is_external_module_file
                    || (in_ambient_module && !is_external_module_name_relative(&name_text))
                {
                    module_augmentations
                        .get_or_insert_with(|| vec![])
                        .push(node_name);
                } else if !in_ambient_module {
                    if file.as_source_file().is_declaration_file() {
                        ambient_modules
                            .get_or_insert_with(|| vec![])
                            .push(name_text);
                    }
                    let body = node.as_module_declaration().body.as_ref();
                    if let Some(body) = body {
                        for statement in &body.as_module_block().statements {
                            self.collect_module_references(
                                imports,
                                file,
                                is_external_module_file,
                                module_augmentations,
                                ambient_modules,
                                statement,
                                true,
                            );
                        }
                    }
                }
            }
        }
    }

    fn collect_dynamic_import_or_require_calls(
        &self,
        is_java_script_file: bool,
        imports: &mut Option<Vec<Rc<Node>>>,
        file: &Node, /*SourceFile*/
    ) {
        lazy_static! {
            static ref r: Regex = Regex::new(r"import|require").unwrap();
        }
        for match_ in r.find_iter(&file.as_source_file().text()) {
            let ref node = self.get_node_at_position(
                is_java_script_file,
                file,
                // TODO: I think this needs to use "char count" rather than "byte count" somehow?
                match_.start().try_into().unwrap(),
            );
            if is_java_script_file && is_require_call(node, true) {
                set_parent_recursive(Some(&**node), false);
                if let Some(node_arguments_0) = node.as_call_expression().arguments.get(0).cloned()
                {
                    append(
                        imports.get_or_insert_with(|| vec![]),
                        Some(node_arguments_0),
                    );
                }
            } else if is_import_call(node) && {
                let node_arguments = &node.as_call_expression().arguments;
                node_arguments.len() >= 1 && is_string_literal_like(&node_arguments[0])
            } {
                set_parent_recursive(Some(&**node), false);
                if let Some(node_arguments_0) = node.as_call_expression().arguments.get(0).cloned()
                {
                    append(
                        imports.get_or_insert_with(|| vec![]),
                        Some(node_arguments_0),
                    );
                }
            } else if is_literal_import_type_node(node) {
                set_parent_recursive(Some(&**node), false);
                append(
                    imports.get_or_insert_with(|| vec![]),
                    Some(
                        node.as_import_type_node()
                            .argument
                            .as_literal_type_node()
                            .literal
                            .clone(),
                    ),
                );
            }
        }
    }

    fn get_node_at_position(
        &self,
        is_java_script_file: bool,
        source_file: &Node, /*SourceFile*/
        position: isize,
    ) -> Rc<Node> {
        let mut current = source_file.node_wrapper();
        loop {
            let child = if is_java_script_file && has_jsdoc_nodes(&current) {
                maybe_for_each(current.maybe_js_doc().as_ref(), |child: &Rc<Node>, _| {
                    self.get_containing_child(position, child)
                })
            } else {
                None
            }
            .or_else(|| {
                for_each_child_returns(
                    &current,
                    |child: &Node| self.get_containing_child(position, child),
                    Option::<fn(&NodeArray) -> Option<Rc<Node>>>::None,
                )
            });
            if child.is_none() {
                return current;
            }
            current = child.unwrap();
        }
    }

    fn get_containing_child(&self, position: isize, child: &Node) -> Option<Rc<Node>> {
        if child.pos() <= position
            && (position < child.end()
                || position == child.end() && child.kind() == SyntaxKind::EndOfFileToken)
        {
            return Some(child.node_wrapper());
        }
        None
    }

    fn get_source_file_from_reference_worker<
        TGetSourceFile: FnMut(&str) -> Option<Rc<Node>>,
        TFail: FnMut(&'static DiagnosticMessage, Option<Vec<String>>),
    >(
        &self,
        file_name: &str,
        mut get_source_file: TGetSourceFile,
        mut fail: Option<TFail>,
        reason: Option<&FileIncludeReason>,
    ) -> Option<Rc<Node>> {
        if has_extension(file_name) {
            let canonical_file_name = self.host().get_canonical_file_name(file_name);
            if self.options.allow_non_ts_extensions != Some(true)
                && !for_each_bool(
                    &flatten(&self.supported_extensions_with_json_if_resolve_json_module()),
                    |extension: &Extension, _| {
                        file_extension_is(&canonical_file_name, extension.to_str())
                    },
                )
            {
                if let Some(fail) = fail.as_mut() {
                    if has_js_file_extension(&canonical_file_name) {
                        fail(&Diagnostics::File_0_is_a_JavaScript_file_Did_you_mean_to_enable_the_allowJs_option, Some(vec![
                            file_name.to_owned(),
                        ]));
                    } else {
                        fail(&Diagnostics::File_0_has_an_unsupported_extension_The_only_supported_extensions_are_1, Some(vec![
                            file_name.to_owned(),
                            format!("'{}'", flatten(&self.supported_extensions()).iter().map(Extension::to_str).collect::<Vec<_>>().join("', '"))
                        ]));
                    }
                }
                return None;
            }

            let source_file = get_source_file(file_name);
            if let Some(fail) = fail.as_mut() {
                if source_file.is_none() {
                    let redirect = self.get_project_reference_redirect_(file_name);
                    if let Some(redirect) = redirect {
                        fail(
                            &Diagnostics::Output_file_0_has_not_been_built_from_source_file_1,
                            Some(vec![redirect, file_name.to_owned()]),
                        );
                    } else {
                        fail(
                            &Diagnostics::File_0_not_found,
                            Some(vec![file_name.to_owned()]),
                        );
                    }
                } else if is_referenced_file(reason)
                    && canonical_file_name
                        == self.host().get_canonical_file_name(
                            &self
                                .get_source_file_by_path(&reason.unwrap().as_referenced_file().file)
                                .unwrap()
                                .as_source_file()
                                .file_name(),
                        )
                {
                    fail(&Diagnostics::A_file_cannot_have_a_reference_to_itself, None);
                }
            }
            source_file
        } else {
            let source_file_no_extension = if self.options.allow_non_ts_extensions == Some(true) {
                get_source_file(file_name)
            } else {
                None
            };
            if source_file_no_extension.is_some() {
                return source_file_no_extension;
            }

            if let Some(fail) = fail.as_mut() {
                if self.options.allow_non_ts_extensions == Some(true) {
                    fail(
                        &Diagnostics::File_0_not_found,
                        Some(vec![file_name.to_owned()]),
                    );
                    return None;
                }
            }

            let source_file_with_added_extension = for_each(
                &self.supported_extensions()[0],
                |extension: &Extension, _| {
                    get_source_file(&format!("{}{}", file_name, extension.to_str()))
                },
            );
            if let Some(fail) = fail.as_mut() {
                if source_file_with_added_extension.is_none() {
                    fail(
                        &Diagnostics::Could_not_resolve_the_path_0_with_the_extensions_Colon_1,
                        Some(vec![
                            file_name.to_owned(),
                            format!(
                                "'{}'",
                                flatten(&self.supported_extensions())
                                    .iter()
                                    .map(Extension::to_str)
                                    .collect::<Vec<_>>()
                                    .join("', '")
                            ),
                        ]),
                    );
                }
            }
            source_file_with_added_extension
        }
    }

    pub fn process_source_file(
        &self,
        file_name: &str,
        is_default_lib: bool,
        ignore_no_default_lib: bool,
        package_id: Option<&PackageId>,
        reason: &FileIncludeReason,
    ) {
        self.get_source_file_from_reference_worker(
            file_name,
            |file_name| {
                self.find_source_file(
                    file_name,
                    is_default_lib,
                    ignore_no_default_lib,
                    reason,
                    package_id,
                )
            },
            Some(
                |diagnostic: &'static DiagnosticMessage, args: Option<Vec<String>>| {
                    self.add_file_preprocessing_file_explaining_diagnostic(
                        Option::<&Node>::None,
                        reason,
                        diagnostic,
                        args,
                    )
                },
            ),
            Some(reason),
        );
    }

    pub fn report_file_names_differ_only_in_casing_error(
        &self,
        file_name: &str,
        existing_file: &Node, /*SourceFile*/
        reason: &FileIncludeReason,
    ) {
        unimplemented!()
    }

    pub fn create_redirect_source_file(
        &self,
        redirect_target: &Node, /*SourceFile*/
        unredirected: &Node,    /*SourceFile*/
        file_name: &str,
        path: &Path,
        resolved_path: &Path,
        original_file_name: &str,
    ) -> Rc<Node /*SourceFile*/> {
        unimplemented!()
    }

    pub fn find_source_file(
        &self,
        file_name: &str,
        is_default_lib: bool,
        ignore_no_default_lib: bool,
        reason: &FileIncludeReason,
        package_id: Option<&PackageId>,
    ) -> Option<Rc<Node>> {
        // tracing?.push(tracing.Phase.Program, "findSourceFile", {
        //     fileName,
        //     isDefaultLib: isDefaultLib || undefined,
        //     fileIncludeKind: (FileIncludeKind as any)[reason.kind],
        // });
        let result = self.find_source_file_worker(
            file_name,
            is_default_lib,
            ignore_no_default_lib,
            reason,
            package_id,
        );
        // tracing?.pop();
        result
    }

    pub fn find_source_file_worker(
        &self,
        file_name: &str,
        is_default_lib: bool,
        ignore_no_default_lib: bool,
        reason: &FileIncludeReason,
        package_id: Option<&PackageId>,
    ) -> Option<Rc<Node>> {
        let path = self.to_path(file_name);
        if self.use_source_of_project_reference_redirect() {
            let mut source = self.get_source_of_project_reference_redirect(&path);
            if source.is_none()
                && self.host().is_realpath_supported()
                && self.options.preserve_symlinks == Some(true)
                && is_declaration_file_name(file_name)
                && string_contains(file_name, node_modules_path_part)
            {
                let real_path = self.to_path(&self.host().realpath(file_name).unwrap());
                if real_path != path {
                    source = self.get_source_of_project_reference_redirect(&real_path);
                }
            }
            if let Some(source) = source.as_ref() {
                let file = match source {
                    SourceOfProjectReferenceRedirect::String(source) => self.find_source_file(
                        source,
                        is_default_lib,
                        ignore_no_default_lib,
                        reason,
                        package_id,
                    ),
                    _ => None,
                };
                if let Some(file) = file.as_ref() {
                    self.add_file_to_files_by_name(Some(&**file), &path, None);
                    return Some(file.clone());
                }
            }
        }
        let original_file_name = file_name;
        let mut file_name = file_name.to_owned();
        if self.files_by_name().contains_key(&*path) {
            let file = self.files_by_name().get(&*path).unwrap().clone();
            let file = match file {
                FilesByNameValue::SourceFile(file) => Some(file),
                _ => None,
            };
            self.add_file_include_reason(file.as_deref(), reason);
            if let Some(file) = file.as_ref() {
                if self.options.force_consistent_casing_in_file_names == Some(true) {
                    let ref checked_name = file.as_source_file().file_name();
                    let is_redirect = self.to_path(checked_name) != self.to_path(&file_name);
                    if is_redirect {
                        file_name = self
                            .get_project_reference_redirect_(&file_name)
                            .unwrap_or(file_name);
                    }
                    let checked_absolute_path = get_normalized_absolute_path_without_root(
                        checked_name,
                        Some(&**self.current_directory()),
                    );
                    let input_absolute_path = get_normalized_absolute_path_without_root(
                        &file_name,
                        Some(&**self.current_directory()),
                    );
                    if checked_absolute_path != input_absolute_path {
                        self.report_file_names_differ_only_in_casing_error(
                            &file_name, file, reason,
                        );
                    }
                }
            }

            if let Some(file) = file.as_ref().filter(|file| {
                matches!(
                    self.source_files_found_searching_node_modules()
                        .get(&**file.as_source_file().path())
                        .cloned(),
                    Some(true)
                ) && self.current_node_modules_depth() == 0
            }) {
                self.source_files_found_searching_node_modules()
                    .insert(file.as_source_file().path().to_string(), false);
                if self.options.no_resolve != Some(true) {
                    self.process_referenced_files(file, is_default_lib);
                    self.process_type_reference_directives(file);
                }
                if self.options.no_lib != Some(true) {
                    self.process_lib_reference_directives(file);
                }

                self.modules_with_elided_imports()
                    .insert(file.as_source_file().path().to_string(), false);
                self.process_imported_modules(file);
            } else if let Some(file) = file.as_ref().filter(|file| {
                matches!(
                    self.modules_with_elided_imports()
                        .get(&**file.as_source_file().path())
                        .cloned(),
                    Some(true)
                )
            }) {
                if self.current_node_modules_depth() < self.max_node_module_js_depth {
                    self.modules_with_elided_imports()
                        .insert(file.as_source_file().path().to_string(), false);
                    self.process_imported_modules(file);
                }
            }

            return file;
        }

        let mut redirected_path: Option<Path> = None;
        if is_referenced_file(Some(reason)) && !self.use_source_of_project_reference_redirect() {
            let redirect_project = self.get_project_reference_redirect_project(&file_name);
            if let Some(redirect_project) = redirect_project.as_ref() {
                if matches!(
                    out_file(&redirect_project.command_line.options),
                    Some(out_file) if !out_file.is_empty()
                ) {
                    return None;
                }
                let redirect = self.get_project_reference_output_name(redirect_project, &file_name);
                file_name = redirect.clone();
                redirected_path = Some(self.to_path(&redirect));
            }
        }

        let file = self.host().get_source_file(
            &file_name,
            get_emit_script_target(&self.options),
            // TODO: this is wrong
            Some(&mut |host_error_message| {
                self.add_file_preprocessing_file_explaining_diagnostic(
                    Option::<&Node>::None,
                    reason,
                    &Diagnostics::Cannot_read_file_0_Colon_1,
                    Some(vec![file_name.clone(), host_error_message.to_owned()]),
                );
            }),
            Some(self.should_create_new_source_file()),
        );

        if let Some(package_id) = package_id {
            let package_id_key = package_id_to_string(package_id);
            let file_from_package_id = self
                .package_id_to_source_file()
                .get(&package_id_key)
                .cloned();
            if let Some(file_from_package_id) = file_from_package_id.as_ref() {
                let dup_file = self.create_redirect_source_file(
                    file_from_package_id,
                    file.as_ref().unwrap(),
                    &file_name,
                    &path,
                    &self.to_path(&file_name),
                    original_file_name,
                );
                self.redirect_targets_map().add(
                    file_from_package_id.as_source_file().path().clone(),
                    file_name.clone(),
                );
                self.add_file_to_files_by_name(Some(&*dup_file), &path, redirected_path.as_ref());
                self.add_file_include_reason(Some(&*dup_file), reason);
                self.source_file_to_package_name()
                    .insert(path.clone(), package_id.name.clone());
                self.processing_other_files
                    .borrow_mut()
                    .as_mut()
                    .unwrap()
                    .push(dup_file.clone());
                return Some(dup_file);
            } else if let Some(file) = file.as_ref() {
                self.package_id_to_source_file()
                    .insert(package_id_key, file.clone());
                self.source_file_to_package_name()
                    .insert(path.clone(), package_id.name.clone());
            }
        }
        self.add_file_to_files_by_name(file.as_deref(), &path, redirected_path.as_ref());

        if let Some(file) = file.as_ref() {
            self.source_files_found_searching_node_modules()
                .insert(path.to_string(), self.current_node_modules_depth() > 0);
            let file_as_source_file = file.as_source_file();
            file_as_source_file.set_file_name(file_name.clone());
            file_as_source_file.set_path(path.clone());
            file_as_source_file.set_resolved_path(Some(self.to_path(&file_name)));
            file_as_source_file.set_original_file_name(Some(original_file_name.to_owned()));
            file_as_source_file.set_implied_node_format(get_implied_node_format_for_file(
                file_as_source_file.maybe_resolved_path().as_ref().unwrap(),
                self.maybe_module_resolution_cache()
                    .as_ref()
                    .map(|module_resolution_cache| {
                        module_resolution_cache.get_package_json_info_cache()
                    })
                    .as_deref(),
                self.host().as_dyn_module_resolution_host(),
                &self.options,
            ));
            self.add_file_include_reason(Some(&**file), reason);

            if CompilerHost::use_case_sensitive_file_names(&*self.host()) {
                let path_lower_case = to_file_name_lower_case(&path);
                let existing_file = self
                    .files_by_name_ignore_case()
                    .get(&path_lower_case)
                    .cloned();
                if let Some(existing_file) = existing_file.as_ref() {
                    self.report_file_names_differ_only_in_casing_error(
                        &file_name,
                        existing_file,
                        reason,
                    );
                } else {
                    self.files_by_name_ignore_case()
                        .insert(path_lower_case, file.clone());
                }
            }

            self.set_skip_default_lib(Some(
                self.maybe_skip_default_lib() == Some(true)
                    || file_as_source_file.has_no_default_lib() && !ignore_no_default_lib,
            ));

            if self.options.no_resolve != Some(true) {
                self.process_referenced_files(file, is_default_lib);
                self.process_type_reference_directives(file);
            }
            if self.options.no_lib != Some(true) {
                self.process_lib_reference_directives(file);
            }

            self.process_imported_modules(file);

            if is_default_lib {
                self.processing_default_lib_files
                    .borrow_mut()
                    .as_mut()
                    .unwrap()
                    .push(file.clone());
            } else {
                self.processing_other_files
                    .borrow_mut()
                    .as_mut()
                    .unwrap()
                    .push(file.clone());
            }
        }
        file
    }

    fn add_file_include_reason<TFile: Borrow<Node>>(
        &self,
        file: Option<TFile /*SourceFile*/>,
        reason: &FileIncludeReason,
    ) {
        if let Some(file) = file {
            let file = file.borrow();
            self.file_reasons()
                .add(file.as_source_file().path().clone(), reason.clone());
        }
    }

    fn add_file_to_files_by_name<TFile: Borrow<Node>>(
        &self,
        file: Option<TFile /*SourceFile*/>,
        path: &Path,
        redirected_path: Option<&Path>,
    ) {
        let file = file.map(|file| file.borrow().node_wrapper());
        if let Some(redirected_path) = redirected_path {
            self.files_by_name().insert(
                redirected_path.to_string(),
                match file.as_ref() {
                    None => FilesByNameValue::Undefined,
                    Some(file) => FilesByNameValue::SourceFile(file.clone()),
                },
            );
            self.files_by_name().insert(
                path.to_string(),
                match file.as_ref() {
                    None => FilesByNameValue::False,
                    Some(file) => FilesByNameValue::SourceFile(file.clone()),
                },
            );
        } else {
            self.files_by_name().insert(
                path.to_string(),
                match file.as_ref() {
                    None => FilesByNameValue::Undefined,
                    Some(file) => FilesByNameValue::SourceFile(file.clone()),
                },
            );
        }
    }

    pub fn get_project_reference_redirect_(&self, file_name: &str) -> Option<String> {
        unimplemented!()
    }

    pub fn get_project_reference_redirect_project(
        &self,
        file_name: &str,
    ) -> Option<Rc<ResolvedProjectReference>> {
        unimplemented!()
    }

    pub fn get_project_reference_output_name(
        &self,
        referenced_project: &ResolvedProjectReference,
        file_name: &str,
    ) -> String {
        unimplemented!()
    }

    pub fn for_each_resolved_project_reference<
        TReturn,
        TCallback: FnMut(&ResolvedProjectReference) -> Option<TReturn>,
    >(
        &self,
        mut cb: TCallback,
    ) -> Option<TReturn> {
        for_each_resolved_project_reference(
            self.maybe_resolved_project_references().as_deref(),
            |resolved_project_reference, _parent| cb(resolved_project_reference),
        )
    }

    pub fn for_each_resolved_project_reference_rc(
        &self,
    ) -> Rc<dyn Fn(&mut dyn FnMut(&ResolvedProjectReference))> {
        let self_clone = self.rc_wrapper();
        Rc::new(move |cb| {
            for_each_resolved_project_reference(
                self_clone.maybe_resolved_project_references().as_deref(),
                |resolved_project_reference, _parent| -> Option<()> {
                    cb(resolved_project_reference);
                    None
                },
            );
        })
    }

    fn get_source_of_project_reference_redirect(
        &self,
        path: &Path,
    ) -> Option<SourceOfProjectReferenceRedirect> {
        if !is_declaration_file_name(path) {
            return None;
        }
        self.maybe_map_from_to_project_reference_redirect_source()
            .get_or_insert_with(|| {
                let mut map_from_to_project_reference_redirect_source = HashMap::new();
                self.for_each_resolved_project_reference(
                    |resolved_ref: &ResolvedProjectReference| -> Option<()> {
                        let out = out_file(&resolved_ref.command_line.options);
                        if let Some(out) = out {
                            let output_dts = change_extension(out, Extension::Dts.to_str());
                            map_from_to_project_reference_redirect_source.insert(
                                self.to_path(&output_dts),
                                SourceOfProjectReferenceRedirect::True,
                            );
                        } else {
                            let mut got_common_source_directory: Option<String> = None;
                            let mut get_common_source_directory = || {
                                if let Some(got_common_source_directory) =
                                    got_common_source_directory.as_ref()
                                {
                                    return got_common_source_directory.clone();
                                }
                                let got_common_source_directory =
                                    Some(get_common_source_directory_of_config(
                                        &resolved_ref.command_line,
                                        !CompilerHost::use_case_sensitive_file_names(&*self.host()),
                                    ));
                                got_common_source_directory.clone().unwrap()
                            };
                            for_each(
                                &resolved_ref.command_line.file_names,
                                |file_name: &String, _| -> Option<()> {
                                    if !file_extension_is(file_name, Extension::Dts.to_str())
                                        && !file_extension_is(file_name, Extension::Json.to_str())
                                    {
                                        let output_dts = get_output_declaration_file_name(
                                            file_name,
                                            &resolved_ref.command_line,
                                            !CompilerHost::use_case_sensitive_file_names(
                                                &*self.host(),
                                            ),
                                            Some(&mut get_common_source_directory),
                                        );
                                        map_from_to_project_reference_redirect_source.insert(
                                            self.to_path(&output_dts),
                                            file_name.clone().into(),
                                        );
                                    }
                                    None
                                },
                            );
                        }
                        None
                    },
                );
                map_from_to_project_reference_redirect_source
            })
            .get(path)
            .cloned()
    }

    pub fn process_referenced_files(&self, file: &Node /*SourceFile*/, is_default_lib: bool) {
        let file_as_source_file = file.as_source_file();
        maybe_for_each(
            file_as_source_file.maybe_referenced_files().as_ref(),
            |ref_: &FileReference, index| -> Option<()> {
                self.process_source_file(
                    &resolve_tripleslash_reference(
                        &ref_.file_name,
                        &file_as_source_file.file_name(),
                    ),
                    is_default_lib,
                    false,
                    None,
                    &FileIncludeReason::ReferencedFile(ReferencedFile {
                        kind: FileIncludeKind::ReferenceFile,
                        file: file_as_source_file.path().clone(),
                        index,
                    }),
                );
                None
            },
        );
    }

    pub fn process_type_reference_directives(&self, file: &Node /*SourceFile*/) {
        let file_as_source_file = file.as_source_file();
        let file_type_reference_directives = file_as_source_file.maybe_type_reference_directives();
        let type_directives = maybe_map(
            file_type_reference_directives.as_ref(),
            |ref_: &FileReference, _| to_file_name_lower_case(&ref_.file_name),
        );
        if type_directives.is_none() {
            return;
        }
        let type_directives = type_directives.unwrap();
        let file_type_reference_directives = file_type_reference_directives.as_ref().unwrap();

        let resolutions = self
            .resolve_type_reference_directive_names_worker(&type_directives, file.node_wrapper());
        for index in 0..type_directives.len() {
            let ref_ = &file_type_reference_directives[index];
            let resolved_type_reference_directive = &resolutions[index];
            let file_name = to_file_name_lower_case(&ref_.file_name);
            set_resolved_type_reference_directive(
                file,
                &file_name,
                resolved_type_reference_directive.clone(),
            );
            self.process_type_reference_directive(
                &file_name,
                resolved_type_reference_directive.clone(),
                &FileIncludeReason::ReferencedFile(ReferencedFile {
                    kind: FileIncludeKind::TypeReferenceDirective,
                    file: file_as_source_file.path().clone(),
                    index,
                }),
            );
        }
    }

    pub fn process_type_reference_directive(
        &self,
        type_reference_directive: &str,
        resolved_type_reference_directive: Option<Rc<ResolvedTypeReferenceDirective>>,
        reason: &FileIncludeReason,
    ) {
        // tracing?.push(tracing.Phase.Program, "processTypeReferenceDirective", { directive: typeReferenceDirective, hasResolved: !!resolveModuleNamesReusingOldState, refKind: reason.kind, refPath: isReferencedFile(reason) ? reason.file : undefined });
        self.process_type_reference_directive_worker(
            type_reference_directive,
            resolved_type_reference_directive,
            reason,
        )
        // tracing?.pop();
    }

    pub fn process_type_reference_directive_worker(
        &self,
        type_reference_directive: &str,
        resolved_type_reference_directive: Option<Rc<ResolvedTypeReferenceDirective>>,
        reason: &FileIncludeReason,
    ) {
        let previous_resolution = self
            .resolved_type_reference_directives()
            .get(type_reference_directive)
            .cloned()
            .flatten();
        if matches!(
            previous_resolution.as_ref(),
            Some(previous_resolution) if previous_resolution.primary
        ) {
            return;
        }
        let mut save_resolution = true;
        if let Some(resolved_type_reference_directive) =
            resolved_type_reference_directive.as_deref()
        {
            if resolved_type_reference_directive.is_external_library_import == Some(true) {
                self.set_current_node_modules_depth(self.current_node_modules_depth() + 1);
            }

            if resolved_type_reference_directive.primary {
                self.process_source_file(
                    resolved_type_reference_directive
                        .resolved_file_name
                        .as_ref()
                        .unwrap(),
                    false,
                    false,
                    resolved_type_reference_directive.package_id.as_ref(),
                    reason,
                );
            } else {
                if let Some(previous_resolution) = previous_resolution.as_ref() {
                    if resolved_type_reference_directive.resolved_file_name
                        != previous_resolution.resolved_file_name
                    {
                        let other_file_text = self.host().read_file(
                            resolved_type_reference_directive
                                .resolved_file_name
                                .as_ref()
                                .unwrap(),
                        );
                        let existing_file = self
                            .get_source_file_(
                                previous_resolution.resolved_file_name.as_ref().unwrap(),
                            )
                            .unwrap();
                        if !matches!(
                            other_file_text.as_ref(),
                            Ok(other_file_text) if other_file_text == &*existing_file.as_source_file().text()
                        ) {
                            self.add_file_preprocessing_file_explaining_diagnostic(
                                Some(&*existing_file),
                                reason,
                                &Diagnostics::Conflicting_definitions_for_0_found_at_1_and_2_Consider_installing_a_specific_version_of_this_library_to_resolve_the_conflict,
                                Some(vec![
                                    type_reference_directive.to_owned(),
                                    resolved_type_reference_directive.resolved_file_name.clone().unwrap(),
                                    previous_resolution.resolved_file_name.clone().unwrap(),
                                ])
                            );
                        }
                    }
                    save_resolution = false;
                } else {
                    self.process_source_file(
                        resolved_type_reference_directive
                            .resolved_file_name
                            .as_ref()
                            .unwrap(),
                        false,
                        false,
                        resolved_type_reference_directive.package_id.as_ref(),
                        reason,
                    );
                }
            }

            if resolved_type_reference_directive.is_external_library_import == Some(true) {
                self.set_current_node_modules_depth(self.current_node_modules_depth() - 1);
            }
        } else {
            self.add_file_preprocessing_file_explaining_diagnostic(
                Option::<&Node>::None,
                reason,
                &Diagnostics::Cannot_find_type_definition_file_for_0,
                Some(vec![type_reference_directive.to_owned()]),
            );
        }

        if save_resolution {
            self.resolved_type_reference_directives().insert(
                type_reference_directive.to_owned(),
                resolved_type_reference_directive,
            );
        }
    }

    pub fn path_for_lib_file(&self, lib_file_name: &str) -> String {
        let components = lib_file_name
            .split(".")
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>();
        let mut path = components[1].clone();
        let mut i = 2;
        while let Some(components_i) = components
            .get(i)
            .filter(|components_i| &***components_i != "d")
        {
            path.push_str(&format!(
                "{}{}",
                if i == 2 { "/" } else { "-" },
                components_i
            ));
            i += 1;
        }
        let resolve_from = combine_paths(
            &self.current_directory(),
            &[Some(&*format!(
                "__lib_node_modules_lookup_{}__.ts",
                lib_file_name
            ))],
        );
        let local_override_module_result = resolve_module_name(
            &format!("@typescript/lib-{}", path),
            &resolve_from,
            Rc::new(
                CompilerOptionsBuilder::default()
                    .module_resolution(Some(ModuleResolutionKind::NodeJs))
                    .build()
                    .unwrap(),
            ),
            self.host().as_dyn_module_resolution_host(),
            self.maybe_module_resolution_cache().clone(),
            None,
            None,
        );
        if let Some(local_override_module_result_resolved_module) =
            local_override_module_result.resolved_module.as_ref()
        {
            return local_override_module_result_resolved_module
                .resolved_file_name
                .clone();
        }
        combine_paths(&self.default_library_path(), &[Some(lib_file_name)])
    }

    pub fn process_lib_reference_directives(&self, file: &Node /*SourceFile*/) {
        let file_as_source_file = file.as_source_file();
        maybe_for_each(
            file_as_source_file
                .maybe_lib_reference_directives()
                .as_ref(),
            |lib_reference: &FileReference, index| -> Option<()> {
                let lib_name = to_file_name_lower_case(&lib_reference.file_name);
                let lib_file_name = lib_map.with(|lib_map_| lib_map_.get(&&*lib_name).copied());
                if let Some(lib_file_name) = lib_file_name {
                    self.process_root_file(
                        &self.path_for_lib_file(lib_file_name),
                        true,
                        true,
                        &FileIncludeReason::ReferencedFile(ReferencedFile {
                            kind: FileIncludeKind::LibReferenceDirective,
                            file: file_as_source_file.path().clone(),
                            index,
                        }),
                    );
                } else {
                    let unqualified_lib_name =
                        remove_suffix(remove_prefix(&lib_name, "lib."), ".d.ts");
                    let suggestion = libs.with(|libs_| {
                        get_spelling_suggestion(unqualified_lib_name, libs_, |lib| {
                            Some((*lib).to_owned())
                        })
                        .map(|suggestion| (*suggestion).to_owned())
                    });
                    let diagnostic = if suggestion.is_some() {
                        &*Diagnostics::Cannot_find_lib_definition_for_0_Did_you_mean_1
                    } else {
                        &*Diagnostics::Cannot_find_lib_definition_for_0
                    };
                    self.maybe_file_processing_diagnostics().get_or_insert_with(|| {
                        vec![]
                    }).push(
                        FilePreprocessingDiagnostics::FilePreprocessingReferencedDiagnostic(FilePreprocessingReferencedDiagnostic {
                            kind: FilePreprocessingDiagnosticsKind::FilePreprocessingReferencedDiagnostic,
                            reason: ReferencedFile {
                                kind: FileIncludeKind::LibReferenceDirective,
                                file: file_as_source_file.path().clone(),
                                index,
                            },
                            diagnostic,
                            args: if let Some(suggestion) = suggestion {
                                Some(vec![
                                    lib_name,
                                    suggestion,
                                ])
                            } else {
                                Some(vec![
                                    lib_name,
                                ])
                            }
                        })
                    );
                }
                None
            },
        );
    }

    pub fn get_canonical_file_name(&self, file_name: &str) -> String {
        self.host().get_canonical_file_name(file_name)
    }

    pub fn get_canonical_file_name_rc(&self) -> Rc<dyn Fn(&str) -> String> {
        let host = self.host();
        Rc::new(move |file_name| host.get_canonical_file_name(file_name))
    }

    pub fn process_imported_modules(&self, file: &Node /*SourceFile*/) {
        self.collect_external_module_references(file);
        let file_as_source_file = file.as_source_file();
        if !file_as_source_file
            .maybe_imports()
            .as_ref()
            .unwrap()
            .is_empty()
            || !file_as_source_file
                .maybe_module_augmentations()
                .as_ref()
                .unwrap()
                .is_empty()
        {
            let module_names = get_module_names(file);
            let resolutions = self.resolve_module_names_reusing_old_state(&module_names, file);
            Debug_.assert(resolutions.len() == module_names.len(), None);
            let options_for_file = if self.use_source_of_project_reference_redirect() {
                self.get_redirect_reference_for_resolution(file)
                    .map(|value| value.command_line.options.clone())
            } else {
                None
            }
            .unwrap_or_else(|| self.options.clone());
            for index in 0..module_names.len() {
                let resolution = &resolutions[index];
                set_resolved_module(
                    file,
                    &module_names[index],
                    resolution.clone(),
                    get_mode_for_resolution_at_index(file_as_source_file, index),
                );

                // if (!resolution) {
                //     continue;
                // }

                let is_from_node_modules_search = resolution.is_external_library_import;
                let is_js_file = !resolution_extension_is_ts_or_json(resolution.extension());
                let is_js_file_from_node_modules =
                    is_from_node_modules_search == Some(true) && is_js_file;
                let resolved_file_name = &resolution.resolved_file_name;

                if is_from_node_modules_search == Some(true) {
                    self.set_current_node_modules_depth(self.current_node_modules_depth() + 1);
                }

                let elide_import = is_js_file_from_node_modules
                    && self.current_node_modules_depth() > self.max_node_module_js_depth;
                let should_add_file = !resolved_file_name.is_empty()
                    && get_resolution_diagnostic(&options_for_file, resolution).is_none()
                    && options_for_file.no_resolve != Some(true)
                    && index < file_as_source_file.maybe_imports().as_ref().unwrap().len()
                    && !elide_import
                    && !(is_js_file && !get_allow_js_compiler_option(&options_for_file))
                    && (is_in_js_file(Some(
                        &*file_as_source_file.maybe_imports().as_ref().unwrap()[index],
                    )) || !file_as_source_file.maybe_imports().as_ref().unwrap()[index]
                        .flags()
                        .intersects(NodeFlags::JSDoc));

                if elide_import {
                    self.modules_with_elided_imports()
                        .insert(file_as_source_file.path().to_string(), true);
                } else if should_add_file {
                    self.find_source_file(
                        resolved_file_name,
                        false,
                        false,
                        &FileIncludeReason::ReferencedFile(ReferencedFile {
                            kind: FileIncludeKind::Import,
                            file: file_as_source_file.path().clone(),
                            index,
                        }),
                        resolution.package_id.as_ref(),
                    );
                }

                if is_from_node_modules_search == Some(true) {
                    self.set_current_node_modules_depth(self.current_node_modules_depth() - 1);
                }
            }
        } else {
            *file_as_source_file.maybe_resolved_modules() = None;
        }
    }

    pub fn parse_project_reference_config_file(
        &self,
        ref_: &ProjectReference,
    ) -> Option<Rc<ResolvedProjectReference>> {
        if self.maybe_project_reference_redirects().is_none() {
            *self.maybe_project_reference_redirects() = Some(HashMap::new());
        }

        let ref_path = resolve_project_reference_path(ref_);
        let source_file_path = self.to_path(&ref_path);
        let from_cache = self
            .maybe_project_reference_redirects()
            .as_ref()
            .unwrap()
            .get(&source_file_path)
            .cloned();
        if let Some(from_cache) = from_cache {
            return from_cache;
        }
        unimplemented!()
    }

    pub fn add_file_preprocessing_file_explaining_diagnostic<TFile: Borrow<Node>>(
        &self,
        file: Option<TFile>,
        file_processing_reason: &FileIncludeReason,
        diagnostic: &'static DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.maybe_file_processing_diagnostics().get_or_insert_with(|| vec![]).push(FilePreprocessingDiagnostics::FilePreprocessingFileExplainingDiagnostic(FilePreprocessingFileExplainingDiagnostic {
            kind: FilePreprocessingDiagnosticsKind::FilePreprocessingFileExplainingDiagnostic,
            file: file.map(|file| file.borrow().as_source_file().path().clone()),
            file_processing_reason: file_processing_reason.clone(),
            diagnostic,
            args,
        }))
    }

    pub fn create_option_diagnostic_in_object_literal_syntax(
        &self,
        object_literal: &Node, /*ObjectLiteralExpression*/
        on_key: bool,
        key1: &str,
        key2: Option<&str>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> bool {
        let props = get_property_assignment(object_literal, key1, key2);
        for prop in &props {
            self.program_diagnostics().add(Rc::new(
                create_diagnostic_for_node_in_source_file(
                    self.options.config_file.as_ref().unwrap(),
                    &*if on_key {
                        prop.as_property_assignment().name()
                    } else {
                        prop.as_property_assignment().initializer.clone()
                    },
                    message,
                    args.clone(),
                )
                .into(),
            ));
        }
        !props.is_empty()
    }

    pub fn block_emitting_of_file(&self, emit_file_name: &str, diag: Rc<Diagnostic>) {
        self.has_emit_blocking_diagnostics()
            .insert(self.to_path(emit_file_name), true);
        self.program_diagnostics().add(diag);
    }

    pub fn is_emitted_file(&self, file: &str) -> bool {
        if self.options.no_emit == Some(true) {
            return false;
        }

        let file_path = self.to_path(file);
        if self.get_source_file_by_path(&file_path).is_some() {
            return false;
        }

        let out = out_file(&self.options);
        if let Some(out) = out {
            return self.is_same_file(&*file_path, out)
                || self.is_same_file(
                    &*file_path,
                    &format!("{}{}", remove_file_extension(out), Extension::Dts.to_str()),
                );
        }

        if matches!(
            self.options.declaration_dir.as_ref(),
            Some(options_declaration_dir) if contains_path(
                options_declaration_dir,
                &*file_path,
                Some(self.current_directory().clone()),
                Some(!CompilerHost::use_case_sensitive_file_names(&*self.host()))
            )
        ) {
            return true;
        }

        if let Some(options_out_dir) = self.options.out_dir.as_ref() {
            return contains_path(
                options_out_dir,
                &*file_path,
                Some(self.current_directory().clone()),
                Some(CompilerHost::use_case_sensitive_file_names(&*self.host())),
            );
        }

        if file_extension_is_one_of(&*file_path, &supported_js_extensions_flat)
            || file_extension_is(&*file_path, Extension::Dts.to_str())
        {
            let file_path_without_extension = remove_file_extension(&*file_path);
            return self
                .get_source_file_by_path(&Path::new(format!(
                    "{}{}",
                    file_path_without_extension,
                    Extension::Ts.to_str()
                )))
                .is_some()
                || self
                    .get_source_file_by_path(&Path::new(format!(
                        "{}{}",
                        file_path_without_extension,
                        Extension::Tsx.to_str()
                    )))
                    .is_some();
        }
        false
    }

    pub fn is_same_file(&self, file1: &str, file2: &str) -> bool {
        compare_paths(
            file1,
            file2,
            Some(self.current_directory().clone()),
            Some(!CompilerHost::use_case_sensitive_file_names(&*self.host())),
        ) == Comparison::EqualTo
    }

    pub fn get_symlink_cache(&self) -> Rc<SymlinkCache> {
        let host_symlink_cache = self.host().get_symlink_cache();
        if let Some(host_symlink_cache) = host_symlink_cache {
            return host_symlink_cache;
        }
        if self.symlinks().is_none() {
            let host = self.host();
            *self.symlinks() = Some(Rc::new(create_symlink_cache(
                &self.current_directory(),
                Rc::new(move |file_name: &str| host.get_canonical_file_name(file_name)),
            )));
        }
        let symlinks = self.symlinks().clone().unwrap();
        if
        /*files && resolvedTypeReferenceDirectives &&*/
        !symlinks.has_processed_resolutions() {
            symlinks.set_symlinks_from_resolutions(
                &self.files(),
                Some(&self.resolved_type_reference_directives()),
            );
        }
        symlinks
    }

    pub fn get_symlink_cache_rc(&self) -> Rc<dyn Fn() -> Rc<SymlinkCache>> {
        let self_clone = self.rc_wrapper();
        Rc::new(move || self_clone.get_symlink_cache())
    }
}

#[derive(Clone)]
pub enum FilesByNameValue {
    SourceFile(Rc<Node /*SourceFile*/>),
    False,
    Undefined,
}

pub trait ActualResolveModuleNamesWorker {
    fn call(
        &self,
        module_names: &[String],
        containing_file: &Node, /*SourceFile*/
        containing_file_name: &str,
        reused_names: Option<&[String]>,
        redirected_reference: Option<&ResolvedProjectReference>,
    ) -> Vec<Option<Rc<ResolvedModuleFull>>>;
}

struct ActualResolveModuleNamesWorkerHost {
    host: Rc<dyn CompilerHost>,
    options: Rc<CompilerOptions>,
}

impl ActualResolveModuleNamesWorkerHost {
    pub fn new(host: Rc<dyn CompilerHost>, options: Rc<CompilerOptions>) -> Self {
        Self { host, options }
    }
}

impl ActualResolveModuleNamesWorker for ActualResolveModuleNamesWorkerHost {
    fn call(
        &self,
        module_names: &[String],
        containing_file: &Node, /*SourceFile*/
        containing_file_name: &str,
        reused_names: Option<&[String]>,
        redirected_reference: Option<&ResolvedProjectReference>,
    ) -> Vec<Option<Rc<ResolvedModuleFull>>> {
        self.host
            .resolve_module_names(
                /*Debug.checkEachDefined(*/ module_names, /*)*/
                containing_file_name,
                reused_names,
                redirected_reference,
                &self.options,
                Some(containing_file),
            )
            .unwrap()
            .into_iter()
            .map(|resolved| {
                if match resolved.as_ref() {
                    None => true,
                    Some(resolved) => resolved.extension.is_some(),
                } {
                    return resolved.map(Rc::new);
                }
                let resolved = resolved.unwrap();
                let mut with_extension = clone(&resolved);
                with_extension.extension = Some(extension_from_path(&resolved.resolved_file_name));
                Some(Rc::new(with_extension))
            })
            .collect()
    }
}

struct ActualResolveModuleNamesWorkerLoadWithModeAwareCache {
    loader: Rc<dyn LoadWithModeAwareCacheLoader<Rc<ResolvedModuleFull>>>,
}

impl ActualResolveModuleNamesWorkerLoadWithModeAwareCache {
    pub fn new(loader: Rc<dyn LoadWithModeAwareCacheLoader<Rc<ResolvedModuleFull>>>) -> Self {
        Self { loader }
    }
}

impl ActualResolveModuleNamesWorker for ActualResolveModuleNamesWorkerLoadWithModeAwareCache {
    fn call(
        &self,
        module_names: &[String],
        containing_file: &Node, /*SourceFile*/
        containing_file_name: &str,
        reused_names: Option<&[String]>,
        redirected_reference: Option<&ResolvedProjectReference>,
    ) -> Vec<Option<Rc<ResolvedModuleFull>>> {
        load_with_mode_aware_cache(
            /*Debug.checkEachDefined(*/ module_names, /*)*/
            containing_file,
            containing_file_name,
            redirected_reference,
            &*self.loader,
        )
        .into_iter()
        .map(Some)
        .collect()
    }
}

pub trait ActualResolveTypeReferenceDirectiveNamesWorker {
    fn call(
        &self,
        type_directive_names: &[String],
        containing_file: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Vec<Option<Rc<ResolvedTypeReferenceDirective>>>;
}

struct ActualResolveTypeReferenceDirectiveNamesWorkerHost {
    host: Rc<dyn CompilerHost>,
    options: Rc<CompilerOptions>,
}

impl ActualResolveTypeReferenceDirectiveNamesWorkerHost {
    pub fn new(host: Rc<dyn CompilerHost>, options: Rc<CompilerOptions>) -> Self {
        Self { host, options }
    }
}

impl ActualResolveTypeReferenceDirectiveNamesWorker
    for ActualResolveTypeReferenceDirectiveNamesWorkerHost
{
    fn call(
        &self,
        type_directive_names: &[String],
        containing_file: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Vec<Option<Rc<ResolvedTypeReferenceDirective>>> {
        self.host
            .resolve_type_reference_directives(
                /*Debug.checkEachDefined(*/ type_directive_names, /*)*/
                containing_file,
                redirected_reference.as_deref(),
                &self.options,
            )
            .unwrap()
    }
}

struct ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache {
    loader: Rc<dyn LoadWithLocalCacheLoader<Rc<ResolvedTypeReferenceDirective>>>,
}

impl ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache {
    pub fn new(
        loader: Rc<dyn LoadWithLocalCacheLoader<Rc<ResolvedTypeReferenceDirective>>>,
    ) -> Self {
        Self { loader }
    }
}

impl ActualResolveTypeReferenceDirectiveNamesWorker
    for ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache
{
    fn call(
        &self,
        type_reference_directive_names: &[String],
        containing_file: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Vec<Option<Rc<ResolvedTypeReferenceDirective>>> {
        load_with_local_cache(
            /*Debug.checkEachDefined(*/ type_reference_directive_names, /*)*/
            containing_file,
            redirected_reference,
            &*self.loader,
        )
        .into_iter()
        .map(Some)
        .collect()
    }
}

impl ScriptReferenceHost for Program {
    fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        self.options.clone()
    }

    fn get_source_file(&self, file_name: &str) -> Option<Rc<Node /*SourceFile*/>> {
        self.get_source_file_(file_name)
    }

    fn get_source_file_by_path(&self, path: &Path) -> Option<Rc<Node /*SourceFile*/>> {
        unimplemented!()
    }

    fn get_current_directory(&self) -> String {
        unimplemented!()
    }
}

impl ModuleSpecifierResolutionHost for Program {
    fn file_exists(&self, path: &str) -> bool {
        self.file_exists_rc().file_exists(path)
    }

    fn get_current_directory(&self) -> String {
        unimplemented!()
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        self.maybe_directory_exists_rc()
            .and_then(|directory_exists_rc| directory_exists_rc.directory_exists(path))
    }

    fn read_file(&self, path: &str) -> Option<io::Result<String>> {
        Some(self.host().read_file(path))
    }

    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>> {
        unimplemented!()
    }

    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String> {
        self.get_project_reference_redirect_(file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        unimplemented!()
    }

    fn get_file_include_reasons(&self) -> Rc<MultiMap<Path, FileIncludeReason>> {
        unimplemented!()
    }
}

impl TypeCheckerHost for Program {
    fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        self.options.clone()
    }

    fn get_source_files(&self) -> Ref<Vec<Rc<Node>>> {
        self.files()
    }

    fn get_source_file(&self, file_name: &str) -> Option<Rc<Node /*SourceFile*/>> {
        self.get_source_file_(file_name)
    }

    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String> {
        self.get_project_reference_redirect_(file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        unimplemented!()
    }

    fn get_common_source_directory(&self) -> Option<String> {
        unimplemented!()
    }
}

impl TypeCheckerHostDebuggable for Program {}

pub fn create_program(root_names_or_options: CreateProgramOptions) -> Rc<Program> {
    let create_program_options = root_names_or_options;
    let program = Program::new(create_program_options);
    program.create();
    program
}

struct HostForUseSourceOfProjectReferenceRedirect {
    compiler_host: Rc<dyn CompilerHost>,
    get_symlink_cache: Rc<dyn Fn() -> Rc<SymlinkCache>>,
    use_source_of_project_reference_redirect: bool,
    to_path: Rc<dyn Fn(&str) -> Path>,
    get_resolved_project_references:
        Rc<dyn Fn() -> Option<Vec<Option<Rc<ResolvedProjectReference>>>>>,
    for_each_resolved_project_reference: Rc<dyn Fn(&mut dyn FnMut(&ResolvedProjectReference))>,
}

fn update_host_for_use_source_of_project_reference_redirect(
    host: HostForUseSourceOfProjectReferenceRedirect,
) -> UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
    let overrider: Rc<dyn ModuleResolutionHostOverrider> = Rc::new(
        UpdateHostForUseSourceOfProjectReferenceRedirectOverrider::new(
            host.compiler_host.clone(),
            host.get_symlink_cache.clone(),
            host.to_path.clone(),
            host.get_resolved_project_references.clone(),
            host.for_each_resolved_project_reference.clone(),
        ),
    );

    if !host.use_source_of_project_reference_redirect {
        return UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
            on_program_create_complete: Rc::new(|| {}),
            directory_exists: None,
            file_exists: overrider,
        };
    }

    host.compiler_host
        .set_overriding_file_exists(Some(overrider.clone()));

    host.compiler_host
        .set_overriding_directory_exists(Some(overrider.clone()));

    host.compiler_host
        .set_overriding_get_directories(Some(overrider.clone()));

    host.compiler_host
        .set_overriding_realpath(Some(overrider.clone()));

    UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
        on_program_create_complete: {
            let host_compiler_host_clone = host.compiler_host.clone();
            Rc::new(move || {
                host_compiler_host_clone.set_overriding_file_exists(None);
                host_compiler_host_clone.set_overriding_directory_exists(None);
                host_compiler_host_clone.set_overriding_get_directories(None);
            })
        },
        directory_exists: Some(overrider.clone()),
        file_exists: overrider,
    }
}

struct UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
    pub on_program_create_complete: Rc<dyn Fn()>,
    pub directory_exists: Option<Rc<dyn ModuleResolutionHostOverrider>>,
    pub file_exists: Rc<dyn ModuleResolutionHostOverrider>,
}

struct UpdateHostForUseSourceOfProjectReferenceRedirectOverrider {
    pub host_compiler_host: Rc<dyn CompilerHost>,
    pub host_get_symlink_cache: Rc<dyn Fn() -> Rc<SymlinkCache>>,
    pub host_to_path: Rc<dyn Fn(&str) -> Path>,
    pub host_get_resolved_project_references:
        Rc<dyn Fn() -> Option<Vec<Option<Rc<ResolvedProjectReference>>>>>,
    pub host_for_each_resolved_project_reference:
        Rc<dyn Fn(&mut dyn FnMut(&ResolvedProjectReference))>,
    set_of_declaration_directories: RefCell<Option<HashSet<Path>>>,
}

impl UpdateHostForUseSourceOfProjectReferenceRedirectOverrider {
    pub fn new(
        host_compiler_host: Rc<dyn CompilerHost>,
        host_get_symlink_cache: Rc<dyn Fn() -> Rc<SymlinkCache>>,
        host_to_path: Rc<dyn Fn(&str) -> Path>,
        host_get_resolved_project_references: Rc<
            dyn Fn() -> Option<Vec<Option<Rc<ResolvedProjectReference>>>>,
        >,
        host_for_each_resolved_project_reference: Rc<
            dyn Fn(&mut dyn FnMut(&ResolvedProjectReference)),
        >,
    ) -> Self {
        Self {
            host_compiler_host,
            host_get_symlink_cache,
            host_to_path,
            host_get_resolved_project_references,
            host_for_each_resolved_project_reference,
            set_of_declaration_directories: RefCell::new(None),
        }
    }

    fn handle_directory_could_be_symlink(&self, directory: &str) {
        unimplemented!()
    }

    fn file_or_directory_exists_using_source(
        &self,
        file_or_directory: &str,
        is_file: bool,
    ) -> bool {
        unimplemented!()
    }
}

impl ModuleResolutionHostOverrider for UpdateHostForUseSourceOfProjectReferenceRedirectOverrider {
    fn file_exists(&self, file: &str) -> bool {
        if self.host_compiler_host.file_exists_non_overridden(file) {
            return true;
        }
        if (self.host_get_resolved_project_references)().is_none() {
            return false;
        }
        if !is_declaration_file_name(file) {
            return false;
        }

        self.file_or_directory_exists_using_source(file, true)
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        if !self.host_compiler_host.is_directory_exists_supported() {
            return None;
        }
        if self
            .host_compiler_host
            .directory_exists_non_overridden(path)
            == Some(true)
        {
            self.handle_directory_could_be_symlink(path);
            return Some(true);
        }

        if (self.host_get_resolved_project_references)().is_none() {
            return Some(false);
        }

        if self.set_of_declaration_directories.borrow().is_none() {
            let mut set_of_declaration_directories =
                self.set_of_declaration_directories.borrow_mut();
            *set_of_declaration_directories = Some(HashSet::new());
            let set_of_declaration_directories = set_of_declaration_directories.as_mut().unwrap();
            (self.host_for_each_resolved_project_reference)(&mut |ref_| {
                let out = out_file(&ref_.command_line.options);
                if let Some(out) = out {
                    set_of_declaration_directories
                        .insert(get_directory_path(&(self.host_to_path)(out)).into());
                } else {
                    let declaration_dir = ref_
                        .command_line
                        .options
                        .declaration_dir
                        .as_ref()
                        .or_else(|| ref_.command_line.options.out_dir.as_ref());
                    if let Some(declaration_dir) = declaration_dir {
                        set_of_declaration_directories.insert((self.host_to_path)(declaration_dir));
                    }
                }
            });
        }

        Some(self.file_or_directory_exists_using_source(path, false))
    }

    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        if !self.host_compiler_host.is_get_directories_supported() {
            return None;
        }
        if (self.host_get_resolved_project_references)().is_none()
            || self
                .host_compiler_host
                .directory_exists_non_overridden(path)
                == Some(true)
        {
            self.host_compiler_host.get_directories_non_overridden(path)
        } else {
            Some(vec![])
        }
    }

    fn realpath(&self, s: &str) -> Option<String> {
        if !self.host_compiler_host.is_realpath_supported() {
            return None;
        }
        (self.host_get_symlink_cache)()
            .get_symlinked_files()
            .as_ref()
            .and_then(|symlinked_files| symlinked_files.get(&(self.host_to_path)(s)).cloned())
            .or_else(|| self.host_compiler_host.realpath_non_overridden(s))
    }
}

fn filter_semantic_diagnostics(diagnostic: Vec<Rc<Diagnostic>>) -> Vec<Rc<Diagnostic>> {
    diagnostic
}

pub trait CompilerHostLike {
    fn use_case_sensitive_file_names(&self) -> bool;
    fn get_current_directory(&self) -> String;
    fn file_exists(&self, file_name: &str) -> bool;
    fn read_file(&self, file_name: &str) -> io::Result<String>;
    fn read_directory(
        &self,
        root_dir: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> Option<Vec<String>> {
        None
    }
    fn trace(&self, s: &str) {}
    fn is_trace_supported(&self) -> bool;
    fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Rc<Diagnostic>) {}

    // These exist to allow "forwarding" CompilerHost -> CompilerHostLike -> DirectoryStructureHost
    fn is_read_directory_implemented(&self) -> bool;
    fn realpath(&self, path: &str) -> Option<String>;
    fn create_directory(&self, path: &str);
    fn write_file(&self, path: &str, data: &str, write_byte_order_mark: Option<bool>);
    fn directory_exists(&self, path: &str) -> Option<bool>;
    fn get_directories(&self, path: &str) -> Option<Vec<String>>;
}

pub struct CompilerHostLikeRcDynCompilerHost {
    host: Rc<dyn CompilerHost>,
}

impl CompilerHostLikeRcDynCompilerHost {
    pub fn new(host: Rc<dyn CompilerHost>) -> Self {
        Self { host }
    }
}

impl CompilerHostLike for CompilerHostLikeRcDynCompilerHost {
    fn use_case_sensitive_file_names(&self) -> bool {
        CompilerHost::use_case_sensitive_file_names(&*self.host)
    }

    fn get_current_directory(&self) -> String {
        CompilerHost::get_current_directory(&*self.host)
    }

    fn file_exists(&self, file_name: &str) -> bool {
        self.host.file_exists(file_name)
    }

    fn read_file(&self, file_name: &str) -> io::Result<String> {
        self.host.read_file(file_name)
    }

    fn read_directory(
        &self,
        root_dir: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> Option<Vec<String>> {
        self.host
            .read_directory(root_dir, extensions, excludes, includes, depth)
    }

    fn trace(&self, s: &str) {
        self.host.trace(s)
    }

    fn is_trace_supported(&self) -> bool {
        self.host.is_trace_supported()
    }

    // fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Rc<Diagnostic>) {}
    fn is_read_directory_implemented(&self) -> bool {
        self.host.is_read_directory_implemented()
    }

    fn realpath(&self, path: &str) -> Option<String> {
        self.host.realpath(path)
    }

    fn create_directory(&self, path: &str) {
        self.host.create_directory(path)
    }

    fn write_file(&self, path: &str, data: &str, write_byte_order_mark: Option<bool>) {
        self.host
            .write_file(path, data, write_byte_order_mark.unwrap(), None, None)
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        self.host.directory_exists(path)
    }

    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        self.host.get_directories(path)
    }
}

pub struct DirectoryStructureHostRcDynCompilerHostLike {
    host: Rc<dyn CompilerHostLike>,
}

impl DirectoryStructureHostRcDynCompilerHostLike {
    pub fn new(host: Rc<dyn CompilerHostLike>) -> Self {
        Self { host }
    }
}

impl DirectoryStructureHost for DirectoryStructureHostRcDynCompilerHostLike {
    fn file_exists(&self, path: &str) -> bool {
        self.host.file_exists(path)
    }

    fn read_file(&self, path: &str, encoding: Option<&str>) -> io::Result<String> {
        self.host.read_file(path)
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        self.host.directory_exists(path)
    }
    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        self.host.get_directories(path)
    }
    fn read_directory(
        &self,
        path: &str,
        extensions: &[&str],
        exclude: Option<&[String]>,
        include: Option<&[String]>,
        depth: Option<usize>,
    ) -> Option<Vec<String>> {
        self.host
            .read_directory(path, extensions, exclude, include.unwrap(), depth)
    }
    fn is_read_directory_implemented(&self) -> bool {
        self.host.is_read_directory_implemented()
    }

    fn realpath(&self, path: &str) -> Option<String> {
        self.host.realpath(path)
    }

    fn create_directory(&self, path: &str) {
        self.host.create_directory(path)
    }

    fn write_file(&self, path: &str, data: &str, write_byte_order_mark: Option<bool>) {
        self.host.write_file(path, data, write_byte_order_mark)
    }
}

pub(crate) fn parse_config_host_from_compiler_host_like(
    host: Rc<dyn CompilerHostLike>,
    directory_structure_host: Option<Rc<dyn DirectoryStructureHost>>,
) -> ParseConfigHostFromCompilerHostLike {
    let directory_structure_host = directory_structure_host.unwrap_or_else(|| {
        Rc::new(DirectoryStructureHostRcDynCompilerHostLike::new(
            host.clone(),
        ))
    });
    ParseConfigHostFromCompilerHostLike::new(host, directory_structure_host)
}

pub struct ParseConfigHostFromCompilerHostLike {
    host: Rc<dyn CompilerHostLike>,
    directory_structure_host: Rc<dyn DirectoryStructureHost>,
}

impl ParseConfigHostFromCompilerHostLike {
    pub fn new(
        host: Rc<dyn CompilerHostLike>,
        directory_structure_host: Rc<dyn DirectoryStructureHost>,
    ) -> Self {
        Self {
            host,
            directory_structure_host,
        }
    }
}

impl ParseConfigFileHost for ParseConfigHostFromCompilerHostLike {
    fn get_current_directory(&self) -> String {
        self.host.get_current_directory()
    }
}

impl ParseConfigHost for ParseConfigHostFromCompilerHostLike {
    fn use_case_sensitive_file_names(&self) -> bool {
        self.host.use_case_sensitive_file_names()
    }

    fn read_directory(
        &self,
        root: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> Vec<String> {
        Debug_.assert(self.directory_structure_host.is_read_directory_implemented(), Some("'CompilerHost.readDirectory' must be implemented to correctly process 'projectReferences'"));
        self.directory_structure_host
            .read_directory(root, extensions, excludes, Some(includes), depth)
            .unwrap()
    }

    fn file_exists(&self, path: &str) -> bool {
        self.directory_structure_host.file_exists(path)
    }

    fn read_file(&self, path: &str) -> io::Result<String> {
        self.directory_structure_host.read_file(path, None)
    }

    fn trace(&self, s: &str) {
        self.host.trace(s)
    }

    fn is_trace_supported(&self) -> bool {
        self.host.is_trace_supported()
    }

    fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
        self
    }
}

impl ConfigFileDiagnosticsReporter for ParseConfigHostFromCompilerHostLike {
    fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Rc<Diagnostic>) {
        self.host
            .on_un_recoverable_config_file_diagnostic(diagnostic)
    }
}

pub fn resolve_project_reference_path(ref_: &ProjectReference) -> ResolvedConfigFileName {
    let passed_in_ref = ref_;
    resolve_config_file_project_name(&passed_in_ref.path)
}

pub fn get_resolution_diagnostic(
    options: &CompilerOptions,
    resolved_module: &ResolvedModuleFull,
) -> Option<&'static DiagnosticMessage> {
    let extension = resolved_module.extension();
    match extension {
        Extension::Ts | Extension::Dts => None,
        Extension::Tsx => need_jsx(options),
        Extension::Jsx => need_jsx(options).or_else(|| need_allow_js(options)),
        Extension::Js => need_allow_js(options),
        Extension::Json => need_resolve_json_module(options),
        _ => None,
    }
}

fn need_jsx(options: &CompilerOptions) -> Option<&'static DiagnosticMessage> {
    if options.jsx.is_some() {
        None
    } else {
        Some(&Diagnostics::Module_0_was_resolved_to_1_but_jsx_is_not_set)
    }
}

fn need_allow_js(options: &CompilerOptions) -> Option<&'static DiagnosticMessage> {
    if get_allow_js_compiler_option(options) || !get_strict_option_value(options, "noImplicitAny") {
        None
    } else {
        Some(&Diagnostics::Could_not_find_a_declaration_file_for_module_0_1_implicitly_has_an_any_type)
    }
}

fn need_resolve_json_module(options: &CompilerOptions) -> Option<&'static DiagnosticMessage> {
    if matches!(options.resolve_json_module, Some(true)) {
        None
    } else {
        Some(&Diagnostics::Module_0_was_resolved_to_1_but_resolveJsonModule_is_not_used)
    }
}

fn get_module_names(file: &Node /*SourceFile*/) -> Vec<String> {
    let file_as_source_file = file.as_source_file();
    let imports = file_as_source_file.maybe_imports();
    let imports = imports.as_ref().unwrap();
    let module_augmentations = file_as_source_file.maybe_module_augmentations();
    let module_augmentations = module_augmentations.as_ref().unwrap();
    unimplemented!()
}
