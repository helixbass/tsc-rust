use std::cell::{Cell, Ref, RefCell, RefMut};
use std::cmp;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io;
use std::rc::Rc;
use std::time;
use std::time::SystemTime;

use crate::{
    combine_paths, compare_paths, concatenate, contains_path, convert_to_relative_path,
    create_diagnostic_collection, create_diagnostic_for_node_in_source_file,
    create_get_canonical_file_name, create_multi_map, create_source_file, create_symlink_cache,
    create_type_checker, diagnostic_category_name, file_extension_is, file_extension_is_one_of,
    for_each, for_each_ancestor_directory_str, generate_djb2_hash, get_allow_js_compiler_option,
    get_default_lib_file_name, get_directory_path, get_emit_script_target,
    get_line_and_character_of_position, get_new_line_character, get_normalized_path_components,
    get_path_from_path_components, get_property_assignment, get_strict_option_value, get_sys,
    is_rooted_disk_path, is_watch_set, missing_file_modified_time, normalize_path, out_file,
    remove_file_extension, supported_js_extensions_flat, to_path as to_path_helper,
    write_file_ensuring_directories, CancellationTokenDebuggable, Comparison, CompilerHost,
    CompilerOptions, CreateProgramOptions, CustomTransformers, Diagnostic, DiagnosticCollection,
    DiagnosticMessage, DiagnosticMessageText, DiagnosticRelatedInformationInterface, Diagnostics,
    EmitResult, Extension, FileIncludeReason, LineAndCharacter, ModuleKind, ModuleResolutionHost,
    ModuleSpecifierResolutionHost, MultiMap, NamedDeclarationInterface, Node, PackageId,
    ParsedCommandLine, Path, Program, ReferencedFile, ResolvedModuleFull, ResolvedProjectReference,
    ResolvedTypeReferenceDirective, ScriptReferenceHost, ScriptTarget, SortedArray, SourceFile,
    StructureIsReused, SymlinkCache, System, TypeChecker, TypeCheckerHost,
    TypeCheckerHostDebuggable, WriteFileCallback,
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
}

impl CompilerHostConcrete {
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

    fn realpath(&self, path: &str) -> Option<String> {
        self.system.realpath(path)
    }

    fn file_exists(&self, file_name: &str) -> bool {
        self.system.file_exists(file_name)
    }

    fn trace(&self, s: &str) {
        self.system.write(&format!("{}{}", s, self.new_line));
    }

    fn directory_exists(&self, directory_name: &str) -> Option<bool> {
        Some(self.system.directory_exists(directory_name))
    }

    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        Some(self.system.get_directories(path))
    }
}

impl CompilerHost for CompilerHostConcrete {
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
    unimplemented!()
}

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
            file_preprocessing_diagnostics: RefCell::new(None),

            max_node_module_js_depth,
            current_node_modules_depth: Cell::new(0),

            modules_with_elided_imports: RefCell::new(HashMap::new()),

            source_files_found_searching_node_modules: RefCell::new(HashMap::new()),

            current_directory: RefCell::new(None),
            host: RefCell::new(None),
            program_diagnostics: RefCell::new(create_diagnostic_collection()),
            has_emit_blocking_diagnostics: RefCell::new(HashMap::new()),
        });
        rc.set_rc_wrapper(Some(rc.clone()));
        rc
    }

    pub fn create(&self) {
        let CreateProgramOptions {
            root_names,
            config_file_parsing_diagnostics,
            project_references,
            mut old_program,
            host,
            ..
        } = self.create_program_options.borrow_mut().take().unwrap();

        // tracing?.push(tracing.Phase.Program, "createProgram", { configFilePath: options.configFilePath, rootDir: options.rootDir }, /*separateBeginAndEnd*/ true);
        // performance.mark("beforeProgram");

        *self.host.borrow_mut() =
            Some(host.unwrap_or_else(|| Rc::new(create_compiler_host(self.options.clone(), None))));

        *self.current_directory.borrow_mut() =
            Some(CompilerHost::get_current_directory(&*self.host()));

        // if host.is_resolve_module_names_supported() {
        //     actual_resolve_module_names_worker
        // } else {
        // }

        let structure_is_reused: StructureIsReused;
        // tracing?.push(tracing.Phase.Program, "tryReuseStructureFromOldProgram", {});
        structure_is_reused = StructureIsReused::Not;
        if structure_is_reused != StructureIsReused::Completely {
            *self.processing_other_files.borrow_mut() = Some(vec![]);
            for_each(root_names, |name, _index| {
                self.process_root_file(&name);
                Option::<()>::None
            });

            *self.files.borrow_mut() = Some(self.processing_other_files.borrow().clone().unwrap());
            println!("files: {:#?}", &*self.files());
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

    pub(super) fn current_directory(&self) -> Ref<String> {
        Ref::map(self.current_directory.borrow(), |current_directory| {
            current_directory.as_ref().unwrap()
        })
    }

    pub(super) fn host(&self) -> Rc<dyn CompilerHost> {
        self.host.borrow().clone().unwrap()
    }

    pub(super) fn symlinks(&self) -> RefMut<Option<Rc<SymlinkCache>>> {
        self.symlinks.borrow_mut()
    }

    pub(super) fn resolved_type_reference_directives(
        &self,
    ) -> RefMut<HashMap<String, Option<Rc<ResolvedTypeReferenceDirective>>>> {
        self.resolved_type_reference_directives.borrow_mut()
    }

    pub(super) fn program_diagnostics(&self) -> RefMut<DiagnosticCollection> {
        self.program_diagnostics.borrow_mut()
    }

    pub(super) fn has_emit_blocking_diagnostics(&self) -> RefMut<HashMap<Path, bool>> {
        self.has_emit_blocking_diagnostics.borrow_mut()
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

    pub fn get_resolved_project_references(&self) -> Option<&[Option<ResolvedProjectReference>]> {
        unimplemented!()
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

    pub fn process_root_file(&self, file_name: &str) {
        self.process_source_file(&normalize_path(file_name));
    }

    fn get_source_file_from_reference_worker<TGetSourceFile: FnMut(&str) -> Option<Rc<Node>>>(
        &self,
        file_name: &str,
        mut get_source_file: TGetSourceFile,
    ) -> Option<Rc<Node>> {
        get_source_file(file_name)
    }

    pub fn process_source_file(&self, file_name: &str) {
        self.get_source_file_from_reference_worker(file_name, |file_name| {
            self.find_source_file(file_name)
        });
    }

    pub fn find_source_file(&self, file_name: &str) -> Option<Rc<Node>> {
        self.find_source_file_worker(file_name)
    }

    pub fn find_source_file_worker(&self, file_name: &str) -> Option<Rc<Node>> {
        let _path = self.to_path(file_name);

        let file = self.host().get_source_file(
            file_name,
            get_emit_script_target(&self.options),
            // TODO: this is wrong
            None,
            None,
        );

        file.map(|file| {
            let file_as_source_file = file.as_source_file();
            file_as_source_file.set_file_name(file_name.to_string());
            file_as_source_file.set_path(_path);
            self.processing_other_files
                .borrow_mut()
                .as_mut()
                .unwrap()
                .push(file.clone());
            file
        })
    }

    pub fn get_canonical_file_name(&self, file_name: &str) -> String {
        self.host().get_canonical_file_name(file_name)
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
}

impl ScriptReferenceHost for Program {
    fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        self.options.clone()
    }

    fn get_source_file(&self, file_name: &str) -> Option<Rc<Node /*SourceFile*/>> {
        unimplemented!()
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
        unimplemented!()
    }

    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String> {
        unimplemented!()
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
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

fn filter_semantic_diagnostics(diagnostic: Vec<Rc<Diagnostic>>) -> Vec<Rc<Diagnostic>> {
    diagnostic
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
