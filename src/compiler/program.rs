use std::cell::RefCell;
use std::cmp;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::io;
use std::rc::Rc;
use std::time;
use std::time::SystemTime;

use crate::{
    FilePreprocessingDiagnostics, ResolvedTypeReferenceDirective, __String, combine_paths,
    concatenate, convert_to_relative_path, create_get_canonical_file_name, create_multi_map,
    create_source_file, create_type_checker, diagnostic_category_name, for_each,
    for_each_ancestor_directory_str, generate_djb2_hash, get_default_lib_file_name,
    get_directory_path, get_emit_script_target, get_line_and_character_of_position,
    get_new_line_character, get_normalized_path_components, get_path_from_path_components, get_sys,
    is_rooted_disk_path, is_watch_set, missing_file_modified_time, normalize_path,
    to_path as to_path_helper, write_file_ensuring_directories, CancellationToken, CompilerHost,
    CompilerOptions, CreateProgramOptions, CustomTransformers, Diagnostic, DiagnosticMessageText,
    DiagnosticRelatedInformationInterface, EmitResult, FileIncludeReason, GetCanonicalFileName,
    LineAndCharacter, ModuleKind, ModuleResolutionHost, ModuleSpecifierResolutionHost, MultiMap,
    Node, PackageId, ParsedCommandLine, Path, Program, ReferencedFile, ResolvedProjectReference,
    ScriptReferenceHost, ScriptTarget, SortedArray, SourceFile, StructureIsReused, SymlinkCache,
    System, TypeChecker, TypeCheckerHost, TypeCheckerHostDebuggable, WriteFileCallback,
};

pub fn find_config_file<TFileExists: FnMut(&str) -> bool>(
    search_path: &str,
    mut file_exists: TFileExists,
    config_name: Option<&str>,
) -> Option<String> {
    let config_name = config_name.unwrap_or("tsconfig.json");
    for_each_ancestor_directory_str(search_path, |ancestor| {
        let file_name = combine_paths(ancestor, &vec![Some(config_name)]);
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
        combine_paths(&base_path, &vec![Some(module_name)])
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
    get_canonical_file_name: GetCanonicalFileName,
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
            &vec![Some(get_default_lib_file_name(options))],
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

#[derive(Debug, Default)]
struct DiagnosticCache {
    pub per_file: Option<HashMap<Path, Vec<Rc<Diagnostic>>>>,
    pub all_diagnostics: Option<Vec<Rc<Diagnostic>>>,
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
        options: Rc<CompilerOptions>,
        files: Vec<Rc<Node>>,
        current_directory: String,
    ) -> Rc<Self> {
        let rc = Rc::new(Program {
            _rc_wrapper: RefCell::new(None),
            options,
            files,
            current_directory,
            diagnostics_producing_type_checker: RefCell::new(None),
        });
        rc.set_rc_wrapper(Some(rc.clone()));
        rc
    }

    pub fn set_rc_wrapper(&self, rc_wrapper: Option<Rc<Program>>) {
        *self._rc_wrapper.borrow_mut() = rc_wrapper;
    }

    pub fn rc_wrapper(&self) -> Rc<Program> {
        self._rc_wrapper.borrow().clone().unwrap()
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
        cancellation_token: Option<&dyn CancellationToken>,
    ) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>> {
        self.get_diagnostics_helper(Program::get_syntactic_diagnostics_for_file)
    }

    pub fn get_semantic_diagnostics(
        &self,
        source_file: Option<&Node /*SourceFile*/>,
        cancellation_token: Option<&dyn CancellationToken>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_helper(Program::get_semantic_diagnostics_for_file)
    }

    pub fn emit(
        &self,
        target_source_file: Option<&Node /*SourceFile*/>,
        write_file: Option<&dyn WriteFileCallback>,
        cancellation_token: Option<Rc<dyn CancellationToken>>,
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
        self.current_directory.clone()
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
                Some(Rc::new(create_type_checker(self.rc_wrapper(), true)));
        }
        diagnostics_producing_type_checker.as_ref().unwrap().clone()
    }

    fn get_diagnostics_helper(
        &self,
        get_diagnostics: fn(&Program, &SourceFile) -> Vec<Rc<Diagnostic>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_source_files()
            .iter()
            .flat_map(|source_file| get_diagnostics(self, source_file.as_source_file()))
            .collect()
    }

    fn get_program_diagnostics(&self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        vec![]
    }

    fn run_with_cancellation_token<TReturn, TClosure: FnOnce() -> TReturn>(
        &self,
        func: TClosure,
    ) -> TReturn {
        func()
    }

    fn get_syntactic_diagnostics_for_file(&self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        source_file.parse_diagnostics().clone()
    }

    fn get_semantic_diagnostics_for_file(&self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        concatenate(
            filter_semantic_diagnostics(self.get_bind_and_check_diagnostics_for_file(source_file)),
            self.get_program_diagnostics(source_file),
        )
    }

    fn get_bind_and_check_diagnostics_for_file(
        &self,
        source_file: &SourceFile,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_and_cache_diagnostics(
            source_file,
            Program::get_bind_and_check_diagnostics_for_file_no_cache,
        )
    }

    fn get_bind_and_check_diagnostics_for_file_no_cache(
        &self,
        source_file: &SourceFile,
    ) -> Vec<Rc<Diagnostic>> {
        // self.run_with_cancellation_token(|| {
        let type_checker = self.get_diagnostics_producing_type_checker();

        let include_bind_and_check_diagnostics = true;
        let check_diagnostics = if include_bind_and_check_diagnostics {
            type_checker.get_diagnostics(source_file)
        } else {
            vec![]
        };

        check_diagnostics
        // })
    }

    fn get_and_cache_diagnostics(
        &self,
        source_file: &SourceFile,
        get_diagnostics: fn(&Program, &SourceFile) -> Vec<Rc<Diagnostic>>,
    ) -> Vec<Rc<Diagnostic>> {
        let result = get_diagnostics(self, source_file);
        result
    }

    pub fn get_options_diagnostics(
        &self,
        _cancellation_token: Option<&dyn CancellationToken>,
    ) -> SortedArray<Rc<Diagnostic>> {
        SortedArray::new(vec![])
    }

    pub fn get_global_diagnostics(
        &self,
        _cancellation_token: Option<&dyn CancellationToken>,
    ) -> SortedArray<Rc<Diagnostic>> {
        SortedArray::new(vec![])
    }

    pub fn get_config_file_parsing_diagnostics(&self) -> Vec<Rc<Diagnostic>> {
        vec![]
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

impl ModuleSpecifierResolutionHost for Program {}

impl TypeCheckerHost for Program {
    fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        self.options.clone()
    }

    fn get_source_files(&self) -> &[Rc<Node>] {
        &self.files
    }
}

impl TypeCheckerHostDebuggable for Program {}

struct CreateProgramHelperContext<'a> {
    processing_other_files: &'a mut Vec<Rc<Node>>,
    host: &'a dyn CompilerHost,
    current_directory: &'a str,
    options: Rc<CompilerOptions>,
}

pub fn create_program(root_names_or_options: CreateProgramOptions) -> Rc<Program> {
    let create_program_options = root_names_or_options;
    let root_names = &create_program_options.root_names;
    let options = &create_program_options.options;
    let config_file_parsing_diagnostics = create_program_options
        .config_file_parsing_diagnostics
        .as_ref();
    let project_references = create_program_options.project_references.as_ref();
    let old_program = create_program_options.old_program.as_ref();

    let mut processing_default_lib_files: Option<Vec<Rc<Node /*SourceFile*/>>> = None;
    let mut processing_other_files: Option<Vec<Rc<Node /*SourceFile*/>>> = None;
    let mut files: Option<Vec<Rc<Node /*SourceFile*/>>> = None;
    let mut symlinks: Option<SymlinkCache> = None;
    let mut common_source_directory: Option<String> = None;
    let mut diagnostics_producing_type_checker: Option<Rc<TypeChecker>> = None;
    let mut no_diagnostics_type_checker: Option<Rc<TypeChecker>> = None;
    let mut classifiable_names: Option<HashSet<__String>> = None;
    let mut ambient_module_name_to_unmodified_file_name: HashMap<String, String> = HashMap::new();
    let mut file_reasons: MultiMap<Path, FileIncludeReason> = create_multi_map();
    let mut cached_bind_and_check_diagnostics_for_file: DiagnosticCache/*<Diagnostic>*/ = Default::default();
    let mut cached_declaration_diagnostics_for_file: DiagnosticCache/*<DiagnosticWithLocation>*/ = Default::default();

    let mut resolved_type_reference_directives: HashMap<
        String,
        Option<ResolvedTypeReferenceDirective>,
    > = HashMap::new();
    let mut file_processing_diagnostics: Option<Vec<FilePreprocessingDiagnostics>> = None;

    let max_node_module_js_depth = options.max_node_module_js_depth.unwrap_or(0);
    let mut current_node_modules_depth = 0;

    let mut modules_with_elided_imports: HashMap<String, bool> = HashMap::new();

    let mut source_files_found_searching_node_modules: HashMap<String, bool> = HashMap::new();

    // tracing?.push(tracing.Phase.Program, "createProgram", { configFilePath: options.configFilePath, rootDir: options.rootDir }, true);
    // performance.mark("beforeProgram");

    let host: Rc<dyn CompilerHost> = create_program_options.host.map_or_else(
        || Rc::new(create_compiler_host(options.clone(), None)),
        |host| host.clone(),
    );
    let config_parsing_host = parse_config_host_from_compiler_host_like(&host);

    let current_directory = CompilerHost::get_current_directory(&host);

    let structure_is_reused = StructureIsReused::Not;
    if structure_is_reused != StructureIsReused::Completely {
        processing_other_files = Some(vec![]);
        let mut processing_other_files_present = processing_other_files.unwrap();
        let mut helper_context = CreateProgramHelperContext {
            processing_other_files: &mut processing_other_files_present,
            host: &host,
            current_directory: &current_directory,
            options: options.clone(),
        };
        for_each(root_names, |name, _index| {
            process_root_file(&mut helper_context, &name);
            Option::<()>::None
        });

        files = processing_other_files_present;
        println!("files: {:#?}", files);
        processing_other_files = None;
    }

    Program::new(options, files, current_directory)
}

fn filter_semantic_diagnostics(diagnostic: Vec<Rc<Diagnostic>>) -> Vec<Rc<Diagnostic>> {
    diagnostic
}

fn process_root_file(helper_context: &mut CreateProgramHelperContext, file_name: &str) {
    process_source_file(helper_context, &normalize_path(file_name));
}

fn get_source_file_from_reference_worker<TClosure: FnMut(&str) -> Option<Rc<Node>>>(
    file_name: &str,
    mut get_source_file: TClosure,
) -> Option<Rc<Node>> {
    get_source_file(file_name)
}

fn process_source_file(helper_context: &mut CreateProgramHelperContext, file_name: &str) {
    get_source_file_from_reference_worker(file_name, |file_name| {
        find_source_file(helper_context, file_name)
    });
}

fn find_source_file(
    helper_context: &mut CreateProgramHelperContext,
    file_name: &str,
) -> Option<Rc<Node>> {
    find_source_file_worker(helper_context, file_name)
}

fn find_source_file_worker(
    helper_context: &mut CreateProgramHelperContext,
    file_name: &str,
) -> Option<Rc<Node>> {
    let _path = to_path(helper_context, file_name);

    let file = helper_context.host.get_source_file(
        file_name,
        get_emit_script_target(&*helper_context.options),
        // TODO: this is wrong
        None,
        None,
    );

    file.map(|file| {
        let file_as_source_file = file.as_source_file();
        file_as_source_file.set_file_name(file_name.to_string());
        file_as_source_file.set_path(_path);
        helper_context.processing_other_files.push(file.clone());
        file
    })
}

fn to_path(helper_context: &mut CreateProgramHelperContext, file_name: &str) -> Path {
    to_path_helper(
        file_name,
        Some(helper_context.current_directory),
        |file_name| get_canonical_file_name(helper_context, file_name),
    )
}

fn get_canonical_file_name<'file_name>(
    helper_context: &mut CreateProgramHelperContext,
    file_name: &'file_name str,
) -> String {
    helper_context.host.get_canonical_file_name(file_name)
}
