use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io;
use std::rc::Rc;
use std::time;
use std::time::SystemTime;

use crate::{
    add_range, combine_paths, convert_to_relative_path, create_get_canonical_file_name,
    create_source_file, diagnostic_category_name, for_each, for_each_ancestor_directory_str,
    generate_djb2_hash, get_default_lib_file_name, get_directory_path, get_emit_declarations,
    get_line_and_character_of_position, get_new_line_character, get_normalized_path_components,
    get_path_from_path_components, get_sys, is_rooted_disk_path, is_watch_set,
    missing_file_modified_time, normalize_path, sort_and_deduplicate_diagnostics,
    write_file_ensuring_directories, CancellationTokenDebuggable, CompilerHost, CompilerOptions,
    Diagnostic, DiagnosticMessageText, DiagnosticRelatedInformationInterface, LineAndCharacter,
    ModuleResolutionHost, ModuleResolutionHostOverrider, Node, NodeInterface, Path,
    ProgramOrBuilderProgram, ScriptTarget, System,
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

pub(super) fn create_compiler_host(
    options: Rc<CompilerOptions>,
    set_parent_nodes: Option<bool>,
) -> impl CompilerHost {
    create_compiler_host_worker(options, set_parent_nodes, None)
}

/*pub(crate) fn create_compiler_host_worker(*/
pub fn create_compiler_host_worker(
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
    fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
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
                text = value;
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

    fn is_on_release_old_source_file_supported(&self) -> bool {
        false
    }

    fn is_on_release_parsed_command_line_supported(&self) -> bool {
        false
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

pub fn get_pre_emit_diagnostics<TSourceFile: Borrow<Node>>(
    program: &ProgramOrBuilderProgram,
    source_file: Option<TSourceFile>,
    cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
) -> Vec<Rc<Diagnostic>> {
    let program = match program {
        ProgramOrBuilderProgram::Program(program) => program,
        _ => unimplemented!(),
    };
    let mut diagnostics: Vec<Rc<Diagnostic>> = vec![];
    add_range(
        &mut diagnostics,
        Some(&program.get_config_file_parsing_diagnostics()),
        None,
        None,
    );
    add_range(
        &mut diagnostics,
        Some(&program.get_options_diagnostics(cancellation_token.clone())),
        None,
        None,
    );
    let source_file = source_file.map(|source_file| source_file.borrow().node_wrapper());
    add_range(
        &mut diagnostics,
        Some(
            &program.get_syntactic_diagnostics(source_file.as_deref(), cancellation_token.clone()),
        ),
        None,
        None,
    );
    add_range(
        &mut diagnostics,
        Some(&program.get_global_diagnostics(cancellation_token.clone())),
        None,
        None,
    );
    add_range(
        &mut diagnostics,
        Some(&program.get_semantic_diagnostics(source_file.as_deref(), cancellation_token.clone())),
        None,
        None,
    );

    if get_emit_declarations(&program.get_compiler_options()) {
        add_range(
            &mut diagnostics,
            Some(
                &program.get_declaration_diagnostics(
                    source_file.as_deref(),
                    cancellation_token.clone(),
                ),
            ),
            None,
            None,
        );
    }

    sort_and_deduplicate_diagnostics(&diagnostics).into()
}

pub trait FormatDiagnosticsHost {
    fn get_current_directory(&self) -> String;
    fn get_new_line(&self) -> &str;
    fn get_canonical_file_name(&self, file_name: &str) -> String;
}

pub fn format_diagnostics<THost: FormatDiagnosticsHost>(
    diagnostics: &[Rc<Diagnostic>],
    host: &THost,
) -> String {
    let mut output = "".to_owned();

    for diagnostic in diagnostics {
        output.push_str(&format_diagnostic(diagnostic, host));
    }
    output
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
