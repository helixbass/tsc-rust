use std::{
    borrow::Borrow, cell::RefCell, cmp, collections::HashMap, convert::TryInto, io, time,
    time::SystemTime,
};

use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;
use regex::Regex;

use crate::{
    add_range, combine_paths, convert_to_relative_path, create_get_canonical_file_name,
    create_source_file, diagnostic_category_name, file_extension_is, for_each,
    for_each_ancestor_directory_str, generate_djb2_hash, get_default_lib_file_name,
    get_directory_path, get_emit_declarations, get_line_and_character_of_position,
    get_new_line_character, get_normalized_path_components, get_path_from_path_components,
    get_position_of_line_and_character, get_sys, is_build_info_file, is_rooted_disk_path,
    is_watch_set, missing_file_modified_time, normalize_path, pad_left,
    sort_and_deduplicate_diagnostics, trim_string_end, write_file_ensuring_directories,
    CancellationTokenDebuggable, CompilerHost, CompilerOptions, Debug_, Diagnostic,
    DiagnosticCategory, DiagnosticInterface, DiagnosticMessageText,
    DiagnosticRelatedInformationInterface, Extension, LineAndCharacter, ModuleResolutionHost,
    ModuleResolutionHostOverrider, Node, NodeInterface, OptionTry, Path, ProgramOrBuilderProgram,
    ScriptTarget, SourceFileLike, System,
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
        let common_path_components = common_path_components.as_mut().unwrap();

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
    options: Gc<CompilerOptions>,
    set_parent_nodes: Option<bool>,
) -> impl CompilerHost {
    create_compiler_host_worker(options, set_parent_nodes, None)
}

/*pub(crate) fn create_compiler_host_worker(*/
pub fn create_compiler_host_worker(
    options: Gc<CompilerOptions>,
    set_parent_nodes: Option<bool>,
    system: Option<Gc<Box<dyn System>>>,
) -> impl CompilerHost {
    let system = system.unwrap_or_else(|| get_sys());
    let existing_directories: HashMap<String, bool> = HashMap::new();
    let get_canonical_file_name =
        create_get_canonical_file_name(system.use_case_sensitive_file_names());
    let new_line = get_new_line_character(options.new_line, Some(|| system.new_line().to_owned()));

    CompilerHostConcrete {
        set_parent_nodes,
        system,
        existing_directories: GcCell::new(existing_directories),
        output_fingerprints: Default::default(),
        options,
        new_line,
        current_directory: Default::default(),
        get_canonical_file_name,
        read_file_override: Default::default(),
        file_exists_override: Default::default(),
        directory_exists_override: Default::default(),
        realpath_override: Default::default(),
        get_directories_override: Default::default(),
        write_file_override: Default::default(),
        create_directory_override: Default::default(),
    }
}

#[derive(Trace, Finalize)]
struct CompilerHostConcrete {
    set_parent_nodes: Option<bool>,
    system: Gc<Box<dyn System>>,
    existing_directories: GcCell<HashMap<String, bool>>,
    #[unsafe_ignore_trace]
    output_fingerprints: RefCell<Option<HashMap<String, OutputFingerprint>>>,
    options: Gc<CompilerOptions>,
    new_line: String,
    current_directory: GcCell<Option<String>>,
    #[unsafe_ignore_trace]
    get_canonical_file_name: fn(&str) -> String,
    read_file_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
    file_exists_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
    directory_exists_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
    realpath_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
    get_directories_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
    write_file_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
    create_directory_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
}

impl CompilerHostConcrete {
    fn maybe_read_file_override(&self) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
        self.read_file_override.borrow().clone()
    }

    fn maybe_file_exists_override(&self) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
        self.file_exists_override.borrow().clone()
    }

    fn maybe_directory_exists_override(
        &self,
    ) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
        self.directory_exists_override.borrow().clone()
    }

    fn maybe_realpath_override(&self) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
        self.realpath_override.borrow().clone()
    }

    fn maybe_get_directories_override(&self) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
        self.get_directories_override.borrow().clone()
    }

    fn maybe_write_file_override(&self) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
        self.write_file_override.borrow().clone()
    }

    fn maybe_create_directory_override(
        &self,
    ) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
        self.create_directory_override.borrow().clone()
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
        if let Some(read_file_override) = self.maybe_read_file_override() {
            read_file_override.read_file(file_name)
        } else {
            self.read_file_non_overridden(file_name)
        }
    }

    fn read_file_non_overridden(&self, file_name: &str) -> io::Result<Option<String>> {
        self.system.read_file(file_name)
    }

    fn set_overriding_read_file(
        &self,
        overriding_read_file: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        let mut read_file_override = self.read_file_override.borrow_mut();
        if read_file_override.is_some() && overriding_read_file.is_some() {
            panic!("Trying to re-override set_overriding_read_file(), need eg a stack instead?");
        }
        *read_file_override = overriding_read_file;
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
        overriding_file_exists: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        let mut file_exists_override = self.file_exists_override.borrow_mut();
        if file_exists_override.is_some() && overriding_file_exists.is_some() {
            panic!("Trying to re-override set_overriding_file_exists(), need eg a stack instead?");
        }
        *file_exists_override = overriding_file_exists;
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
        overriding_directory_exists: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        let mut directory_exists_override = self.directory_exists_override.borrow_mut();
        if directory_exists_override.is_some() && overriding_directory_exists.is_some() {
            panic!(
                "Trying to re-override set_overriding_directory_exists(), need eg a stack instead?"
            );
        }
        *directory_exists_override = overriding_directory_exists;
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
        overriding_realpath: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        let mut realpath_override = self.realpath_override.borrow_mut();
        if realpath_override.is_some() && overriding_realpath.is_some() {
            panic!("Trying to re-override set_overriding_realpath(), need eg a stack instead?");
        }
        *realpath_override = overriding_realpath;
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
        overriding_get_directories: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        let mut get_directories_override = self.get_directories_override.borrow_mut();
        if get_directories_override.is_some() && overriding_get_directories.is_some() {
            panic!(
                "Trying to re-override set_overriding_get_directories(), need eg a stack instead?"
            );
        }
        *get_directories_override = overriding_get_directories;
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
        _should_create_new_source_file: Option<bool>,
    ) -> io::Result<Option<Id<Node /*SourceFile*/>>> {
        let text: Option<String>;
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
        text.try_map(|text| {
            create_source_file(
                file_name,
                text,
                language_version,
                self.set_parent_nodes,
                None,
            )
        })
    }

    fn get_default_lib_location(&self) -> io::Result<Option<String>> {
        // Some(get_directory_path(&normalize_path(
        //     &self.system.get_executing_file_path(),
        // )))
        Ok(Some("/Users/jrosse/prj/TypeScript/built/local".to_owned()))
    }

    fn get_default_lib_file_name(&self, options: &CompilerOptions) -> io::Result<String> {
        Ok(combine_paths(
            &self.get_default_lib_location()?.unwrap(),
            &[Some(get_default_lib_file_name(options))],
        ))
    }

    fn get_current_directory(&self) -> io::Result<String> {
        let mut current_directory = self.current_directory.borrow_mut();
        if current_directory.is_none() {
            *current_directory = Some(self.system.get_current_directory()?);
        }
        Ok(current_directory.clone().unwrap())
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
        _source_files: Option<&[Id<Node /*SourceFile*/>]>,
    ) -> io::Result<()> {
        if let Some(write_file_override) = self.maybe_write_file_override() {
            write_file_override.write_file(
                file_name,
                data,
                write_byte_order_mark,
                on_error,
                _source_files,
            )
        } else {
            self.write_file_non_overridden(
                file_name,
                data,
                write_byte_order_mark,
                on_error,
                _source_files,
            )
        }
    }

    fn write_file_non_overridden(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        _source_files: Option<&[Id<Node /*SourceFile*/>]>,
    ) -> io::Result<()> {
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
        Ok(())
    }

    fn is_write_file_supported(&self) -> bool {
        true
    }

    fn set_overriding_write_file(
        &self,
        overriding_write_file: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        let mut write_file_override = self.write_file_override.borrow_mut();
        if write_file_override.is_some() && overriding_write_file.is_some() {
            panic!("Trying to re-override set_overriding_write_file(), need eg a stack instead?");
        }
        *write_file_override = overriding_write_file;
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
    ) -> Option<io::Result<Vec<String>>> {
        Some(
            self.system
                .read_directory(path, Some(extensions), excludes, Some(includes), depth),
        )
    }

    fn is_read_directory_implemented(&self) -> bool {
        true
    }

    fn create_directory(&self, d: &str) -> io::Result<()> {
        Ok(
            if let Some(create_directory_override) = self.maybe_create_directory_override() {
                create_directory_override.create_directory(d)?
            } else {
                self.create_directory_non_overridden(d)?
            },
        )
    }

    fn create_directory_non_overridden(&self, d: &str) -> io::Result<()> {
        self.system.create_directory(d)
    }

    fn is_create_directory_supported(&self) -> bool {
        true
    }

    fn set_overriding_create_directory(
        &self,
        overriding_create_directory: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        let mut create_directory_override = self.create_directory_override.borrow_mut();
        if create_directory_override.is_some() && overriding_create_directory.is_some() {
            panic!(
                "Trying to re-override set_overriding_create_directory(), need eg a stack instead?"
            );
        }
        *create_directory_override = overriding_create_directory;
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

    fn is_get_source_file_by_path_supported(&self) -> bool {
        false
    }

    fn is_get_parsed_command_line_supported(&self) -> bool {
        false
    }
}

pub(crate) fn change_compiler_host_like_to_use_cache(
    host: Gc<Box<dyn CompilerHost>>,
    to_path: Gc<Box<dyn ToPath>>,
    get_source_file: Option<Gc<Box<dyn GetSourceFile>>>,
) /*-> */
{
    let overrider: Gc<Box<dyn ModuleResolutionHostOverrider>> = Gc::new(Box::new(
        ChangeCompilerHostLikeToUseCacheOverrider::new(host.clone(), to_path, get_source_file),
    ));

    host.set_overriding_read_file(Some(overrider.clone()));

    host.set_overriding_file_exists(Some(overrider.clone()));

    if host.is_write_file_supported() {
        host.set_overriding_write_file(Some(overrider.clone()));
    }

    if host.is_directory_exists_supported() && host.is_create_directory_supported() {
        host.set_overriding_directory_exists(Some(overrider.clone()));
        host.set_overriding_create_directory(Some(overrider.clone()));
    }

    // return {
    //     originalReadFile,
    //     originalFileExists,
    //     originalDirectoryExists,
    //     originalCreateDirectory,
    //     originalWriteFile,
    //     getSourceFileWithCache,
    //     readFileWithCache,
    // };
}

#[derive(Trace, Finalize)]
struct ChangeCompilerHostLikeToUseCacheOverrider {
    host: Gc<Box<dyn CompilerHost>>,
    to_path: Gc<Box<dyn ToPath>>,
    get_source_file: Option<Gc<Box<dyn GetSourceFile>>>,
    #[unsafe_ignore_trace]
    read_file_cache: RefCell<HashMap<String, Option<String>>>,
    #[unsafe_ignore_trace]
    file_exists_cache: RefCell<HashMap<String, bool>>,
    #[unsafe_ignore_trace]
    directory_exists_cache: RefCell<HashMap<String, bool>>,
    source_file_cache: GcCell<HashMap<String, Id<Node /*SourceFile*/>>>,
    has_get_source_file_with_cache: bool,
}

impl ChangeCompilerHostLikeToUseCacheOverrider {
    pub fn new(
        host: Gc<Box<dyn CompilerHost>>,
        to_path: Gc<Box<dyn ToPath>>,
        get_source_file: Option<Gc<Box<dyn GetSourceFile>>>,
    ) -> Self {
        Self {
            host,
            to_path,
            has_get_source_file_with_cache: get_source_file.is_some(),
            get_source_file,
            read_file_cache: Default::default(),
            file_exists_cache: Default::default(),
            directory_exists_cache: Default::default(),
            source_file_cache: Default::default(),
        }
    }

    #[allow(dead_code)]
    pub fn read_file_with_cache(&self, file_name: &str) -> Option<String> {
        let key = self.to_path.call(file_name);
        {
            let read_file_cache = self.read_file_cache.borrow();
            let value = read_file_cache.get(&*key);
            if let Some(value) = value {
                return value.clone();
            }
        }
        self.set_read_file_cache(key, file_name)
    }

    fn set_read_file_cache(&self, key: Path, file_name: &str) -> Option<String> {
        let new_value = self.host.read_file_non_overridden(file_name).ok().flatten();
        self.read_file_cache
            .borrow_mut()
            .insert(key.to_string(), new_value.clone());
        new_value
    }

    // TODO getSourceFileWithCache()
}

impl ModuleResolutionHostOverrider for ChangeCompilerHostLikeToUseCacheOverrider {
    fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
        let key = self.to_path.call(file_name);
        {
            let read_file_cache = self.read_file_cache.borrow();
            let value = read_file_cache.get(&*key);
            if let Some(value) = value {
                return Ok(value.clone());
            }
        }
        if !file_extension_is(file_name, Extension::Json.to_str()) && !is_build_info_file(file_name)
        {
            return self.host.read_file_non_overridden(file_name);
        }

        Ok(self.set_read_file_cache(key, file_name))
    }

    fn file_exists(&self, file_name: &str) -> bool {
        let key = self.to_path.call(file_name);
        {
            let value = self.file_exists_cache.borrow().get(&*key).copied();
            if let Some(value) = value {
                return value;
            }
        }
        let new_value = self.host.file_exists_non_overridden(file_name);
        self.file_exists_cache
            .borrow_mut()
            .insert(key.to_string(), new_value);
        new_value
    }

    fn write_file(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        source_files: Option<&[Id<Node /*SourceFile*/>]>,
    ) -> io::Result<()> {
        let key = self.to_path.call(file_name);
        self.file_exists_cache.borrow_mut().remove(&*key);

        let value_is_different = {
            let read_file_cache = self.read_file_cache.borrow();
            let value = read_file_cache.get(&*key);
            matches!(
                value,
                Some(value) if match value.as_ref() {
                    None => true,
                    Some(value) => &**value != data
                }
            )
        };
        if value_is_different {
            self.read_file_cache.borrow_mut().remove(&*key);
            self.source_file_cache.borrow_mut().remove(&*key);
        } else if self.has_get_source_file_with_cache {
            let source_file_is_different = {
                let source_file_cache = self.source_file_cache.borrow();
                let source_file = source_file_cache.get(&*key);
                matches!(
                    source_file,
                    Some(source_file) if &**source_file.as_source_file().text() != data
                )
            };
            if source_file_is_different {
                self.source_file_cache.borrow_mut().remove(&*key);
            }
        }
        self.host.write_file_non_overridden(
            file_name,
            data,
            write_byte_order_mark,
            on_error,
            source_files,
        )?;
        Ok(())
    }

    fn directory_exists(&self, directory: &str) -> Option<bool> {
        let key = self.to_path.call(directory);
        {
            let directory_exists_cache = self.directory_exists_cache.borrow();
            let value = directory_exists_cache.get(&*key);
            if value.is_some() {
                return value.copied();
            }
        }
        let new_value = self
            .host
            .directory_exists_non_overridden(directory)
            .unwrap();
        self.directory_exists_cache
            .borrow_mut()
            .insert(key.to_string(), new_value);
        Some(new_value)
    }

    fn create_directory(&self, directory: &str) -> io::Result<()> {
        let key = self.to_path.call(directory);
        self.directory_exists_cache.borrow_mut().remove(&*key);
        self.host.create_directory_non_overridden(directory)?;

        Ok(())
    }

    fn get_directories(&self, _path: &str) -> Option<Vec<String>> {
        unreachable!()
    }

    fn realpath(&self, _s: &str) -> Option<String> {
        unreachable!()
    }
}

pub trait ToPath: Trace + Finalize {
    fn call(&self, file_name: &str) -> Path;
}

pub trait GetSourceFile: Trace + Finalize {
    fn call(
        &self,
        file_name: &str,
        script_target: ScriptTarget,
        something: Option<&mut dyn FnMut(&str)>,
        something_else: Option<bool>,
    ) -> Option<Id<Node>>;
}

pub fn get_pre_emit_diagnostics(
    program: &ProgramOrBuilderProgram,
    source_file: Option<Id<Node>>,
    cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
) -> io::Result<Vec<Gc<Diagnostic>>> {
    let program = match program {
        ProgramOrBuilderProgram::Program(program) => program,
        _ => unimplemented!(),
    };
    let mut diagnostics: Vec<Gc<Diagnostic>> = vec![];
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
        Some(&program.get_global_diagnostics(cancellation_token.clone())?),
        None,
        None,
    );
    add_range(
        &mut diagnostics,
        Some(
            &program
                .get_semantic_diagnostics(source_file.as_deref(), cancellation_token.clone())?,
        ),
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
                )?,
            ),
            None,
            None,
        );
    }

    Ok(sort_and_deduplicate_diagnostics(&diagnostics).into())
}

pub trait FormatDiagnosticsHost {
    fn get_current_directory(&self) -> io::Result<String>;
    fn get_new_line(&self) -> &str;
    fn get_canonical_file_name(&self, file_name: &str) -> String;
}

pub fn format_diagnostics(
    diagnostics: &[Gc<Diagnostic>],
    host: &impl FormatDiagnosticsHost,
) -> io::Result<String> {
    let mut output = "".to_owned();

    for diagnostic in diagnostics {
        output.push_str(&format_diagnostic(diagnostic, host)?);
    }
    Ok(output)
}

pub fn format_diagnostic(
    diagnostic: &Diagnostic,
    host: &impl FormatDiagnosticsHost,
) -> io::Result<String> {
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
            convert_to_relative_path(&file_name, &host.get_current_directory()?, |file_name| {
                host.get_canonical_file_name(file_name)
            });
        return Ok(format!(
            "{}({},{}): {}",
            relative_file_name,
            line + 1,
            character + 1,
            error_message
        ));
    }

    Ok(error_message)
}

pub(crate) struct ForegroundColorEscapeSequences;
impl ForegroundColorEscapeSequences {
    pub const Grey: &'static str = "\u{001b}[90m";
    pub const Red: &'static str = "\u{001b}[91m";
    pub const Yellow: &'static str = "\u{001b}[93m";
    pub const Blue: &'static str = "\u{001b}[94m";
    pub const Cyan: &'static str = "\u{001b}[96m";
}
const gutter_style_sequence: &str = "\u{001b}[7m";
const gutter_separator: &str = " ";
const reset_escape_sequence: &str = "\u{001b}[0m";
const ellipsis: &str = "...";
const half_indent: &str = "  ";
const indent_: &str = "    ";
fn get_category_format(category: DiagnosticCategory) -> &'static str /*ForegroundColorEscapeSequences*/
{
    match category {
        DiagnosticCategory::Error => ForegroundColorEscapeSequences::Red,
        DiagnosticCategory::Warning => ForegroundColorEscapeSequences::Yellow,
        DiagnosticCategory::Suggestion => Debug_.fail(Some(
            "Should never get an Info diagnostic on the command line.",
        )),
        DiagnosticCategory::Message => ForegroundColorEscapeSequences::Blue,
    }
}

pub(crate) fn format_color_and_reset(text: &str, format_style: &str) -> String {
    format!("{}{}{}", format_style, text, reset_escape_sequence)
}

fn format_code_span(
    file: Id<Node>, /*SourceFile*/
    start: isize,
    length: isize,
    indent: &str,
    squiggle_color: &str, /*ForegroundColorEscapeSequences*/
    host: &impl FormatDiagnosticsHost,
) -> String {
    let start_as_usize: usize = start.try_into().unwrap();
    let length_as_usize: usize = length.try_into().unwrap();
    let file_as_source_file = file.as_source_file();
    let LineAndCharacter {
        line: first_line,
        character: first_line_char,
    } = get_line_and_character_of_position(file_as_source_file, start_as_usize);
    let LineAndCharacter {
        line: last_line,
        character: last_line_char,
    } = get_line_and_character_of_position(file_as_source_file, start_as_usize + length_as_usize);
    let file_text = file_as_source_file.text_as_chars();
    let last_line_in_file =
        get_line_and_character_of_position(file_as_source_file, file_text.len()).line;

    let has_more_than_five_lines = last_line - first_line >= 4;
    let mut gutter_width = (last_line + 1).to_string().len();
    if has_more_than_five_lines {
        gutter_width = cmp::max(ellipsis.len(), gutter_width);
    }

    let mut context = "".to_owned();
    let mut i = first_line;
    while i <= last_line {
        context.push_str(host.get_new_line());
        if has_more_than_five_lines && first_line + 1 < i && i < last_line - 1 {
            context.push_str(&format!(
                "{}{}{}{}",
                indent,
                format_color_and_reset(
                    &pad_left(ellipsis, gutter_width, None,),
                    gutter_style_sequence,
                ),
                gutter_separator,
                host.get_new_line(),
            ));
            i = last_line - 1;
        }

        let line_start = get_position_of_line_and_character(file_as_source_file, i, 0, None);
        let line_end = if i < last_line_in_file {
            get_position_of_line_and_character(file_as_source_file, i + 1, 0, None)
        } else {
            file_text.len()
        };
        let line_content: String = file_text[line_start..line_end].into_iter().collect();
        let line_content = trim_string_end(&line_content);
        lazy_static! {
            static ref tab_regex: Regex = Regex::new(r"\t").unwrap();
        }
        let line_content = tab_regex.replace_all(line_content, " ");

        context.push_str(&format!(
            "{}{}{}",
            indent,
            format_color_and_reset(
                &pad_left(&(i + 1).to_string(), gutter_width, None,),
                gutter_style_sequence,
            ),
            gutter_separator,
        ));
        context.push_str(&format!("{}{}", line_content, host.get_new_line()));

        context.push_str(&format!(
            "{}{}{}",
            indent,
            format_color_and_reset(&pad_left("", gutter_width, None,), gutter_style_sequence,),
            gutter_separator,
        ));
        context.push_str(squiggle_color);
        lazy_static! {
            static ref dot_regex: Regex = Regex::new(r".").unwrap();
        }
        if i == first_line {
            let last_char_for_line = if i == last_line {
                Some(last_line_char)
            } else {
                None
            };

            lazy_static! {
                static ref non_space_regex: Regex = Regex::new(r"\S").unwrap();
            }
            // TODO: this needs to be chars/bytes-aware?
            context.push_str(&non_space_regex.replace_all(&line_content[0..first_line_char], " "));
            context.push_str(&dot_regex.replace_all(
                if let Some(last_char_for_line) = last_char_for_line {
                    &line_content[first_line_char..last_char_for_line]
                } else {
                    &line_content[first_line_char..]
                },
                "~",
            ));
        } else if i == last_line {
            context.push_str(&dot_regex.replace_all(&line_content[0..last_line_char], "~"));
        } else {
            context.push_str(&dot_regex.replace_all(&line_content, "~"));
        }
        context.push_str(reset_escape_sequence);
        i += 1;
    }
    context
}

pub fn format_location(
    file: Id<Node>, /*SourceFile*/
    start: isize,
    host: &impl FormatDiagnosticsHost,
    color: Option<impl Fn(&str, &str) -> String>,
) -> io::Result<String> {
    let color_present = |text: &str, format_style: &str| {
        if let Some(color) = color.as_ref() {
            color(text, format_style)
        } else {
            format_color_and_reset(text, format_style)
        }
    };
    let file_as_source_file = file.as_source_file();
    let LineAndCharacter {
        line: first_line,
        character: first_line_char,
    } = get_line_and_character_of_position(file_as_source_file, start.try_into().unwrap());
    let relative_file_name = /*host ? */convert_to_relative_path(
        &file_as_source_file.file_name(),
        &host.get_current_directory()?,
        |file_name: &str| host.get_canonical_file_name(file_name)
    ) /*: file.fileName*/;

    let mut output = "".to_owned();
    output.push_str(&color_present(
        &relative_file_name,
        ForegroundColorEscapeSequences::Cyan,
    ));
    output.push_str(":");
    output.push_str(&color_present(
        &format!("{}", first_line + 1),
        ForegroundColorEscapeSequences::Yellow,
    ));
    output.push_str(":");
    output.push_str(&color_present(
        &format!("{}", first_line_char + 1),
        ForegroundColorEscapeSequences::Yellow,
    ));
    Ok(output)
}

pub fn format_diagnostics_with_color_and_context(
    diagnostics: &[Gc<Diagnostic>],
    host: &impl FormatDiagnosticsHost,
) -> io::Result<String> {
    let mut output = "".to_owned();
    for diagnostic in diagnostics {
        if let Some(diagnostic_file) = diagnostic.maybe_file().as_ref() {
            let file = diagnostic_file;
            let start = diagnostic.start();
            output.push_str(&format_location(
                file,
                start,
                host,
                Option::<fn(&str, &str) -> String>::None,
            )?);
            output.push_str(" - ");
        }

        output.push_str(&format_color_and_reset(
            &diagnostic_category_name(diagnostic.category(), None),
            get_category_format(diagnostic.category()),
        ));
        output.push_str(&format_color_and_reset(
            &format!(" TS{}: ", diagnostic.code()),
            ForegroundColorEscapeSequences::Grey,
        ));
        output.push_str(&flatten_diagnostic_message_text(
            Some(diagnostic.message_text()),
            host.get_new_line(),
            None,
        ));

        if let Some(diagnostic_file) = diagnostic.maybe_file().as_ref() {
            output.push_str(host.get_new_line());
            output.push_str(&format_code_span(
                diagnostic_file,
                diagnostic.start(),
                diagnostic.length(),
                "",
                get_category_format(diagnostic.category()),
                host,
            ));
        }
        if let Some(diagnostic_related_information) =
            diagnostic.maybe_related_information().as_ref()
        {
            output.push_str(host.get_new_line());
            for related_information in diagnostic_related_information {
                let file = related_information.maybe_file();
                let start = related_information.maybe_start();
                let length = related_information.maybe_length();
                let message_text = related_information.message_text();
                if let Some(file) = file.as_ref() {
                    output.push_str(host.get_new_line());
                    output.push_str(&format!(
                        "{}{}",
                        half_indent,
                        format_location(
                            file,
                            start.unwrap(),
                            host,
                            Option::<fn(&str, &str) -> String>::None
                        )?
                    ));
                    output.push_str(&format_code_span(
                        file,
                        start.unwrap(),
                        length.unwrap(),
                        indent_,
                        ForegroundColorEscapeSequences::Cyan,
                        host,
                    ));
                }
                output.push_str(host.get_new_line());
                output.push_str(&format!(
                    "{}{}",
                    indent_,
                    flatten_diagnostic_message_text(Some(message_text), host.get_new_line(), None,)
                ));
            }
        }
        output.push_str(host.get_new_line());
    }
    Ok(output)
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
