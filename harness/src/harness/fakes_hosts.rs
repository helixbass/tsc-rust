pub mod fakes {
    use gc::{Finalize, Gc, GcCell, GcCellRef, Trace};
    use std::borrow::Cow;
    use std::cell::{Cell, Ref, RefCell};
    use std::collections::HashMap;
    use std::io;
    use std::mem;
    use std::rc::Rc;
    use std::time::SystemTime;

    use typescript_rust::{
        continue_if_err, create_source_file, generate_djb2_hash, get_default_lib_file_name,
        get_new_line_character, match_files, millis_since_epoch_to_system_time, not_implemented,
        return_ok_default_if_none, CompilerOptions, ConvertToTSConfigHost,
        DirectoryWatcherCallback, ExitStatus, FileSystemEntries, FileWatcher, FileWatcherCallback,
        ModuleResolutionHost, ModuleResolutionHostOverrider, Node, ScriptTarget, System as _,
        WatchOptions,
    };
    use typescript_services_rust::{get_default_compiler_options, NodeServicesInterface};

    use crate::vfs::SortOptionsComparerFromStringComparer;
    use crate::{collections, documents, get_light_mode, vfs, vpath, Utils};

    // const processExitSentinel = new Error("System exit");

    #[derive(Default)]
    pub struct SystemOptions {
        pub executing_file_path: Option<String>,
        pub new_line: Option<&'static str /*"\r\n" | "\n"*/>,
        pub env: Option<HashMap<String, String>>,
    }

    #[derive(Trace, Finalize)]
    pub struct System {
        pub vfs: Gc<vfs::FileSystem>,
        pub args: Vec<String>,
        #[unsafe_ignore_trace]
        output: RefCell<Vec<String>>,
        pub new_line: &'static str,
        pub use_case_sensitive_file_names: bool,
        pub exit_code: Option<u32>,

        _executing_file_path: Option<String>,
        _env: Option<HashMap<String, String>>,
        #[unsafe_ignore_trace]
        test_terminal_width: Cell<Option<Option<usize>>>,
    }

    impl System {
        pub fn new(vfs: Gc<vfs::FileSystem>, options: Option<SystemOptions>) -> io::Result<Self> {
            let SystemOptions {
                executing_file_path,
                new_line,
                env,
            } = options.unwrap_or_default();
            let new_line = new_line.unwrap_or("\r\n");
            let use_case_sensitive_file_names = !vfs.ignore_case;
            Ok(Self {
                args: Default::default(),
                output: Default::default(),
                exit_code: Default::default(),
                vfs: if vfs.is_readonly() {
                    Gc::new(vfs::FileSystem::shadow(vfs, None)?)
                } else {
                    vfs
                },
                use_case_sensitive_file_names,
                new_line,
                _executing_file_path: executing_file_path,
                _env: env,
                test_terminal_width: Default::default(),
            })
        }

        pub fn get_accessible_file_system_entries(&self, path: &str) -> FileSystemEntries {
            let mut files: Vec<String> = vec![];
            let mut directories: Vec<String> = vec![];
            for file in self.vfs.readdir_sync(path).unwrap_or_default() {
                let stats =
                    continue_if_err!(self.vfs.stat_sync(&vpath::combine(path, &[Some(&file)])));
                if stats.is_file() {
                    files.push(file);
                } else if stats.is_directory() {
                    directories.push(file);
                }
            }
            FileSystemEntries { files, directories }
        }

        fn _get_stats(&self, path: &str) -> Option<vfs::Stats> {
            // try {
            if self.vfs.exists_sync(path).ok()? {
                Some(self.vfs.stat_sync(path).ok()?)
            } else {
                None
            }
            // }
            // catch {
            //     return undefined;
            // }
        }
    }

    impl typescript_rust::System for System {
        fn get_width_of_terminal(&self) -> Option<usize> {
            self.test_terminal_width.get().unwrap_or_else(|| {
                let test_terminal_width = self
                    .get_environment_variable("TS_TEST_TERMINAL_WIDTH")
                    .parse::<usize>()
                    .ok();
                self.test_terminal_width.set(Some(test_terminal_width));
                test_terminal_width
            })
        }

        fn write_output_is_tty(&self) -> Option<bool> {
            Some(true)
        }

        fn write(&self, message: &str) {
            self.output.borrow_mut().push(message.to_owned());
        }

        // TODO: at least for this implementor, the Typescript version looks like it catches all exceptions and
        // translates to undefined so maybe the return type should just be Option<String>?
        fn read_file(&self, path: &str) -> io::Result<Option<String>> {
            // try {
            let content = self
                .vfs
                .read_file_sync(path, Some("utf8"))
                .ok()
                .map(|string_or_buffer| string_or_buffer.as_string_owned());
            Ok(content.map(Utils::remove_byte_order_mark))
            // }
            // catch {
            //     return undefined;
            // }
        }

        fn write_file(
            &self,
            path: &str,
            data: &str,
            write_byte_order_mark: Option<bool>,
        ) -> io::Result<()> {
            self.vfs.mkdirp_sync(&vpath::dirname(path))?;
            self.vfs.write_file_sync(
                path,
                if write_byte_order_mark == Some(true) {
                    Utils::add_utf8_byte_order_mark(data.to_owned())
                } else {
                    data.to_owned()
                },
                None,
            )?;
            Ok(())
        }

        fn is_delete_file_supported(&self) -> bool {
            true
        }

        fn delete_file(&self, path: &str) {
            self.vfs.unlink_sync(path);
        }

        fn file_exists(&self, path: &str) -> bool {
            let stats = self._get_stats(path);
            stats.map_or(false, |stats| stats.is_file())
        }

        fn directory_exists(&self, path: &str) -> bool {
            let stats = self._get_stats(path);
            stats.map_or(false, |stats| stats.is_directory())
        }

        fn create_directory(&self, path: &str) -> io::Result<()> {
            self.vfs.mkdirp_sync(path)?;

            Ok(())
        }

        fn get_directories(&self, path: &str) -> Vec<String> {
            let mut result: Vec<String> = Default::default();
            for file in self.vfs.readdir_sync(path).unwrap_or_default() {
                match self.vfs.stat_sync(&vpath::combine(path, &[Some(&file)])) {
                    Err(_) => break,
                    Ok(value) if value.is_directory() => {
                        result.push(file);
                    }
                    _ => (),
                }
            }
            result
        }

        fn read_directory(
            &self,
            path: &str,
            extensions: Option<&[&str]>,
            exclude: Option<&[String]>,
            include: Option<&[String]>,
            depth: Option<usize>,
        ) -> io::Result<Vec<String>> {
            Ok(match_files(
                path,
                extensions,
                exclude,
                include,
                self.use_case_sensitive_file_names,
                &self.get_current_directory()?,
                depth,
                |path: &str| self.get_accessible_file_system_entries(path),
                |path: &str| self.realpath(path).unwrap(),
            ))
        }

        fn exit(&self, _exit_code: Option<ExitStatus>) -> ! {
            // this.exitCode = exitCode;
            // throw processExitSentinel;
            panic!("System exit");
        }

        fn get_file_size(&self, path: &str) -> Option<usize> {
            let stats = self._get_stats(path);
            Some(
                stats
                    .filter(|stats| stats.is_file())
                    .map_or(0, |stats| stats.size),
            )
        }

        fn resolve_path(&self, path: &str) -> io::Result<String> {
            Ok(vpath::resolve(&self.vfs.cwd()?, &[Some(path)]))
        }

        fn get_executing_file_path(&self) -> Cow<'static, str> {
            if self._executing_file_path.is_none() {
                not_implemented();
            }
            self._executing_file_path.clone().unwrap().into()
        }

        fn is_get_modified_time_supported(&self) -> bool {
            true
        }

        fn get_modified_time(&self, path: &str) -> Option<SystemTime> {
            let stats = self._get_stats(path);
            stats.map(|stats| stats.mtime)
        }

        fn is_set_modified_time_supported(&self) -> bool {
            true
        }

        fn set_modified_time(&self, path: &str, time: SystemTime) {
            self.vfs.utimes_sync(path, time, time);
        }

        fn is_create_hash_supported(&self) -> bool {
            true
        }

        fn create_hash(&self, data: &str) -> String {
            format!("{}-{}", generate_djb2_hash(data), data)
        }

        fn realpath(&self, path: &str) -> Option<String> {
            match self.vfs.realpath_sync(path) {
                Err(_) => Some(path.to_owned()),
                Ok(value) => Some(value),
            }
        }

        fn is_realpath_supported(&self) -> bool {
            true
        }

        fn get_environment_variable(&self, name: &str) -> String {
            self._env.as_ref().unwrap().get(name).unwrap().clone()
        }

        fn now(&self) -> Option<SystemTime> {
            Some(millis_since_epoch_to_system_time(self.vfs.time()))
        }

        fn args(&self) -> &[String] {
            &self.args
        }

        fn new_line(&self) -> &str {
            self.new_line
        }

        fn is_watch_file_supported(&self) -> bool {
            false
        }

        fn watch_file(
            &self,
            _path: &str,
            _callback: FileWatcherCallback,
            _polling_interval: Option<u32>,
            _options: Option<&WatchOptions>,
            // TODO: shouldn't this return type be Option<...> (same with watch_directory())?
        ) -> Rc<dyn FileWatcher> {
            unreachable!()
        }

        fn is_watch_directory_supported(&self) -> bool {
            false
        }

        fn watch_directory(
            &self,
            _path: &str,
            _callback: DirectoryWatcherCallback,
            _recursive: Option<bool>,
            _options: Option<&WatchOptions>,
        ) -> Rc<dyn FileWatcher> {
            unreachable!()
        }

        fn is_clear_screen_supported(&self) -> bool {
            false
        }

        fn as_convert_to_tsconfig_host(&self) -> &dyn ConvertToTSConfigHost {
            self
        }
    }

    impl ConvertToTSConfigHost for System {
        fn get_current_directory(&self) -> io::Result<String> {
            self.vfs.cwd()
        }

        fn use_case_sensitive_file_names(&self) -> bool {
            self.use_case_sensitive_file_names
        }
    }

    #[derive(Trace, Finalize)]
    pub struct ParseConfigHost {
        pub sys: Gc<System>,
    }

    impl ParseConfigHost {
        pub fn new(sys: impl Into<RcSystemOrRcFileSystem>) -> io::Result<Self> {
            let sys = sys.into();
            let sys = match sys {
                RcSystemOrRcFileSystem::RcSystem(sys) => sys,
                RcSystemOrRcFileSystem::RcFileSystem(sys) => Gc::new(System::new(sys, None)?),
            };
            Ok(Self { sys })
        }
    }

    #[derive(Trace, Finalize)]
    pub struct CompilerHost {
        _dyn_wrapper: GcCell<Option<Gc<Box<dyn typescript_rust::CompilerHost>>>>,
        pub sys: Gc<System>,
        pub default_lib_location: String,
        outputs: GcCell<Vec<Gc<documents::TextDocument>>>,
        _outputs_map: GcCell<collections::SortedMap<String, usize>>,
        #[unsafe_ignore_trace]
        traces: RefCell<Vec<String>>,
        pub should_assert_invariants: bool,

        _set_parent_nodes: bool,
        _source_files: GcCell<collections::SortedMap<String, Gc<Node /*SourceFile*/>>>,
        _parse_config_host: GcCell<Option<Gc<ParseConfigHost>>>,
        _new_line: String,

        file_exists_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
        directory_exists_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
        read_file_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
        write_file_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
        realpath_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
        get_directories_override: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
    }

    impl CompilerHost {
        pub fn new(
            sys: impl Into<RcSystemOrRcFileSystem>,
            options: Option<Gc<CompilerOptions>>,
            set_parent_nodes: Option<bool>,
        ) -> io::Result<Gc<Box<Self>>> {
            let sys = sys.into();
            let options = options.unwrap_or_else(|| Gc::new(get_default_compiler_options()));
            let set_parent_nodes = set_parent_nodes.unwrap_or(false);
            let sys = match sys {
                RcSystemOrRcFileSystem::RcSystem(sys) => sys,
                RcSystemOrRcFileSystem::RcFileSystem(sys) => Gc::new(System::new(sys, None)?),
            };
            let dyn_wrapper: Gc<Box<dyn typescript_rust::CompilerHost>> = Gc::new(Box::new(Self {
                _dyn_wrapper: Default::default(),
                sys: sys.clone(),
                default_lib_location: {
                    let value = sys
                        .vfs
                        .meta()
                        .borrow()
                        .get("defaultLibLocation")
                        .unwrap_or_else(|| "".to_owned());
                    value
                },
                _new_line: get_new_line_character(
                    options.new_line,
                    Some(|| sys.new_line.to_owned()),
                ),
                _source_files: GcCell::new(collections::SortedMap::new(
                    collections::SortOptions {
                        comparer: Gc::new(Box::new(SortOptionsComparerFromStringComparer::new(
                            sys.vfs.string_comparer.clone(),
                        ))),
                        sort: Some(collections::SortOptionsSort::Insertion),
                    },
                    Option::<HashMap<String, Gc<Node>>>::None,
                )),
                _set_parent_nodes: set_parent_nodes,
                _outputs_map: GcCell::new(collections::SortedMap::new(
                    collections::SortOptions {
                        comparer: Gc::new(Box::new(SortOptionsComparerFromStringComparer::new(
                            sys.vfs.string_comparer.clone(),
                        ))),
                        sort: None,
                    },
                    Option::<HashMap<String, usize>>::None,
                )),
                outputs: Default::default(),
                traces: Default::default(),
                should_assert_invariants: !get_light_mode(),
                _parse_config_host: Default::default(),
                file_exists_override: Default::default(),
                directory_exists_override: Default::default(),
                read_file_override: Default::default(),
                write_file_override: Default::default(),
                realpath_override: Default::default(),
                get_directories_override: Default::default(),
            }));
            let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(dyn_wrapper.clone()) };
            *downcasted._dyn_wrapper.borrow_mut() = Some(dyn_wrapper);
            Ok(downcasted)
        }

        pub fn as_dyn_compiler_host(&self) -> Gc<Box<dyn typescript_rust::CompilerHost>> {
            self._dyn_wrapper.borrow().clone().unwrap()
        }

        pub fn outputs(&self) -> GcCellRef<Vec<Gc<documents::TextDocument>>> {
            self.outputs.borrow()
        }

        pub fn traces(&self) -> Ref<Vec<String>> {
            self.traces.borrow()
        }

        fn maybe_file_exists_override(&self) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
            self.file_exists_override.borrow().clone()
        }

        fn maybe_directory_exists_override(
            &self,
        ) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
            self.directory_exists_override.borrow().clone()
        }

        fn maybe_read_file_override(&self) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
            self.read_file_override.borrow().clone()
        }

        fn maybe_write_file_override(&self) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
            self.write_file_override.borrow().clone()
        }

        fn maybe_realpath_override(&self) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
            self.realpath_override.borrow().clone()
        }

        fn maybe_get_directories_override(
            &self,
        ) -> Option<Gc<Box<dyn ModuleResolutionHostOverrider>>> {
            self.get_directories_override.borrow().clone()
        }

        pub fn vfs(&self) -> Gc<vfs::FileSystem> {
            self.sys.vfs.clone()
        }

        pub fn parse_config_host(&self) -> io::Result<Gc<ParseConfigHost>> {
            let mut parse_config_host = self._parse_config_host.borrow_mut();
            if parse_config_host.is_none() {
                *parse_config_host = Some(Gc::new(ParseConfigHost::new(self.sys.clone())?));
            }
            Ok(parse_config_host.as_ref().unwrap().clone())
        }

        pub fn delete_file(&self, file_name: &str) {
            self.sys.delete_file(file_name);
        }

        pub fn get_modified_time(&self, file_name: &str) -> SystemTime {
            self.sys.get_modified_time(file_name).unwrap()
        }

        pub fn set_modified_time(&self, file_name: &str, time: SystemTime) {
            self.sys.set_modified_time(file_name, time);
        }
    }

    impl typescript_rust::CompilerHost for CompilerHost {
        fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
            self
        }

        fn get_current_directory(&self) -> io::Result<String> {
            self.sys.get_current_directory()
        }

        fn use_case_sensitive_file_names(&self) -> bool {
            self.sys.use_case_sensitive_file_names()
        }

        fn get_new_line(&self) -> String {
            self._new_line.clone()
        }

        fn get_canonical_file_name(&self, file_name: &str) -> String {
            if self.sys.use_case_sensitive_file_names() {
                file_name.to_owned()
            } else {
                file_name.to_lowercase()
            }
        }

        fn read_directory(
            &self,
            path: &str,
            extensions: &[&str],
            exclude: Option<&[String]>,
            include: &[String],
            depth: Option<usize>,
        ) -> Option<io::Result<Vec<String>>> {
            Some(
                self.sys
                    .read_directory(path, Some(extensions), exclude, Some(include), depth),
            )
        }

        fn is_read_directory_implemented(&self) -> bool {
            true
        }

        fn write_file(
            &self,
            file_name: &str,
            data: &str,
            write_byte_order_mark: bool,
            on_error: Option<&mut dyn FnMut(&str)>,
            source_files: Option<&[Gc<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            if let Some(write_file_override) = self.maybe_write_file_override() {
                write_file_override.write_file(
                    file_name,
                    data,
                    write_byte_order_mark,
                    on_error,
                    source_files,
                )
            } else {
                self.write_file_non_overridden(
                    file_name,
                    data,
                    write_byte_order_mark,
                    on_error,
                    source_files,
                )
            }
        }

        fn write_file_non_overridden(
            &self,
            file_name: &str,
            content: &str,
            write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Gc<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            let mut content = content.to_owned();
            if write_byte_order_mark {
                content = Utils::add_utf8_byte_order_mark(content);
            }
            // TODO: is this correct to ignore here?
            let _ = self.sys.write_file(file_name, &content, None);

            let mut document = documents::TextDocument::new(file_name.to_owned(), content, None);
            document
                .meta
                .insert("fileName".to_owned(), file_name.to_owned());
            let document = Gc::new(document);
            self.vfs()
                .filemeta(file_name)?
                .borrow_mut()
                .set("document", document.clone().into());
            let mut _outputs_map = self._outputs_map.borrow_mut();
            let mut outputs = self.outputs.borrow_mut();
            if !_outputs_map.has(&document.file) {
                _outputs_map.set(document.file.clone(), outputs.len());
                outputs.push(document.clone());
            }
            let index = *_outputs_map.get(&document.file).unwrap();
            outputs[index] = document;
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
                panic!(
                    "Trying to re-override set_overriding_write_file(), need eg a stack instead?"
                );
            }
            *write_file_override = overriding_write_file;
        }

        fn get_default_lib_location(&self) -> io::Result<Option<String>> {
            Ok(Some(vpath::resolve(
                &typescript_rust::CompilerHost::get_current_directory(self)?,
                &[Some(&self.default_lib_location)],
            )))
        }

        fn get_default_lib_file_name(&self, options: &CompilerOptions) -> io::Result<String> {
            Ok(vpath::resolve(
                &self.get_default_lib_location()?.unwrap(),
                &[Some(get_default_lib_file_name(options))],
            ))
        }

        fn get_source_file(
            &self,
            file_name: &str,
            language_version: ScriptTarget,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _should_create_new_source_file: Option<bool>,
        ) -> io::Result<Option<Gc<Node /*SourceFile*/>>> {
            let canonical_file_name = self.get_canonical_file_name(&vpath::resolve(
                &typescript_rust::CompilerHost::get_current_directory(self)?,
                &[Some(file_name)],
            ));
            let existing = self
                ._source_files
                .borrow()
                .get(&canonical_file_name)
                .cloned();
            if existing.is_some() {
                return Ok(existing);
            }

            let content = return_ok_default_if_none!(self.read_file(&canonical_file_name)?);

            let cache_key = if self.vfs().shadow_root().is_some() {
                Some(format!(
                    "SourceFile[languageVersion={language_version:?},setParentNodes={}]",
                    self._set_parent_nodes
                ))
            } else {
                None
            };
            if let Some(cache_key) = cache_key.as_ref() {
                let meta = self.vfs().filemeta(&canonical_file_name)?;
                let source_file_from_metadata = meta
                    .borrow()
                    .get(cache_key)
                    .map(|value| value.as_rc_node().clone());
                if let Some(source_file_from_metadata) =
                    source_file_from_metadata.filter(|source_file_from_metadata| {
                        source_file_from_metadata.get_full_text(None) == content
                    })
                {
                    self._source_files
                        .borrow_mut()
                        .set(canonical_file_name, source_file_from_metadata.clone());
                    return Ok(Some(source_file_from_metadata));
                }
            }

            let parsed = create_source_file(
                file_name,
                content,
                language_version,
                Some(self._set_parent_nodes || self.should_assert_invariants),
                None,
            )?;
            if self.should_assert_invariants {
                Utils::assert_invariants(Some(&parsed), None);
            }

            self._source_files
                .borrow_mut()
                .set(canonical_file_name.clone(), parsed.clone());

            if let Some(cache_key) = cache_key.as_ref() {
                let stats = self.vfs().stat_sync(&canonical_file_name)?;

                let mut fs = self.vfs();
                while let Some(fs_shadow_root) = fs.shadow_root() {
                    let shadow_root_stats =
                        if match fs_shadow_root.exists_sync(&canonical_file_name) {
                            Err(_) => break,
                            Ok(value) => value,
                        } {
                            match fs_shadow_root.stat_sync(&canonical_file_name) {
                                Err(_) => break,
                                Ok(value) => value,
                            }
                        } else {
                            break;
                        };

                    if shadow_root_stats.dev != stats.dev
                        || shadow_root_stats.ino != stats.ino
                        || shadow_root_stats.mtime_ms != stats.mtime_ms
                    {
                        break;
                    }

                    fs = fs_shadow_root;
                }

                if !Gc::ptr_eq(&fs, &self.vfs()) {
                    fs.filemeta(&canonical_file_name)?
                        .borrow_mut()
                        .set(cache_key, parsed.clone().into());
                }
            }

            Ok(Some(parsed))
        }

        fn is_resolve_module_names_supported(&self) -> bool {
            false
        }

        fn is_resolve_type_reference_directives_supported(&self) -> bool {
            false
        }

        fn is_on_release_old_source_file_supported(&self) -> bool {
            false
        }

        fn is_on_release_parsed_command_line_supported(&self) -> bool {
            false
        }

        fn is_create_directory_supported(&self) -> bool {
            false
        }

        fn set_overriding_create_directory(
            &self,
            _overriding_create_directory: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_get_source_file_by_path_supported(&self) -> bool {
            false
        }

        fn is_get_parsed_command_line_supported(&self) -> bool {
            false
        }
    }

    impl ModuleResolutionHost for CompilerHost {
        fn file_exists(&self, file_name: &str) -> bool {
            if let Some(file_exists_override) = self.maybe_file_exists_override() {
                file_exists_override.file_exists(file_name)
            } else {
                self.file_exists_non_overridden(file_name)
            }
        }

        fn file_exists_non_overridden(&self, file_name: &str) -> bool {
            self.sys.file_exists(file_name)
        }

        fn set_overriding_file_exists(
            &self,
            overriding_file_exists: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            let mut file_exists_override = self.file_exists_override.borrow_mut();
            if file_exists_override.is_some() && overriding_file_exists.is_some() {
                panic!(
                    "Trying to re-override set_overriding_file_exists(), need eg a stack instead?"
                );
            }
            *file_exists_override = overriding_file_exists;
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
            Some(self.sys.directory_exists(directory_name))
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
            Some(self.sys.get_directories(path))
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

        fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
            if let Some(read_file_override) = self.maybe_read_file_override() {
                read_file_override.read_file(file_name)
            } else {
                self.read_file_non_overridden(file_name)
            }
        }

        fn set_overriding_read_file(
            &self,
            overriding_read_file: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            let mut read_file_override = self.read_file_override.borrow_mut();
            if read_file_override.is_some() && overriding_read_file.is_some() {
                panic!(
                    "Trying to re-override set_overriding_read_file(), need eg a stack instead?"
                );
            }
            *read_file_override = overriding_read_file;
        }

        fn read_file_non_overridden(&self, path: &str) -> io::Result<Option<String>> {
            self.sys.read_file(path)
        }

        fn trace(&self, s: &str) {
            self.traces.borrow_mut().push(s.to_owned());
        }

        fn is_trace_supported(&self) -> bool {
            true
        }

        fn realpath(&self, path: &str) -> Option<String> {
            if let Some(realpath_override) = self.maybe_realpath_override() {
                realpath_override.realpath(path)
            } else {
                self.realpath_non_overridden(path)
            }
        }

        fn realpath_non_overridden(&self, path: &str) -> Option<String> {
            self.sys.realpath(path)
        }

        fn is_realpath_supported(&self) -> bool {
            self.sys.is_realpath_supported()
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

        fn get_current_directory(&self) -> Option<io::Result<String>> {
            Some(typescript_rust::CompilerHost::get_current_directory(self))
        }

        fn use_case_sensitive_file_names(&self) -> Option<bool> {
            Some(typescript_rust::CompilerHost::use_case_sensitive_file_names(self))
        }
    }

    pub enum RcSystemOrRcFileSystem {
        RcSystem(Gc<System>),
        RcFileSystem(Gc<vfs::FileSystem>),
    }

    impl From<Gc<System>> for RcSystemOrRcFileSystem {
        fn from(value: Gc<System>) -> Self {
            Self::RcSystem(value)
        }
    }

    impl From<Gc<vfs::FileSystem>> for RcSystemOrRcFileSystem {
        fn from(value: Gc<vfs::FileSystem>) -> Self {
            Self::RcFileSystem(value)
        }
    }
}
