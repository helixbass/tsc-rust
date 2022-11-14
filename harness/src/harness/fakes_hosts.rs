pub mod fakes {
    use std::borrow::Cow;
    use std::cell::{Ref, RefCell};
    use std::collections::HashMap;
    use std::io;
    use std::rc::Rc;
    use std::time::SystemTime;
    use typescript_rust::{
        create_source_file, get_default_lib_file_name, get_new_line_character, CompilerOptions,
        ConvertToTSConfigHost, DirectoryWatcherCallback, ExitStatus, FileWatcher,
        FileWatcherCallback, ModuleResolutionHost, ModuleResolutionHostOverrider, Node,
        ScriptTarget, System as _, WatchOptions,
    };
    use typescript_services_rust::{get_default_compiler_options, NodeServicesInterface};

    use crate::{collections, documents, get_light_mode, vfs, vpath, Utils};

    #[derive(Default)]
    pub struct SystemOptions {
        pub executing_file_path: Option<String>,
        pub new_line: Option<&'static str /*"\r\n" | "\n"*/>,
        pub env: Option<HashMap<String, String>>,
    }

    pub struct System {
        pub vfs: Rc<vfs::FileSystem>,
        pub args: Vec<String>,
        pub output: Vec<String>,
        pub new_line: &'static str,
        pub use_case_sensitive_file_names: bool,
        pub exit_code: Option<u32>,

        _executing_file_path: Option<String>,
        _env: Option<HashMap<String, String>>,
    }

    impl System {
        pub fn new(vfs: Rc<vfs::FileSystem>, options: Option<SystemOptions>) -> Self {
            let SystemOptions {
                executing_file_path,
                new_line,
                env,
            } = options.unwrap_or_default();
            let new_line = new_line.unwrap_or("\r\n");
            let use_case_sensitive_file_names = !vfs.ignore_case;
            Self {
                args: vec![],
                output: vec![],
                exit_code: None,
                vfs: if vfs.is_readonly() {
                    Rc::new(vfs.shadow(None))
                } else {
                    vfs
                },
                use_case_sensitive_file_names,
                new_line,
                _executing_file_path: executing_file_path,
                _env: env,
            }
        }
    }

    impl typescript_rust::System for System {
        fn args(&self) -> &[String] {
            unimplemented!()
        }

        fn new_line(&self) -> &str {
            unimplemented!()
        }

        fn write(&self, s: &str) {
            unimplemented!()
        }

        fn write_output_is_tty(&self) -> Option<bool> {
            Some(true)
        }

        fn get_width_of_terminal(&self) -> Option<usize> {
            unimplemented!()
        }

        fn read_file(&self, path: &str) -> io::Result<Option<String>> {
            unimplemented!()
        }

        fn get_file_size(&self, path: &str) -> Option<usize> {
            unimplemented!()
        }

        fn write_file(
            &self,
            path: &str,
            data: &str,
            write_byte_order_mark: Option<bool>,
        ) -> io::Result<()> {
            unimplemented!()
        }

        fn is_watch_file_supported(&self) -> bool {
            unimplemented!()
        }

        fn watch_file(
            &self,
            path: &str,
            callback: FileWatcherCallback,
            polling_interval: Option<u32>,
            options: Option<&WatchOptions>,
        ) -> Rc<dyn FileWatcher> {
            unimplemented!()
        }

        fn is_watch_directory_supported(&self) -> bool {
            unimplemented!()
        }

        fn watch_directory(
            &self,
            path: &str,
            callback: DirectoryWatcherCallback,
            recursive: Option<bool>,
            options: Option<&WatchOptions>,
        ) -> Rc<dyn FileWatcher> {
            unimplemented!()
        }

        fn resolve_path(&self, path: &str) -> String {
            unimplemented!()
        }

        fn file_exists(&self, path: &str) -> bool {
            unimplemented!()
        }

        fn directory_exists(&self, path: &str) -> bool {
            unimplemented!()
        }

        fn create_directory(&self, path: &str) {
            unimplemented!()
        }

        fn get_executing_file_path(&self) -> Cow<'static, str> {
            unimplemented!()
        }

        fn get_directories(&self, path: &str) -> Vec<String> {
            unimplemented!()
        }

        fn read_directory(
            &self,
            path: &str,
            extensions: Option<&[&str]>,
            excludes: Option<&[String]>,
            includes: Option<&[String]>,
            depth: Option<usize>,
        ) -> Vec<String> {
            unimplemented!()
        }

        fn is_get_modified_time_supported(&self) -> bool {
            true
        }

        fn get_modified_time(&self, path: &str) -> Option<SystemTime> {
            unimplemented!()
        }

        fn is_set_modified_time_supported(&self) -> bool {
            true
        }

        fn set_modified_time(&self, path: &str, time: SystemTime) {
            unimplemented!()
        }

        fn is_delete_file_supported(&self) -> bool {
            true
        }

        fn delete_file(&self, path: &str) {
            unimplemented!()
        }

        fn is_create_hash_supported(&self) -> bool {
            true
        }

        fn create_hash(&self, data: &str) -> String {
            unimplemented!()
        }

        fn exit(&self, exit_code: Option<ExitStatus>) -> ! {
            unimplemented!()
        }

        fn realpath(&self, path: &str) -> Option<String> {
            unimplemented!()
        }

        fn is_realpath_supported(&self) -> bool {
            true
        }

        fn get_environment_variable(&self, name: &str) -> String {
            unimplemented!()
        }

        fn is_clear_screen_supported(&self) -> bool {
            false
        }

        fn now(&self) -> Option<SystemTime> {
            unimplemented!()
        }

        fn as_convert_to_tsconfig_host(&self) -> &dyn ConvertToTSConfigHost {
            unimplemented!()
        }
    }

    impl ConvertToTSConfigHost for System {
        fn get_current_directory(&self) -> String {
            self.vfs.cwd()
        }

        fn use_case_sensitive_file_names(&self) -> bool {
            self.use_case_sensitive_file_names
        }
    }

    pub struct ParseConfigHost {
        pub sys: Rc<System>,
    }

    impl ParseConfigHost {
        pub fn new<TSys: Into<RcSystemOrRcFileSystem>>(sys: TSys) -> Self {
            let sys = sys.into();
            let sys = match sys {
                RcSystemOrRcFileSystem::RcSystem(sys) => sys,
                RcSystemOrRcFileSystem::RcFileSystem(sys) => Rc::new(System::new(sys, None)),
            };
            Self { sys }
        }
    }

    pub struct CompilerHost {
        pub sys: Rc<System>,
        pub default_lib_location: String,
        outputs: RefCell<Vec<Rc<documents::TextDocument>>>,
        _outputs_map: RefCell<collections::SortedMap<String, usize>>,
        traces: RefCell<Vec<String>>,
        pub should_assert_invariants: bool,

        _set_parent_nodes: bool,
        _source_files: RefCell<collections::SortedMap<String, Rc<Node /*SourceFile*/>>>,
        _parse_config_host: RefCell<Option<Rc<ParseConfigHost>>>,
        _new_line: String,

        file_exists_override: RefCell<Option<Rc<dyn ModuleResolutionHostOverrider>>>,
        directory_exists_override: RefCell<Option<Rc<dyn ModuleResolutionHostOverrider>>>,
        read_file_override: RefCell<Option<Rc<dyn ModuleResolutionHostOverrider>>>,
        write_file_override: RefCell<Option<Rc<dyn ModuleResolutionHostOverrider>>>,
        realpath_override: RefCell<Option<Rc<dyn ModuleResolutionHostOverrider>>>,
        get_directories_override: RefCell<Option<Rc<dyn ModuleResolutionHostOverrider>>>,
    }

    impl CompilerHost {
        pub fn new<TSys: Into<RcSystemOrRcFileSystem>>(
            sys: TSys,
            options: Option<Rc<CompilerOptions>>,
            set_parent_nodes: Option<bool>,
        ) -> Self {
            let sys = sys.into();
            let options = options.unwrap_or_else(|| Rc::new(get_default_compiler_options()));
            let set_parent_nodes = set_parent_nodes.unwrap_or(false);
            let sys = match sys {
                RcSystemOrRcFileSystem::RcSystem(sys) => sys,
                RcSystemOrRcFileSystem::RcFileSystem(sys) => Rc::new(System::new(sys, None)),
            };
            Self {
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
                _source_files: RefCell::new(collections::SortedMap::new(
                    collections::SortOptions {
                        comparer: {
                            let sys_vfs_string_comparer = sys.vfs.string_comparer.clone();
                            Rc::new(move |a: &String, b: &String| sys_vfs_string_comparer(a, b))
                        },
                        sort: Some(collections::SortOptionsSort::Insertion),
                    },
                    Option::<HashMap<String, Rc<Node>>>::None,
                )),
                _set_parent_nodes: set_parent_nodes,
                _outputs_map: RefCell::new(collections::SortedMap::new(
                    collections::SortOptions {
                        comparer: {
                            let sys_vfs_string_comparer = sys.vfs.string_comparer.clone();
                            Rc::new(move |a: &String, b: &String| sys_vfs_string_comparer(a, b))
                        },
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
            }
        }

        pub fn outputs(&self) -> Ref<Vec<Rc<documents::TextDocument>>> {
            self.outputs.borrow()
        }

        fn maybe_file_exists_override(&self) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
            self.file_exists_override.borrow().clone()
        }

        fn maybe_directory_exists_override(&self) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
            self.directory_exists_override.borrow().clone()
        }

        fn maybe_read_file_override(&self) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
            self.read_file_override.borrow().clone()
        }

        fn maybe_write_file_override(&self) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
            self.write_file_override.borrow().clone()
        }

        fn maybe_realpath_override(&self) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
            self.realpath_override.borrow().clone()
        }

        fn maybe_get_directories_override(&self) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
            self.get_directories_override.borrow().clone()
        }

        pub fn vfs(&self) -> Rc<vfs::FileSystem> {
            self.sys.vfs.clone()
        }

        pub fn parse_config_host(&self) -> Rc<ParseConfigHost> {
            self._parse_config_host
                .borrow_mut()
                .get_or_insert_with(|| Rc::new(ParseConfigHost::new(self.sys.clone())))
                .clone()
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

        fn get_current_directory(&self) -> String {
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
        ) -> Option<Vec<String>> {
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
            source_files: Option<&[Rc<Node /*SourceFile*/>]>,
        ) {
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
            on_error: Option<&mut dyn FnMut(&str)>,
            source_files: Option<&[Rc<Node /*SourceFile*/>]>,
        ) {
            let mut content = content.to_owned();
            if write_byte_order_mark {
                content = Utils::add_utf8_byte_order_mark(content);
            }
            self.sys.write_file(file_name, &content, None);

            let mut document = documents::TextDocument::new(file_name.to_owned(), content, None);
            document
                .meta
                .insert("fileName".to_owned(), file_name.to_owned());
            let document = Rc::new(document);
            self.vfs()
                .filemeta(file_name)
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
        }

        fn is_write_file_supported(&self) -> bool {
            true
        }

        fn set_overriding_write_file(
            &self,
            overriding_write_file: Option<Rc<dyn ModuleResolutionHostOverrider>>,
        ) {
            let mut write_file_override = self.write_file_override.borrow_mut();
            if write_file_override.is_some() && overriding_write_file.is_some() {
                panic!(
                    "Trying to re-override set_overriding_write_file(), need eg a stack instead?"
                );
            }
            *write_file_override = overriding_write_file;
        }

        fn get_default_lib_location(&self) -> Option<String> {
            Some(vpath::resolve(
                &typescript_rust::CompilerHost::get_current_directory(self),
                &[Some(&self.default_lib_location)],
            ))
        }

        fn get_default_lib_file_name(&self, options: &CompilerOptions) -> String {
            vpath::resolve(
                &self.get_default_lib_location().unwrap(),
                &[Some(get_default_lib_file_name(options))],
            )
        }

        fn get_source_file(
            &self,
            file_name: &str,
            language_version: ScriptTarget,
            on_error: Option<&mut dyn FnMut(&str)>,
            should_create_new_source_file: Option<bool>,
        ) -> Option<Rc<Node /*SourceFile*/>> {
            let canonical_file_name = self.get_canonical_file_name(&vpath::resolve(
                &typescript_rust::CompilerHost::get_current_directory(self),
                &[Some(file_name)],
            ));
            let existing = self
                ._source_files
                .borrow()
                .get(&canonical_file_name)
                .cloned();
            if existing.is_some() {
                return existing;
            }

            let content = self.read_file(&canonical_file_name).ok().flatten()?;

            let cache_key = if self.vfs().shadow_root().is_some() {
                Some(format!(
                    "SourceFile[languageVersion={language_version:?},setParentNodes={}]",
                    self._set_parent_nodes
                ))
            } else {
                None
            };
            if let Some(cache_key) = cache_key.as_ref() {
                let meta = self.vfs().filemeta(&canonical_file_name);
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
                    return Some(source_file_from_metadata);
                }
            }

            let parsed = create_source_file(
                file_name,
                content,
                language_version,
                Some(self._set_parent_nodes || self.should_assert_invariants),
                None,
            );
            if self.should_assert_invariants {
                Utils::assert_invariants(Some(&parsed), None);
            }

            self._source_files
                .borrow_mut()
                .set(canonical_file_name.clone(), parsed.clone());

            if let Some(cache_key) = cache_key.as_ref() {
                let stats = self.vfs().stat_sync(&canonical_file_name);

                let mut fs = self.vfs();
                while let Some(fs_shadow_root) = fs.shadow_root() {
                    // try {
                    let shadow_root_stats = if fs_shadow_root.exists_sync(&canonical_file_name) {
                        fs_shadow_root.stat_sync(&canonical_file_name)
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
                    // }
                    // catch {
                    //     break;
                    // }
                }

                if !Rc::ptr_eq(&fs, &self.vfs()) {
                    fs.filemeta(&canonical_file_name)
                        .borrow_mut()
                        .set(cache_key, parsed.clone().into());
                }
            }

            Some(parsed)
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
            overriding_create_directory: Option<Rc<dyn ModuleResolutionHostOverrider>>,
        ) {
            unreachable!()
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
            overriding_file_exists: Option<Rc<dyn ModuleResolutionHostOverrider>>,
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
            overriding_directory_exists: Option<Rc<dyn ModuleResolutionHostOverrider>>,
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
            overriding_get_directories: Option<Rc<dyn ModuleResolutionHostOverrider>>,
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
            overriding_read_file: Option<Rc<dyn ModuleResolutionHostOverrider>>,
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
            overriding_realpath: Option<Rc<dyn ModuleResolutionHostOverrider>>,
        ) {
            let mut realpath_override = self.realpath_override.borrow_mut();
            if realpath_override.is_some() && overriding_realpath.is_some() {
                panic!("Trying to re-override set_overriding_realpath(), need eg a stack instead?");
            }
            *realpath_override = overriding_realpath;
        }

        fn get_current_directory(&self) -> Option<String> {
            Some(typescript_rust::CompilerHost::get_current_directory(self))
        }

        fn use_case_sensitive_file_names(&self) -> Option<bool> {
            Some(typescript_rust::CompilerHost::use_case_sensitive_file_names(self))
        }
    }

    pub enum RcSystemOrRcFileSystem {
        RcSystem(Rc<System>),
        RcFileSystem(Rc<vfs::FileSystem>),
    }

    impl From<Rc<System>> for RcSystemOrRcFileSystem {
        fn from(value: Rc<System>) -> Self {
            Self::RcSystem(value)
        }
    }

    impl From<Rc<vfs::FileSystem>> for RcSystemOrRcFileSystem {
        fn from(value: Rc<vfs::FileSystem>) -> Self {
            Self::RcFileSystem(value)
        }
    }
}
