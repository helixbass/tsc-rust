pub mod fakes {
    use std::collections::HashMap;
    use std::io;
    use std::rc::Rc;
    use typescript_rust::{
        get_new_line_character, CompilerOptions, ModuleResolutionHost,
        ModuleResolutionHostOverrider, Node, ScriptTarget,
    };
    use typescript_services_rust::get_default_compiler_options;

    use crate::{collections, documents, get_light_mode, vfs};

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

    pub struct ParseConfigHost {}

    pub struct CompilerHost {
        pub sys: Rc<System>,
        pub default_lib_location: String,
        pub outputs: Vec<Rc<documents::TextDocument>>,
        _outputs_map: collections::SortedMap<String, usize>,
        pub traces: Vec<String>,
        pub should_assert_invariants: bool,

        _set_parent_nodes: bool,
        _source_files: collections::SortedMap<String, Rc<Node /*SourceFile*/>>,
        _parse_config_host: Option<Rc<ParseConfigHost>>,
        _new_line: String,
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
                _source_files: collections::SortedMap::new(
                    collections::SortOptions {
                        comparer: {
                            let sys_vfs_string_comparer = sys.vfs.string_comparer.clone();
                            Rc::new(move |a: &String, b: &String| sys_vfs_string_comparer(a, b))
                        },
                        sort: Some(collections::SortOptionsSort::Insertion),
                    },
                    Option::<HashMap<String, Rc<Node>>>::None,
                ),
                _set_parent_nodes: set_parent_nodes,
                _outputs_map: collections::SortedMap::new(
                    collections::SortOptions {
                        comparer: {
                            let sys_vfs_string_comparer = sys.vfs.string_comparer.clone();
                            Rc::new(move |a: &String, b: &String| sys_vfs_string_comparer(a, b))
                        },
                        sort: None,
                    },
                    Option::<HashMap<String, usize>>::None,
                ),
                outputs: vec![],
                traces: vec![],
                should_assert_invariants: !get_light_mode(),
                _parse_config_host: None,
            }
        }

        pub fn vfs(&self) -> Rc<vfs::FileSystem> {
            self.sys.vfs.clone()
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

    impl typescript_rust::CompilerHost for CompilerHost {
        fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
            unimplemented!()
        }

        fn get_source_file(
            &self,
            file_name: &str,
            language_version: ScriptTarget,
            on_error: Option<&mut dyn FnMut(&str)>,
            should_create_new_source_file: Option<bool>,
        ) -> Option<Rc<Node /*SourceFile*/>> {
            unimplemented!()
        }

        fn get_default_lib_file_name(&self, options: &CompilerOptions) -> String {
            unimplemented!()
        }

        fn get_default_lib_location(&self) -> Option<String> {
            unimplemented!()
        }

        fn write_file(
            &self,
            file_name: &str,
            data: &str,
            write_byte_order_mark: bool,
            on_error: Option<&mut dyn FnMut(&str)>,
            source_files: Option<&[Rc<Node /*SourceFile*/>]>,
        ) {
            unimplemented!()
        }

        fn write_file_non_overridden(
            &self,
            file_name: &str,
            data: &str,
            write_byte_order_mark: bool,
            on_error: Option<&mut dyn FnMut(&str)>,
            source_files: Option<&[Rc<Node /*SourceFile*/>]>,
        ) {
            unimplemented!()
        }

        fn is_write_file_supported(&self) -> bool {
            unimplemented!()
        }

        fn set_overriding_write_file(
            &self,
            overriding_write_file: Option<Rc<dyn ModuleResolutionHostOverrider>>,
        ) {
            unimplemented!()
        }

        fn get_current_directory(&self) -> String {
            unimplemented!()
        }

        fn get_canonical_file_name(&self, file_name: &str) -> String {
            unimplemented!()
        }

        fn use_case_sensitive_file_names(&self) -> bool {
            unimplemented!()
        }

        fn get_new_line(&self) -> String {
            unimplemented!()
        }

        fn read_directory(
            &self,
            root_dir: &str,
            extensions: &[&str],
            excludes: Option<&[String]>,
            includes: &[String],
            depth: Option<usize>,
        ) -> Option<Vec<String>> {
            unimplemented!()
        }

        fn is_read_directory_implemented(&self) -> bool {
            unimplemented!()
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
            unimplemented!()
        }

        fn file_exists_non_overridden(&self, file_name: &str) -> bool {
            unimplemented!()
        }

        fn set_overriding_file_exists(
            &self,
            overriding_file_exists: Option<Rc<dyn ModuleResolutionHostOverrider>>,
        ) {
            unimplemented!()
        }

        fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
            unimplemented!()
        }

        fn set_overriding_read_file(
            &self,
            overriding_read_file: Option<Rc<dyn ModuleResolutionHostOverrider>>,
        ) {
            unimplemented!()
        }

        fn read_file_non_overridden(&self, file_name: &str) -> io::Result<Option<String>> {
            unimplemented!()
        }

        fn trace(&self, s: &str) {
            unimplemented!()
        }

        fn is_trace_supported(&self) -> bool {
            unimplemented!()
        }

        fn directory_exists(&self, directory_name: &str) -> Option<bool> {
            unimplemented!()
        }

        fn is_directory_exists_supported(&self) -> bool {
            unimplemented!()
        }

        fn directory_exists_non_overridden(&self, directory_name: &str) -> Option<bool> {
            unimplemented!()
        }

        fn set_overriding_directory_exists(
            &self,
            overriding_directory_exists: Option<Rc<dyn ModuleResolutionHostOverrider>>,
        ) {
            unimplemented!()
        }

        fn realpath(&self, path: &str) -> Option<String> {
            unimplemented!()
        }

        fn realpath_non_overridden(&self, path: &str) -> Option<String> {
            unimplemented!()
        }

        fn is_realpath_supported(&self) -> bool {
            unimplemented!()
        }

        fn set_overriding_realpath(
            &self,
            overriding_realpath: Option<Rc<dyn ModuleResolutionHostOverrider>>,
        ) {
            unimplemented!()
        }

        fn get_current_directory(&self) -> Option<String> {
            unimplemented!()
        }

        fn get_directories(&self, path: &str) -> Option<Vec<String>> {
            unimplemented!()
        }

        fn is_get_directories_supported(&self) -> bool {
            unimplemented!()
        }

        fn get_directories_non_overridden(&self, path: &str) -> Option<Vec<String>> {
            unimplemented!()
        }

        fn set_overriding_get_directories(
            &self,
            overriding_get_directories: Option<Rc<dyn ModuleResolutionHostOverrider>>,
        ) {
            unimplemented!()
        }

        fn use_case_sensitive_file_names(&self) -> Option<bool> {
            unimplemented!()
        }
    }
}
