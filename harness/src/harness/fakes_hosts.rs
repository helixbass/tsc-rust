pub mod fakes {
    use std::collections::HashMap;
    use std::rc::Rc;
    use typescript_rust::{get_new_line_character, CompilerOptions, Node};
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
        pub outputs: Vec<documents::TextDocument>,
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

    // impl typescript_rust::CompilerHost for CompilerHost {}
}
