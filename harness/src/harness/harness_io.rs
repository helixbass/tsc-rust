use gc::{Finalize, Gc, GcCell, Trace};
use regex::Regex;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::io;
use std::iter::FromIterator;
use std::mem;
use std::path::{Path as StdPath, PathBuf};

use typescript_rust::{
    compare_strings_case_insensitive, compare_strings_case_sensitive, comparison_to_ordering,
    ends_with, equate_strings_case_insensitive, find, find_index, for_each, fs_exists_sync,
    fs_mkdir_sync, fs_readdir_sync, fs_stat_sync, fs_unlink_sync, get_base_file_name, get_sys, map,
    normalize_slashes, option_declarations, ordered_remove_item_at, path_join, starts_with,
    CommandLineOption, CommandLineOptionInterface, CommandLineOptionMapTypeValue,
    CommandLineOptionType, Extension, FileSystemEntries, StatLike,
};

use crate::{vfs, vpath, RunnerBase, StringOrFileBasedTest};

pub trait IO: vfs::FileSystemResolverHost {
    fn new_line(&self) -> &'static str;
    fn get_current_directory(&self) -> String;
    fn read_file(&self, path: &StdPath) -> Option<String>;
    fn write_file(&self, path: &StdPath, contents: &str);
    fn directory_name(&self, path: &StdPath) -> Option<String>;
    fn create_directory(&self, path: &StdPath) -> io::Result<()>;
    fn delete_file(&self, file_name: &StdPath);
    fn enumerate_test_files(&self, runner: &RunnerBase) -> Vec<StringOrFileBasedTest>;
    fn list_files(
        &self,
        path: &str,
        filter: Option<&Regex>,
        options: Option<ListFilesOptions>,
    ) -> Vec<PathBuf>;
    fn join_path(&self, components: &[&StdPath]) -> String;
    // fn as_file_system_resolver_host(self: Rc<Self>) -> Rc<dyn vfs::FileSystemResolverHost>;
    fn as_file_system_resolver_host(&self) -> Gc<Box<dyn vfs::FileSystemResolverHost>>;
}

#[derive(Copy, Clone)]
pub struct ListFilesOptions {
    pub recursive: Option<bool>,
}

thread_local! {
    // static IO_: GcCell<Gc<Box<dyn IO>>> = GcCell::new(create_node_io());
    static IO_: GcCell<Gc<Box<NodeIO>>> = GcCell::new(create_node_io());
}

pub fn with_io<TReturn, TCallback: FnMut(&dyn IO) -> TReturn>(mut callback: TCallback) -> TReturn {
    IO_.with(|io| callback(&***io.borrow()))
}

// pub fn get_io() -> Gc<Box<dyn IO>> {
pub fn get_io() -> Gc<Box<NodeIO>> {
    IO_.with(|io| io.borrow().clone())
}

pub const harness_new_line: &'static str = "\r\n";

fn create_node_io() -> Gc<Box<NodeIO>> {
    NodeIO::new()
}

#[derive(Trace, Finalize)]
// struct NodeIO {
pub struct NodeIO {
    _dyn_wrapper: GcCell<Option<Gc<Box<dyn vfs::FileSystemResolverHost>>>>,
}

impl NodeIO {
    pub fn new() -> Gc<Box<Self>> {
        let dyn_wrapper: Gc<Box<dyn vfs::FileSystemResolverHost>> = Gc::new(Box::new(Self {
            _dyn_wrapper: Default::default(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(dyn_wrapper.clone()) };
        *downcasted._dyn_wrapper.borrow_mut() = Some(dyn_wrapper);
        downcasted
    }

    fn files_in_folder<TFolder: AsRef<StdPath>>(
        &self,
        spec: Option<&Regex>,
        options: Option<ListFilesOptions>,
        folder: TFolder,
    ) -> Vec<PathBuf> {
        let folder = folder.as_ref();
        let options = options.unwrap_or_else(|| ListFilesOptions { recursive: None });
        let mut paths: Vec<PathBuf> = vec![];

        for file in fs_readdir_sync(folder).unwrap() {
            let path_to_file = path_join(&[folder.as_ref(), &file.as_ref()]);
            if !fs_exists_sync(&path_to_file) {
                continue;
            }
            let stat = fs_stat_sync(&path_to_file).unwrap();
            if options.recursive == Some(true) && stat.is_directory() {
                paths.append(&mut self.files_in_folder(spec, Some(options), &path_to_file));
            } else if stat.is_file()
                && match spec {
                    None => true,
                    Some(spec) => spec.is_match(file.to_str().unwrap()),
                }
            {
                paths.push(path_to_file);
            }
        }

        paths
    }
}

impl IO for NodeIO {
    fn new_line(&self) -> &'static str {
        harness_new_line
    }

    fn delete_file(&self, file_name: &StdPath) {
        let _ = fs_unlink_sync(file_name);
    }

    fn directory_name(&self, path: &StdPath) -> Option<String> {
        path.parent().map(|path| path.to_str().unwrap().to_owned())
    }

    fn join_path(&self, components: &[&StdPath]) -> String {
        path_join(components).to_str().unwrap().to_owned()
    }

    fn get_current_directory(&self) -> String {
        get_sys().get_current_directory()
    }

    fn read_file(&self, path: &StdPath) -> Option<String> {
        get_sys().read_file(path.to_str().unwrap()).unwrap()
    }

    fn write_file(&self, path: &StdPath, content: &str) {
        get_sys()
            .write_file(path.to_str().unwrap(), content, None)
            .unwrap()
    }

    fn enumerate_test_files(&self, runner: &RunnerBase) -> Vec<StringOrFileBasedTest> {
        runner.get_test_files()
    }

    fn create_directory(&self, path: &StdPath) -> io::Result<()> {
        fs_mkdir_sync(path).or_else(|_| {
            // if (e.code === "ENOENT") {
            let path_as_str = path.to_str().unwrap();
            self.create_directory(vpath::dirname(path_as_str).as_ref())
                .and_then(|_| self.create_directory(path))
            // }
            // else if (!ts.sys.directoryExists(path)) {
            //     throw e;
            // }
        })
    }

    fn list_files(
        &self,
        path: &str,
        spec: Option<&Regex>,
        options: Option<ListFilesOptions>,
    ) -> Vec<PathBuf> {
        self.files_in_folder(spec, options, path)
    }

    fn as_file_system_resolver_host(&self) -> Gc<Box<dyn vfs::FileSystemResolverHost>> {
        self._dyn_wrapper.borrow().clone().unwrap()
    }
}

impl vfs::FileSystemResolverHost for NodeIO {
    fn get_accessible_file_system_entries(&self, dirname: &str) -> FileSystemEntries {
        // try {
        let mut entries = fs_readdir_sync(if !dirname.is_empty() { dirname } else { "." })
            .unwrap_or_default()
            .into_iter()
            .map(|os_string| os_string.into_string().unwrap())
            .collect::<Vec<_>>();
        let use_case_sensitive_file_names = get_sys().use_case_sensitive_file_names();
        entries.sort_by(|a: &String, b: &String| {
            comparison_to_ordering(if use_case_sensitive_file_names {
                compare_strings_case_sensitive(a, b)
            } else {
                compare_strings_case_insensitive(a, b)
            })
        });
        let mut files: Vec<String> = vec![];
        let mut directories: Vec<String> = vec![];
        for entry in entries {
            if matches!(&*entry, "." | "..") {
                continue;
            }
            let name = vpath::combine(dirname, &[Some(&entry)]);
            // try {
            let stat = fs_stat_sync(&name);
            if stat.is_none() {
                continue;
            }
            let stat = stat.unwrap();
            if stat.is_file() {
                files.push(entry);
            } else if stat.is_directory() {
                directories.push(entry);
            }
            // }
            // catch { /*ignore*/ }
        }
        FileSystemEntries { files, directories }
        // }
        // catch (e) {
        //     return { files: [], directories: [] };
        // }
    }

    fn get_file_size(&self, path: &str) -> usize {
        get_sys().get_file_size(path).unwrap()
    }

    fn read_file(&self, path: &str) -> Option<String> {
        get_sys().read_file(path).ok().flatten()
    }

    fn file_exists(&self, path: &str) -> bool {
        get_sys().file_exists(path)
    }

    fn directory_exists(&self, path: &str) -> bool {
        get_sys().directory_exists(path)
    }

    fn get_workspace_root(&self) -> String {
        "/Users/jrosse/prj/tsc-rust/typescript_rust/typescript_src/".to_owned()
    }
}

pub const lib_folder: &'static str = "built/local/";

// pub const user_specified_root: &'static str = "";
pub const user_specified_root: &'static str = "../typescript_rust/typescript_src/";

thread_local! {
    static light_mode_: Cell<bool> = Cell::new(false);
}

pub fn get_light_mode() -> bool {
    light_mode_.with(|light_mode| light_mode.get())
}

pub fn set_light_mode(flag: bool) {
    light_mode_.with(|light_mode| {
        light_mode.set(flag);
    })
}

pub mod Compiler {
    use gc::{Finalize, Gc, GcCell, Trace};
    use regex::Regex;
    use std::cell::RefCell;
    use std::cmp;
    use std::collections::HashMap;
    use std::convert::TryInto;

    use typescript_rust::{
        compare_diagnostics, compare_paths, compute_line_starts, count_where,
        create_get_canonical_file_name, diagnostic_category_name, file_extension_is,
        flatten_diagnostic_message_text, format_diagnostics,
        format_diagnostics_with_color_and_context, format_location, get_emit_script_target,
        get_error_count_for_summary, get_error_summary_text, get_normalized_absolute_path, map,
        normalize_slashes, option_declarations, parse_custom_type_option, parse_list_type_option,
        regex, sort, starts_with, text_span_end, to_path, CommandLineOption, CommandLineOptionBase,
        CommandLineOptionInterface, CommandLineOptionOfBooleanType, CommandLineOptionOfStringType,
        CommandLineOptionType, Comparison, CompilerOptions, CompilerOptionsBuilder,
        CompilerOptionsValue, Diagnostic, DiagnosticInterface,
        DiagnosticRelatedInformationInterface, Extension, FormatDiagnosticsHost, NewLineKind,
        Number, StringOrDiagnosticMessage, TextSpan,
    };

    use super::{is_built_file, is_default_library_file, Baseline, TestCaseParser};

    use crate::{compiler, documents, fakes, get_io, vfs, vpath, with_io, Utils, IO};

    pub fn get_canonical_file_name(file_name: &str) -> String {
        file_name.to_owned()
    }

    #[derive(Default)]
    struct HarnessOptions {
        pub use_case_sensitive_file_names: Option<bool>,
        pub include_built_file: Option<String>,
        pub baseline_file: Option<String>,
        pub lib_files: Option<String>,
        pub no_types_and_symbols: Option<bool>,
        // these aren't in the Typescript version but look like they should be?
        pub file_name: Option<String>,
        pub no_implicit_references: Option<bool>,
        pub current_directory: Option<String>,
        pub symlink: Option<String>,
        pub link: Option<String>,
        pub full_emit_paths: Option<bool>,
    }

    impl HarnessOptions {
        pub fn set_value_from_command_line_option(
            &mut self,
            option: &CommandLineOption,
            value: CompilerOptionsValue,
        ) {
            self.set_value(option.name(), value)
        }

        pub fn set_value(&mut self, option_name: &str, value: CompilerOptionsValue) {
            match option_name {
                "useCaseSensitiveFileNames" => {
                    self.use_case_sensitive_file_names = value.into_option_bool();
                }
                "includeBuiltFile" => {
                    self.include_built_file = value.into_option_string();
                }
                "baselineFile" => {
                    self.baseline_file = value.into_option_string();
                }
                "libFiles" => {
                    self.lib_files = value.into_option_string();
                }
                "noTypesAndSymbols" => {
                    self.no_types_and_symbols = value.into_option_bool();
                }
                "fileName" => {
                    self.file_name = value.into_option_string();
                }
                "noImplicitReferences" => {
                    self.no_implicit_references = value.into_option_bool();
                }
                "currentDirectory" => {
                    self.current_directory = value.into_option_string();
                }
                "symlink" => {
                    self.symlink = value.into_option_string();
                }
                "link" => {
                    self.link = value.into_option_string();
                }
                "fullEmitPaths" => {
                    self.full_emit_paths = value.into_option_bool();
                }
                _ => panic!("Unknown compiler option: {:?}", option_name),
            }
        }

        fn is_harness_option(name: &str) -> bool {
            matches!(
                &*name.to_lowercase(),
                "usecasesensitivefilenames"
                    | "includebuiltfile"
                    | "baselinefile"
                    | "libfiles"
                    | "notypesandsymbols"
                    | "filename"
                    | "noimplicitreferences"
                    | "currentdirectory"
                    | "symlink"
                    | "link"
                    | "fullemitpaths"
            )
        }
    }

    thread_local! {
        pub(crate) static harness_option_declarations: Vec<Gc<CommandLineOption>> = vec![
            CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "allowNonTsExtensions".to_string(),
                type_: CommandLineOptionType::Boolean,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "useCaseSensitiveFileNames".to_string(),
                type_: CommandLineOptionType::Boolean,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfStringType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "baselineFile".to_string(),
                type_: CommandLineOptionType::String,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: None,
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfStringType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "includeBuiltFile".to_string(),
                type_: CommandLineOptionType::String,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: None,
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfStringType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "fileName".to_string(),
                type_: CommandLineOptionType::String,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: None,
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfStringType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "libFiles".to_string(),
                type_: CommandLineOptionType::String,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: None,
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "noErrorTruncation".to_string(),
                type_: CommandLineOptionType::Boolean,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "suppressOutputPathCheck".to_string(),
                type_: CommandLineOptionType::Boolean,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "noImplicitReferences".to_string(),
                type_: CommandLineOptionType::Boolean,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfStringType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "currentDirectory".to_string(),
                type_: CommandLineOptionType::String,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: None,
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfStringType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "symlink".to_string(),
                type_: CommandLineOptionType::String,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: None,
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfStringType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "link".to_string(),
                type_: CommandLineOptionType::String,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: None,
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "noTypesAndSymbols".to_string(),
                type_: CommandLineOptionType::Boolean,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
            CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "fullEmitPaths".to_string(),
                type_: CommandLineOptionType::Boolean,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
        ];
    }

    thread_local! {
        static options_index: GcCell<Option<HashMap<String, Gc<CommandLineOption>>>> = GcCell::new(None);
    }
    fn get_command_line_option(name: &str) -> Option<Gc<CommandLineOption>> {
        options_index.with(|options_index_| {
            options_index_
                .borrow_mut()
                .get_or_insert_with(|| {
                    let mut options_index_ = HashMap::new();
                    option_declarations.with(|option_declarations_| {
                        for option in option_declarations_ {
                            options_index_.insert(option.name().to_lowercase(), option.clone());
                        }
                    });
                    harness_option_declarations.with(|harness_option_declarations_| {
                        for option in harness_option_declarations_ {
                            options_index_.insert(option.name().to_lowercase(), option.clone());
                        }
                    });
                    options_index_
                })
                .get(&name.to_lowercase())
                .cloned()
        })
    }

    fn set_compiler_options_from_harness_setting(
        settings: &HashMap<String, String>,
        options: &mut CompilerOptionsAndHarnessOptions,
    ) {
        for (name, value) in settings {
            let option = get_command_line_option(name);
            if let Some(option) = option.as_ref() {
                let mut errors: Vec<Gc<Diagnostic>> = vec![];
                if HarnessOptions::is_harness_option(name) {
                    options.harness_options.set_value_from_command_line_option(
                        option,
                        option_value(option, value, &mut errors),
                    );
                } else {
                    options.compiler_options.set_value_from_command_line_option(
                        option,
                        option_value(option, value, &mut errors),
                    );
                }
                if !errors.is_empty() {
                    panic!("Unknown value '{}' for compiler option '{}'.", value, name);
                }
            } else {
                panic!("Unknown compiler option '{}'.", name);
            }
        }
    }

    fn option_value(
        option: &CommandLineOption,
        value: &str,
        errors: &mut Vec<Gc<Diagnostic>>,
    ) -> CompilerOptionsValue {
        match option.type_() {
            CommandLineOptionType::Boolean => Some(value.to_lowercase() == "true").into(),
            // TODO: this seems worth trying to upstream some tests with presumably "unexpected"
            // boolean option settings (eg moduleAugmentationInAmbientModule3.ts seems to have
            // `@declaration: true;` interpreted as false because of the trailing semicolon)?
            // CommandLineOptionType::Boolean => Some(match &*value.to_lowercase() {
            //     "true" => true,
            //     "false" => false,
            //     _ => panic!("Unexpected boolean value: {:?}", value),
            // })
            // .into(),
            CommandLineOptionType::String => Some(value.to_owned()).into(),
            CommandLineOptionType::Number => {
                // TODO: this worked for one test case but how would we know if this should be a
                // Number vs usize?
                let numver_value = usize::from_str_radix(value, 10);
                if numver_value.is_err() {
                    panic!("Value must be a number, got {:?}", value);
                }
                Some(numver_value.unwrap()).into()
            }
            CommandLineOptionType::Object => unimplemented!(),
            CommandLineOptionType::List => {
                CompilerOptionsValue::VecString(parse_list_type_option(option, Some(value), errors))
            }
            CommandLineOptionType::Map(_) => parse_custom_type_option(option, Some(value), errors),
        }
    }

    #[derive(Trace, Finalize)]
    pub struct TestFile {
        pub unit_name: String,
        pub content: String,
        pub file_options: Option<HashMap<String, String>>,
    }

    pub fn compile_files(
        input_files: &[Gc<TestFile>],
        other_files: &[Gc<TestFile>],
        harness_settings: Option<&TestCaseParser::CompilerSettings>,
        compiler_options: Option<&CompilerOptions>,
        current_directory: Option<&str>,
        symlinks: Option<&vfs::FileSet>,
    ) -> compiler::CompilationResult {
        let mut options: CompilerOptionsAndHarnessOptions =
            if let Some(compiler_options) = compiler_options {
                CompilerOptionsAndHarnessOptions {
                    compiler_options: compiler_options.clone(),
                    harness_options: Default::default(),
                }
            } else {
                CompilerOptionsAndHarnessOptions {
                    compiler_options: CompilerOptionsBuilder::default()
                        .no_resolve(Some(false))
                        .build()
                        .unwrap(),
                    harness_options: Default::default(),
                }
            };
        options.compiler_options.target = Some(get_emit_script_target(&options.compiler_options));
        options.compiler_options.new_line = Some(
            options
                .compiler_options
                .new_line
                .unwrap_or(NewLineKind::CarriageReturnLineFeed),
        );
        options.compiler_options.no_error_truncation = Some(true);
        options.compiler_options.skip_default_lib_check =
            if options.compiler_options.skip_default_lib_check.is_none() {
                Some(true)
            } else {
                options.compiler_options.skip_default_lib_check
            };

        let current_directory = current_directory.unwrap_or(vfs::src_folder);

        if let Some(harness_settings) = harness_settings {
            set_compiler_options_from_harness_setting(harness_settings, &mut options);
        }
        if let Some(options_root_dirs) = options.compiler_options.root_dirs.clone().as_ref() {
            options.compiler_options.root_dirs = Some(map(options_root_dirs, |d: &String, _| {
                get_normalized_absolute_path(d, Some(current_directory))
            }));
        }

        let use_case_sensitive_file_names = options
            .harness_options
            .use_case_sensitive_file_names
            .unwrap_or(true);
        let mut program_file_names = input_files
            .into_iter()
            .map(|file| file.unit_name.clone())
            .filter(|file_name| !file_extension_is(file_name, Extension::Json.to_str()))
            .collect::<Vec<_>>();

        if let Some(options_include_built_file) =
            options.harness_options.include_built_file.as_ref()
        {
            program_file_names.push(vpath::combine(
                vfs::built_folder,
                &[Some(options_include_built_file)],
            ));
        }

        if let Some(options_lib_files) = options.harness_options.lib_files.as_ref() {
            for file_name in options_lib_files.split(",") {
                program_file_names.push(vpath::combine(vfs::test_lib_folder, &[Some(file_name)]));
            }
        }

        let docs = input_files
            .into_iter()
            .chain(other_files.into_iter())
            .map(|file| Gc::new(documents::TextDocument::from_test_file(file)))
            .collect::<Vec<_>>();
        let fs = Gc::new(vfs::create_from_file_system(
            get_io().as_file_system_resolver_host(),
            !use_case_sensitive_file_names,
            Some(vfs::FileSystemCreateOptions {
                documents: Some(docs),
                cwd: Some(current_directory.to_owned()),
                ..Default::default()
            }),
        ));
        if let Some(symlinks) = symlinks {
            fs.apply(symlinks);
        }
        let host =
            fakes::CompilerHost::new(fs, Some(Gc::new(options.compiler_options.clone())), None);
        let mut result =
            compiler::compile_files(host, Some(&program_file_names), &options.compiler_options);
        result.symlinks = symlinks.cloned();
        result
    }

    struct CompilerOptionsAndHarnessOptions {
        compiler_options: CompilerOptions,
        harness_options: HarnessOptions,
    }

    pub fn minimal_diagnostics_to_string(
        diagnostics: &[Gc<Diagnostic>],
        pretty: Option<bool>,
    ) -> String {
        let host = MinimalDiagnosticsToStringFormatDiagnosticsHost;
        if pretty == Some(true) {
            format_diagnostics_with_color_and_context(diagnostics, &host)
        } else {
            format_diagnostics(diagnostics, &host)
        }
    }

    struct MinimalDiagnosticsToStringFormatDiagnosticsHost;

    impl FormatDiagnosticsHost for MinimalDiagnosticsToStringFormatDiagnosticsHost {
        fn get_current_directory(&self) -> String {
            "".to_owned()
        }

        fn get_new_line(&self) -> &str {
            with_io(|IO| IO.new_line())
        }

        fn get_canonical_file_name(&self, file_name: &str) -> String {
            get_canonical_file_name(file_name)
        }
    }

    pub fn get_error_baseline(
        input_files: &[Gc<TestFile>],
        diagnostics: &[Gc<Diagnostic>],
        pretty: Option<bool>,
    ) -> String {
        let mut output_lines = "".to_owned();
        for value in iterate_error_baseline(
            input_files,
            diagnostics,
            Some(IterateErrorBaselineOptions {
                pretty,
                case_sensitive: None,
                current_directory: None,
            }),
        ) {
            let (_, content, _) = value;
            output_lines.push_str(&content);
        }
        if pretty == Some(true) {
            output_lines.push_str(&get_error_summary_text(
                get_error_count_for_summary(diagnostics),
                with_io(|IO| IO.new_line()),
            ));
        }
        output_lines
    }

    pub const diagnostic_summary_marker: &'static str = "__diagnosticSummary";
    pub const global_errors_marker: &'static str = "__globalErrors";

    pub fn iterate_error_baseline(
        input_files: &[Gc<TestFile>],
        diagnostics: &[Gc<Diagnostic>],
        options: Option<IterateErrorBaselineOptions>,
    ) -> Vec<(String, String, usize)> {
        let diagnostics: Vec<_> = sort(diagnostics, |a: &Gc<Diagnostic>, b: &Gc<Diagnostic>| {
            compare_diagnostics(&**a, &**b)
        })
        .into();
        let mut output_lines = "".to_owned();
        let mut total_errors_reported_in_non_library_files = 0;

        let mut errors_reported = 0;

        let mut first_line = true;

        let format_diagnsotic_host = FormatDiagnsoticHost::new(options.as_ref());

        let mut ret: Vec<(String, String, usize)> = vec![];
        ret.push((
            diagnostic_summary_marker.to_owned(),
            format!(
                "{}{}{}",
                Utils::remove_test_path_prefixes(
                    &minimal_diagnostics_to_string(
                        &diagnostics,
                        options.as_ref().and_then(|options| options.pretty)
                    ),
                    None,
                ),
                with_io(|IO| IO.new_line()),
                with_io(|IO| IO.new_line()),
            ),
            diagnostics.len(),
        ));

        let global_errors = diagnostics.iter().filter(|err| err.maybe_file().is_none());
        global_errors.for_each(|diagnostic| {
            output_error_text(
                &format_diagnsotic_host,
                &mut output_lines,
                &mut first_line,
                &mut errors_reported,
                &mut total_errors_reported_in_non_library_files,
                diagnostic,
            )
        });
        ret.push((
            global_errors_marker.to_owned(),
            output_lines.clone(),
            errors_reported,
        ));
        output_lines.clear();
        errors_reported = 0;

        let mut dupe_case: HashMap<String, usize> = HashMap::new();
        for input_file in input_files
            .into_iter()
            .filter(|_f| /*f.content !== undefined*/ true)
        {
            let file_errors = diagnostics.iter().filter(|e| {
                let err_fn = e.maybe_file();
                matches!(
                    err_fn.as_ref(),
                    Some(err_fn) if compare_paths(
                        &Utils::remove_test_path_prefixes(
                            &err_fn.as_source_file().file_name(),
                            None,
                        ),
                        &Utils::remove_test_path_prefixes(
                            &input_file.unit_name,
                            None,
                        ),
                        Some(
                            options.as_ref().and_then(|options| options.current_directory.clone()).unwrap_or_else(|| "".to_owned()),
                        ),
                        Some(options.as_ref().and_then(|options| options.case_sensitive) != Some(true))
                    ) == Comparison::EqualTo
                )
            }).collect::<Vec<_>>();

            output_lines.push_str(&format!(
                "{}==== {} ({} errors) ====",
                new_line(&mut first_line),
                input_file.unit_name,
                file_errors.len(),
            ));

            let mut marked_error_count = 0;

            let input_file_content_as_chars = input_file.content.chars().collect::<Vec<_>>();
            let line_starts = compute_line_starts(&input_file_content_as_chars);
            let mut lines = input_file_content_as_chars
                .split(|ch| *ch == '\n')
                .collect::<Vec<_>>();
            if lines.len() == 1 {
                lines = input_file_content_as_chars
                    .split(|ch| *ch == '\r')
                    .collect::<Vec<_>>();
            }

            let lines_len = lines.len();
            for (line_index, mut line) in lines.into_iter().enumerate() {
                if !line.is_empty() && line[line.len() - 1] == '\r' {
                    line = &line[0..line.len() - 1];
                }

                let this_line_start = line_starts[line_index];
                let next_line_start: usize;
                if line_index == lines_len - 1 {
                    next_line_start = input_file_content_as_chars.len();
                } else {
                    next_line_start = line_starts[line_index + 1];
                }
                output_lines.push_str(&format!(
                    "{}    {}",
                    new_line(&mut first_line),
                    line.iter().collect::<String>(),
                ));
                for &err_diagnostic in &file_errors {
                    let err = TextSpan {
                        start: err_diagnostic.start(),
                        length: err_diagnostic.length(),
                    };
                    let end = text_span_end(&err);
                    let this_line_start_as_isize: isize = this_line_start.try_into().unwrap();
                    let next_line_start_as_isize: isize = next_line_start.try_into().unwrap();
                    if end >= this_line_start_as_isize
                        && (err.start < next_line_start_as_isize || line_index == lines_len - 1)
                    {
                        let relative_offset = err.start - this_line_start_as_isize;
                        let length =
                            (end - err.start) - cmp::max(0, this_line_start_as_isize - err.start);
                        let squiggle_start: usize =
                            cmp::max(0, relative_offset).try_into().unwrap();
                        output_lines.push_str(&format!(
                            "{}    {}{}",
                            new_line(&mut first_line),
                            regex!(r"[^\s]").replace_all(
                                &line[0..squiggle_start].iter().collect::<String>(),
                                " "
                            ),
                            "~".repeat(cmp::min(
                                length.try_into().unwrap(),
                                line.len() - squiggle_start
                            ))
                        ));

                        if line_index == lines_len - 1 || next_line_start_as_isize > end {
                            output_error_text(
                                &format_diagnsotic_host,
                                &mut output_lines,
                                &mut first_line,
                                &mut errors_reported,
                                &mut total_errors_reported_in_non_library_files,
                                err_diagnostic,
                            );
                            marked_error_count += 1;
                        }
                    }
                }
            }

            assert_eq!(
                marked_error_count,
                file_errors.len(),
                "count of errors in {}",
                input_file.unit_name
            );
            let is_dupe = dupe_case.contains_key(&sanitize_test_file_path(&input_file.unit_name));
            ret.push((
                check_duplicated_file_name(&input_file.unit_name, &mut dupe_case),
                output_lines.clone(),
                errors_reported,
            ));
            if is_dupe && options.as_ref().and_then(|options| options.case_sensitive) != Some(true)
            {
                total_errors_reported_in_non_library_files -= errors_reported;
            }
            output_lines.clear();
            errors_reported = 0;
        }

        let num_library_diagnostics = count_where(
            Some(&diagnostics),
            |diagnostic: &Gc<Diagnostic>, _| {
                matches!(
                    diagnostic.maybe_file().as_ref(),
                    Some(diagnostic_file) if is_default_library_file(&diagnostic_file.as_source_file().file_name()) ||
                        is_built_file(&diagnostic_file.as_source_file().file_name())
                )
            },
        );

        let num_test262_harness_diagnostics = count_where(
            Some(&diagnostics),
            |diagnostic: &Gc<Diagnostic>, _| {
                matches!(
                    diagnostic.maybe_file().as_ref(),
                    Some(diagnostic_file) if diagnostic_file.as_source_file().file_name().contains("test262-harness")
                )
            },
        );

        assert_eq!(
            total_errors_reported_in_non_library_files
                + num_library_diagnostics
                + num_test262_harness_diagnostics,
            diagnostics.len(),
            "total number of errors"
        );

        ret
    }

    fn new_line(first_line: &mut bool) -> &'static str {
        if *first_line {
            *first_line = false;
            return "";
        }
        "\r\n"
    }

    struct FormatDiagnsoticHost<'iterate_error_baseline> {
        options: Option<&'iterate_error_baseline IterateErrorBaselineOptions>,
        get_canonical_file_name: fn(&str) -> String,
    }

    impl<'iterate_error_baseline> FormatDiagnsoticHost<'iterate_error_baseline> {
        pub fn new(options: Option<&'iterate_error_baseline IterateErrorBaselineOptions>) -> Self {
            Self {
                options,
                get_canonical_file_name: create_get_canonical_file_name(
                    options
                        .and_then(|options| options.case_sensitive)
                        .unwrap_or(true),
                ),
            }
        }
    }

    impl<'iterate_error_baseline> FormatDiagnosticsHost
        for FormatDiagnsoticHost<'iterate_error_baseline>
    {
        fn get_current_directory(&self) -> String {
            self.options
                .and_then(|options| options.current_directory.clone())
                .unwrap_or_else(|| "".to_owned())
        }

        fn get_new_line(&self) -> &str {
            with_io(|IO| IO.new_line())
        }

        fn get_canonical_file_name(&self, file_name: &str) -> String {
            (self.get_canonical_file_name)(file_name)
        }
    }

    fn output_error_text(
        format_diagnsotic_host: &FormatDiagnsoticHost<'_>,
        output_lines: &mut String,
        first_line: &mut bool,
        errors_reported: &mut usize,
        total_errors_reported_in_non_library_files: &mut usize,
        error: &Diagnostic,
    ) {
        let message = flatten_diagnostic_message_text(
            Some(error.message_text()),
            with_io(|IO| IO.new_line()),
            None,
        );

        let mut err_lines = Utils::remove_test_path_prefixes(&message, None)
            .split("\n")
            .map(|s| {
                if !s.is_empty() && s.ends_with("\r") {
                    &s[0..s.len() - 1]
                } else {
                    s
                }
            })
            .filter(|s| !s.is_empty())
            .map(|s| {
                format!(
                    "!!! {} TS{}: {}",
                    diagnostic_category_name(error.category(), None,),
                    error.code(),
                    s,
                )
            })
            .collect::<Vec<_>>();
        if let Some(error_related_information) = error.maybe_related_information().as_ref() {
            for info in error_related_information {
                err_lines.push(format!(
                    "!!! related TS{}{}: {}",
                    info.code(),
                    if let Some(info_file) = info.maybe_file().as_ref() {
                        format!(
                            " {}",
                            format_location(
                                info_file,
                                info.start(),
                                format_diagnsotic_host,
                                Some(|a: &str, _b: &str| a.to_owned()),
                            )
                        )
                    } else {
                        "".to_owned()
                    },
                    flatten_diagnostic_message_text(
                        Some(info.message_text()),
                        with_io(|IO| IO.new_line()),
                        None,
                    )
                ));
            }
        }
        err_lines.iter().for_each(|e| {
            output_lines.push_str(&format!("{}{}", new_line(first_line), e,));
        });
        *errors_reported += 1;

        if match error.maybe_file().as_ref() {
            None => true,
            Some(error_file) => !is_default_library_file(&error_file.as_source_file().file_name()),
        } {
            *total_errors_reported_in_non_library_files += 1;
        }
    }

    #[derive(Default)]
    pub struct IterateErrorBaselineOptions {
        pub pretty: Option<bool>,
        pub case_sensitive: Option<bool>,
        pub current_directory: Option<String>,
    }

    pub fn do_error_baseline(
        baseline_path: &str,
        input_files: &[Gc<TestFile>],
        errors: &[Gc<Diagnostic>],
        pretty: Option<bool>,
    ) {
        lazy_static! {
            static ref ts_extension_regex: Regex = Regex::new(r"\.tsx?$").unwrap();
        }
        Baseline::run_baseline(
            &ts_extension_regex.replace(baseline_path, ".errors.txt"),
            if
            /* !errors ||*/
            !errors.is_empty() {
                Some(get_error_baseline(input_files, errors, pretty))
            } else {
                None
            }
            .as_deref(),
            None,
        );
    }

    fn check_duplicated_file_name(
        result_name: &str,
        dupe_case: &mut HashMap<String, usize>,
    ) -> String {
        let mut result_name = sanitize_test_file_path(result_name);
        if dupe_case.contains_key(&result_name) {
            let count = 1 + *dupe_case.get(&result_name).unwrap();
            dupe_case.insert(result_name.clone(), count);
            result_name = format!("{}.dupe{}", result_name, count,);
        } else {
            dupe_case.insert(result_name.clone(), 0);
        }
        result_name
    }

    pub fn sanitize_test_file_path(name: &str) -> String {
        let path = to_path(
            &normalize_slashes(&regex!(r"\.\./").replace_all(
                &regex!(r#"[\^<>:"|?*%]"#).replace_all(name, "_"),
                "__dotdot/",
            )),
            Some(""),
            |file_name: &str| Utils::canonicalize_for_harness(file_name),
        );
        if starts_with(&path, "/") {
            return path[1..].to_owned();
        }
        path.to_string()
    }
}

#[derive(Clone, Trace, Finalize)]
pub struct FileBasedTest {
    pub file: String, /*PathBuf*/
    pub configurations: Option<Vec<FileBasedTestConfiguration>>,
    // CompilerFileBasedTest fields
    pub content: Option<String>,
}

pub type FileBasedTestConfiguration = HashMap<String, String>;

fn split_vary_by_setting_value(text: &str, vary_by: &str) -> Option<Vec<String>> {
    if text.is_empty() {
        return None;
    }

    let mut star = false;
    let mut includes: Vec<String> = vec![];
    let mut excludes: Vec<String> = vec![];
    lazy_static! {
        static ref comma_regex: Regex = Regex::new(r",").unwrap();
    }
    for s in comma_regex.split(text) {
        let s = s.trim().to_lowercase();
        if s.is_empty() {
            continue;
        }
        if s == "*" {
            star = true;
        } else if starts_with(&s, "-") || starts_with(&s, "!") {
            excludes.push(s[1..].to_owned());
        } else {
            includes.push(s);
        }
    }

    if includes.len() <= 1 && !star && excludes.is_empty() {
        return None;
    }

    let mut variations: Vec<Variation> = vec![];
    let values = get_vary_by_star_setting_values(vary_by);

    for include in includes {
        let value = values.as_ref().and_then(|values| values.get(&&*include));
        if find_index(
            &variations,
            |v: &Variation, _| {
                v.key == include
                    || matches!(
                        value,
                        Some(value) if v.value.as_ref() == Some(value)
                    )
            },
            None,
        )
        .is_none()
        {
            variations.push(Variation {
                key: include.clone(),
                value: value.cloned(),
            });
        }
    }

    if star {
        if let Some(values) = values.as_ref() {
            for (&key, value) in values {
                if find_index(
                    &variations,
                    |v: &Variation, _| v.key == key || v.value.as_ref() == Some(value),
                    None,
                )
                .is_none()
                {
                    variations.push(Variation {
                        key: key.to_owned(),
                        value: Some(value.clone()),
                    });
                }
            }
        }
    }

    for exclude in excludes {
        let value = values.as_ref().and_then(|values| values.get(&&*exclude));
        while let Some(index) = find_index(
            &variations,
            |v: &Variation, _| {
                v.key == exclude
                    || matches!(
                        value,
                        Some(value) if v.value.as_ref() == Some(value)
                    )
            },
            None,
        ) {
            ordered_remove_item_at(&mut variations, index);
        }
    }

    if variations.is_empty() {
        panic!(
            "Variations in test option '@{}' resulted in an empty set.",
            vary_by
        );
    }

    Some(map(variations, |variation, _| variation.key))
}

#[derive(Clone)]
struct Variation {
    pub key: String,
    pub value: Option<CommandLineOptionMapTypeValueOrUsize>,
}

fn compute_file_based_test_configuration_variations(
    configurations: &mut Vec<FileBasedTestConfiguration>,
    variation_state: &mut FileBasedTestConfiguration,
    vary_by_entries: &[(String, Vec<String>)],
    offset: usize,
) {
    if offset >= vary_by_entries.len() {
        configurations.push(variation_state.clone());
        return;
    }

    let (vary_by, entries) = &vary_by_entries[offset];
    for entry in entries {
        variation_state.insert(vary_by.clone(), entry.clone());
        compute_file_based_test_configuration_variations(
            configurations,
            variation_state,
            vary_by_entries,
            offset + 1,
        );
    }
}

thread_local! {
    static boolean_vary_by_star_setting_values: RefCell<Option<HashMap<&'static str, CommandLineOptionMapTypeValueOrUsize>>> = Default::default();
}

fn get_vary_by_star_setting_values(
    vary_by: &str,
) -> Option<HashMap<&'static str, CommandLineOptionMapTypeValueOrUsize>> {
    let option = option_declarations.with(|option_declarations_| {
        for_each(option_declarations_, |decl: &Gc<CommandLineOption>, _| {
            if equate_strings_case_insensitive(decl.name(), vary_by) {
                Some(decl.clone())
            } else {
                None
            }
        })
    })?;
    if let CommandLineOptionType::Map(option_type) = option.type_() {
        return Some(HashMap::from_iter(
            option_type
                .into_iter()
                .map(|(key, value)| (*key, value.clone().into())),
        ));
    }
    if matches!(option.type_(), CommandLineOptionType::Boolean) {
        return Some(boolean_vary_by_star_setting_values.with(
            |boolean_vary_by_star_setting_values_| {
                boolean_vary_by_star_setting_values_
                    .borrow_mut()
                    .get_or_insert_with(|| {
                        HashMap::from_iter(IntoIterator::into_iter([
                            ("true", 1.into()),
                            ("false", 0.into()),
                        ]))
                    })
                    .clone()
            },
        ));
    }
    None
}

#[derive(Clone, Eq, PartialEq)]
enum CommandLineOptionMapTypeValueOrUsize {
    CommandLineOptionMapTypeValue(CommandLineOptionMapTypeValue),
    Usize(usize),
}

impl From<CommandLineOptionMapTypeValue> for CommandLineOptionMapTypeValueOrUsize {
    fn from(value: CommandLineOptionMapTypeValue) -> Self {
        Self::CommandLineOptionMapTypeValue(value)
    }
}

impl From<usize> for CommandLineOptionMapTypeValueOrUsize {
    fn from(value: usize) -> Self {
        Self::Usize(value)
    }
}

pub fn get_file_based_test_configurations<TVaryBy: AsRef<str>>(
    settings: &TestCaseParser::CompilerSettings,
    vary_by: &[TVaryBy],
) -> Option<Vec<FileBasedTestConfiguration>> {
    let mut vary_by_entries: Option<Vec<(String, Vec<String>)>> = None;
    let mut variation_count = 1;
    for vary_by_key in vary_by {
        let vary_by_key = vary_by_key.as_ref();
        if settings.contains_key(vary_by_key) {
            let entries =
                split_vary_by_setting_value(settings.get(vary_by_key).unwrap(), vary_by_key);
            if let Some(entries) = entries {
                variation_count *= entries.len();
                if variation_count > 25 {
                    panic!(
                        "Provided test options exceeded the maximum number of variations: {}",
                        vary_by
                            .into_iter()
                            .map(|v| format!("'@{}'", v.as_ref()))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
                vary_by_entries
                    .get_or_insert_with(|| vec![])
                    .push((vary_by_key.to_owned(), entries));
            }
        }
    }

    let vary_by_entries = vary_by_entries?;

    let mut configurations: Vec<FileBasedTestConfiguration> = vec![];
    compute_file_based_test_configuration_variations(
        &mut configurations,
        &mut FileBasedTestConfiguration::new(),
        &vary_by_entries,
        0,
    );
    Some(configurations)
}

pub fn get_file_based_test_configuration_description(
    configuration: &FileBasedTestConfiguration,
) -> String {
    let mut name = "".to_owned();
    // if (configuration) {
    let mut keys = configuration.keys().collect::<Vec<_>>();
    keys.sort();
    for key in keys {
        if !name.is_empty() {
            name.push_str(", ");
        }
        name.push_str(&format!("@{}: {}", key, configuration.get(key).unwrap(),));
    }
    // }
    name
}

pub mod TestCaseParser {
    use gc::Gc;
    use regex::Regex;
    use std::collections::HashMap;
    use std::io;

    use typescript_rust::{
        for_each, get_base_file_name, get_directory_path, get_normalized_absolute_path,
        normalize_path, ordered_remove_item_at, parse_json_source_file_config_file_content,
        parse_json_text, ModuleResolutionHost, ParseConfigHost, ParsedCommandLine,
    };

    use super::get_config_name_from_file_name;
    use crate::{vfs, Utils};

    pub type CompilerSettings = HashMap<String, String>;

    #[derive(Clone)]
    pub struct TestUnitData {
        pub content: String,
        pub name: String,
        pub file_options: HashMap<String, String>,
        pub original_file_path: String,
        pub references: Vec<String>,
    }

    lazy_static! {
        static ref line_ending_regex: Regex = Regex::new(r"\r?\n|\r").unwrap();
        static ref option_regex: Regex = Regex::new(r"^[/]{2}\s*@(\w+)\s*:\s*([^\r\n]*)").unwrap();
        static ref link_regex: Regex =
            Regex::new(r"^[/]{2}\s*@link\s*:\s*([^r\n]*)\s*->\s*([^\r\n]*)").unwrap();
    }

    pub fn parse_symlink_from_test(line: &str, symlinks: &mut Option<vfs::FileSet>) -> bool /*Option<vfs::FileSet>*/
    {
        let link_meta_data = link_regex.captures(line);
        if link_meta_data.is_none() {
            return false /*None*/;
        }
        let link_meta_data = link_meta_data.unwrap();

        symlinks.get_or_insert_with(|| vfs::FileSet::new()).insert(
            link_meta_data.get(2).unwrap().as_str().to_owned(),
            Some(
                vfs::Symlink::new(link_meta_data.get(1).unwrap().as_str().to_owned(), None).into(),
            ),
        );
        true
        // symlinks
    }

    pub fn extract_compiler_settings(content: &str) -> CompilerSettings {
        let mut opts: CompilerSettings = CompilerSettings::new();

        for line in line_ending_regex.split(content) {
            if let Some(match_) = option_regex.captures(line) {
                opts.insert(
                    match_.get(1).unwrap().as_str().to_owned(),
                    match_.get(2).unwrap().as_str().trim().to_owned(),
                );
            }
        }

        opts
    }

    pub struct TestCaseContent {
        pub settings: CompilerSettings,
        pub test_unit_data: Vec<TestUnitData>,
        pub ts_config: Option<ParsedCommandLine>,
        pub ts_config_file_unit_data: Option<TestUnitData>,
        pub symlinks: Option<vfs::FileSet>,
    }

    pub fn make_units_from_test(
        code: &str,
        file_name: &str,
        root_dir: Option<&str>,
        settings: Option<CompilerSettings>,
    ) -> TestCaseContent {
        let settings = settings.unwrap_or_else(|| extract_compiler_settings(code));
        let mut test_unit_data: Vec<TestUnitData> = vec![];

        let lines = Utils::split_content_by_newlines(code);

        let mut current_file_content: Option<String> = None;
        let mut current_file_options: HashMap<String, String> = HashMap::new();
        let mut current_file_name: Option<String> = None;
        let mut refs: Vec<String> = vec![];
        let mut symlinks: Option<vfs::FileSet> = None;

        for line in lines {
            let possibly_symlinks = parse_symlink_from_test(line, &mut symlinks);
            if possibly_symlinks {
                // symlinks = possiblySymlinks;
            } else if let Some(test_meta_data) = option_regex.captures(line) {
                let meta_data_name = test_meta_data.get(1).unwrap().as_str().to_lowercase();
                current_file_options.insert(
                    test_meta_data.get(1).unwrap().as_str().to_owned(),
                    test_meta_data.get(2).unwrap().as_str().trim().to_owned(),
                );
                if meta_data_name != "filename" {
                    continue;
                }

                if let Some(current_file_name_present) = current_file_name.as_ref() {
                    let new_test_file = TestUnitData {
                        content: current_file_content.clone().unwrap(),
                        name: current_file_name_present.clone(),
                        file_options: current_file_options.clone(),
                        original_file_path: file_name.to_owned(),
                        references: refs.clone(),
                    };
                    test_unit_data.push(new_test_file);

                    current_file_content = None;
                    current_file_options = HashMap::new();
                    current_file_name =
                        Some(test_meta_data.get(2).unwrap().as_str().trim().to_owned());
                    refs = vec![];
                } else {
                    current_file_name =
                        Some(test_meta_data.get(2).unwrap().as_str().trim().to_owned());
                }
            } else {
                match current_file_content.as_mut() {
                    None => {
                        current_file_content = Some("".to_owned());
                    }
                    Some(current_file_content) if !current_file_content.is_empty() => {
                        current_file_content.push_str("\n");
                    }
                    _ => (),
                }
                current_file_content.as_mut().unwrap().push_str(line);
            }
        }

        current_file_name = if !test_unit_data.is_empty()
            || matches!(
                current_file_name.as_ref(),
                Some(current_file_name) if !current_file_name.is_empty()
            ) {
            current_file_name
        } else {
            Some(get_base_file_name(file_name, None, None))
        };

        let new_test_file2 = TestUnitData {
            content: current_file_content.unwrap_or_else(|| "".to_owned()),
            name: current_file_name.unwrap(),
            file_options: current_file_options,
            original_file_path: file_name.to_owned(),
            references: refs,
        };
        test_unit_data.push(new_test_file2);

        let parse_config_host = MakeUnitsFromTestParseConfigHost::new(&test_unit_data);

        let mut ts_config: Option<ParsedCommandLine> = None;
        let mut ts_config_file_unit_data: Option<TestUnitData> = None;
        let mut indexes_to_remove: Vec<usize> = vec![];
        for (i, data) in test_unit_data.iter().enumerate() {
            if get_config_name_from_file_name(&data.name).is_some() {
                let config_json = parse_json_text(&data.name, data.content.clone());
                // assert!(config_json.as_source_file().end_of_file_token().is_some());
                let mut base_dir = normalize_path(&get_directory_path(&data.name));
                if let Some(root_dir) = root_dir.filter(|root_dir| !root_dir.is_empty()) {
                    base_dir = get_normalized_absolute_path(&base_dir, Some(root_dir));
                }
                ts_config = Some(parse_json_source_file_config_file_content(
                    &config_json,
                    &parse_config_host,
                    &base_dir,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                ));
                let mut options = (*ts_config.as_ref().unwrap().options).clone();
                options.config_file_path = Some(data.name.clone());
                ts_config.as_mut().unwrap().options = Gc::new(options);
                ts_config_file_unit_data = Some(data.clone());

                indexes_to_remove.push(i);

                break;
            }
        }
        drop(parse_config_host);
        for i in indexes_to_remove {
            ordered_remove_item_at(&mut test_unit_data, i);
        }
        TestCaseContent {
            settings,
            test_unit_data,
            ts_config,
            ts_config_file_unit_data,
            symlinks,
        }
    }

    struct MakeUnitsFromTestParseConfigHost<'test_unit_data> {
        test_unit_data: &'test_unit_data [TestUnitData],
    }

    impl<'test_unit_data> MakeUnitsFromTestParseConfigHost<'test_unit_data> {
        pub fn new(test_unit_data: &'test_unit_data [TestUnitData]) -> Self {
            Self { test_unit_data }
        }
    }

    impl ParseConfigHost for MakeUnitsFromTestParseConfigHost<'_> {
        fn use_case_sensitive_file_names(&self) -> bool {
            false
        }

        fn read_directory(
            &self,
            root_dir: &str,
            extensions: &[&str],
            excludes: Option<&[String]>,
            includes: &[String],
            depth: Option<usize>,
        ) -> Vec<String> {
            vec![]
        }

        fn file_exists(&self, path: &str) -> bool {
            true
        }

        fn read_file(&self, name: &str) -> io::Result<Option<String>> {
            Ok(for_each(self.test_unit_data, |data: &TestUnitData, _| {
                if data.name.to_lowercase() == name.to_lowercase() {
                    Some(data.content.clone())
                } else {
                    None
                }
            }))
        }

        fn is_trace_supported(&self) -> bool {
            false
        }

        fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
            unreachable!()
        }
    }
}

pub mod Baseline {
    use pretty_assertions::StrComparison;
    use std::cell::RefCell;
    use std::collections::HashMap;

    use super::{user_specified_root, with_io, IO};
    use crate::Utils;

    const no_content: &'static str = "<no content>";

    #[derive(Debug)]
    pub struct BaselineOptions {
        pub Subfolder: Option<String>,
        pub Baselinefolder: Option<String>,
        pub PrintDiff: Option<bool /*true*/>,
    }

    pub fn local_path(
        file_name: &str,
        baseline_folder: Option<&str>,
        subfolder: Option<&str>,
    ) -> String {
        match baseline_folder {
            None => baseline_path(file_name, "local", "tests/baselines", subfolder),
            Some(baseline_folder) => baseline_path(file_name, "local", baseline_folder, subfolder),
        }
    }

    fn reference_path(
        file_name: &str,
        baseline_folder: Option<&str>,
        subfolder: Option<&str>,
    ) -> String {
        match baseline_folder {
            None => baseline_path(file_name, "reference", "tests/baselines", subfolder),
            Some(baseline_folder) => {
                baseline_path(file_name, "reference", baseline_folder, subfolder)
            }
        }
    }

    fn baseline_path(
        file_name: &str,
        type_: &str,
        baseline_folder: &str,
        subfolder: Option<&str>,
    ) -> String {
        if let Some(subfolder) = subfolder {
            format!(
                "{}{baseline_folder}/{subfolder}/{type_}/{file_name}",
                user_specified_root
            )
        } else {
            format!(
                "{}{baseline_folder}/{type_}/{file_name}",
                user_specified_root
            )
        }
    }

    thread_local! {
        static file_cache: RefCell<HashMap<String, bool>> = Default::default();
    }

    fn compare_to_baseline(
        actual: Option<&str>,
        relative_file_name: &str,
        opts: Option<&BaselineOptions>,
    ) -> CompareToBaselineReturn {
        // if (actual === undefined) {
        //     return undefined!;
        // }

        let ref_file_name = reference_path(
            relative_file_name,
            opts.and_then(|opts| opts.Baselinefolder.as_deref()),
            opts.and_then(|opts| opts.Subfolder.as_deref()),
        );

        let actual = actual.unwrap_or(no_content);

        let mut expected = "<no content>".to_owned();
        if with_io(|IO| IO.file_exists(&ref_file_name)) {
            expected = with_io(|io| IO::read_file(&*io, ref_file_name.as_ref())).unwrap();
        }

        CompareToBaselineReturn {
            expected,
            actual: actual.to_owned(),
        }
    }

    struct CompareToBaselineReturn {
        pub expected: String,
        pub actual: String,
    }

    fn write_comparison(
        expected: &str,
        actual: &str,
        relative_file_name: &str,
        actual_file_name: &str,
        opts: Option<&BaselineOptions>,
    ) {
        create_directory_structure(&with_io(|IO| {
            IO.directory_name(actual_file_name.as_ref()).unwrap()
        }));

        if with_io(|IO| IO.file_exists(actual_file_name)) {
            with_io(|IO| {
                IO.delete_file(actual_file_name.as_ref());
            });
        }

        let encoded_actual = Utils::encode_string(actual);
        if expected != encoded_actual {
            if actual == no_content {
                with_io(|IO| IO.write_file(format!("{}.delete", actual_file_name).as_ref(), ""));
            } else {
                with_io(|IO| IO.write_file(actual_file_name.as_ref(), &encoded_actual));
            }
            if
            /* !!require &&*/
            opts.and_then(|opts| opts.PrintDiff) == Some(true) || true {
                // const Diff = require("diff");
                // const patch = Diff.createTwoFilesPatch("Expected", "Actual", expected, actual, "The current baseline", "The new version");
                // throw new Error(`The baseline file ${relativeFileName} has changed.${ts.ForegroundColorEscapeSequences.Grey}\n\n${patch}`);
                panic!(
                    "The baseline file {} has changed.\n\n{}",
                    relative_file_name,
                    StrComparison::new(expected, &encoded_actual)
                );
            } else {
                // TODO: this looks like a bug, `expected` would never be a file path, no?
                if with_io(|IO| !IO.file_exists(expected)) {
                    panic!(
                        "New baseline created at {}",
                        with_io(|IO| IO.join_path(&[
                            "tests".as_ref(),
                            "baselines".as_ref(),
                            "local".as_ref(),
                            relative_file_name.as_ref(),
                        ]))
                    );
                } else {
                    panic!("The baseline file {} has changed.", relative_file_name);
                }
            }
        }
    }

    fn create_directory_structure(dir_name: &str) {
        if file_cache.with(|file_cache_| file_cache_.borrow().get(dir_name).copied() == Some(true))
            || with_io(|IO| IO.directory_exists(dir_name))
        {
            file_cache.with(|file_cache_| {
                file_cache_.borrow_mut().insert(dir_name.to_owned(), true);
            });
            return;
        }

        let parent_directory = with_io(|IO| IO.directory_name(dir_name.as_ref()).unwrap());
        if !parent_directory.is_empty() && parent_directory != dir_name {
            create_directory_structure(&parent_directory);
        }
        with_io(|IO| {
            IO.create_directory(dir_name.as_ref()).unwrap();
        });
        file_cache.with(|file_cache_| {
            file_cache_.borrow_mut().insert(dir_name.to_owned(), true);
        });
    }

    pub fn run_baseline(
        relative_file_name: &str,
        actual: Option<&str>,
        opts: Option<BaselineOptions>,
    ) {
        let actual_file_name = local_path(
            relative_file_name,
            opts.as_ref()
                .and_then(|opts| opts.Baselinefolder.as_deref()),
            opts.as_ref().and_then(|opts| opts.Subfolder.as_deref()),
        );
        // if actual.is_none() {
        //     panic!("The generated content was \"undefined\". Return \"null\" if no baselining is required.\"");
        // }
        let comparison = compare_to_baseline(actual, relative_file_name, opts.as_ref());
        write_comparison(
            &comparison.expected,
            &comparison.actual,
            relative_file_name,
            &actual_file_name,
            opts.as_ref(),
        );
    }
}

pub fn is_default_library_file(file_path: &str) -> bool {
    let file_name = get_base_file_name(&normalize_slashes(file_path), None, None);
    starts_with(&file_name, "lib.") && ends_with(&file_name, Extension::Dts.to_str())
}

pub fn is_built_file(file_path: &str) -> bool {
    file_path.starts_with(lib_folder)
        || file_path.starts_with(&vpath::add_trailing_separator(vfs::built_folder))
}

pub fn get_config_name_from_file_name(
    filename: &str,
) -> Option<&'static str /*"tsconfig.json" | "jsconfig.json"*/> {
    let flc = get_base_file_name(filename, None, None).to_lowercase();
    find(&["tsconfig.json", "jsconfig.json"], |x: &&str, _| *x == flc).copied()
}
