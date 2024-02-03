use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    io,
    iter::FromIterator,
    mem,
    path::{Path as StdPath, PathBuf},
};

use gc::{Finalize, Gc, GcCell, Trace};
use regex::Regex;
use typescript_rust::{
    compare_strings_case_insensitive, compare_strings_case_sensitive, comparison_to_ordering,
    debug_cell, ends_with, equate_strings_case_insensitive, find, find_index, for_each,
    fs_exists_sync, fs_mkdir_sync, fs_readdir_sync, fs_stat_sync, fs_unlink_sync,
    get_base_file_name, get_sys, id_arena::Id, map, normalize_slashes, option_declarations,
    ordered_remove_item_at, path_join, per_arena, starts_with, AllArenas, CommandLineOption,
    CommandLineOptionInterface, CommandLineOptionMapTypeValue, CommandLineOptionType, Extension,
    FileSystemEntries, HasArena, InArena, StatLike,
};

use crate::{vfs, vpath, HasArenaHarness, InArenaHarness, RunnerBase, StringOrFileBasedTest};

pub trait IO: vfs::FileSystemResolverHost {
    fn new_line(&self) -> &'static str;
    fn get_current_directory(&self) -> io::Result<String>;
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
}

#[derive(Copy, Clone)]
pub struct ListFilesOptions {
    pub recursive: Option<bool>,
}

pub fn get_io_id(arena: &impl HasArenaHarness) -> Id<NodeIO> {
    per_arena!(NodeIO, arena, create_node_io(arena))
}

pub fn get_io(arena: &impl HasArenaHarness) -> debug_cell::Ref<NodeIO> {
    get_io_id(arena).ref_(arena)
}

pub const harness_new_line: &'static str = "\r\n";

fn create_node_io(arena: &impl HasArenaHarness) -> Id<NodeIO> {
    NodeIO::new(arena)
}

#[derive(Trace, Finalize)]
// struct NodeIO {
pub struct NodeIO;

impl NodeIO {
    pub fn new(arena: &impl HasArenaHarness) -> Id<Self> {
        arena.alloc_node_io(Self)
    }

    fn files_in_folder(
        &self,
        spec: Option<&Regex>,
        options: Option<ListFilesOptions>,
        folder: impl AsRef<StdPath>,
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

    fn get_current_directory(&self) -> io::Result<String> {
        get_sys(self).ref_(self).get_current_directory()
    }

    fn read_file(&self, path: &StdPath) -> Option<String> {
        get_sys(self)
            .ref_(self)
            .read_file(path.to_str().unwrap())
            .unwrap()
    }

    fn write_file(&self, path: &StdPath, content: &str) {
        get_sys(self)
            .ref_(self)
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
}

impl vfs::FileSystemResolverHost for NodeIO {
    fn get_accessible_file_system_entries(&self, dirname: &str) -> FileSystemEntries {
        // try {
        let mut entries = fs_readdir_sync(if !dirname.is_empty() { dirname } else { "." })
            .unwrap_or_default()
            .into_iter()
            .map(|os_string| os_string.into_string().unwrap())
            .collect::<Vec<_>>();
        let use_case_sensitive_file_names =
            get_sys(self).ref_(self).use_case_sensitive_file_names();
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
        get_sys(self).ref_(self).get_file_size(path).unwrap()
    }

    fn read_file(&self, path: &str) -> Option<String> {
        get_sys(self).ref_(self).read_file(path).ok().flatten()
    }

    fn file_exists(&self, path: &str) -> bool {
        get_sys(self).ref_(self).file_exists(path)
    }

    fn directory_exists(&self, path: &str) -> bool {
        get_sys(self).ref_(self).directory_exists(path)
    }

    fn get_workspace_root(&self) -> String {
        "/Users/jrosse/prj/tsc-rust/typescript_rust/typescript_src/".to_owned()
    }
}

impl HasArena for NodeIO {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
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

#[allow(dead_code)]
pub fn set_light_mode(flag: bool) {
    light_mode_.with(|light_mode| {
        light_mode.set(flag);
    })
}

pub mod Compiler {
    use std::{borrow::Cow, cell::RefCell, cmp, collections::HashMap, convert::TryInto, io};

    use gc::{Finalize, Gc, GcCell, Trace};
    use regex::Regex;
    use typescript_rust::{
        NonEmpty, _d, combine_paths, compare_diagnostics, compare_paths, compute_line_starts,
        count_where, create_get_canonical_file_name, create_source_file, debug_cell,
        diagnostic_category_name, file_extension_is, flatten_diagnostic_message_text, for_each,
        format_diagnostics, format_diagnostics_with_color_and_context, format_location,
        get_allow_js_compiler_option, get_base_file_name, get_declaration_emit_extension_for_path,
        get_emit_script_target, get_error_count_for_summary, get_error_summary_text,
        get_normalized_absolute_path, id_arena::Id, map, normalize_slashes, option_declarations,
        parse_custom_type_option, parse_list_type_option, per_arena, regex, remove_file_extension,
        return_ok_default_if_none, sort, starts_with, text_span_end, to_path, AllArenas,
        CommandLineOption, CommandLineOptionBaseBuilder, CommandLineOptionInterface,
        CommandLineOptionType, Comparison, CompilerOptions, CompilerOptionsBuilder,
        CompilerOptionsValue, Diagnostic, DiagnosticInterface,
        DiagnosticRelatedInformationInterface, Extension, FormatDiagnosticsHost,
        GetOrInsertDefault, HasArena, InArena, NewLineKind, ScriptKind, ScriptReferenceHost,
        StringOrDiagnosticMessage, TextSpan,
    };

    use super::{get_io_id, is_built_file, is_default_library_file, Baseline, TestCaseParser};
    use crate::{
        compiler, documents, fakes, get_io, vfs, vpath, AllArenasHarness, HasArenaHarness, Utils,
        IO,
    };

    #[derive(Default)]
    pub struct WriterAggregator {
        pub lines: Vec<String>,
        pub current_line: Option<String>,
    }

    impl WriterAggregator {
        pub fn write(&mut self, str_: &str) {
            self.current_line.get_or_insert_default_().push_str(str_);
        }

        pub fn write_line(&mut self, str_: &str) {
            self.current_line.get_or_insert_default_().push_str(str_);
            self.lines.push(self.current_line.take().unwrap());
        }

        pub fn close(&mut self) {
            if self.current_line.is_some() {
                self.lines.push(self.current_line.take().unwrap());
            }
        }

        pub fn reset(&mut self) {
            *self = Default::default();
        }
    }

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

    pub(crate) fn harness_option_declarations(
        arena: &impl HasArena,
    ) -> debug_cell::Ref<Vec<Id<CommandLineOption>>> {
        per_arena!(
            Vec<Id<CommandLineOption>>,
            arena,
            arena.alloc_vec_command_line_option(vec![
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("allowNonTsExtensions".to_string())
                        .type_(CommandLineOptionType::Boolean)
                        .default_value_description(StringOrDiagnosticMessage::String(
                            "false".to_string()
                        ))
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("useCaseSensitiveFileNames".to_string())
                        .type_(CommandLineOptionType::Boolean)
                        .default_value_description(StringOrDiagnosticMessage::String(
                            "false".to_string()
                        ))
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("baselineFile".to_string())
                        .type_(CommandLineOptionType::String)
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("includeBuiltFile".to_string())
                        .type_(CommandLineOptionType::String)
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("fileName".to_string())
                        .type_(CommandLineOptionType::String)
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("libFiles".to_string())
                        .type_(CommandLineOptionType::String)
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("noErrorTruncation".to_string())
                        .type_(CommandLineOptionType::Boolean)
                        .default_value_description(StringOrDiagnosticMessage::String(
                            "false".to_string()
                        ))
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("suppressOutputPathCheck".to_string())
                        .type_(CommandLineOptionType::Boolean)
                        .default_value_description(StringOrDiagnosticMessage::String(
                            "false".to_string()
                        ))
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("noImplicitReferences".to_string())
                        .type_(CommandLineOptionType::Boolean)
                        .default_value_description(StringOrDiagnosticMessage::String(
                            "false".to_string()
                        ))
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("currentDirectory".to_string())
                        .type_(CommandLineOptionType::String)
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("symlink".to_string())
                        .type_(CommandLineOptionType::String)
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("link".to_string())
                        .type_(CommandLineOptionType::String)
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("noTypesAndSymbols".to_string())
                        .type_(CommandLineOptionType::Boolean)
                        .default_value_description(StringOrDiagnosticMessage::String(
                            "false".to_string()
                        ))
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
                arena.alloc_command_line_option(
                    CommandLineOptionBaseBuilder::default()
                        .name("fullEmitPaths".to_string())
                        .type_(CommandLineOptionType::Boolean)
                        .default_value_description(StringOrDiagnosticMessage::String(
                            "false".to_string()
                        ))
                        .build()
                        .unwrap()
                        .try_into()
                        .unwrap()
                ),
            ])
        )
    }

    thread_local! {
        static options_index: RefCell<Option<HashMap<String, Id<CommandLineOption>>>> = RefCell::new(None);
    }
    fn get_command_line_option(name: &str, arena: &impl HasArena) -> Option<Id<CommandLineOption>> {
        options_index.with(|options_index_| {
            options_index_
                .borrow_mut()
                .get_or_insert_with(|| {
                    let mut options_index_ = HashMap::new();
                    for &option in &*option_declarations(arena).ref_(arena) {
                        options_index_.insert(option.ref_(arena).name().to_lowercase(), option);
                    }
                    for &option in &*harness_option_declarations(arena) {
                        options_index_.insert(option.ref_(arena).name().to_lowercase(), option);
                    }
                    options_index_
                })
                .get(&name.to_lowercase())
                .cloned()
        })
    }

    fn set_compiler_options_from_harness_setting(
        settings: &HashMap<String, String>,
        options: &mut CompilerOptionsAndHarnessOptions,
        arena: &impl HasArena,
    ) {
        for (name, value) in settings {
            let option = get_command_line_option(name, arena);
            if let Some(option) = option.as_ref() {
                let mut errors: Vec<Id<Diagnostic>> = vec![];
                if HarnessOptions::is_harness_option(name) {
                    options.harness_options.set_value_from_command_line_option(
                        &option.ref_(arena),
                        option_value(&option.ref_(arena), value, &mut errors, arena),
                    );
                } else {
                    options.compiler_options.set_value_from_command_line_option(
                        &option.ref_(arena),
                        option_value(&option.ref_(arena), value, &mut errors, arena),
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
        errors: &mut Vec<Id<Diagnostic>>,
        arena: &impl HasArena,
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
            CommandLineOptionType::List => CompilerOptionsValue::VecString(parse_list_type_option(
                option,
                Some(value),
                errors,
                arena,
            )),
            CommandLineOptionType::Map(_) => {
                parse_custom_type_option(option, Some(value), errors, arena)
            }
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
        arena: &impl HasArenaHarness,
    ) -> io::Result<compiler::CompilationResult> {
        let mut options: CompilerOptionsAndHarnessOptions =
            if let Some(compiler_options) = compiler_options {
                CompilerOptionsAndHarnessOptions {
                    compiler_options: compiler_options.clone(),
                    harness_options: Default::default(),
                }
            } else {
                CompilerOptionsAndHarnessOptions {
                    compiler_options: CompilerOptionsBuilder::default()
                        .no_resolve(false)
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
            set_compiler_options_from_harness_setting(harness_settings, &mut options, arena);
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
            get_io_id(arena),
            !use_case_sensitive_file_names,
            Some(vfs::FileSystemCreateOptions {
                documents: Some(docs),
                cwd: Some(current_directory.to_owned()),
                ..Default::default()
            }),
            arena,
        )?);
        if let Some(symlinks) = symlinks {
            fs.apply(symlinks)?;
        }
        let host = fakes::CompilerHost::new(
            fs,
            Some(arena.alloc_compiler_options(options.compiler_options.clone())),
            None,
            arena,
        )?;
        let mut result = compiler::compile_files(
            host,
            Some(&program_file_names),
            &options.compiler_options,
            arena,
        )?;
        result.symlinks = symlinks.cloned();
        Ok(result)
    }

    struct CompilerOptionsAndHarnessOptions {
        compiler_options: CompilerOptions,
        harness_options: HarnessOptions,
    }

    pub struct DeclarationCompilationContext {
        pub decl_input_files: Vec<Gc<TestFile>>,
        pub decl_other_files: Vec<Gc<TestFile>>,
        pub harness_settings: Option<TestCaseParser::CompilerSettings /*& HarnessOptions*/>,
        pub options: Id<CompilerOptions>,
        pub current_directory: Option<String>,
    }

    pub fn prepare_declaration_compilation_context(
        input_files: &[Gc<TestFile>],
        other_files: &[Gc<TestFile>],
        result: &compiler::CompilationResult,
        harness_settings: &TestCaseParser::CompilerSettings, /*& HarnessOptions*/
        options: Id<CompilerOptions>,
        current_directory: Option<&str>,
        arena: &impl HasArena,
    ) -> io::Result<Option<DeclarationCompilationContext>> {
        if options.ref_(arena).declaration == Some(true) && result.diagnostics.is_empty() {
            if options.ref_(arena).emit_declaration_only == Some(true) {
                if !result.js.is_empty() || result.dts.is_empty() {
                    panic!(
                        "Only declaration files should be generated when emitDeclarationOnly:true"
                    );
                }
            } else if result.dts.len() != result.get_number_of_js_files(false) {
                panic!("There were no errors and declFiles generated did not match number of js files generated");
            }
        }

        let mut decl_input_files: Vec<Gc<TestFile>> = _d();
        let mut decl_other_files: Vec<Gc<TestFile>> = _d();

        if options.ref_(arena).declaration == Some(true)
            && result.diagnostics.is_empty()
            && !result.dts.is_empty()
        {
            for file in input_files {
                add_dts_file(
                    &options.ref_(arena),
                    result,
                    &decl_input_files.clone(),
                    &decl_other_files,
                    file.clone(),
                    &mut decl_input_files,
                    arena,
                )?;
            }
            for file in other_files {
                add_dts_file(
                    &options.ref_(arena),
                    result,
                    &decl_input_files,
                    &decl_other_files.clone(),
                    file.clone(),
                    &mut decl_other_files,
                    arena,
                )?;
            }
            return Ok(Some(DeclarationCompilationContext {
                decl_input_files,
                decl_other_files,
                harness_settings: Some(harness_settings.clone()),
                options,
                current_directory: current_directory
                    .non_empty()
                    .map(|current_directory| current_directory.to_owned())
                    .or_else(|| harness_settings.get("currentDirectory").cloned()),
            }));
        }

        Ok(None)
    }

    fn add_dts_file(
        options: &CompilerOptions,
        result: &compiler::CompilationResult,
        decl_input_files: &[Gc<TestFile>],
        decl_other_files: &[Gc<TestFile>],
        file: Gc<TestFile>,
        dts_files: &mut Vec<Gc<TestFile>>,
        arena: &impl HasArena,
    ) -> io::Result<()> {
        if vpath::is_declaration(&file.unit_name) || vpath::is_json(&file.unit_name) {
            dts_files.push(file);
        } else if vpath::is_type_script(&file.unit_name)
            || vpath::is_java_script(&file.unit_name) && get_allow_js_compiler_option(options)
        {
            let decl_file = find_result_code_file(result, options, &file.unit_name, arena)?;
            if let Some(decl_file) = decl_file.filter(|decl_file| {
                find_unit(&decl_file.file, decl_input_files).is_none()
                    && find_unit(&decl_file.file, decl_other_files).is_none()
            }) {
                dts_files.push(Gc::new(TestFile {
                    unit_name: decl_file.file.clone(),
                    content: Utils::remove_byte_order_mark(decl_file.text.clone()),
                    file_options: None,
                }));
            }
        }

        Ok(())
    }

    fn find_result_code_file(
        result: &compiler::CompilationResult,
        options: &CompilerOptions,
        file_name: &str,
        arena: &impl HasArena,
    ) -> io::Result<Option<Gc<documents::TextDocument>>> {
        let source_file = result
            .program
            .unwrap()
            .ref_(arena)
            .get_source_file(file_name);
        assert!(
            source_file.is_some(),
            "Program has no source file with name '{file_name}'",
        );
        let source_file = source_file.unwrap();
        let source_file_ref = source_file.ref_(arena);
        let source_file_as_source_file = source_file_ref.as_source_file();
        let out_file = options
            .out_file
            .as_ref()
            .non_empty()
            .or_else(|| options.out.as_ref());
        let source_file_name = match out_file {
            None => match options.out_dir.as_ref().non_empty() {
                Some(options_out_dir) => {
                    let mut source_file_path = get_normalized_absolute_path(
                        &source_file_as_source_file.file_name(),
                        Some(&*result.vfs().cwd()?),
                    );
                    source_file_path = source_file_path.replace(
                        &result
                            .program
                            .unwrap()
                            .ref_(arena)
                            .get_common_source_directory(),
                        "",
                    );
                    combine_paths(options_out_dir, &[Some(&source_file_path)])
                }
                None => source_file_as_source_file.file_name().clone(),
            },
            Some(out_file) => out_file.clone(),
        };

        let d_ts_file_name = format!(
            "{}{}",
            remove_file_extension(&source_file_name),
            get_declaration_emit_extension_for_path(&source_file_name)
        );
        Ok(result.dts.get(&d_ts_file_name).cloned())
    }

    fn find_unit(file_name: &str, units: &[Gc<TestFile>]) -> Option<Gc<TestFile>> {
        for_each(units, |unit: &Gc<TestFile>, _| {
            (unit.unit_name == file_name).then(|| unit.clone())
        })
    }

    pub struct CompileDeclarationFilesReturn {
        pub decl_input_files: Vec<Gc<TestFile>>,
        pub decl_other_files: Vec<Gc<TestFile>>,
        pub decl_result: compiler::CompilationResult,
    }

    pub fn compile_declaration_files(
        context: Option<&DeclarationCompilationContext>,
        symlinks: Option<&vfs::FileSet>,
        arena: &impl HasArenaHarness,
    ) -> io::Result<Option<CompileDeclarationFilesReturn>> {
        let context = return_ok_default_if_none!(context);
        let decl_input_files = &context.decl_input_files;
        let decl_other_files = &context.decl_other_files;
        let harness_settings = context.harness_settings.as_ref();
        let options = context.options;
        let current_directory = context.current_directory.as_deref();
        let output = compile_files(
            decl_input_files,
            decl_other_files,
            harness_settings,
            Some(&options.ref_(arena)),
            current_directory,
            symlinks,
            arena,
        )?;
        Ok(Some(CompileDeclarationFilesReturn {
            decl_input_files: decl_input_files.to_owned(),
            decl_other_files: decl_other_files.to_owned(),
            decl_result: output,
        }))
    }

    pub fn minimal_diagnostics_to_string(
        diagnostics: &[Id<Diagnostic>],
        pretty: Option<bool>,
        arena: &impl HasArena,
    ) -> io::Result<String> {
        let host = MinimalDiagnosticsToStringFormatDiagnosticsHost;
        if pretty == Some(true) {
            format_diagnostics_with_color_and_context(diagnostics, &host, arena)
        } else {
            format_diagnostics(diagnostics, &host, arena)
        }
    }

    struct MinimalDiagnosticsToStringFormatDiagnosticsHost;

    impl FormatDiagnosticsHost for MinimalDiagnosticsToStringFormatDiagnosticsHost {
        fn get_current_directory(&self) -> io::Result<String> {
            Ok("".to_owned())
        }

        fn get_new_line(&self) -> Cow<str> {
            get_io(self).new_line().into()
        }

        fn get_canonical_file_name(&self, file_name: &str) -> String {
            get_canonical_file_name(file_name)
        }
    }

    impl HasArena for MinimalDiagnosticsToStringFormatDiagnosticsHost {
        fn arena(&self) -> &AllArenas {
            unimplemented!()
        }
    }

    impl HasArenaHarness for MinimalDiagnosticsToStringFormatDiagnosticsHost {
        fn arena_harness(&self) -> &AllArenasHarness {
            unimplemented!()
        }
    }

    pub fn get_error_baseline(
        input_files: &[Gc<TestFile>],
        diagnostics: &[Id<Diagnostic>],
        pretty: Option<bool>,
        arena: &impl HasArenaHarness,
    ) -> io::Result<String> {
        let mut output_lines = "".to_owned();
        for value in iterate_error_baseline(
            input_files,
            diagnostics,
            Some(IterateErrorBaselineOptions {
                pretty,
                case_sensitive: None,
                current_directory: None,
            }),
            arena,
        )? {
            let (_, content, _) = value;
            output_lines.push_str(&content);
        }
        if pretty == Some(true) {
            output_lines.push_str(&get_error_summary_text(
                get_error_count_for_summary(diagnostics, arena),
                get_io(arena).new_line(),
            ));
        }
        Ok(output_lines)
    }

    pub const diagnostic_summary_marker: &'static str = "__diagnosticSummary";
    pub const global_errors_marker: &'static str = "__globalErrors";

    pub fn iterate_error_baseline(
        input_files: &[Gc<TestFile>],
        diagnostics: &[Id<Diagnostic>],
        options: Option<IterateErrorBaselineOptions>,
        arena: &impl HasArenaHarness,
    ) -> io::Result<Vec<(String, String, usize)>> {
        let diagnostics: Vec<_> = sort(diagnostics, |a: &Id<Diagnostic>, b: &Id<Diagnostic>| {
            compare_diagnostics(&*a.ref_(arena), &*b.ref_(arena), arena)
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
                    &*minimal_diagnostics_to_string(
                        &diagnostics,
                        options.as_ref().and_then(|options| options.pretty),
                        arena,
                    )?,
                    None,
                ),
                get_io(arena).new_line(),
                get_io(arena).new_line(),
            ),
            diagnostics.len(),
        ));

        let mut global_errors = diagnostics
            .iter()
            .filter(|err| err.ref_(arena).maybe_file().is_none());
        global_errors.try_for_each(|diagnostic| {
            output_error_text(
                &format_diagnsotic_host,
                &mut output_lines,
                &mut first_line,
                &mut errors_reported,
                &mut total_errors_reported_in_non_library_files,
                &diagnostic.ref_(arena),
                arena,
            )
        })?;
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
                let err_fn = e.ref_(arena).maybe_file();
                matches!(
                    err_fn.as_ref(),
                    Some(err_fn) if compare_paths(
                        &Utils::remove_test_path_prefixes(
                            &err_fn.ref_(arena).as_source_file().file_name(),
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
                        start: err_diagnostic.ref_(arena).start(),
                        length: err_diagnostic.ref_(arena).length(),
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
                                &err_diagnostic.ref_(arena),
                                arena,
                            )?;
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
            |diagnostic: &Id<Diagnostic>, _| {
                matches!(
                    diagnostic.ref_(arena).maybe_file(),
                    Some(diagnostic_file) if is_default_library_file(&diagnostic_file.ref_(arena).as_source_file().file_name()) ||
                        is_built_file(&diagnostic_file.ref_(arena).as_source_file().file_name())
                )
            },
        );

        let num_test262_harness_diagnostics = count_where(
            Some(&diagnostics),
            |diagnostic: &Id<Diagnostic>, _| {
                matches!(
                    diagnostic.ref_(arena).maybe_file(),
                    Some(diagnostic_file) if diagnostic_file.ref_(arena).as_source_file().file_name().contains("test262-harness")
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

        Ok(ret)
    }

    fn new_line(first_line: &mut bool) -> &'static str {
        if *first_line {
            *first_line = false;
            return "";
        }
        "\r\n"
    }

    struct FormatDiagnsoticHost<'a> {
        options: Option<&'a IterateErrorBaselineOptions>,
        get_canonical_file_name: fn(&str) -> String,
    }

    impl<'a> FormatDiagnsoticHost<'a> {
        pub fn new(options: Option<&'a IterateErrorBaselineOptions>) -> Self {
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

    impl FormatDiagnosticsHost for FormatDiagnsoticHost<'_> {
        fn get_current_directory(&self) -> io::Result<String> {
            Ok(self
                .options
                .and_then(|options| options.current_directory.clone())
                .unwrap_or_else(|| "".to_owned()))
        }

        fn get_new_line(&self) -> Cow<str> {
            get_io(self).new_line().into()
        }

        fn get_canonical_file_name(&self, file_name: &str) -> String {
            (self.get_canonical_file_name)(file_name)
        }
    }

    impl HasArena for FormatDiagnsoticHost<'_> {
        fn arena(&self) -> &AllArenas {
            unimplemented!()
        }
    }

    impl HasArenaHarness for FormatDiagnsoticHost<'_> {
        fn arena_harness(&self) -> &AllArenasHarness {
            unimplemented!()
        }
    }

    fn output_error_text(
        format_diagnsotic_host: &FormatDiagnsoticHost<'_>,
        output_lines: &mut String,
        first_line: &mut bool,
        errors_reported: &mut usize,
        total_errors_reported_in_non_library_files: &mut usize,
        error: &Diagnostic,
        arena: &impl HasArenaHarness,
    ) -> io::Result<()> {
        let message = flatten_diagnostic_message_text(
            Some(error.message_text()),
            get_io(arena).new_line(),
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
                    info.ref_(arena).code(),
                    if let Some(info_file) = info.ref_(arena).maybe_file() {
                        format!(
                            " {}",
                            format_location(
                                info_file,
                                info.ref_(arena).start(),
                                format_diagnsotic_host,
                                Some(|a: &str, _b: &str| a.to_owned()),
                                arena,
                            )?
                        )
                    } else {
                        "".to_owned()
                    },
                    flatten_diagnostic_message_text(
                        Some(info.ref_(arena).message_text()),
                        get_io(arena).new_line(),
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
            Some(error_file) => {
                !is_default_library_file(&error_file.ref_(arena).as_source_file().file_name())
            }
        } {
            *total_errors_reported_in_non_library_files += 1;
        }

        Ok(())
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
        errors: &[Id<Diagnostic>],
        pretty: Option<bool>,
        arena: &impl HasArenaHarness,
    ) -> io::Result<()> {
        lazy_static! {
            static ref ts_extension_regex: Regex = Regex::new(r"\.tsx?$").unwrap();
        }
        Baseline::run_baseline(
            &ts_extension_regex.replace(baseline_path, ".errors.txt"),
            if
            /* !errors ||*/
            !errors.is_empty() {
                Some(get_error_baseline(input_files, errors, pretty, arena)?)
            } else {
                None
            }
            .as_deref(),
            None,
            arena,
        );

        Ok(())
    }

    pub fn do_js_emit_baseline(
        baseline_path: &str,
        header: &str,
        options: Id<CompilerOptions>,
        result: &compiler::CompilationResult,
        ts_config_files: &[Gc<TestFile>],
        to_be_compiled: &[Gc<TestFile>],
        other_files: &[Gc<TestFile>],
        harness_settings: &TestCaseParser::CompilerSettings,
        arena: &impl HasArenaHarness,
    ) -> io::Result<()> {
        if options.ref_(arena).no_emit != Some(true)
            && options.ref_(arena).emit_declaration_only != Some(true)
            && result.js.is_empty()
            && result.diagnostics.is_empty()
        {
            panic!(
                "Expected at least one js file to be emitted or at least one error to be created."
            );
        }

        let mut ts_code = "".to_owned();
        let ts_sources = [other_files, to_be_compiled].concat();
        if ts_sources.len() > 1 {
            ts_code.push_str(&format!("//// [{}] ////\r\n\r\n", header));
        }
        for (i, ts_source) in ts_sources.iter().enumerate() {
            ts_code.push_str(&format!(
                "//// [{}]\r\n",
                get_base_file_name(&ts_source.unit_name, None, None)
            ));
            ts_code.push_str(&format!(
                "{}{}",
                ts_source.content,
                if i < ts_sources.len() - 1 { "\r\n" } else { "" }
            ));
        }

        let mut js_code = "".to_owned();
        result.js.try_for_each(|file, _, _| {
            if !js_code.is_empty() && !js_code.ends_with("\n") {
                js_code.push_str("\r\n");
            }
            if result.diagnostics.is_empty() && !file.file.ends_with(Extension::Json.to_str()) {
                let file_parse_result = create_source_file(
                    &file.file,
                    file.text.clone(),
                    get_emit_script_target(&options.ref_(arena)),
                    Some(false),
                    Some(if file.file.ends_with("x") {
                        ScriptKind::JSX
                    } else {
                        ScriptKind::JS
                    }),
                    arena,
                )?;
                let file_parse_result_parse_diagnostics = file_parse_result
                    .ref_(arena)
                    .as_source_file()
                    .parse_diagnostics()
                    .ref_(arena);
                if !file_parse_result_parse_diagnostics.is_empty() {
                    js_code.push_str(&get_error_baseline(
                        &[file.as_test_file()],
                        &file_parse_result_parse_diagnostics,
                        None,
                        arena,
                    )?);
                    return Ok(());
                }
            }
            js_code.push_str(&file_output(file, harness_settings));

            Ok(())
        })?;

        if !result.dts.is_empty() {
            js_code.push_str("\r\n\r\n");
            result.dts.for_each(|decl_file, _, _| {
                js_code.push_str(&file_output(decl_file, harness_settings));
            });
        }

        let decl_file_context = prepare_declaration_compilation_context(
            to_be_compiled,
            other_files,
            result,
            harness_settings,
            options.clone(),
            None,
            arena,
        )?;
        let decl_file_compilation_result =
            compile_declaration_files(decl_file_context.as_ref(), result.symlinks.as_ref(), arena)?;

        if let Some(decl_file_compilation_result) =
            decl_file_compilation_result.filter(|decl_file_compilation_result| {
                !decl_file_compilation_result
                    .decl_result
                    .diagnostics
                    .is_empty()
            })
        {
            js_code.push_str("\r\n\r\n//// [DtsFileErrors]\r\n");
            js_code.push_str("\r\n\r\n");
            js_code.push_str(&get_error_baseline(
                &[
                    ts_config_files,
                    &decl_file_compilation_result.decl_input_files,
                    &decl_file_compilation_result.decl_other_files,
                ]
                .concat(),
                &decl_file_compilation_result.decl_result.diagnostics,
                None,
                arena,
            )?);
        }

        Baseline::run_baseline(
            &regex!(r#"\.tsx?"#).replace(baseline_path, Extension::Js.to_str()),
            (!js_code.is_empty())
                .then(|| format!("{}\r\n\r\n{}", ts_code, js_code))
                .as_deref(),
            None,
            arena,
        );

        Ok(())
    }

    fn file_output(
        file: &documents::TextDocument,
        harness_settings: &TestCaseParser::CompilerSettings,
    ) -> String {
        let file_name = if harness_settings.get("fullEmitPaths").non_empty().is_some() {
            Utils::remove_test_path_prefixes(&file.file, None).into_owned()
        } else {
            get_base_file_name(&file.file, None, None)
        };
        format!(
            "//// [{}]\r\n{}",
            file_name,
            Utils::remove_test_path_prefixes(&file.text, None,)
        )
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

fn split_vary_by_setting_value(
    text: &str,
    vary_by: &str,
    arena: &impl HasArena,
) -> Option<Vec<String>> {
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
    let values = get_vary_by_star_setting_values(vary_by, arena);

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
    arena: &impl HasArena,
) -> Option<HashMap<&'static str, CommandLineOptionMapTypeValueOrUsize>> {
    let option = for_each(
        &*option_declarations(arena).ref_(arena),
        |decl: &Id<CommandLineOption>, _| {
            if equate_strings_case_insensitive(decl.ref_(arena).name(), vary_by) {
                Some(decl.clone())
            } else {
                None
            }
        },
    )?;
    if let CommandLineOptionType::Map(option_type) = option.ref_(arena).type_() {
        return Some(HashMap::from_iter(
            option_type
                .into_iter()
                .map(|(key, value)| (*key, value.clone().into())),
        ));
    }
    if matches!(option.ref_(arena).type_(), CommandLineOptionType::Boolean) {
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

pub fn get_file_based_test_configurations(
    settings: &TestCaseParser::CompilerSettings,
    vary_by: &[impl AsRef<str>],
    arena: &impl HasArena,
) -> Option<Vec<FileBasedTestConfiguration>> {
    let mut vary_by_entries: Option<Vec<(String, Vec<String>)>> = None;
    let mut variation_count = 1;
    for vary_by_key in vary_by {
        let vary_by_key = vary_by_key.as_ref();
        if settings.contains_key(vary_by_key) {
            let entries =
                split_vary_by_setting_value(settings.get(vary_by_key).unwrap(), vary_by_key, arena);
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
    use std::{collections::HashMap, io};

    use gc::Gc;
    use regex::Regex;
    use typescript_rust::{
        for_each, get_base_file_name, get_directory_path, get_normalized_absolute_path,
        normalize_path, ordered_remove_item_at, parse_json_source_file_config_file_content,
        parse_json_text, HasArena, InArena, ModuleResolutionHost, ParseConfigHost,
        ParsedCommandLine,
    };

    use super::get_config_name_from_file_name;
    use crate::{vfs, Utils};

    pub type CompilerSettings = HashMap<String, String>;

    #[derive(Clone)]
    pub struct TestUnitData {
        pub content: Option<String>,
        pub name: String,
        pub file_options: HashMap<String, String>,
        pub original_file_path: String,
        pub references: Vec<String>,
    }

    lazy_static! {
        static ref line_ending_regex: Regex = Regex::new(r"\r?\n|\r").unwrap();
        static ref option_regex: Regex = Regex::new(r"^[/]{2}\s*@(\w+)\s*:\s*([^\r\n]*)").unwrap();
        static ref link_regex: Regex =
            Regex::new(r"^[/]{2}\s*@link\s*:\s*([^\r\n]*)\s*->\s*([^\r\n]*)").unwrap();
    }

    pub fn parse_symlink_from_test(line: &str, symlinks: &mut Option<vfs::FileSet>) -> bool /*Option<vfs::FileSet>*/
    {
        let link_meta_data = link_regex.captures(line);
        if link_meta_data.is_none() {
            return false /*None*/;
        }
        let link_meta_data = link_meta_data.unwrap();

        symlinks.get_or_insert_with(|| vfs::FileSet::new()).insert(
            link_meta_data.get(2).unwrap().as_str().trim().to_owned(),
            Some(
                vfs::Symlink::new(
                    link_meta_data.get(1).unwrap().as_str().trim().to_owned(),
                    None,
                )
                .into(),
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
        arena: &impl HasArena,
    ) -> io::Result<TestCaseContent> {
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
                        content: current_file_content.clone(),
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
            content: Some(current_file_content.unwrap_or_else(|| "".to_owned())),
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
                let config_json = parse_json_text(&data.name, data.content.clone().unwrap(), arena);
                // assert!(config_json.as_source_file().end_of_file_token().is_some());
                let mut base_dir = normalize_path(&get_directory_path(&data.name));
                if let Some(root_dir) = root_dir.filter(|root_dir| !root_dir.is_empty()) {
                    base_dir = get_normalized_absolute_path(&base_dir, Some(root_dir));
                }
                ts_config = Some(parse_json_source_file_config_file_content(
                    config_json,
                    &parse_config_host,
                    &base_dir,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    arena,
                )?);
                let mut options = ts_config.as_ref().unwrap().options.ref_(arena).clone();
                options.config_file_path = Some(data.name.clone());
                ts_config.as_mut().unwrap().options = arena.alloc_compiler_options(options);
                ts_config_file_unit_data = Some(data.clone());

                indexes_to_remove.push(i);

                break;
            }
        }
        drop(parse_config_host);
        for i in indexes_to_remove {
            ordered_remove_item_at(&mut test_unit_data, i);
        }
        Ok(TestCaseContent {
            settings,
            test_unit_data,
            ts_config,
            ts_config_file_unit_data,
            symlinks,
        })
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
            _root_dir: &str,
            _extensions: &[&str],
            _excludes: Option<&[String]>,
            _includes: &[String],
            _depth: Option<usize>,
        ) -> io::Result<Vec<String>> {
            Ok(Default::default())
        }

        fn file_exists(&self, _path: &str) -> bool {
            true
        }

        fn read_file(&self, name: &str) -> io::Result<Option<String>> {
            Ok(for_each(self.test_unit_data, |data: &TestUnitData, _| {
                if data.name.to_lowercase() == name.to_lowercase() {
                    Some(data.content.clone().unwrap())
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
    use std::{cell::RefCell, collections::HashMap};

    use pretty_assertions::StrComparison;

    use super::{get_io, user_specified_root, IO};
    use crate::{vfs::FileSystemResolverHost, HasArenaHarness, Utils};

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
        arena: &impl HasArenaHarness,
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
        if get_io(arena).file_exists(&ref_file_name) {
            expected = IO::read_file(&*get_io(arena), ref_file_name.as_ref()).unwrap();
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
        arena: &impl HasArenaHarness,
    ) {
        create_directory_structure(
            &get_io(arena)
                .directory_name(actual_file_name.as_ref())
                .unwrap(),
            arena,
        );

        if get_io(arena).file_exists(actual_file_name) {
            get_io(arena).delete_file(actual_file_name.as_ref());
        }

        let encoded_actual = Utils::encode_string(actual);
        if expected != encoded_actual {
            if actual == no_content {
                get_io(arena).write_file(format!("{}.delete", actual_file_name).as_ref(), "");
            } else {
                get_io(arena).write_file(actual_file_name.as_ref(), &encoded_actual);
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
                if !get_io(arena).file_exists(expected) {
                    panic!(
                        "New baseline created at {}",
                        get_io(arena).join_path(&[
                            "tests".as_ref(),
                            "baselines".as_ref(),
                            "local".as_ref(),
                            relative_file_name.as_ref(),
                        ])
                    );
                } else {
                    panic!("The baseline file {} has changed.", relative_file_name);
                }
            }
        }
    }

    fn create_directory_structure(dir_name: &str, arena: &impl HasArenaHarness) {
        if file_cache.with(|file_cache_| file_cache_.borrow().get(dir_name).copied() == Some(true))
            || get_io(arena).directory_exists(dir_name)
        {
            file_cache.with(|file_cache_| {
                file_cache_.borrow_mut().insert(dir_name.to_owned(), true);
            });
            return;
        }

        let parent_directory = get_io(arena).directory_name(dir_name.as_ref()).unwrap();
        if !parent_directory.is_empty() && parent_directory != dir_name {
            create_directory_structure(&parent_directory, arena);
        }
        get_io(arena).create_directory(dir_name.as_ref()).unwrap();
        file_cache.with(|file_cache_| {
            file_cache_.borrow_mut().insert(dir_name.to_owned(), true);
        });
    }

    pub fn run_baseline(
        relative_file_name: &str,
        actual: Option<&str>,
        opts: Option<BaselineOptions>,
        arena: &impl HasArenaHarness,
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
        let comparison = compare_to_baseline(actual, relative_file_name, opts.as_ref(), arena);
        write_comparison(
            &comparison.expected,
            &comparison.actual,
            relative_file_name,
            &actual_file_name,
            opts.as_ref(),
            arena,
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
