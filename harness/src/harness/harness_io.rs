use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::path::{Path as StdPath, PathBuf};
use std::rc::Rc;
use typescript_rust::{
    equate_strings_case_insensitive, find, find_index, for_each, fs_exists_sync, fs_readdir_sync,
    fs_stat_sync, get_base_file_name, get_sys, map, option_declarations, ordered_remove_item_at,
    path_join, starts_with, CommandLineOption, CommandLineOptionInterface,
    CommandLineOptionMapTypeValue, CommandLineOptionType, StatLike,
};

use crate::{RunnerBase, StringOrFileBasedTest};

pub trait IO {
    fn read_file(&self, path: &StdPath) -> Option<String>;
    fn enumerate_test_files(&self, runner: &RunnerBase) -> Vec<StringOrFileBasedTest>;
    fn list_files(
        &self,
        path: &str,
        filter: Option<&Regex>,
        options: Option<ListFilesOptions>,
    ) -> Vec<PathBuf>;
}

#[derive(Copy, Clone)]
pub struct ListFilesOptions {
    pub recursive: Option<bool>,
}

thread_local! {
    static IO_: RefCell<Rc<dyn IO>> = RefCell::new(Rc::new(create_node_io()));
}

pub fn with_io<TReturn, TCallback: FnMut(&dyn IO) -> TReturn>(mut callback: TCallback) -> TReturn {
    IO_.with(|io| callback(&**io.borrow()))
}

fn create_node_io() -> NodeIO {
    NodeIO {}
}

struct NodeIO {}

impl NodeIO {
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
    fn read_file(&self, path: &StdPath) -> Option<String> {
        get_sys().read_file(path.to_str().unwrap()).unwrap()
    }

    fn enumerate_test_files(&self, runner: &RunnerBase) -> Vec<StringOrFileBasedTest> {
        runner.get_test_files()
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

pub const user_specified_root: &'static str = "";

pub mod Compiler {
    use std::rc::Rc;
    use typescript_rust::CompilerOptions;

    use super::TestCaseParser;
    use crate::{compiler, vfs};

    pub struct TestFile {
        pub unit_name: String,
        pub content: String,
        pub file_options: Option<()>,
    }

    pub fn compile_files(
        input_files: &[TestFile],
        other_files: &[TestFile],
        harness_settings: Option<&TestCaseParser::CompilerSettings>,
        compiler_options: Option<Rc<CompilerOptions>>,
        current_directory: Option<&str>,
        symlinks: Option<&vfs::FileSet>,
    ) -> compiler::CompilationResult {
        unimplemented!()
    }
}

#[derive(Clone)]
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
    static boolean_vary_by_star_setting_values: RefCell<Option<HashMap<&'static str, CommandLineOptionMapTypeValueOrUsize>>> = RefCell::new(None);
}

fn get_vary_by_star_setting_values(
    vary_by: &str,
) -> Option<HashMap<&'static str, CommandLineOptionMapTypeValueOrUsize>> {
    let option = option_declarations.with(|option_declarations_| {
        for_each(option_declarations_, |decl: &Rc<CommandLineOption>, _| {
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
    use regex::Regex;
    use std::collections::HashMap;
    use std::io;
    use std::rc::Rc;
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
                ts_config.as_mut().unwrap().options = Rc::new(options);
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

pub fn get_config_name_from_file_name(
    filename: &str,
) -> Option<&'static str /*"tsconfig.json" | "jsconfig.json"*/> {
    let flc = get_base_file_name(filename, None, None).to_lowercase();
    find(&["tsconfig.json", "jsconfig.json"], |x: &&str, _| *x == flc).copied()
}
