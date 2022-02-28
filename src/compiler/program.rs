use std::cell::RefCell;
use std::cmp;
use std::rc::Rc;
use std::time::SystemTime;

use crate::{
    combine_paths, concatenate, create_source_file, create_type_checker, for_each,
    for_each_ancestor_directory_str, get_directory_path, get_emit_script_target,
    get_normalized_path_components, get_path_from_path_components, get_sys, is_rooted_disk_path,
    normalize_path, to_path as to_path_helper, CompilerHost, CompilerOptions, CreateProgramOptions,
    Diagnostic, ModuleKind, ModuleResolutionHost, ModuleSpecifierResolutionHost, Node, Path,
    Program, ScriptTarget, SourceFile, StructureIsReused, System, TypeChecker, TypeCheckerHost,
    TypeCheckerHostDebuggable,
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
    options: &CompilerOptions,
    set_parent_nodes: Option<bool>,
) -> impl CompilerHost {
    create_compiler_host_worker(options, set_parent_nodes)
}

fn create_compiler_host_worker(
    options: &CompilerOptions,
    set_parent_nodes: Option<bool>,
) -> impl CompilerHost {
    CompilerHostConcrete {
        set_parent_nodes,
        system: get_sys(),
    }
}

struct CompilerHostConcrete {
    set_parent_nodes: Option<bool>,
    system: Rc<dyn System>,
}

impl ModuleResolutionHost for CompilerHostConcrete {
    fn read_file(&self, file_name: &str) -> Option<String> {
        self.system.read_file(file_name)
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
        let text = self.read_file(file_name);
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

    fn get_canonical_file_name(&self, file_name: &str) -> String {
        file_name.to_string()
    }

    fn get_current_directory(&self) -> String {
        self.system.get_current_directory()
    }

    fn get_default_lib_file_name(&self, options: &CompilerOptions) -> String {
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

    fn use_case_sensitive_file_names(&self) -> bool {
        unimplemented!()
    }

    fn get_new_line(&self) -> String {
        unimplemented!()
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

impl Program {
    pub fn new(options: Rc<CompilerOptions>, files: Vec<Rc<Node>>) -> Rc<Self> {
        let rc = Rc::new(Program {
            _rc_wrapper: RefCell::new(None),
            options,
            files,
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

    pub fn get_syntactic_diagnostics(&self) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>> {
        self.get_diagnostics_helper(Program::get_syntactic_diagnostics_for_file)
    }

    pub fn get_semantic_diagnostics(&self) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_helper(Program::get_semantic_diagnostics_for_file)
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
    let CreateProgramOptions {
        root_names,
        options,
    } = root_names_or_options;

    let mut processing_other_files: Option<Vec<Rc<Node>>> = None;
    let mut files: Vec<Rc<Node>> = vec![];

    let host = create_compiler_host(&options, None);

    let current_directory = host.get_current_directory();

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
            process_root_file(&mut helper_context, name);
            Option::<()>::None
        });

        files = processing_other_files_present;
        println!("files: {:#?}", files);
        processing_other_files = None;
    }

    Program::new(options, files)
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

fn get_canonical_file_name(
    helper_context: &mut CreateProgramHelperContext,
    file_name: &str,
) -> String {
    helper_context.host.get_canonical_file_name(file_name)
}
