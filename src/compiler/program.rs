use std::rc::Rc;

use crate::{
    concatenate, create_source_file, create_type_checker, for_each, get_emit_script_target,
    get_sys, normalize_path, to_path as to_path_helper, CompilerHost, CompilerOptions,
    CreateProgramOptions, Diagnostic, ModuleKind, ModuleResolutionHost,
    ModuleSpecifierResolutionHost, Node, Path, Program, ScriptTarget, SourceFile,
    StructureIsReused, System, TypeChecker, TypeCheckerHost,
};

fn create_compiler_host(
    options: &CompilerOptions,
    set_parent_nodes: Option<bool>,
) -> impl CompilerHost {
    create_compiler_host_worker(options, set_parent_nodes)
}

struct CompilerHostConcrete {
    set_parent_nodes: Option<bool>,
    system: &'static dyn System,
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

pub(crate) trait SourceFileImportsList {}

impl SourceFileImportsList for SourceFile {}

pub(crate) fn get_mode_for_resolution_at_index<TFile: SourceFileImportsList>(
    file: &TFile,
    index: usize,
) -> Option<ModuleKind> {
    unimplemented!()
}

struct ProgramConcrete {
    options: Rc<CompilerOptions>,
    files: Vec<Rc</*SourceFile*/ Node>>,
    diagnostics_producing_type_checker: Option<TypeChecker>,
}

impl ProgramConcrete {
    pub fn new(options: Rc<CompilerOptions>, files: Vec<Rc<Node>>) -> Self {
        ProgramConcrete {
            options,
            files,
            diagnostics_producing_type_checker: None,
        }
    }

    fn get_diagnostics_producing_type_checker(&mut self) -> &mut TypeChecker {
        // self.diagnostics_producing_type_checker
        //     .get_or_insert_with(|| create_type_checker(self, true))

        // if let Some(type_checker) = self.diagnostics_producing_type_checker.as_ref() {
        //     return type_checker;
        // } else {
        //     self.diagnostics_producing_type_checker = Some(create_type_checker(self, true));
        //     self.diagnostics_producing_type_checker.as_ref().unwrap()
        // }
        if self.diagnostics_producing_type_checker.is_none() {
            self.diagnostics_producing_type_checker = Some(create_type_checker(self, true));
        }
        self.diagnostics_producing_type_checker.as_mut().unwrap()
    }

    fn get_diagnostics_helper(
        &mut self,
        get_diagnostics: fn(&mut ProgramConcrete, &SourceFile) -> Vec<Rc<Diagnostic>>,
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

    fn get_syntactic_diagnostics_for_file(
        &mut self,
        source_file: &SourceFile,
    ) -> Vec<Rc<Diagnostic>> {
        source_file.parse_diagnostics().clone()
    }

    fn get_semantic_diagnostics_for_file(
        &mut self,
        source_file: &SourceFile,
    ) -> Vec<Rc<Diagnostic>> {
        concatenate(
            filter_semantic_diagnostics(self.get_bind_and_check_diagnostics_for_file(source_file)),
            self.get_program_diagnostics(source_file),
        )
    }

    fn get_bind_and_check_diagnostics_for_file(
        &mut self,
        source_file: &SourceFile,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_and_cache_diagnostics(
            source_file,
            ProgramConcrete::get_bind_and_check_diagnostics_for_file_no_cache,
        )
    }

    fn get_bind_and_check_diagnostics_for_file_no_cache(
        &mut self,
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
        &mut self,
        source_file: &SourceFile,
        get_diagnostics: fn(&mut ProgramConcrete, &SourceFile) -> Vec<Rc<Diagnostic>>,
    ) -> Vec<Rc<Diagnostic>> {
        let result = get_diagnostics(self, source_file);
        result
    }
}

impl ModuleSpecifierResolutionHost for ProgramConcrete {}

impl Program for ProgramConcrete {
    fn get_syntactic_diagnostics(&mut self) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>> {
        self.get_diagnostics_helper(ProgramConcrete::get_syntactic_diagnostics_for_file)
    }

    fn get_semantic_diagnostics(&mut self) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_helper(ProgramConcrete::get_semantic_diagnostics_for_file)
    }
}

impl TypeCheckerHost for ProgramConcrete {
    fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        self.options.clone()
    }

    fn get_source_files(&self) -> Vec<Rc<Node>> {
        self.files.clone()
    }
}

struct CreateProgramHelperContext<'a> {
    processing_other_files: &'a mut Vec<Rc<Node>>,
    host: &'a dyn CompilerHost,
    current_directory: &'a str,
    options: Rc<CompilerOptions>,
}

pub fn create_program(root_names_or_options: CreateProgramOptions) -> impl Program {
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

    ProgramConcrete::new(options, files)
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

    let file = helper_context
        .host
        .get_source_file(file_name, get_emit_script_target(&*helper_context.options));

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
