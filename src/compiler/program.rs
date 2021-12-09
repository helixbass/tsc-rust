use crate::{
    create_source_file, for_each, get_sys, normalize_path, to_path as to_path_helper, CompilerHost,
    CreateProgramOptions, Diagnostic, ModuleResolutionHost, Path, Program, SourceFile,
    StructureIsReused, System,
};

fn create_compiler_host() -> impl CompilerHost {
    create_compiler_host_worker()
}

struct CompilerHostConcrete {
    system: &'static dyn System,
}

impl ModuleResolutionHost for CompilerHostConcrete {
    fn read_file(&self, file_name: &str) -> Option<String> {
        self.system.read_file(file_name)
    }
}

impl CompilerHost for CompilerHostConcrete {
    fn get_source_file(&self, file_name: &str) -> Option<SourceFile> {
        let text = self.read_file(file_name);
        match text {
            Some(text) => Some(create_source_file(file_name, &text)),
            None => None,
        }
    }

    fn get_canonical_file_name(&self, file_name: &str) -> String {
        file_name.to_string()
    }

    fn get_current_directory(&self) -> String {
        self.system.get_current_directory()
    }
}

fn create_compiler_host_worker() -> impl CompilerHost {
    CompilerHostConcrete { system: get_sys() }
}

struct ProgramConcrete {
    files: Vec<SourceFile>,
}

impl ProgramConcrete {
    pub fn new(files: Vec<SourceFile>) -> Self {
        ProgramConcrete { files }
    }

    fn get_diagnostics_helper(
        &self,
        get_diagnostics: fn(&ProgramConcrete, &SourceFile) -> Vec<Diagnostic>,
    ) -> Vec<Diagnostic> {
        self.get_source_files()
            .iter()
            .flat_map(|source_file| get_diagnostics(self, source_file))
            .collect()
    }

    fn get_semantic_diagnostics_for_file(&self, source_file: &SourceFile) -> Vec<Diagnostic> {
        vec![]
    }
}

impl Program for ProgramConcrete {
    fn get_semantic_diagnostics(&self) -> Vec<Diagnostic> {
        self.get_diagnostics_helper(ProgramConcrete::get_semantic_diagnostics_for_file)
    }

    fn get_source_files(&self) -> &[SourceFile] {
        &self.files
    }
}

struct CreateProgramHelperContext<'a> {
    processing_other_files: &'a mut Vec<SourceFile>,
    host: &'a dyn CompilerHost,
    current_directory: &'a str,
}

pub fn create_program(root_names_or_options: CreateProgramOptions) -> impl Program {
    let CreateProgramOptions { root_names } = root_names_or_options;

    let mut processing_other_files: Option<Vec<SourceFile>> = None;
    let mut files: Vec<SourceFile> = vec![];

    let host = create_compiler_host();

    let current_directory = host.get_current_directory();

    let structure_is_reused = StructureIsReused::Not;
    if !matches!(structure_is_reused, StructureIsReused::Completely) {
        processing_other_files = Some(vec![]);
        let mut processing_other_files_present = processing_other_files.unwrap();
        let mut helper_context = CreateProgramHelperContext {
            processing_other_files: &mut processing_other_files_present,
            host: &host,
            current_directory: &current_directory,
        };
        for_each(root_names, &mut |name, _index| {
            process_root_file(&mut helper_context, name);
            Option::<()>::None
        });

        files = processing_other_files_present;
        processing_other_files = None;
    }

    ProgramConcrete::new(files)
}

fn process_root_file(helper_context: &mut CreateProgramHelperContext, file_name: &str) {
    process_source_file(helper_context, &normalize_path(file_name));
}

fn get_source_file_from_reference_worker(
    file_name: &str,
    get_source_file: &mut dyn FnMut(&str) -> Option<SourceFile>,
) -> Option<SourceFile> {
    get_source_file(file_name)
}

fn process_source_file(helper_context: &mut CreateProgramHelperContext, file_name: &str) {
    get_source_file_from_reference_worker(file_name, &mut |file_name| {
        find_source_file(helper_context, file_name)
    });
}

fn find_source_file(
    helper_context: &mut CreateProgramHelperContext,
    file_name: &str,
) -> Option<SourceFile> {
    find_source_file_worker(helper_context, file_name)
}

fn find_source_file_worker(
    helper_context: &mut CreateProgramHelperContext,
    file_name: &str,
) -> Option<SourceFile> {
    let path = to_path(helper_context, file_name);

    let file = helper_context.host.get_source_file(file_name);

    file
}

fn to_path(helper_context: &mut CreateProgramHelperContext, file_name: &str) -> Path {
    to_path_helper(
        file_name,
        Some(helper_context.current_directory),
        &mut |file_name| get_canonical_file_name(helper_context, file_name),
    )
}

fn get_canonical_file_name(
    helper_context: &mut CreateProgramHelperContext,
    file_name: &str,
) -> String {
    helper_context.host.get_canonical_file_name(file_name)
}
