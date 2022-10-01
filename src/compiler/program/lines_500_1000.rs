use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::io;
use std::rc::Rc;

use super::{
    create_compiler_host, get_module_name_string_literal_at,
    parse_config_host_from_compiler_host_like,
    update_host_for_use_source_of_project_reference_redirect, CompilerHostLikeRcDynCompilerHost,
    HostForUseSourceOfProjectReferenceRedirect,
    UpdateHostForUseSourceOfProjectReferenceRedirectReturn,
};
use crate::{
    clone, combine_paths, create_diagnostic_collection, create_module_resolution_cache,
    create_multi_map, create_type_reference_directive_resolution_cache, extension_from_path,
    file_extension_is_one_of, for_each, get_automatic_type_directive_names, get_directory_path,
    get_emit_module_resolution_kind, get_package_scope_for_path, get_supported_extensions,
    get_supported_extensions_with_json_if_resolve_json_module, get_sys, is_import_call,
    is_import_equals_declaration, map_defined, maybe_for_each, options_have_changes,
    resolve_module_name, resolve_type_reference_directive, source_file_affecting_compiler_options,
    stable_sort, walk_up_parenthesized_expressions, AutomaticTypeDirectiveFile, CompilerHost,
    CompilerOptions, CreateProgramOptions, Diagnostic, DiagnosticCollection, Extension,
    FileIncludeKind, FileIncludeReason, FilePreprocessingDiagnostics, LibFile, ModuleKind,
    ModuleResolutionCache, ModuleResolutionHost, ModuleResolutionHostOverrider,
    ModuleResolutionKind, ModuleSpecifierResolutionHost, MultiMap, Node, NodeInterface, PackageId,
    PackageJsonInfoCache, ParsedCommandLine, Path, Program, ProjectReference, RedirectTargetsMap,
    ReferencedFile, ResolvedModuleFull, ResolvedProjectReference, ResolvedTypeReferenceDirective,
    RootFile, ScriptReferenceHost, SourceFile, SourceOfProjectReferenceRedirect, StructureIsReused,
    SymlinkCache, TypeCheckerHost, TypeCheckerHostDebuggable,
    TypeReferenceDirectiveResolutionCache,
};

pub trait LoadWithLocalCacheLoader<TValue> {
    fn call(
        &self,
        name: &str,
        containing_file: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> TValue;
}

pub struct LoadWithLocalCacheLoaderResolveTypeReferenceDirective {
    options: Rc<CompilerOptions>,
    host: Rc<dyn CompilerHost>,
    type_reference_directive_resolution_cache: Option<Rc<TypeReferenceDirectiveResolutionCache>>,
}

impl LoadWithLocalCacheLoaderResolveTypeReferenceDirective {
    pub fn new(
        options: Rc<CompilerOptions>,
        host: Rc<dyn CompilerHost>,
        type_reference_directive_resolution_cache: Option<
            Rc<TypeReferenceDirectiveResolutionCache>,
        >,
    ) -> Self {
        Self {
            options,
            host,
            type_reference_directive_resolution_cache,
        }
    }
}

impl LoadWithLocalCacheLoader<Rc<ResolvedTypeReferenceDirective>>
    for LoadWithLocalCacheLoaderResolveTypeReferenceDirective
{
    fn call(
        &self,
        types_ref: &str,
        containing_file: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Rc<ResolvedTypeReferenceDirective> {
        resolve_type_reference_directive(
            types_ref,
            Some(containing_file),
            self.options.clone(),
            self.host.as_dyn_module_resolution_host(),
            redirected_reference,
            self.type_reference_directive_resolution_cache.clone(),
        )
        .resolved_type_reference_directive
        .clone()
        .unwrap()
    }
}

pub(crate) fn load_with_local_cache<TValue: Clone>(
    names: &[String],
    containing_file: &str,
    redirected_reference: Option<Rc<ResolvedProjectReference>>,
    loader: &dyn LoadWithLocalCacheLoader<TValue>,
) -> Vec<TValue> {
    if names.is_empty() {
        return vec![];
    }
    let mut resolutions: Vec<TValue> = vec![];
    let mut cache: HashMap<String, TValue> = HashMap::new();
    for name in names {
        let result: TValue;
        if cache.contains_key(name) {
            result = cache.get(name).cloned().unwrap();
        } else {
            result = loader.call(name, containing_file, redirected_reference.clone());
            cache.insert(name.clone(), result.clone());
        }
        resolutions.push(result);
    }
    resolutions
}

pub(crate) trait SourceFileImportsList {
    fn maybe_implied_node_format(&self) -> Option<ModuleKind>;
}

impl SourceFileImportsList for SourceFile {
    fn maybe_implied_node_format(&self) -> Option<ModuleKind> {
        self.maybe_implied_node_format()
    }
}

pub(crate) fn get_mode_for_resolution_at_index<TFile: SourceFileImportsList>(
    file: &TFile,
    index: usize,
) -> Option<ModuleKind> {
    if file.maybe_implied_node_format().is_none() {
        return None;
    }
    get_mode_for_usage_location(
        file.maybe_implied_node_format(),
        &get_module_name_string_literal_at(file, index),
    )
}

pub(crate) fn get_mode_for_usage_location(
    implied_node_format: Option<ModuleKind>,
    usage: &Node, /*StringLiteralLike*/
) -> Option<ModuleKind> {
    let implied_node_format = implied_node_format?;
    if implied_node_format != ModuleKind::ESNext {
        return Some(
            if is_import_call(&walk_up_parenthesized_expressions(&usage.parent()).unwrap()) {
                ModuleKind::ESNext
            } else {
                ModuleKind::CommonJS
            },
        );
    }
    let expr_parent_parent =
        walk_up_parenthesized_expressions(&usage.parent()).and_then(|node| node.maybe_parent());
    Some(
        if matches!(
            expr_parent_parent.as_ref(),
            Some(expr_parent_parent) if is_import_equals_declaration(expr_parent_parent)
        ) {
            ModuleKind::CommonJS
        } else {
            ModuleKind::ESNext
        },
    )
}

pub trait LoadWithModeAwareCacheLoader<TValue> {
    fn call(
        &self,
        name: &str,
        resolver_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
        containing_file_name: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> TValue;
}

pub struct LoadWithModeAwareCacheLoaderResolveModuleName {
    options: Rc<CompilerOptions>,
    host: Rc<dyn CompilerHost>,
    module_resolution_cache: Option<Rc<ModuleResolutionCache>>,
}

impl LoadWithModeAwareCacheLoaderResolveModuleName {
    pub fn new(
        options: Rc<CompilerOptions>,
        host: Rc<dyn CompilerHost>,
        module_resolution_cache: Option<Rc<ModuleResolutionCache>>,
    ) -> Self {
        Self {
            options,
            host,
            module_resolution_cache,
        }
    }
}

impl LoadWithModeAwareCacheLoader<Option<Rc<ResolvedModuleFull>>>
    for LoadWithModeAwareCacheLoaderResolveModuleName
{
    fn call(
        &self,
        module_name: &str,
        resolver_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
        containing_file_name: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Option<Rc<ResolvedModuleFull>> {
        resolve_module_name(
            module_name,
            containing_file_name,
            self.options.clone(),
            self.host.as_dyn_module_resolution_host(),
            self.module_resolution_cache.clone(),
            redirected_reference,
            resolver_mode,
        )
        .resolved_module
        .clone()
    }
}

pub(crate) fn load_with_mode_aware_cache<TValue: Clone>(
    names: &[String],
    containing_file: &Node, /*SourceFile*/
    containing_file_name: &str,
    redirected_reference: Option<Rc<ResolvedProjectReference>>,
    loader: &dyn LoadWithModeAwareCacheLoader<TValue>,
) -> Vec<TValue> {
    if names.is_empty() {
        return vec![];
    }
    let mut resolutions: Vec<TValue> = vec![];
    let mut cache: HashMap<String, TValue> = HashMap::new();
    let mut i = 0;
    let containing_file_as_source_file = containing_file.as_source_file();
    for name in names {
        let result: TValue;
        let mode = get_mode_for_resolution_at_index(containing_file_as_source_file, i);
        i += 1;
        let cache_key = if let Some(mode) = mode {
            format!("{:?}|{}", mode, name)
        } else {
            name.clone()
        };
        if cache.contains_key(&cache_key) {
            result = cache.get(&cache_key).unwrap().clone();
        } else {
            result = loader.call(
                name,
                mode,
                containing_file_name,
                redirected_reference.clone(),
            );
            cache.insert(cache_key, result.clone());
        }
        resolutions.push(result);
    }
    resolutions
}

pub fn for_each_resolved_project_reference<
    TReturn,
    TCallback: FnMut(Rc<ResolvedProjectReference>, Option<&ResolvedProjectReference>) -> Option<TReturn>,
>(
    resolved_project_references: Option<&[Option<Rc<ResolvedProjectReference>>]>,
    mut cb: TCallback,
) -> Option<TReturn> {
    for_each_project_reference(
        None,
        resolved_project_references,
        |resolved_ref: Option<Rc<ResolvedProjectReference>>,
         parent: Option<&ResolvedProjectReference>,
         _| { resolved_ref.and_then(|resolved_ref| cb(resolved_ref, parent)) },
        Option::<
            fn(
                Option<&[Rc<ProjectReference>]>,
                Option<&ResolvedProjectReference>,
            ) -> Option<TReturn>,
        >::None,
    )
}

pub fn for_each_project_reference<
    TReturn,
    TCallbackResolvedRef: FnMut(
        Option<Rc<ResolvedProjectReference>>,
        Option<&ResolvedProjectReference>,
        usize,
    ) -> Option<TReturn>,
    TCallbackRef: Fn(Option<&[Rc<ProjectReference>]>, Option<&ResolvedProjectReference>) -> Option<TReturn>,
>(
    project_references: Option<&[Rc<ProjectReference>]>,
    resolved_project_references: Option<&[Option<Rc<ResolvedProjectReference>>]>,
    mut cb_resolved_ref: TCallbackResolvedRef,
    cb_ref: Option<TCallbackRef>,
) -> Option<TReturn> {
    let mut seen_resolved_refs: Option<HashSet<Path>> = None;

    for_each_project_reference_worker(
        cb_ref.as_ref(),
        &mut cb_resolved_ref,
        &mut seen_resolved_refs,
        project_references,
        resolved_project_references,
        None,
    )
}

fn for_each_project_reference_worker<
    TReturn,
    TCallbackResolvedRef: FnMut(
        Option<Rc<ResolvedProjectReference>>,
        Option<&ResolvedProjectReference>,
        usize,
    ) -> Option<TReturn>,
    TCallbackRef: Fn(Option<&[Rc<ProjectReference>]>, Option<&ResolvedProjectReference>) -> Option<TReturn>,
>(
    cb_ref: Option<&TCallbackRef>,
    cb_resolved_ref: &mut TCallbackResolvedRef,
    seen_resolved_refs: &mut Option<HashSet<Path>>,
    project_references: Option<&[Rc<ProjectReference>]>,
    resolved_project_references: Option<&[Option<Rc<ResolvedProjectReference>>]>,
    parent: Option<&ResolvedProjectReference>,
) -> Option<TReturn> {
    if let Some(cb_ref) = cb_ref {
        let result = cb_ref(project_references, parent);
        if result.is_some() {
            return result;
        }
    }

    maybe_for_each(
        resolved_project_references,
        |resolved_ref: &Option<Rc<ResolvedProjectReference>>, index| {
            if matches!(
                resolved_ref.as_ref(),
                Some(resolved_ref) if matches!(
                    seen_resolved_refs,
                    Some(seen_resolved_refs) if seen_resolved_refs.contains(
                        &*resolved_ref.source_file.as_source_file().path()
                    )
                )
            ) {
                return None;
            }

            let result = cb_resolved_ref(resolved_ref.clone(), parent, index);
            if result.is_some() || resolved_ref.is_none() {
                return result;
            }
            let resolved_ref = resolved_ref.as_ref().unwrap();

            seen_resolved_refs
                .get_or_insert_with(|| HashSet::new())
                .insert(resolved_ref.source_file.as_source_file().path().clone());
            for_each_project_reference_worker(
                cb_ref,
                cb_resolved_ref,
                seen_resolved_refs,
                resolved_ref.command_line.project_references.as_deref(),
                resolved_ref.references.as_deref(),
                Some(&**resolved_ref),
            )
        },
    )
}

pub(crate) const inferred_types_containing_file: &str = "__inferred type names__.ts";

pub(crate) struct DiagnosticCache {
    pub per_file: Option<HashMap<Path, Vec<Rc<Diagnostic>>>>,
    pub all_diagnostics: Option<Vec<Rc<Diagnostic>>>,
}

impl Default for DiagnosticCache {
    fn default() -> Self {
        Self {
            per_file: None,
            all_diagnostics: None,
        }
    }
}

pub(crate) fn is_referenced_file(reason: Option<&FileIncludeReason>) -> bool {
    matches!(reason, Some(FileIncludeReason::ReferencedFile(_)))
}

#[derive(Debug)]
pub(crate) struct ReferenceFileLocation {
    pub file: Rc<Node /*SourceFile*/>,
    pub pos: usize,
    pub end: usize,
    pub package_id: Option<PackageId>,
}

#[derive(Debug)]
pub(crate) struct SyntheticReferenceFileLocation {
    pub file: Rc<Node /*SourceFile*/>,
    pub package_id: Option<PackageId>,
    pub text: String,
}

pub(crate) fn is_reference_file_location(
    location: &ReferenceFileLocationOrSyntheticReferenceFileLocation,
) -> bool {
    matches!(
        location,
        ReferenceFileLocationOrSyntheticReferenceFileLocation::ReferenceFileLocation(_)
    )
}

#[derive(Debug)]
pub(crate) enum ReferenceFileLocationOrSyntheticReferenceFileLocation {
    ReferenceFileLocation(ReferenceFileLocation),
    SyntheticReferenceFileLocation(SyntheticReferenceFileLocation),
}

impl ReferenceFileLocationOrSyntheticReferenceFileLocation {
    pub fn maybe_package_id(&self) -> Option<&PackageId> {
        match self {
            Self::ReferenceFileLocation(location) => location.package_id.as_ref(),
            Self::SyntheticReferenceFileLocation(location) => location.package_id.as_ref(),
        }
    }

    pub fn file(&self) -> Rc<Node> {
        match self {
            Self::ReferenceFileLocation(location) => location.file.clone(),
            Self::SyntheticReferenceFileLocation(location) => location.file.clone(),
        }
    }
}

pub(crate) fn get_referenced_file_location<
    TGetSourceFileByPath: FnMut(&Path) -> Option<Rc<Node /*SourceFile*/>>,
>(
    get_source_file_by_path: TGetSourceFileByPath,
    ref_: &ReferencedFile,
) -> ReferenceFileLocationOrSyntheticReferenceFileLocation {
    unimplemented!()
}

pub fn get_config_file_parsing_diagnostics(
    config_file_parse_result: &ParsedCommandLine,
) -> Vec<Rc<Diagnostic>> {
    // unimplemented!()
    vec![]
}

pub fn get_implied_node_format_for_file(
    file_name: &Path,
    package_json_info_cache: Option<&dyn PackageJsonInfoCache>,
    host: &dyn ModuleResolutionHost,
    options: &CompilerOptions,
) -> Option<ModuleKind /*ModuleKind.ESNext | ModuleKind.CommonJS*/> {
    match get_emit_module_resolution_kind(options) {
        ModuleResolutionKind::Node12 | ModuleResolutionKind::NodeNext => {
            if file_extension_is_one_of(
                file_name,
                &[Extension::Dmts, Extension::Mts, Extension::Mjs],
            ) {
                Some(ModuleKind::ESNext)
            } else if file_extension_is_one_of(
                file_name,
                &[Extension::Dcts, Extension::Cts, Extension::Cjs],
            ) {
                Some(ModuleKind::CommonJS)
            } else if file_extension_is_one_of(
                file_name,
                &[
                    Extension::Dts,
                    Extension::Ts,
                    Extension::Tsx,
                    Extension::Js,
                    Extension::Jsx,
                ],
            ) {
                Some(lookup_from_package_json(
                    file_name,
                    package_json_info_cache,
                    host,
                    options,
                ))
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn lookup_from_package_json(
    file_name: &Path,
    package_json_info_cache: Option<&dyn PackageJsonInfoCache>,
    host: &dyn ModuleResolutionHost,
    options: &CompilerOptions,
) -> ModuleKind /*ModuleKind.ESNext | ModuleKind.CommonJS*/ {
    let scope = get_package_scope_for_path(file_name, package_json_info_cache, host, options);
    if matches!(
        scope.as_ref(),
        Some(scope) if matches!(
            scope.package_json_content.as_object().and_then(|scope_package_json_content| {
                scope_package_json_content.get("type_")
            }),
            Some(serde_json::Value::String(value)) if &**value == "module"
        )
    ) {
        ModuleKind::ESNext
    } else {
        ModuleKind::CommonJS
    }
}

fn should_program_create_new_source_files(
    program: Option<&Program>,
    new_options: &CompilerOptions,
) -> bool {
    if program.is_none() {
        return false;
    }
    let program = program.unwrap();
    source_file_affecting_compiler_options.with(|source_file_affecting_compiler_options_| {
        options_have_changes(
            &program.get_compiler_options(),
            new_options,
            source_file_affecting_compiler_options_,
        )
    })
}

pub fn create_program(root_names_or_options: CreateProgramOptions) -> Rc<Program> {
    let create_program_options = root_names_or_options;
    let program = Program::new(create_program_options);
    program.create();
    program
}

impl Program {
    pub fn new(
        create_program_options: CreateProgramOptions,
        // options: Rc<CompilerOptions>,
        // files: Vec<Rc<Node>>,
        // current_directory: String,
        // host: Rc<dyn CompilerHost>,
    ) -> Rc<Self> {
        let options = create_program_options.options.clone();
        let max_node_module_js_depth = options.max_node_module_js_depth.unwrap_or(0);
        let rc = Rc::new(Program {
            _rc_wrapper: RefCell::new(None),
            create_program_options: RefCell::new(Some(create_program_options)),
            options,
            processing_default_lib_files: RefCell::new(None),
            processing_other_files: RefCell::new(None),
            files: RefCell::new(None),
            symlinks: RefCell::new(None),
            common_source_directory: RefCell::new(None),
            diagnostics_producing_type_checker: RefCell::new(None),
            no_diagnostics_type_checker: RefCell::new(None),
            classifiable_names: RefCell::new(None),
            ambient_module_name_to_unmodified_file_name: RefCell::new(HashMap::new()),
            file_reasons: RefCell::new(create_multi_map()),
            cached_bind_and_check_diagnostics_for_file: RefCell::new(Default::default()),
            cached_declaration_diagnostics_for_file: RefCell::new(Default::default()),

            resolved_type_reference_directives: RefCell::new(HashMap::new()),
            file_processing_diagnostics: RefCell::new(None),

            max_node_module_js_depth,
            current_node_modules_depth: Cell::new(0),

            modules_with_elided_imports: RefCell::new(HashMap::new()),

            source_files_found_searching_node_modules: RefCell::new(HashMap::new()),

            old_program: RefCell::new(None),
            host: RefCell::new(None),
            config_parsing_host: RefCell::new(None),

            skip_default_lib: Cell::new(None),
            get_default_library_file_name_memoized: RefCell::new(None),
            default_library_path: RefCell::new(None),
            program_diagnostics: RefCell::new(None),
            current_directory: RefCell::new(None),
            supported_extensions: RefCell::new(None),
            supported_extensions_with_json_if_resolve_json_module: RefCell::new(None),
            has_emit_blocking_diagnostics: RefCell::new(None),
            _compiler_options_object_literal_syntax: RefCell::new(None),
            module_resolution_cache: RefCell::new(None),
            type_reference_directive_resolution_cache: RefCell::new(None),
            actual_resolve_module_names_worker: RefCell::new(None),
            actual_resolve_type_reference_directive_names_worker: RefCell::new(None),
            package_id_to_source_file: RefCell::new(None),
            source_file_to_package_name: RefCell::new(None),
            redirect_targets_map: RefCell::new(None),
            uses_uri_style_node_core_modules: Cell::new(None),
            files_by_name: RefCell::new(None),
            missing_file_paths: RefCell::new(None),
            files_by_name_ignore_case: RefCell::new(None),
            resolved_project_references: RefCell::new(None),
            project_reference_redirects: RefCell::new(None),
            map_from_file_to_project_reference_redirects: RefCell::new(None),
            map_from_to_project_reference_redirect_source: RefCell::new(None),
            use_source_of_project_reference_redirect: Cell::new(None),
            file_exists_rc: RefCell::new(None),
            directory_exists_rc: RefCell::new(None),
            should_create_new_source_file: Cell::new(None),
            structure_is_reused: Cell::new(None),
        });
        rc.set_rc_wrapper(Some(rc.clone()));
        rc
    }

    pub fn create(&self) {
        let CreateProgramOptions {
            root_names,
            config_file_parsing_diagnostics,
            project_references,
            old_program,
            host,
            ..
        } = self.create_program_options.borrow_mut().take().unwrap();
        *self.old_program.borrow_mut() = old_program;

        // tracing?.push(tracing.Phase.Program, "createProgram", { configFilePath: options.configFilePath, rootDir: options.rootDir }, /*separateBeginAndEnd*/ true);
        // performance.mark("beforeProgram");

        *self.host.borrow_mut() =
            Some(host.unwrap_or_else(|| Rc::new(create_compiler_host(self.options.clone(), None))));
        *self.config_parsing_host.borrow_mut() =
            Some(Rc::new(parse_config_host_from_compiler_host_like(
                Rc::new(CompilerHostLikeRcDynCompilerHost::new(self.host())),
                None,
            )));

        self.skip_default_lib.set(self.options.no_lib);
        *self.default_library_path.borrow_mut() = Some(
            self.host()
                .get_default_lib_location()
                .unwrap_or_else(|| get_directory_path(&self.get_default_library_file_name())),
        );
        *self.program_diagnostics.borrow_mut() = Some(create_diagnostic_collection());
        *self.current_directory.borrow_mut() =
            Some(CompilerHost::get_current_directory(&*self.host()));
        *self.supported_extensions.borrow_mut() =
            Some(get_supported_extensions(Some(&self.options), None));
        *self
            .supported_extensions_with_json_if_resolve_json_module
            .borrow_mut() = Some(get_supported_extensions_with_json_if_resolve_json_module(
            Some(&self.options),
            &self.supported_extensions(),
        ));

        *self.has_emit_blocking_diagnostics.borrow_mut() = Some(HashMap::new());

        if self.host().is_resolve_module_names_supported() {
            *self.actual_resolve_module_names_worker.borrow_mut() = Some(Rc::new(
                ActualResolveModuleNamesWorkerHost::new(self.host(), self.options.clone()),
            ));
            *self.module_resolution_cache.borrow_mut() = self.host().get_module_resolution_cache();
        } else {
            *self.module_resolution_cache.borrow_mut() =
                Some(Rc::new(create_module_resolution_cache(
                    &self.current_directory(),
                    self.get_canonical_file_name_rc(),
                    Some(self.options.clone()),
                    None,
                    None,
                )));
            let loader = LoadWithModeAwareCacheLoaderResolveModuleName::new(
                self.options.clone(),
                self.host(),
                self.maybe_module_resolution_cache().clone(),
            );
            *self.actual_resolve_module_names_worker.borrow_mut() = Some(Rc::new(
                ActualResolveModuleNamesWorkerLoadWithModeAwareCache::new(Rc::new(loader)),
            ));
        }

        if self.host().is_resolve_type_reference_directives_supported() {
            *self
                .actual_resolve_type_reference_directive_names_worker
                .borrow_mut() = Some(Rc::new(
                ActualResolveTypeReferenceDirectiveNamesWorkerHost::new(
                    self.host(),
                    self.options.clone(),
                ),
            ));
        } else {
            *self.type_reference_directive_resolution_cache.borrow_mut() =
                Some(Rc::new(create_type_reference_directive_resolution_cache(
                    &self.current_directory(),
                    self.get_canonical_file_name_rc(),
                    None,
                    self.maybe_module_resolution_cache()
                        .as_ref()
                        .map(|module_resolution_cache| {
                            module_resolution_cache.get_package_json_info_cache()
                        }),
                    None,
                )));
            let loader = LoadWithLocalCacheLoaderResolveTypeReferenceDirective::new(
                self.options.clone(),
                self.host(),
                self.maybe_type_reference_directive_resolution_cache()
                    .clone(),
            );
            *self
                .actual_resolve_type_reference_directive_names_worker
                .borrow_mut() = Some(Rc::new(
                ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache::new(Rc::new(
                    loader,
                )),
            ));
        }

        *self.package_id_to_source_file.borrow_mut() = Some(HashMap::new());
        *self.source_file_to_package_name.borrow_mut() = Some(HashMap::new());
        *self.redirect_targets_map.borrow_mut() = Some(create_multi_map());
        self.uses_uri_style_node_core_modules.set(Some(false));

        *self.files_by_name.borrow_mut() = Some(HashMap::new());
        *self.files_by_name_ignore_case.borrow_mut() =
            if CompilerHost::use_case_sensitive_file_names(&*self.host()) {
                Some(HashMap::new())
            } else {
                None
            };

        self.use_source_of_project_reference_redirect.set(Some(
            self.host().use_source_of_project_reference_redirect() == Some(true)
                && self.options.disable_source_of_project_reference_redirect != Some(true),
        ));
        let UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
            on_program_create_complete,
            file_exists,
            directory_exists,
        } = update_host_for_use_source_of_project_reference_redirect(
            HostForUseSourceOfProjectReferenceRedirect {
                compiler_host: self.host(),
                get_symlink_cache: self.get_symlink_cache_rc(),
                use_source_of_project_reference_redirect: self
                    .use_source_of_project_reference_redirect(),
                to_path: self.to_path_rc(),
                get_resolved_project_references: {
                    let self_clone = self.rc_wrapper();
                    Rc::new(move || self_clone.get_resolved_project_references().clone())
                },
                for_each_resolved_project_reference: self.for_each_resolved_project_reference_rc(),
            },
        );
        *self.file_exists_rc.borrow_mut() = Some(file_exists);
        *self.directory_exists_rc.borrow_mut() = directory_exists;

        // tracing?.push(tracing.Phase.Program, "shouldProgramCreateNewSourceFiles", { hasOldProgram: !!oldProgram });
        self.should_create_new_source_file
            .set(Some(should_program_create_new_source_files(
                self.maybe_old_program().as_deref(),
                &self.options,
            )));
        // tracing?.pop();
        // tracing?.push(tracing.Phase.Program, "tryReuseStructureFromOldProgram", {});
        self.set_structure_is_reused(self.try_reuse_structure_from_old_program());
        // tracing?.pop();
        if self.structure_is_reused() != StructureIsReused::Completely {
            *self.processing_default_lib_files.borrow_mut() = Some(vec![]);
            *self.processing_other_files.borrow_mut() = Some(vec![]);

            if let Some(project_references) = project_references.as_ref() {
                if self.resolved_project_references.borrow().is_none() {
                    *self.resolved_project_references.borrow_mut() = Some(
                        project_references
                            .into_iter()
                            .map(|project_reference| {
                                self.parse_project_reference_config_file(project_reference)
                            })
                            .collect(),
                    );
                }
                unimplemented!()
            }

            // tracing?.push(tracing.Phase.Program, "processRootFiles", { count: rootNames.length });
            for_each(&root_names, |name, index| {
                self.process_root_file(
                    name,
                    false,
                    false,
                    &FileIncludeReason::RootFile(RootFile {
                        kind: FileIncludeKind::RootFile,
                        index,
                    }),
                );
                Option::<()>::None
            });
            // tracing?.pop();

            let type_references = if !root_names.is_empty() {
                get_automatic_type_directive_names(
                    &self.options,
                    self.host().as_dyn_module_resolution_host(),
                )
            } else {
                vec![]
            };

            if !type_references.is_empty() {
                // tracing?.push(tracing.Phase.Program, "processTypeReferences", { count: typeReferences.length });
                let containing_directory = if let Some(options_config_file_path) =
                    self.options.config_file_path.as_ref()
                {
                    get_directory_path(options_config_file_path)
                } else {
                    CompilerHost::get_current_directory(&*self.host())
                };
                let containing_filename = combine_paths(
                    &containing_directory,
                    &[Some(inferred_types_containing_file)],
                );
                let resolutions = self.resolve_type_reference_directive_names_worker(
                    &type_references,
                    containing_filename,
                );
                for i in 0..type_references.len() {
                    self.process_type_reference_directive(
                        &type_references[i],
                        resolutions.get(i).and_then(|resolution| resolution.clone()),
                        &FileIncludeReason::AutomaticTypeDirectiveFile(
                            AutomaticTypeDirectiveFile {
                                kind: FileIncludeKind::AutomaticTypeDirectiveFile,
                                type_reference: type_references[i].clone(),
                                package_id: resolutions
                                    .get(i)
                                    .cloned()
                                    .flatten()
                                    .and_then(|resolution| resolution.package_id.clone()),
                            },
                        ),
                    );
                }
                // tracing?.pop();
            }

            if !root_names.is_empty() && self.maybe_skip_default_lib() != Some(true) {
                let default_library_file_name = self.get_default_library_file_name();
                if self.options.lib.is_none() && *default_library_file_name != "" {
                    self.process_root_file(
                        &default_library_file_name,
                        true,
                        false,
                        &FileIncludeReason::LibFile(LibFile {
                            kind: FileIncludeKind::LibFile,
                            index: None,
                        }),
                    );
                } else {
                    maybe_for_each(
                        self.options.lib.as_ref(),
                        |lib_file_name: &String, index| -> Option<()> {
                            self.process_root_file(
                                &self.path_for_lib_file(lib_file_name),
                                true,
                                false,
                                &FileIncludeReason::LibFile(LibFile {
                                    kind: FileIncludeKind::LibFile,
                                    index: Some(index),
                                }),
                            );
                            None
                        },
                    );
                }
            }

            *self.missing_file_paths.borrow_mut() = Some(map_defined(
                Some(&*self.files_by_name()),
                |(path, file), _| {
                    if matches!(file, FilesByNameValue::Undefined,) {
                        Some(path.clone().into())
                    } else {
                        None
                    }
                },
            ));

            println!(
                "processing_default_lib_files: {:?}",
                self.processing_default_lib_files
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .into_iter()
                    .map(|file| file.as_source_file().file_name().clone())
                    .collect::<Vec<_>>()
            );
            println!(
                "processing_other_files: {:?}",
                self.processing_other_files
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .into_iter()
                    .map(|file| file.as_source_file().file_name().clone())
                    .collect::<Vec<_>>()
            );
            *self.files.borrow_mut() = Some({
                let mut files: Vec<Rc<Node>> = stable_sort(
                    self.processing_default_lib_files.borrow().as_ref().unwrap(),
                    |a, b| self.compare_default_lib_files(a, b),
                )
                .into();
                let mut processing_other_files =
                    self.processing_other_files.borrow().clone().unwrap();
                files.append(&mut processing_other_files);
                files
            });
            // println!("files: {:#?}", &*self.files());
            *self.processing_default_lib_files.borrow_mut() = None;
            *self.processing_other_files.borrow_mut() = None;
        }

        // performance.mark("afterProgram");
        // performance.measure("Program", "beforeProgram", "afterProgram");
        // tracing?.pop();
    }

    pub fn set_rc_wrapper(&self, rc_wrapper: Option<Rc<Program>>) {
        *self._rc_wrapper.borrow_mut() = rc_wrapper;
    }

    pub fn rc_wrapper(&self) -> Rc<Program> {
        self._rc_wrapper.borrow().clone().unwrap()
    }

    pub(super) fn files(&self) -> Ref<Vec<Rc<Node>>> {
        Ref::map(self.files.borrow(), |files| files.as_ref().unwrap())
    }

    pub(super) fn get_default_library_file_name(&self) -> Ref<String> {
        if self
            .get_default_library_file_name_memoized
            .borrow()
            .is_none()
        {
            *self.get_default_library_file_name_memoized.borrow_mut() =
                Some(self.host().get_default_lib_file_name(&self.options));
        }
        Ref::map(
            self.get_default_library_file_name_memoized.borrow(),
            |default_library_file_name_memoized| {
                default_library_file_name_memoized.as_ref().unwrap()
            },
        )
    }

    pub(super) fn maybe_skip_default_lib(&self) -> Option<bool> {
        self.skip_default_lib.get()
    }

    pub(super) fn set_skip_default_lib(&self, skip_default_lib: Option<bool>) {
        self.skip_default_lib.set(skip_default_lib);
    }

    pub(super) fn default_library_path(&self) -> Ref<String> {
        Ref::map(self.default_library_path.borrow(), |default_library_path| {
            default_library_path.as_ref().unwrap()
        })
    }

    pub(super) fn current_node_modules_depth(&self) -> usize {
        self.current_node_modules_depth.get()
    }

    pub(super) fn set_current_node_modules_depth(&self, current_node_modules_depth: usize) {
        self.current_node_modules_depth
            .set(current_node_modules_depth);
    }

    pub(super) fn modules_with_elided_imports(&self) -> RefMut<HashMap<String, bool>> {
        self.modules_with_elided_imports.borrow_mut()
    }

    pub(super) fn source_files_found_searching_node_modules(
        &self,
    ) -> RefMut<HashMap<String, bool>> {
        self.source_files_found_searching_node_modules.borrow_mut()
    }

    pub(super) fn maybe_old_program(&self) -> Option<Rc<Program>> {
        self.old_program.borrow().clone()
    }

    pub(super) fn host(&self) -> Rc<dyn CompilerHost> {
        self.host.borrow().clone().unwrap()
    }

    pub(super) fn symlinks(&self) -> RefMut<Option<Rc<SymlinkCache>>> {
        self.symlinks.borrow_mut()
    }

    pub(super) fn ambient_module_name_to_unmodified_file_name(
        &self,
    ) -> RefMut<HashMap<String, String>> {
        self.ambient_module_name_to_unmodified_file_name
            .borrow_mut()
    }

    pub(super) fn file_reasons(&self) -> RefMut<MultiMap<Path, FileIncludeReason>> {
        self.file_reasons.borrow_mut()
    }

    pub(super) fn maybe_file_processing_diagnostics(
        &self,
    ) -> RefMut<Option<Vec<FilePreprocessingDiagnostics>>> {
        self.file_processing_diagnostics.borrow_mut()
    }

    pub(super) fn resolved_type_reference_directives(
        &self,
    ) -> RefMut<HashMap<String, Option<Rc<ResolvedTypeReferenceDirective>>>> {
        self.resolved_type_reference_directives.borrow_mut()
    }

    pub(super) fn program_diagnostics(&self) -> RefMut<DiagnosticCollection> {
        RefMut::map(
            self.program_diagnostics.borrow_mut(),
            |program_diagnostics| program_diagnostics.as_mut().unwrap(),
        )
    }

    pub(super) fn current_directory(&self) -> Ref<String> {
        Ref::map(self.current_directory.borrow(), |current_directory| {
            current_directory.as_ref().unwrap()
        })
    }

    pub(super) fn supported_extensions(&self) -> Ref<Vec<Vec<Extension>>> {
        Ref::map(self.supported_extensions.borrow(), |supported_extensions| {
            supported_extensions.as_ref().unwrap()
        })
    }

    pub(super) fn supported_extensions_with_json_if_resolve_json_module(
        &self,
    ) -> Ref<Vec<Vec<Extension>>> {
        Ref::map(
            self.supported_extensions_with_json_if_resolve_json_module
                .borrow(),
            |supported_extensions_with_json_if_resolve_json_module| {
                supported_extensions_with_json_if_resolve_json_module
                    .as_ref()
                    .unwrap()
            },
        )
    }

    pub(super) fn has_emit_blocking_diagnostics(&self) -> RefMut<HashMap<Path, bool>> {
        RefMut::map(
            self.has_emit_blocking_diagnostics.borrow_mut(),
            |has_emit_blocking_diagnostics| has_emit_blocking_diagnostics.as_mut().unwrap(),
        )
    }

    pub(super) fn maybe_compiler_options_object_literal_syntax(
        &self,
    ) -> RefMut<Option<Option<Rc<Node>>>> {
        self._compiler_options_object_literal_syntax.borrow_mut()
    }

    pub(super) fn maybe_module_resolution_cache(
        &self,
    ) -> RefMut<Option<Rc<ModuleResolutionCache>>> {
        self.module_resolution_cache.borrow_mut()
    }

    pub(super) fn maybe_type_reference_directive_resolution_cache(
        &self,
    ) -> RefMut<Option<Rc<TypeReferenceDirectiveResolutionCache>>> {
        self.type_reference_directive_resolution_cache.borrow_mut()
    }

    pub(super) fn actual_resolve_module_names_worker(
        &self,
    ) -> Rc<dyn ActualResolveModuleNamesWorker> {
        self.actual_resolve_module_names_worker
            .borrow_mut()
            .clone()
            .unwrap()
    }

    pub(super) fn actual_resolve_type_reference_directive_names_worker(
        &self,
    ) -> Rc<dyn ActualResolveTypeReferenceDirectiveNamesWorker> {
        self.actual_resolve_type_reference_directive_names_worker
            .borrow_mut()
            .clone()
            .unwrap()
    }

    pub(super) fn package_id_to_source_file(
        &self,
    ) -> RefMut<HashMap<String, Rc<Node /*SourceFile*/>>> {
        RefMut::map(
            self.package_id_to_source_file.borrow_mut(),
            |package_id_to_source_file| package_id_to_source_file.as_mut().unwrap(),
        )
    }

    pub(super) fn source_file_to_package_name(&self) -> RefMut<HashMap<Path, String>> {
        RefMut::map(
            self.source_file_to_package_name.borrow_mut(),
            |source_file_to_package_name| source_file_to_package_name.as_mut().unwrap(),
        )
    }

    pub(super) fn redirect_targets_map(&self) -> RefMut<MultiMap<Path, String>> {
        RefMut::map(
            self.redirect_targets_map.borrow_mut(),
            |redirect_targets_map| redirect_targets_map.as_mut().unwrap(),
        )
    }

    pub(super) fn uses_uri_style_node_core_modules(&self) -> bool {
        self.uses_uri_style_node_core_modules.get().unwrap()
    }

    pub(super) fn set_uses_uri_style_node_core_modules(
        &self,
        uses_uri_style_node_core_modules: bool,
    ) {
        self.uses_uri_style_node_core_modules
            .set(Some(uses_uri_style_node_core_modules));
    }

    pub(super) fn files_by_name(&self) -> RefMut<HashMap<String, FilesByNameValue>> {
        RefMut::map(self.files_by_name.borrow_mut(), |files_by_name| {
            files_by_name.as_mut().unwrap()
        })
    }

    pub(super) fn maybe_missing_file_paths(&self) -> RefMut<Option<Vec<Path>>> {
        self.missing_file_paths.borrow_mut()
    }

    pub(super) fn files_by_name_ignore_case(&self) -> RefMut<HashMap<String, Rc<Node>>> {
        RefMut::map(
            self.files_by_name_ignore_case.borrow_mut(),
            |files_by_name_ignore_case| files_by_name_ignore_case.as_mut().unwrap(),
        )
    }

    pub(super) fn maybe_resolved_project_references(
        &self,
    ) -> RefMut<Option<Vec<Option<Rc<ResolvedProjectReference>>>>> {
        self.resolved_project_references.borrow_mut()
    }

    pub(super) fn maybe_project_reference_redirects(
        &self,
    ) -> RefMut<Option<HashMap<Path, Option<Rc<ResolvedProjectReference>>>>> {
        self.project_reference_redirects.borrow_mut()
    }

    pub(super) fn maybe_map_from_file_to_project_reference_redirects(
        &self,
    ) -> RefMut<Option<HashMap<Path, Path>>> {
        self.map_from_file_to_project_reference_redirects
            .borrow_mut()
    }

    pub(super) fn maybe_map_from_to_project_reference_redirect_source(
        &self,
    ) -> RefMut<Option<HashMap<Path, SourceOfProjectReferenceRedirect>>> {
        self.map_from_to_project_reference_redirect_source
            .borrow_mut()
    }

    pub(super) fn use_source_of_project_reference_redirect(&self) -> bool {
        self.use_source_of_project_reference_redirect.get().unwrap()
    }

    pub(super) fn file_exists_rc(&self) -> Rc<dyn ModuleResolutionHostOverrider> {
        self.file_exists_rc.borrow().clone().unwrap()
    }

    pub(super) fn maybe_directory_exists_rc(
        &self,
    ) -> Option<Rc<dyn ModuleResolutionHostOverrider>> {
        self.directory_exists_rc.borrow().clone()
    }

    pub(super) fn should_create_new_source_file(&self) -> bool {
        self.should_create_new_source_file.get().unwrap()
    }

    pub(super) fn structure_is_reused(&self) -> StructureIsReused {
        self.structure_is_reused.get().unwrap()
    }

    pub(super) fn set_structure_is_reused(&self, structure_is_reused: StructureIsReused) {
        self.structure_is_reused.set(Some(structure_is_reused));
    }
}

#[derive(Clone)]
pub enum FilesByNameValue {
    SourceFile(Rc<Node /*SourceFile*/>),
    False,
    Undefined,
}

pub trait ActualResolveModuleNamesWorker {
    fn call(
        &self,
        module_names: &[String],
        containing_file: &Node, /*SourceFile*/
        containing_file_name: &str,
        reused_names: Option<&[String]>,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Vec<Option<Rc<ResolvedModuleFull>>>;
}

struct ActualResolveModuleNamesWorkerHost {
    host: Rc<dyn CompilerHost>,
    options: Rc<CompilerOptions>,
}

impl ActualResolveModuleNamesWorkerHost {
    pub fn new(host: Rc<dyn CompilerHost>, options: Rc<CompilerOptions>) -> Self {
        Self { host, options }
    }
}

impl ActualResolveModuleNamesWorker for ActualResolveModuleNamesWorkerHost {
    fn call(
        &self,
        module_names: &[String],
        containing_file: &Node, /*SourceFile*/
        containing_file_name: &str,
        reused_names: Option<&[String]>,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Vec<Option<Rc<ResolvedModuleFull>>> {
        self.host
            .resolve_module_names(
                /*Debug.checkEachDefined(*/ module_names, /*)*/
                containing_file_name,
                reused_names,
                redirected_reference.as_deref(),
                &self.options,
                Some(containing_file),
            )
            .unwrap()
            .into_iter()
            .map(|resolved| {
                if match resolved.as_ref() {
                    None => true,
                    Some(resolved) => resolved.extension.is_some(),
                } {
                    return resolved.map(Rc::new);
                }
                let resolved = resolved.unwrap();
                let mut with_extension = clone(&resolved);
                with_extension.extension = Some(extension_from_path(&resolved.resolved_file_name));
                Some(Rc::new(with_extension))
            })
            .collect()
    }
}

struct ActualResolveModuleNamesWorkerLoadWithModeAwareCache {
    loader: Rc<dyn LoadWithModeAwareCacheLoader<Option<Rc<ResolvedModuleFull>>>>,
}

impl ActualResolveModuleNamesWorkerLoadWithModeAwareCache {
    pub fn new(
        loader: Rc<dyn LoadWithModeAwareCacheLoader<Option<Rc<ResolvedModuleFull>>>>,
    ) -> Self {
        Self { loader }
    }
}

impl ActualResolveModuleNamesWorker for ActualResolveModuleNamesWorkerLoadWithModeAwareCache {
    fn call(
        &self,
        module_names: &[String],
        containing_file: &Node, /*SourceFile*/
        containing_file_name: &str,
        reused_names: Option<&[String]>,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Vec<Option<Rc<ResolvedModuleFull>>> {
        load_with_mode_aware_cache(
            /*Debug.checkEachDefined(*/ module_names, /*)*/
            containing_file,
            containing_file_name,
            redirected_reference,
            &*self.loader,
        )
    }
}

pub trait ActualResolveTypeReferenceDirectiveNamesWorker {
    fn call(
        &self,
        type_directive_names: &[String],
        containing_file: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Vec<Option<Rc<ResolvedTypeReferenceDirective>>>;
}

struct ActualResolveTypeReferenceDirectiveNamesWorkerHost {
    host: Rc<dyn CompilerHost>,
    options: Rc<CompilerOptions>,
}

impl ActualResolveTypeReferenceDirectiveNamesWorkerHost {
    pub fn new(host: Rc<dyn CompilerHost>, options: Rc<CompilerOptions>) -> Self {
        Self { host, options }
    }
}

impl ActualResolveTypeReferenceDirectiveNamesWorker
    for ActualResolveTypeReferenceDirectiveNamesWorkerHost
{
    fn call(
        &self,
        type_directive_names: &[String],
        containing_file: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Vec<Option<Rc<ResolvedTypeReferenceDirective>>> {
        self.host
            .resolve_type_reference_directives(
                /*Debug.checkEachDefined(*/ type_directive_names, /*)*/
                containing_file,
                redirected_reference.as_deref(),
                &self.options,
            )
            .unwrap()
    }
}

struct ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache {
    loader: Rc<dyn LoadWithLocalCacheLoader<Rc<ResolvedTypeReferenceDirective>>>,
}

impl ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache {
    pub fn new(
        loader: Rc<dyn LoadWithLocalCacheLoader<Rc<ResolvedTypeReferenceDirective>>>,
    ) -> Self {
        Self { loader }
    }
}

impl ActualResolveTypeReferenceDirectiveNamesWorker
    for ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache
{
    fn call(
        &self,
        type_reference_directive_names: &[String],
        containing_file: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Vec<Option<Rc<ResolvedTypeReferenceDirective>>> {
        load_with_local_cache(
            /*Debug.checkEachDefined(*/ type_reference_directive_names, /*)*/
            containing_file,
            redirected_reference,
            &*self.loader,
        )
        .into_iter()
        .map(Some)
        .collect()
    }
}

impl ScriptReferenceHost for Program {
    fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        self.options.clone()
    }

    fn get_source_file(&self, file_name: &str) -> Option<Rc<Node /*SourceFile*/>> {
        self.get_source_file_(file_name)
    }

    fn get_source_file_by_path(&self, path: &Path) -> Option<Rc<Node /*SourceFile*/>> {
        self.files_by_name()
            .get(&**path)
            .and_then(|value| match value {
                FilesByNameValue::SourceFile(value) => Some(value.clone()),
                _ => None,
            })
    }

    fn get_current_directory(&self) -> String {
        unimplemented!()
    }
}

impl ModuleSpecifierResolutionHost for Program {
    fn file_exists(&self, path: &str) -> bool {
        self.file_exists_rc().file_exists(path)
    }

    fn get_current_directory(&self) -> String {
        unimplemented!()
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        self.maybe_directory_exists_rc()
            .and_then(|directory_exists_rc| directory_exists_rc.directory_exists(path))
    }

    fn read_file(&self, path: &str) -> Option<io::Result<String>> {
        Some(self.host().read_file(path))
    }

    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>> {
        unimplemented!()
    }

    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String> {
        self.get_project_reference_redirect_(file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        unimplemented!()
    }

    fn get_file_include_reasons(&self) -> Rc<MultiMap<Path, FileIncludeReason>> {
        unimplemented!()
    }
}

impl TypeCheckerHost for Program {
    fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        self.options.clone()
    }

    fn get_source_files(&self) -> Ref<Vec<Rc<Node>>> {
        self.files()
    }

    fn get_source_file(&self, file_name: &str) -> Option<Rc<Node /*SourceFile*/>> {
        self.get_source_file_(file_name)
    }

    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String> {
        self.get_project_reference_redirect_(file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        unimplemented!()
    }

    fn get_common_source_directory(&self) -> Option<String> {
        unimplemented!()
    }
}

impl TypeCheckerHostDebuggable for Program {}
