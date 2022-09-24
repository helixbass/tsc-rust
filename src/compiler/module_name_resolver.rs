use bitflags::bitflags;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    combine_paths, directory_probably_exists, first_defined, for_each_ancestor_directory,
    format_message, get_base_file_name, get_directory_path, normalize_path,
    options_have_module_resolution_changes, read_json, to_path, CharacterCodes, CompilerOptions,
    DiagnosticMessage, Diagnostics, Extension, MapLike, ModuleKind, ModuleResolutionHost,
    PackageId, Path, ResolvedModuleWithFailedLookupLocations, ResolvedProjectReference,
    ResolvedTypeReferenceDirective, ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
    StringOrBool,
};

pub(crate) fn trace(
    host: &dyn ModuleResolutionHost,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) {
    host.trace(&format_message(None, message, args))
}

pub(crate) fn is_trace_enabled(
    compiler_options: &CompilerOptions,
    host: &dyn ModuleResolutionHost,
) -> bool {
    compiler_options.trace_resolution == Some(true) && host.is_trace_supported()
}

fn with_package_id(
    package_info: Option<&PackageJsonInfo>,
    r: Option<&PathAndExtension>,
) -> Option<Resolved> {
    unimplemented!()
}

struct Resolved {
    pub path: String,
    pub extension: Extension,
    pub package_id: Option<PackageId>,
    pub original_path: Option<StringOrBool>,
}

struct PathAndExtension {
    pub path: String,
    pub ext: Extension,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Extensions {
    TypeScript,
    JavaScript,
    Json,
    TSConfig,
    DtsOnly,
}

struct PathAndPackageId {
    pub file_name: String,
    pub package_id: Option<PackageId>,
}

fn resolved_type_script_only(resolved: Option<&Resolved>) -> Option<PathAndPackageId> {
    unimplemented!()
}

pub(crate) struct ModuleResolutionState<'host, TPackageJsonInfoCache: PackageJsonInfoCache> {
    pub host: &'host dyn ModuleResolutionHost,
    pub compiler_options: Rc<CompilerOptions>,
    pub trace_enabled: bool,
    pub failed_lookup_locations: RefCell<Vec<String>>,
    pub result_from_cache: Option<Rc<ResolvedModuleWithFailedLookupLocations>>,
    pub package_json_info_cache: Option<Rc<TPackageJsonInfoCache>>,
    pub features: NodeResolutionFeatures,
    pub conditions: Vec<String>,
}

// #[derive(Deserialize)]
// pub struct PackageJson {
//     pub typings: Option<String>,
//     pub types: Option<String>,
//     pub types_versions: Option<HashMap<String, HashMap<String, Vec<String>>>>,
//     pub main: Option<String>,
//     pub tsconfig: Option<String>,
//     pub type_: Option<String>,
//     pub imports: Option<serde_json::Map>,
//     pub exports: Option<serde_json::Map>,
//     pub name: Option<String>,
//     pub version: Option<String>,
// }

// impl From<serde_json::Value> for PackageJson {
//     fn from(value: serde_json::Value) -> Self {
//         serde_json::from_str(&value.to_string()).unwrap()
//     }
// }

pub type PackageJson = serde_json::Value;

pub struct VersionPaths {
    pub version: String,
    pub paths: MapLike<Vec<String>>,
}

fn read_package_json_types_version_paths(
    json_content: &PackageJson,
    state: &ModuleResolutionState<TypeReferenceDirectiveResolutionCache>,
) -> Option<Rc<VersionPaths>> {
    unimplemented!()
}

pub fn get_effective_type_roots<
    THostGetCurrentDirectory: FnMut() -> Option<String>,
    THostDirectoryExists: FnMut(&str) -> Option<bool>,
    THostIsDirectoryExistsSupported: FnMut() -> bool,
>(
    options: &CompilerOptions,
    mut host_get_current_directory: THostGetCurrentDirectory,
    host_directory_exists: THostDirectoryExists,
    host_is_directory_exists_supported: THostIsDirectoryExistsSupported,
) -> Option<Vec<String>> {
    if options.type_roots.is_some() {
        return options.type_roots.clone();
    }

    let mut current_directory: Option<String> = None;
    if let Some(options_config_file_path) = options.config_file_path.as_ref() {
        current_directory = Some(get_directory_path(options_config_file_path));
    } else
    /*if host.getCurrentDirectory*/
    {
        current_directory = host_get_current_directory();
    }

    if let Some(current_directory) = current_directory.as_ref() {
        return get_default_type_roots(
            current_directory,
            host_directory_exists,
            host_is_directory_exists_supported,
        );
    }
    None
}

fn get_default_type_roots<
    THostIsDirectoryExistsSupported: FnMut() -> bool,
    THostDirectoryExists: FnMut(&str) -> Option<bool>,
>(
    current_directory: &str,
    mut host_directory_exists: THostDirectoryExists,
    mut host_is_directory_exists_supported: THostIsDirectoryExistsSupported,
) -> Option<Vec<String>> {
    if !host_is_directory_exists_supported() {
        return Some(vec![combine_paths(
            current_directory,
            &[Some(&node_modules_at_types)],
        )]);
    }

    let mut type_roots: Option<Vec<String>> = None;
    for_each_ancestor_directory(
        &normalize_path(current_directory).into(),
        |directory| -> Option<()> {
            let at_types = combine_paths(directory, &[Some(&node_modules_at_types)]);
            if host_directory_exists(&at_types).unwrap() {
                type_roots.get_or_insert_with(|| vec![]).push(at_types);
            }
            None
        },
    );
    type_roots
}

lazy_static! {
    static ref node_modules_at_types: String = combine_paths("node_modules", &[Some("@types")]);
}

fn are_paths_equal(path1: &str, path2: &str, host: &dyn ModuleResolutionHost) -> bool {
    unimplemented!()
}

pub fn resolve_type_reference_directive(
    type_reference_directive_name: &str,
    containing_file: Option<&str>,
    mut options: Rc<CompilerOptions>,
    host: &dyn ModuleResolutionHost,
    redirected_reference: Option<Rc<ResolvedProjectReference>>,
    cache: Option<Rc<TypeReferenceDirectiveResolutionCache>>,
) -> Rc<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
    let trace_enabled = is_trace_enabled(&options, host);
    if let Some(redirected_reference) = redirected_reference.as_ref() {
        options = redirected_reference.command_line.options.clone();
    }

    let containing_directory =
        containing_file.map(|containing_file| get_directory_path(containing_file));
    let per_folder_cache =
        if let (Some(containing_directory), Some(cache)) =
            (containing_directory.as_ref(), cache.as_ref())
        {
            Some(cache.get_or_create_cache_for_directory(
                containing_directory,
                redirected_reference.clone(),
            ))
        } else {
            None
        };
    let mut result = per_folder_cache
        .as_ref()
        .and_then(|per_folder_cache| per_folder_cache.get(type_reference_directive_name, None));
    if let Some(result) = result.as_ref() {
        if trace_enabled {
            trace(
                host,
                &Diagnostics::Resolving_type_reference_directive_0_containing_file_1,
                Some(vec![
                    type_reference_directive_name.to_owned(),
                    containing_file.unwrap().to_owned(),
                ]),
            );
            if let Some(redirected_reference) = redirected_reference.as_ref() {
                trace(
                    host,
                    &Diagnostics::Using_compiler_options_of_project_reference_redirect_0,
                    Some(vec![redirected_reference
                        .source_file
                        .as_source_file()
                        .file_name()
                        .clone()]),
                );
            }
            trace(
                host,
                &Diagnostics::Resolution_for_type_reference_directive_0_was_found_in_cache_from_location_1,
                Some(vec![
                    type_reference_directive_name.to_owned(),
                    containing_directory.clone().unwrap(),
                ])
            );
            trace_result(result);
        }
        return result.clone();
    }

    let type_roots = get_effective_type_roots(
        &options,
        || host.get_current_directory(),
        |directory_name| host.directory_exists(directory_name),
        || host.is_directory_exists_supported(),
    );
    if trace_enabled {
        match containing_file {
            None => {
                match type_roots.as_ref() {
                    None => {
                        trace(
                            host,
                            &Diagnostics::Resolving_type_reference_directive_0_containing_file_not_set_root_directory_not_set,
                            Some(vec![
                                type_reference_directive_name.to_owned(),
                            ])
                        );
                    }
                    Some(type_roots) => {
                        trace(
                            host,
                            &Diagnostics::Resolving_type_reference_directive_0_containing_file_not_set_root_directory_1,
                            Some(vec![
                                type_reference_directive_name.to_owned(),
                                // TODO: not sure if this is the correct string-ification? (same below)
                                type_roots.join(", "),
                            ])
                        );
                    }
                }
            }
            Some(containing_file) => match type_roots.as_ref() {
                None => {
                    trace(
                            host,
                            &Diagnostics::Resolving_type_reference_directive_0_containing_file_1_root_directory_not_set,
                            Some(vec![
                                type_reference_directive_name.to_owned(),
                                containing_file.to_owned(),
                            ])
                        );
                }
                Some(type_roots) => {
                    trace(
                            host,
                            &Diagnostics::Resolving_type_reference_directive_0_containing_file_1_root_directory_2,
                            Some(vec![
                                type_reference_directive_name.to_owned(),
                                containing_file.to_owned(),
                                type_roots.join(", "),
                            ])
                        );
                }
            },
        }
        if let Some(redirected_reference) = redirected_reference.as_ref() {
            trace(
                host,
                &Diagnostics::Using_compiler_options_of_project_reference_redirect_0,
                Some(vec![redirected_reference
                    .source_file
                    .as_source_file()
                    .file_name()
                    .clone()]),
            );
        }
    }

    let failed_lookup_locations: Vec<String> = vec![];
    let module_resolution_state = ModuleResolutionState {
        compiler_options: options.clone(),
        host,
        trace_enabled,
        failed_lookup_locations: RefCell::new(failed_lookup_locations),
        package_json_info_cache: cache.clone(),
        features: NodeResolutionFeatures::AllFeatures,
        conditions: vec!["node".to_owned(), "require".to_owned(), "types".to_owned()],
        result_from_cache: None,
    };
    let mut resolved = primary_lookup(
        type_roots.as_deref(),
        trace_enabled,
        host,
        type_reference_directive_name,
        &module_resolution_state,
    );
    let mut primary = true;
    if resolved.is_none() {
        resolved = secondary_lookup();
        primary = false;
    }

    let mut resolved_type_reference_directive: Option<Rc<ResolvedTypeReferenceDirective>> = None;
    if let Some(resolved) = resolved {
        let PathAndPackageId {
            file_name,
            package_id,
        } = resolved;
        let resolved_file_name = if options.preserve_symlinks == Some(true) {
            file_name.clone()
        } else {
            real_path(&file_name, host, trace_enabled)
        };
        resolved_type_reference_directive = Some(Rc::new(ResolvedTypeReferenceDirective {
            primary,
            resolved_file_name: Some(resolved_file_name.clone()),
            original_path: if are_paths_equal(&file_name, &resolved_file_name, host) {
                None
            } else {
                Some(file_name.clone())
            },
            package_id,
            is_external_library_import: Some(path_contains_node_modules(&file_name)),
        }));
    }
    result = Some(Rc::new(
        ResolvedTypeReferenceDirectiveWithFailedLookupLocations {
            resolved_type_reference_directive,
            failed_lookup_locations: module_resolution_state
                .failed_lookup_locations
                .borrow()
                .clone(),
        },
    ));
    let result = result.unwrap();
    if let Some(per_folder_cache) = per_folder_cache.as_ref() {
        per_folder_cache.set(type_reference_directive_name, None, result.clone());
    }
    if trace_enabled {
        trace_result(&result);
    }
    result
}

fn primary_lookup(
    type_roots: Option<&[String]>,
    trace_enabled: bool,
    host: &dyn ModuleResolutionHost,
    type_reference_directive_name: &str,
    module_resolution_state: &ModuleResolutionState<TypeReferenceDirectiveResolutionCache>,
) -> Option<PathAndPackageId> {
    if let Some(type_roots) = type_roots.filter(|type_roots| !type_roots.is_empty()) {
        if trace_enabled {
            trace(
                host,
                &Diagnostics::Resolving_with_primary_search_path_0,
                Some(vec![type_roots.join(", ")]),
            );
        }
        first_defined(type_roots, |type_root: &String, _| {
            let candidate = combine_paths(type_root, &[Some(type_reference_directive_name)]);
            let candidate_directory = get_directory_path(&candidate);
            let directory_exists = directory_probably_exists(
                &candidate_directory,
                |directory_name| host.directory_exists(directory_name),
                || host.is_directory_exists_supported(),
            );
            if !directory_exists && trace_enabled {
                trace(
                    host,
                    &Diagnostics::Directory_0_does_not_exist_skipping_all_lookups_in_it,
                    Some(vec![candidate_directory]),
                );
            }
            resolved_type_script_only(
                load_node_module_from_directory(
                    Extensions::DtsOnly,
                    &candidate,
                    !directory_exists,
                    module_resolution_state,
                    None,
                )
                .as_ref(),
            )
        })
    } else {
        if trace_enabled {
            trace(
                host,
                &Diagnostics::Root_directory_cannot_be_determined_skipping_primary_search_paths,
                None,
            );
        }
        None
    }
}

fn secondary_lookup() -> Option<PathAndPackageId> {
    unimplemented!()
}

fn trace_result(result: &ResolvedTypeReferenceDirectiveWithFailedLookupLocations) {
    unimplemented!()
}

pub fn get_automatic_type_directive_names(
    options: &CompilerOptions,
    host: &dyn ModuleResolutionHost,
) -> Vec<String> {
    if let Some(options_types) = options.types.clone() {
        return options_types;
    }

    let mut result: Vec<String> = vec![];
    if host.is_directory_exists_supported() && host.is_get_directories_supported() {
        let type_roots = get_effective_type_roots(
            options,
            || host.get_current_directory(),
            |directory_name| host.directory_exists(directory_name),
            || host.is_directory_exists_supported(),
        );
        if let Some(type_roots) = type_roots.as_ref() {
            for root in type_roots {
                if host.directory_exists(root).unwrap() {
                    for type_directive_path in &host.get_directories(root).unwrap() {
                        let normalized = normalize_path(type_directive_path);
                        let package_json_path =
                            combine_paths(root, &[Some(&normalized), Some("package.json")]);
                        let is_not_needed_package = host.file_exists(&package_json_path)
                            && matches!(
                                read_json(&package_json_path, |file_name| host
                                    .read_file(file_name))
                                .get("typings"),
                                Some(&serde_json::Value::Null),
                            );
                        if !is_not_needed_package {
                            let base_file_name = get_base_file_name(&normalized, None, None);

                            if base_file_name.chars().next() != Some(CharacterCodes::dot) {
                                result.push(base_file_name);
                            }
                        }
                    }
                }
            }
        }
    }
    result
}

pub struct TypeReferenceDirectiveResolutionCache {
    pub pre_directory_resolution_cache: PerDirectoryResolutionCacheConcrete<
        Rc<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>,
    >,
    pub package_json_info_cache: Rc<dyn PackageJsonInfoCache>,
}

#[derive(Debug)]
pub struct ModeAwareCache<TValue> {
    underlying: RefCell<HashMap<String, TValue>>,
    memoized_reverse_keys: RefCell<HashMap<String, (String, Option<ModuleKind>)>>,
}

pub trait PerDirectoryResolutionCache<TValue> {
    fn get_or_create_cache_for_directory(
        &self,
        directory_name: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Rc<ModeAwareCache<TValue>>;
    fn clear(&self);
    fn update(&self, options: &CompilerOptions);
}

pub struct ModuleResolutionCache {
    package_json_info_cache: Rc<dyn PackageJsonInfoCache>,
}

pub trait NonRelativeModuleNameResolutionCache: PackageJsonInfoCache {
    fn get_or_create_cache_for_module_name(
        &self,
        non_relative_module_name: &str,
        mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
        redirected_reference: Option<ResolvedProjectReference>,
    ) -> &PerModuleNameCache;
}

pub trait PackageJsonInfoCache {
    fn get_package_json_info(&self, package_json_path: &str) -> Option<&PackageJsonInfoOrBool>;
    fn set_package_json_info(&self, package_json_path: &str, info: PackageJsonInfoOrBool);
    fn entries(&self) -> &[(&Path, &PackageJsonInfoOrBool)];
    fn clear(&self);
}

pub struct PerModuleNameCache {}

pub struct CacheWithRedirects<TCache> {
    options: RefCell<Option<Rc<CompilerOptions>>>,
    own_map: RefCell<Rc<RefCell<HashMap<String, Rc<TCache>>>>>,
    redirects_map: Rc<RefCell<HashMap<Path, Rc<RefCell<HashMap<String, Rc<TCache>>>>>>>,
}

impl<TCache> CacheWithRedirects<TCache> {
    pub fn new(options: Option<Rc<CompilerOptions>>) -> Self {
        Self {
            options: RefCell::new(options),
            own_map: RefCell::new(Rc::new(RefCell::new(HashMap::new()))),
            redirects_map: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn get_own_map(&self) -> Rc<RefCell<HashMap<String, Rc<TCache>>>> {
        self.own_map.borrow().clone()
    }

    pub fn redirects_map(
        &self,
    ) -> Rc<RefCell<HashMap<Path, Rc<RefCell<HashMap<String, Rc<TCache>>>>>>> {
        self.redirects_map.clone()
    }

    pub fn set_own_options(&self, new_options: Rc<CompilerOptions>) {
        *self.options.borrow_mut() = Some(new_options);
    }

    pub fn set_own_map(&self, new_own_map: Rc<RefCell<HashMap<String, Rc<TCache>>>>) {
        *self.own_map.borrow_mut() = new_own_map;
    }

    pub fn get_or_create_map_of_cache_redirects(
        &self,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Rc<RefCell<HashMap<String, Rc<TCache>>>> {
        if redirected_reference.is_none() {
            return self.own_map.borrow().clone();
        }
        let redirected_reference = redirected_reference.unwrap();
        let path = redirected_reference.source_file.as_source_file().path();
        let mut redirects = (*self.redirects_map).borrow().get(&path).cloned();
        if redirects.is_none() {
            redirects = Some(
                if match self.options.borrow().as_ref() {
                    None => true,
                    Some(options) => options_have_module_resolution_changes(
                        options,
                        &redirected_reference.command_line.options,
                    ),
                } {
                    Rc::new(RefCell::new(HashMap::new()))
                } else {
                    self.own_map.borrow().clone()
                },
            );
            self.redirects_map
                .borrow_mut()
                .insert(path.clone(), redirects.clone().unwrap());
        }
        redirects.unwrap()
    }

    pub fn clear(&self) {
        self.own_map.borrow().borrow_mut().clear();
        self.redirects_map.borrow_mut().clear();
    }
}

pub(crate) fn create_cache_with_redirects<TCache>(
    options: Option<Rc<CompilerOptions>>,
) -> CacheWithRedirects<TCache> {
    CacheWithRedirects::new(options)
}

pub fn create_package_json_info_cache(
    current_directory: &str,
    get_canonical_file_name: Rc<dyn Fn(&str) -> String>,
) -> PackageJsonInfoCacheConcrete {
    PackageJsonInfoCacheConcrete {}
}

pub struct PackageJsonInfoCacheConcrete {}

impl PackageJsonInfoCache for PackageJsonInfoCacheConcrete {
    fn get_package_json_info(&self, package_json_path: &str) -> Option<&PackageJsonInfoOrBool> {
        unimplemented!()
    }

    fn set_package_json_info(&self, package_json_path: &str, info: PackageJsonInfoOrBool) {
        unimplemented!()
    }

    fn entries(&self) -> &[(&Path, &PackageJsonInfoOrBool)] {
        unimplemented!()
    }

    fn clear(&self) {
        unimplemented!()
    }
}

fn get_or_create_cache<TCache, TCreate: FnMut() -> TCache>(
    cache_with_redirects: &CacheWithRedirects<TCache>,
    redirected_reference: Option<Rc<ResolvedProjectReference>>,
    key: &str,
    mut create: TCreate,
) -> Rc<TCache> {
    let cache = cache_with_redirects.get_or_create_map_of_cache_redirects(redirected_reference);
    let mut result: Option<Rc<TCache>> = (*cache).borrow().get(key).cloned();
    if result.is_none() {
        result = Some(Rc::new(create()));
        cache
            .borrow_mut()
            .insert(key.to_owned(), result.clone().unwrap());
    }
    result.unwrap()
}

fn create_per_directory_resolution_cache<TValue>(
    current_directory: &str,
    get_canonical_file_name: Rc<dyn Fn(&str) -> String>,
    directory_to_module_name_map: Rc<CacheWithRedirects<ModeAwareCache<TValue>>>,
) -> PerDirectoryResolutionCacheConcrete<TValue> {
    PerDirectoryResolutionCacheConcrete {
        current_directory: current_directory.to_owned(),
        get_canonical_file_name,
        directory_to_module_name_map,
    }
}

pub struct PerDirectoryResolutionCacheConcrete<TValue> {
    pub current_directory: String,
    pub get_canonical_file_name: Rc<dyn Fn(&str) -> String>,
    pub directory_to_module_name_map: Rc<CacheWithRedirects<ModeAwareCache<TValue>>>,
}

impl<TValue: Clone> PerDirectoryResolutionCache<TValue>
    for PerDirectoryResolutionCacheConcrete<TValue>
{
    fn get_or_create_cache_for_directory(
        &self,
        directory_name: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Rc<ModeAwareCache<TValue>> {
        let path = to_path(directory_name, Some(&self.current_directory), |path| {
            (self.get_canonical_file_name)(path)
        });
        get_or_create_cache(
            &self.directory_to_module_name_map,
            redirected_reference,
            &path,
            || create_mode_aware_cache(),
        )
    }

    fn clear(&self) {
        self.directory_to_module_name_map.clear();
    }

    fn update(&self, options: &CompilerOptions) {
        unimplemented!()
    }
}

pub(crate) fn create_mode_aware_cache<TValue: Clone>() -> ModeAwareCache<TValue> {
    ModeAwareCache::new()
}

impl<TValue: Clone> ModeAwareCache<TValue> {
    pub fn new() -> Self {
        Self {
            underlying: RefCell::new(HashMap::new()),
            memoized_reverse_keys: RefCell::new(HashMap::new()),
        }
    }

    pub fn get(&self, specifier: &str, mode: Option<ModuleKind>) -> Option<TValue> {
        self.underlying
            .borrow()
            .get(&self.get_underlying_cache_key(specifier, mode))
            .cloned()
    }

    pub fn set(&self, specifier: &str, mode: Option<ModuleKind>, value: TValue) -> &Self {
        self.underlying
            .borrow_mut()
            .insert(self.get_underlying_cache_key(specifier, mode), value);
        self
    }

    pub fn delete(&self, specifier: &str, mode: Option<ModuleKind>) -> &Self {
        self.underlying
            .borrow_mut()
            .remove(&self.get_underlying_cache_key(specifier, mode));
        self
    }

    pub fn has(&self, specifier: &str, mode: Option<ModuleKind>) -> bool {
        self.underlying
            .borrow()
            .contains_key(&self.get_underlying_cache_key(specifier, mode))
    }

    pub fn for_each<TCallback: FnMut(&TValue, &str, Option<ModuleKind>)>(&self, mut cb: TCallback) {
        for (key, elem) in &*self.underlying.borrow() {
            let memoized_reverse_keys = self.memoized_reverse_keys.borrow();
            let memoized_reverse_key = memoized_reverse_keys.get(key).unwrap();
            let (specifier, mode) = memoized_reverse_key;
            cb(elem, specifier, mode.clone())
        }
    }

    pub fn size(&self) -> usize {
        self.underlying.borrow().len()
    }

    fn get_underlying_cache_key(&self, specifier: &str, mode: Option<ModuleKind>) -> String {
        let result = match mode {
            None => specifier.to_owned(),
            Some(mode) => format!("{:?}|{}", mode, specifier),
        };
        self.memoized_reverse_keys
            .borrow_mut()
            .insert(result.clone(), (specifier.to_owned(), mode));
        result
    }
}

pub fn create_module_resolution_cache(
    current_directory: &str,
    get_canonical_file_name: Rc<dyn Fn(&str) -> String>,
    options: Option<Rc<CompilerOptions>>,
    directory_to_module_name_map: Option<
        Rc<CacheWithRedirects<ModeAwareCache<ResolvedModuleWithFailedLookupLocations>>>,
    >,
    module_name_to_directory_map: Option<Rc<CacheWithRedirects<PerModuleNameCache>>>,
) -> ModuleResolutionCache {
    let package_json_info_cache: Rc<dyn PackageJsonInfoCache> = Rc::new(
        create_package_json_info_cache(current_directory, get_canonical_file_name),
    );

    ModuleResolutionCache {
        package_json_info_cache,
    }
}

impl ModuleResolutionCache {
    pub fn get_package_json_info_cache(&self) -> Rc<dyn PackageJsonInfoCache> {
        self.package_json_info_cache.clone()
    }
}

impl PerDirectoryResolutionCache<ResolvedModuleWithFailedLookupLocations>
    for ModuleResolutionCache
{
    fn get_or_create_cache_for_directory(
        &self,
        directory_name: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Rc<ModeAwareCache<ResolvedModuleWithFailedLookupLocations>> {
        unimplemented!()
    }

    fn clear(&self) {
        unimplemented!()
    }

    fn update(&self, options: &CompilerOptions) {
        unimplemented!()
    }
}

impl NonRelativeModuleNameResolutionCache for ModuleResolutionCache {
    fn get_or_create_cache_for_module_name(
        &self,
        non_relative_module_name: &str,
        mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
        redirected_reference: Option<ResolvedProjectReference>,
    ) -> &PerModuleNameCache {
        unimplemented!()
    }
}

impl PackageJsonInfoCache for ModuleResolutionCache {
    fn get_package_json_info(&self, package_json_path: &str) -> Option<&PackageJsonInfoOrBool> {
        unimplemented!()
    }

    fn set_package_json_info(&self, package_json_path: &str, info: PackageJsonInfoOrBool) {
        unimplemented!()
    }

    fn entries(&self) -> &[(&Path, &PackageJsonInfoOrBool)] {
        unimplemented!()
    }

    fn clear(&self) {
        unimplemented!()
    }
}

pub fn create_type_reference_directive_resolution_cache(
    current_directory: &str,
    get_canonical_file_name: Rc<dyn Fn(&str) -> String>,
    options: Option<Rc<CompilerOptions>>,
    package_json_info_cache: Option<Rc<dyn PackageJsonInfoCache>>,
    directory_to_module_name_map: Option<
        Rc<
            CacheWithRedirects<
                ModeAwareCache<Rc<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
            >,
        >,
    >,
) -> TypeReferenceDirectiveResolutionCache {
    let pre_directory_resolution_cache = create_per_directory_resolution_cache(
        current_directory,
        get_canonical_file_name.clone(),
        directory_to_module_name_map
            .unwrap_or_else(|| Rc::new(create_cache_with_redirects(options))),
    );
    let package_json_info_cache = package_json_info_cache.unwrap_or_else(|| {
        Rc::new(create_package_json_info_cache(
            current_directory,
            get_canonical_file_name,
        ))
    });

    TypeReferenceDirectiveResolutionCache {
        pre_directory_resolution_cache,
        package_json_info_cache,
    }
}

impl PerDirectoryResolutionCache<Rc<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>
    for TypeReferenceDirectiveResolutionCache
{
    fn get_or_create_cache_for_directory(
        &self,
        directory_name: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Rc<ModeAwareCache<Rc<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
        self.pre_directory_resolution_cache
            .get_or_create_cache_for_directory(directory_name, redirected_reference)
    }

    fn clear(&self) {
        unimplemented!()
    }

    fn update(&self, options: &CompilerOptions) {
        unimplemented!()
    }
}

impl PackageJsonInfoCache for TypeReferenceDirectiveResolutionCache {
    fn get_package_json_info(&self, package_json_path: &str) -> Option<&PackageJsonInfoOrBool> {
        self.package_json_info_cache
            .get_package_json_info(package_json_path)
    }

    fn set_package_json_info(&self, package_json_path: &str, info: PackageJsonInfoOrBool) {
        self.package_json_info_cache
            .set_package_json_info(package_json_path, info)
    }

    fn entries(&self) -> &[(&Path, &PackageJsonInfoOrBool)] {
        self.package_json_info_cache.entries()
    }

    fn clear(&self) {
        self.package_json_info_cache.clear()
    }
}

pub fn resolve_module_name(
    module_name: &str,
    containing_file: &str,
    compiler_options: &CompilerOptions,
    host: &dyn ModuleResolutionHost,
    cache: Option<&ModuleResolutionCache>,
    redirected_reference: Option<&ResolvedProjectReference>,
    resolution_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) -> ResolvedModuleWithFailedLookupLocations {
    unimplemented!()
}

bitflags! {
    pub(crate) struct NodeResolutionFeatures: u32 {
        const None = 0;
        const Imports = 1 << 1;
        const SelfName = 1 << 2;
        const Exports = 1 << 3;
        const ExportsPatternTrailers = 1 << 4;
        const AllFeatures = Self::Imports.bits | Self::SelfName.bits | Self::Exports.bits | Self::ExportsPatternTrailers.bits;

        const EsmMode = 1 << 5;
    }
}

pub fn node_module_name_resolver<THost: ModuleResolutionHost>(
    module_name: &str,
    containing_file: &str,
    compiler_options: &CompilerOptions,
    host: &THost,
    cache: Option<&ModuleResolutionCache>,
    redirected_reference: Option<ResolvedProjectReference>,
    lookup_config: Option<bool>,
) -> ResolvedModuleWithFailedLookupLocations {
    unimplemented!()
}

fn real_path(path: &str, host: &dyn ModuleResolutionHost, trace_enabled: bool) -> String {
    unimplemented!()
}

fn path_contains_node_modules(path: &str) -> bool {
    unimplemented!()
}

fn load_node_module_from_directory(
    extensions: Extensions,
    candidate: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState<TypeReferenceDirectiveResolutionCache>,
    consider_package_json: Option<bool>,
) -> Option<Resolved> {
    let consider_package_json = consider_package_json.unwrap_or(true);
    let package_info = if consider_package_json {
        get_package_json_info(candidate, only_record_failures, state)
    } else {
        None
    };
    let package_json_content = package_info
        .as_ref()
        .map(|package_info| package_info.package_json_content.clone());
    let version_paths = package_info
        .as_ref()
        .and_then(|package_info| package_info.version_paths.clone());
    with_package_id(
        package_info.as_deref(),
        load_node_module_from_directory_worker(
            extensions,
            candidate,
            only_record_failures,
            state,
            package_json_content.as_deref(),
            version_paths.as_deref(),
        )
        .as_ref(),
    )
}

pub struct PackageJsonInfo {
    pub package_directory: String,
    pub package_json_content: Rc<PackageJson /*PackageJsonPathFields*/>,
    pub version_paths: Option<Rc<VersionPaths>>,
}

pub(crate) fn get_package_json_info(
    package_directory: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState<TypeReferenceDirectiveResolutionCache>,
) -> Option<Rc<PackageJsonInfo>> {
    let host = state.host;
    let trace_enabled = state.trace_enabled;
    let package_json_path = combine_paths(package_directory, &[Some("package.json")]);
    if only_record_failures {
        state
            .failed_lookup_locations
            .borrow_mut()
            .push(package_json_path.clone());
        return None;
    }

    let existing =
        state
            .package_json_info_cache
            .as_ref()
            .and_then(|state_package_json_info_cache| {
                state_package_json_info_cache.get_package_json_info(&package_json_path)
            });
    if let Some(existing) = existing {
        match existing {
            PackageJsonInfoOrBool::PackageJsonInfo(existing) => {
                if trace_enabled {
                    trace(
                        host,
                        &Diagnostics::File_0_exists_according_to_earlier_cached_lookups,
                        Some(vec![package_json_path]),
                    );
                }
                return Some(existing.clone());
            }
            PackageJsonInfoOrBool::Bool(existing) => {
                if *existing && trace_enabled {
                    trace(
                        host,
                        &Diagnostics::File_0_does_not_exist_according_to_earlier_cached_lookups,
                        Some(vec![package_json_path.clone()]),
                    );
                }
                state
                    .failed_lookup_locations
                    .borrow_mut()
                    .push(package_json_path);
                return None;
            }
        }
    }
    let directory_exists = directory_probably_exists(
        package_directory,
        |directory_name| host.directory_exists(directory_name),
        || host.is_directory_exists_supported(),
    );
    if directory_exists && host.file_exists(&package_json_path) {
        let package_json_content =
            Rc::new(read_json(&package_json_path, |path| host.read_file(path)));
        if trace_enabled {
            trace(
                host,
                &Diagnostics::Found_package_json_at_0,
                Some(vec![package_json_path.clone()]),
            );
        }
        let version_paths = read_package_json_types_version_paths(&package_json_content, state);
        let result = Rc::new(PackageJsonInfo {
            package_directory: package_directory.to_owned(),
            package_json_content,
            version_paths,
        });
        if let Some(state_package_json_info_cache) = state.package_json_info_cache.as_ref() {
            state_package_json_info_cache
                .set_package_json_info(&package_json_path, result.clone().into());
        }
        Some(result)
    } else {
        if directory_exists && trace_enabled {
            trace(
                host,
                &Diagnostics::File_0_does_not_exist,
                Some(vec![package_json_path.clone()]),
            );
        }
        if let Some(state_package_json_info_cache) = state.package_json_info_cache.as_ref() {
            state_package_json_info_cache
                .set_package_json_info(&package_json_path, directory_exists.into());
        }
        state
            .failed_lookup_locations
            .borrow_mut()
            .push(package_json_path);
        None
    }
}

fn load_node_module_from_directory_worker(
    extensions: Extensions,
    candidate: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState<TypeReferenceDirectiveResolutionCache>,
    json_content: Option<&PackageJson /*PackageJsonPathFields*/>,
    version_paths: Option<&VersionPaths>,
) -> Option<PathAndExtension> {
    unimplemented!()
}

pub enum PackageJsonInfoOrBool {
    PackageJsonInfo(Rc<PackageJsonInfo>),
    Bool(bool),
}

impl From<Rc<PackageJsonInfo>> for PackageJsonInfoOrBool {
    fn from(value: Rc<PackageJsonInfo>) -> Self {
        Self::PackageJsonInfo(value)
    }
}

impl From<bool> for PackageJsonInfoOrBool {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

pub(crate) fn get_types_package_name(package_name: &str) -> String {
    unimplemented!()
}

pub(crate) fn mangle_scoped_package_name(package_name: &str) -> String {
    unimplemented!()
}
