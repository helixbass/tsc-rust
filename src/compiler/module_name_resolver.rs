use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    combine_paths, for_each_ancestor_directory, get_base_file_name, get_directory_path,
    normalize_path, read_json, CharacterCodes, CompilerOptions, MapLike, ModuleKind,
    ModuleResolutionHost, Path, ResolvedModuleWithFailedLookupLocations, ResolvedProjectReference,
    ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
};

pub(crate) fn is_trace_enabled(
    compiler_options: &CompilerOptions,
    host: &dyn ModuleResolutionHost,
) -> bool {
    compiler_options.trace_resolution == Some(true) && host.is_trace_supported()
}

pub struct PackageJsonPathFields {}

pub struct VersionPaths {
    pub version: String,
    pub paths: MapLike<Vec<String>>,
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

pub fn resolve_type_reference_directive(
    type_reference_directive_name: &str,
    containing_file: Option<&str>,
    options: &CompilerOptions,
    host: &dyn ModuleResolutionHost,
    redirected_reference: Option<&ResolvedProjectReference>,
    cache: Option<&TypeReferenceDirectiveResolutionCache>,
) -> ResolvedTypeReferenceDirectiveWithFailedLookupLocations {
    let trace_enabled = is_trace_enabled(options, host);
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

pub struct TypeReferenceDirectiveResolutionCache {}

#[derive(Debug)]
pub struct ModeAwareCache<TValue> {
    underlying: HashMap<String, TValue>,
}

impl<TValue> ModeAwareCache<TValue> {
    pub fn get(&self, key: &str, mode: Option<ModuleKind>) -> Option<&TValue> {
        unimplemented!()
    }

    pub fn set(&self, key: String, mode: Option<ModuleKind>, value: TValue) {
        unimplemented!()
    }

    pub fn for_each<TCallback: FnMut(&TValue, &str, Option<ModuleKind>)>(&self, cb: TCallback) {
        unimplemented!()
    }
}

pub trait PerDirectoryResolutionCache<TValue> {
    fn get_or_create_cache_for_directory(
        &self,
        directory_name: &str,
        redirected_reference: Option<ResolvedProjectReference>,
    ) -> &ModeAwareCache<TValue>;
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

pub struct CacheWithRedirects<TValue> {
    own_map: HashMap<String, TValue>,
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

pub(crate) fn create_mode_aware_cache<TValue>() -> ModeAwareCache<TValue> {
    unimplemented!()
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
        redirected_reference: Option<ResolvedProjectReference>,
    ) -> &ModeAwareCache<ResolvedModuleWithFailedLookupLocations> {
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
                ModeAwareCache<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>,
            >,
        >,
    >,
) -> TypeReferenceDirectiveResolutionCache {
    TypeReferenceDirectiveResolutionCache {}
}

impl PerDirectoryResolutionCache<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>
    for TypeReferenceDirectiveResolutionCache
{
    fn get_or_create_cache_for_directory(
        &self,
        directory_name: &str,
        redirected_reference: Option<ResolvedProjectReference>,
    ) -> &ModeAwareCache<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
        unimplemented!()
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

pub struct PackageJsonInfo {
    pub package_directory: String,
    pub package_json_content: PackageJsonPathFields,
    pub version_paths: Option<VersionPaths>,
}

pub enum PackageJsonInfoOrBool {
    PackageJsonInfo(PackageJsonInfo),
    Bool(bool),
}

impl From<PackageJsonInfo> for PackageJsonInfoOrBool {
    fn from(value: PackageJsonInfo) -> Self {
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
