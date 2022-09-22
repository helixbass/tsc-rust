use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    CompilerOptions, MapLike, ModuleKind, ModuleResolutionHost, Path,
    ResolvedModuleWithFailedLookupLocations, ResolvedProjectReference,
    ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
};

pub struct PackageJsonPathFields {}

pub struct VersionPaths {
    pub version: String,
    pub paths: MapLike<Vec<String>>,
}

pub fn resolve_type_reference_directive(
    type_reference_directive_name: &str,
    containing_file: Option<&str>,
    options: &CompilerOptions,
    host: &dyn ModuleResolutionHost,
    redirected_reference: Option<&ResolvedProjectReference>,
    cache: Option<&TypeReferenceDirectiveResolutionCache>,
) -> ResolvedTypeReferenceDirectiveWithFailedLookupLocations {
    unimplemented!()
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
