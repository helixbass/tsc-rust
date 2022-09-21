use std::collections::HashMap;

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

pub trait TypeReferenceDirectiveResolutionCache:
    PerDirectoryResolutionCache<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>
    + PackageJsonInfoCache
{
}

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

pub trait ModuleResolutionCache:
    PerDirectoryResolutionCache<ResolvedModuleWithFailedLookupLocations>
    + NonRelativeModuleNameResolutionCache
    + PackageJsonInfoCache
{
    fn get_package_json_info_cache(&self) -> &dyn PackageJsonInfoCache;
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

pub(crate) fn create_mode_aware_cache<TValue>() -> ModeAwareCache<TValue> {
    unimplemented!()
}

pub fn node_module_name_resolver<THost: ModuleResolutionHost>(
    module_name: &str,
    containing_file: &str,
    compiler_options: &CompilerOptions,
    host: &THost,
    cache: Option<&dyn ModuleResolutionCache>,
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
