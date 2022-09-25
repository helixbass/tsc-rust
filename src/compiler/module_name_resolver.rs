use bitflags::bitflags;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    combine_paths, compare_paths, contains_path, directory_probably_exists,
    directory_separator_str, extension_is_ts, first_defined, for_each_ancestor_directory,
    format_message, get_base_file_name, get_directory_path, get_emit_module_kind,
    get_relative_path_from_directory, has_trailing_directory_separator,
    is_external_module_name_relative, normalize_path, options_have_module_resolution_changes,
    package_id_to_string, read_json, string_contains, to_path, try_get_extension_from_path,
    try_remove_extension, version, version_major_minor, CharacterCodes, Comparison,
    CompilerOptions, Debug_, DiagnosticMessage, Diagnostics, Extension, ModuleKind,
    ModuleResolutionHost, ModuleResolutionKind, PackageId, Path,
    ResolvedModuleWithFailedLookupLocations, ResolvedProjectReference,
    ResolvedTypeReferenceDirective, ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
    StringOrBool, StringOrPattern, Version, VersionRange,
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
    let mut package_id: Option<PackageId> = None;
    if let (Some(r), Some(package_info)) = (r, package_info) {
        let package_json_content = package_info.package_json_content.clone();
        if let (
            Some(serde_json::Value::String(package_json_content_name)),
            Some(serde_json::Value::String(package_json_content_version)),
        ) = (
            package_json_content.get("name"),
            package_json_content.get("version"),
        ) {
            package_id = Some(PackageId {
                name: package_json_content_name.clone(),
                sub_module_name: r.path
                    [package_info.package_directory.len() + directory_separator_str.len()..]
                    .to_owned(),
                version: package_json_content_version.clone(),
            });
        }
    }
    r.map(|r| Resolved {
        path: r.path.clone(),
        extension: r.ext,
        package_id,
        original_path: None,
    })
}

fn no_package_id(r: Option<&PathAndExtension>) -> Option<Resolved> {
    with_package_id(None, r)
}

fn remove_ignored_package_id(r: Option<&Resolved>) -> Option<PathAndExtension> {
    if let Some(r) = r {
        Debug_.assert(r.package_id.is_none(), None);
        return Some(PathAndExtension {
            path: r.path.clone(),
            ext: r.extension,
        });
    }
    None
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
    let resolved = resolved?;
    Debug_.assert(extension_is_ts(resolved.extension), None);
    Some(PathAndPackageId {
        file_name: resolved.path.clone(),
        package_id: resolved.package_id.clone(),
    })
}

pub(crate) struct ModuleResolutionState<'host_and_package_json_info_cache> {
    pub host: &'host_and_package_json_info_cache dyn ModuleResolutionHost,
    pub compiler_options: Rc<CompilerOptions>,
    pub trace_enabled: bool,
    pub failed_lookup_locations: RefCell<Vec<String>>,
    pub result_from_cache: Option<Rc<ResolvedModuleWithFailedLookupLocations>>,
    pub package_json_info_cache:
        Option<&'host_and_package_json_info_cache dyn PackageJsonInfoCache>,
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

#[derive(Copy, Clone, Eq, PartialEq)]
enum StringOrObject {
    String,
    Object,
}

fn does_serde_json_value_match_string_or_object(
    value: &serde_json::Value,
    string_or_object: StringOrObject,
) -> bool {
    match (value, string_or_object) {
        (serde_json::Value::Object(_), StringOrObject::Object) => true,
        (serde_json::Value::String(_), StringOrObject::String) => true,
        _ => false,
    }
}

fn read_package_json_field<'json_content>(
    json_content: &'json_content PackageJson,
    field_name: &str,
    type_of_tag: StringOrObject,
    state: &ModuleResolutionState,
) -> Option<&'json_content serde_json::Value> {
    if json_content.get(field_name).is_none() {
        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::package_json_does_not_have_a_0_field,
                Some(vec![field_name.to_owned()]),
            );
        }
        return None;
    }
    let value = json_content.get(field_name).unwrap();
    if !does_serde_json_value_match_string_or_object(value, type_of_tag) {
        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::Expected_type_of_0_field_in_package_json_to_be_1_got_2,
                Some(vec![
                    field_name.to_owned(),
                    match type_of_tag {
                        StringOrObject::String => "string".to_owned(),
                        StringOrObject::Object => "object".to_owned(),
                    },
                    if matches!(value, serde_json::Value::Null) {
                        "null".to_owned()
                    } else {
                        unimplemented!()
                    },
                ]),
            );
        }
        return None;
    }
    Some(value)
}

fn read_package_json_path_field(
    json_content: &PackageJson,
    field_name: &str, /*"typings" | "types" | "main" | "tsconfig"*/
    base_directory: &str,
    state: &ModuleResolutionState,
) -> Option<String> {
    let file_name =
        read_package_json_field(json_content, field_name, StringOrObject::String, state)?;
    let file_name = file_name.as_str().unwrap();
    if file_name.is_empty() {
        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::package_json_had_a_falsy_0_field,
                Some(vec![field_name.to_owned()]),
            );
        }
        return None;
    }
    let path = normalize_path(&combine_paths(base_directory, &[Some(file_name)]));
    if state.trace_enabled {
        trace(
            state.host,
            &Diagnostics::package_json_has_0_field_1_that_references_2,
            Some(vec![
                field_name.to_owned(),
                file_name.to_owned(),
                path.clone(),
            ]),
        );
    }
    Some(path)
}

fn read_package_json_types_field(
    json_content: &PackageJson,
    base_directory: &str,
    state: &ModuleResolutionState,
) -> Option<String> {
    read_package_json_path_field(json_content, "typings", base_directory, state)
        .or_else(|| read_package_json_path_field(json_content, "types", base_directory, state))
}

fn read_package_json_tsconfig_field(
    json_content: &PackageJson,
    base_directory: &str,
    state: &ModuleResolutionState,
) -> Option<String> {
    read_package_json_path_field(json_content, "tsconfig", base_directory, state)
}

fn read_package_json_main_field(
    json_content: &PackageJson,
    base_directory: &str,
    state: &ModuleResolutionState,
) -> Option<String> {
    read_package_json_path_field(json_content, "main", base_directory, state)
}

fn read_package_json_types_versions_field<'json_content>(
    json_content: &'json_content PackageJson,
    state: &ModuleResolutionState,
) -> Option<&'json_content serde_json::Value> {
    let types_versions =
        read_package_json_field(json_content, "typesVersions", StringOrObject::Object, state)?;

    if state.trace_enabled {
        trace(
            state.host,
            &Diagnostics::package_json_has_a_typesVersions_field_with_version_specific_path_mappings,
            None,
        );
    }

    Some(types_versions)
}

pub struct VersionPaths {
    pub version: String,
    pub paths: serde_json::Value, /*MapLike<string[]>*/
}

fn read_package_json_types_version_paths(
    json_content: &PackageJson,
    state: &ModuleResolutionState,
) -> Option<VersionPaths> {
    let types_versions = read_package_json_types_versions_field(json_content, state)?;
    let types_versions = types_versions.as_object().unwrap();

    if state.trace_enabled {
        for key in types_versions.keys() {
            if
            /*hasProperty(typesVersions, key) &&*/
            VersionRange::try_parse(key).is_none() {
                trace(
                    state.host,
                    &Diagnostics::package_json_has_a_typesVersions_entry_0_that_is_not_a_valid_semver_range,
                    Some(vec![
                        key.clone(),
                    ])
                );
            }
        }
    }

    let result = get_package_json_types_version_paths(types_versions);
    if result.is_none() {
        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::package_json_does_not_have_a_typesVersions_entry_that_matches_version_0,
                Some(vec![
                    version_major_minor.to_owned(),
                ])
            );
        }
        return None;
    }
    let result = result.unwrap();

    let best_version_key = &result.version;
    let best_version_paths = &result.paths;
    if !matches!(best_version_paths, serde_json::Value::Object(_)) {
        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::Expected_type_of_0_field_in_package_json_to_be_1_got_2,
                Some(vec![
                    format!("typesVersions['{}']", best_version_key),
                    "object".to_owned(),
                    unimplemented!(),
                ]),
            );
        }
        return None;
    }

    Some(result)
}

thread_local! {
    static type_script_version: RefCell<Option<Rc<Version>>> = RefCell::new(None);
}

pub(crate) fn get_package_json_types_version_paths(
    types_versions: &serde_json::Map<String, serde_json::Value>, /*MapLike<MapLike<string[]>>*/
) -> Option<VersionPaths> {
    type_script_version.with(|type_script_version_| {
        let mut type_script_version_ = type_script_version_.borrow_mut();
        if type_script_version_.is_none() {
            *type_script_version_ = Some(Rc::new(version.into()));
        }
        let type_script_version_ = type_script_version_.as_ref().unwrap();

        for key in types_versions.keys() {
            // if (!hasProperty(typesVersions, key)) continue;

            let key_range = VersionRange::try_parse(key);
            if key_range.is_none() {
                continue;
            }
            let key_range = key_range.unwrap();

            if key_range.test(type_script_version_.clone()) {
                return Some(VersionPaths {
                    version: key.clone(),
                    paths: types_versions.get(key).cloned().unwrap(),
                });
            }
        }
        None
    })
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
    let use_case_sensitive_file_names = host.use_case_sensitive_file_names();
    compare_paths(
        path1,
        path2,
        Some(use_case_sensitive_file_names != Some(true)),
        None,
    ) == Comparison::EqualTo
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
    let cache_clone = cache.clone();
    let cache_clone = cache_clone.as_ref();
    let module_resolution_state = ModuleResolutionState {
        compiler_options: options.clone(),
        host,
        trace_enabled,
        failed_lookup_locations: RefCell::new(failed_lookup_locations),
        package_json_info_cache: cache_clone.map(|cache| cache.as_dyn_package_json_info_cache()),
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
    module_resolution_state: &ModuleResolutionState,
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
    pre_directory_resolution_cache:
        PerDirectoryResolutionCacheConcrete<Rc<ResolvedModuleWithFailedLookupLocations>>,
    package_json_info_cache: Rc<dyn PackageJsonInfoCache>,
}

pub trait NonRelativeModuleNameResolutionCache: PackageJsonInfoCache {
    fn get_or_create_cache_for_module_name(
        &self,
        non_relative_module_name: &str,
        mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Rc<PerModuleNameCache>;
}

pub trait PackageJsonInfoCache {
    fn get_package_json_info(&self, package_json_path: &str) -> Option<PackageJsonInfoOrBool>;
    fn set_package_json_info(&self, package_json_path: &str, info: PackageJsonInfoOrBool);
    fn entries(&self) -> Vec<(Path, PackageJsonInfoOrBool)>;
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
    PackageJsonInfoCacheConcrete {
        current_directory: current_directory.to_owned(),
        cache: RefCell::new(None),
        get_canonical_file_name,
    }
}

pub struct PackageJsonInfoCacheConcrete {
    pub current_directory: String,
    pub cache: RefCell<Option<HashMap<Path, PackageJsonInfoOrBool>>>,
    pub get_canonical_file_name: Rc<dyn Fn(&str) -> String>,
}

impl PackageJsonInfoCache for PackageJsonInfoCacheConcrete {
    fn get_package_json_info(&self, package_json_path: &str) -> Option<PackageJsonInfoOrBool> {
        self.cache.borrow().as_ref().and_then(|cache| {
            cache
                .get(&to_path(
                    package_json_path,
                    Some(&self.current_directory),
                    |path| (self.get_canonical_file_name)(path),
                ))
                .cloned()
        })
    }

    fn set_package_json_info(&self, package_json_path: &str, info: PackageJsonInfoOrBool) {
        self.cache
            .borrow_mut()
            .get_or_insert_with(|| HashMap::new())
            .insert(
                to_path(package_json_path, Some(&self.current_directory), |path| {
                    (self.get_canonical_file_name)(path)
                }),
                info,
            );
    }

    fn clear(&self) {
        *self.cache.borrow_mut() = None;
    }

    fn entries(&self) -> Vec<(Path, PackageJsonInfoOrBool)> {
        self.cache.borrow().as_ref().map_or_else(
            || vec![],
            |cache| {
                cache
                    .into_iter()
                    .map(|(key, value)| (key.clone(), value.clone()))
                    .collect()
            },
        )
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
        Rc<CacheWithRedirects<ModeAwareCache<Rc<ResolvedModuleWithFailedLookupLocations>>>>,
    >,
    module_name_to_directory_map: Option<Rc<CacheWithRedirects<PerModuleNameCache>>>,
) -> ModuleResolutionCache {
    let directory_to_module_name_map = directory_to_module_name_map
        .unwrap_or_else(|| Rc::new(create_cache_with_redirects(options.clone())));
    let pre_directory_resolution_cache = create_per_directory_resolution_cache(
        current_directory,
        get_canonical_file_name.clone(),
        directory_to_module_name_map.clone(),
    );
    let package_json_info_cache: Rc<dyn PackageJsonInfoCache> = Rc::new(
        create_package_json_info_cache(current_directory, get_canonical_file_name),
    );

    ModuleResolutionCache {
        pre_directory_resolution_cache,
        package_json_info_cache,
    }
}

impl ModuleResolutionCache {
    pub fn get_package_json_info_cache(&self) -> Rc<dyn PackageJsonInfoCache> {
        self.package_json_info_cache.clone()
    }

    pub fn as_dyn_package_json_info_cache(&self) -> &dyn PackageJsonInfoCache {
        self
    }
}

impl PerDirectoryResolutionCache<Rc<ResolvedModuleWithFailedLookupLocations>>
    for ModuleResolutionCache
{
    fn get_or_create_cache_for_directory(
        &self,
        directory_name: &str,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Rc<ModeAwareCache<Rc<ResolvedModuleWithFailedLookupLocations>>> {
        self.pre_directory_resolution_cache
            .get_or_create_cache_for_directory(directory_name, redirected_reference)
    }

    fn clear(&self) {
        self.pre_directory_resolution_cache.clear()
    }

    fn update(&self, options: &CompilerOptions) {
        self.pre_directory_resolution_cache.update(options)
    }
}

impl NonRelativeModuleNameResolutionCache for ModuleResolutionCache {
    fn get_or_create_cache_for_module_name(
        &self,
        non_relative_module_name: &str,
        mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
        redirected_reference: Option<Rc<ResolvedProjectReference>>,
    ) -> Rc<PerModuleNameCache> {
        unimplemented!()
    }
}

impl PackageJsonInfoCache for ModuleResolutionCache {
    fn get_package_json_info(&self, package_json_path: &str) -> Option<PackageJsonInfoOrBool> {
        unimplemented!()
    }

    fn set_package_json_info(&self, package_json_path: &str, info: PackageJsonInfoOrBool) {
        unimplemented!()
    }

    fn entries(&self) -> Vec<(Path, PackageJsonInfoOrBool)> {
        unimplemented!()
    }

    fn clear(&self) {
        unimplemented!()
    }
}

impl PerModuleNameCache {
    pub fn get(&self, directory: &str) -> Option<Rc<ResolvedModuleWithFailedLookupLocations>> {
        unimplemented!()
    }

    pub fn set(&self, directory: &str, result: Rc<ResolvedModuleWithFailedLookupLocations>) {
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

impl TypeReferenceDirectiveResolutionCache {
    pub fn as_dyn_package_json_info_cache(&self) -> &dyn PackageJsonInfoCache {
        self
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
    fn get_package_json_info(&self, package_json_path: &str) -> Option<PackageJsonInfoOrBool> {
        self.package_json_info_cache
            .get_package_json_info(package_json_path)
    }

    fn set_package_json_info(&self, package_json_path: &str, info: PackageJsonInfoOrBool) {
        self.package_json_info_cache
            .set_package_json_info(package_json_path, info)
    }

    fn entries(&self) -> Vec<(Path, PackageJsonInfoOrBool)> {
        self.package_json_info_cache.entries()
    }

    fn clear(&self) {
        self.package_json_info_cache.clear()
    }
}

pub fn resolve_module_name(
    module_name: &str,
    containing_file: &str,
    mut compiler_options: Rc<CompilerOptions>,
    host: &dyn ModuleResolutionHost,
    cache: Option<&ModuleResolutionCache>,
    redirected_reference: Option<Rc<ResolvedProjectReference>>,
    resolution_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) -> Rc<ResolvedModuleWithFailedLookupLocations> {
    let trace_enabled = is_trace_enabled(&compiler_options, host);
    if let Some(redirected_reference) = redirected_reference.as_ref() {
        compiler_options = redirected_reference.command_line.options.clone();
    }
    if trace_enabled {
        trace(
            host,
            &Diagnostics::Resolving_module_0_from_1,
            Some(vec![module_name.to_owned(), containing_file.to_owned()]),
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
    }
    let containing_directory = get_directory_path(containing_file);
    let per_folder_cache = cache.map(|cache| {
        cache.get_or_create_cache_for_directory(&containing_directory, redirected_reference.clone())
    });
    let mut result = per_folder_cache
        .as_ref()
        .and_then(|per_folder_cache| per_folder_cache.get(module_name, resolution_mode));

    if let Some(result) = result.as_ref() {
        if trace_enabled {
            trace(
                host,
                &Diagnostics::Resolution_for_module_0_was_found_in_cache_from_location_1,
                Some(vec![module_name.to_owned(), containing_directory.clone()]),
            );
        }
    } else {
        let mut module_resolution = compiler_options.module_resolution.clone();
        if module_resolution.is_none() {
            match get_emit_module_kind(&compiler_options) {
                ModuleKind::CommonJS => {
                    module_resolution = Some(ModuleResolutionKind::NodeJs);
                }
                ModuleKind::Node12 => {
                    module_resolution = Some(ModuleResolutionKind::Node12);
                }
                ModuleKind::NodeNext => {
                    module_resolution = Some(ModuleResolutionKind::NodeNext);
                }
                _ => {
                    module_resolution = Some(ModuleResolutionKind::Classic);
                }
            }
            if trace_enabled {
                trace(
                    host,
                    &Diagnostics::Module_resolution_kind_is_not_specified_using_0,
                    Some(vec![format!("{:?}", module_resolution)]),
                );
            }
        } else {
            if trace_enabled {
                trace(
                    host,
                    &Diagnostics::Explicitly_specified_module_resolution_kind_Colon_0,
                    Some(vec![format!("{:?}", module_resolution.unwrap())]),
                );
            }
        }
        let module_resolution = module_resolution.unwrap();

        // perfLogger.logStartResolveModule(moduleName /* , containingFile, ModuleResolutionKind[moduleResolution]*/);
        match module_resolution {
            ModuleResolutionKind::Node12 => {
                result = Some(node12_module_name_resolver(
                    module_name,
                    containing_file,
                    &compiler_options,
                    host,
                    cache,
                    redirected_reference.as_deref(),
                    resolution_mode,
                ));
            }
            ModuleResolutionKind::NodeNext => {
                result = Some(node_next_module_name_resolver(
                    module_name,
                    containing_file,
                    &compiler_options,
                    host,
                    cache,
                    redirected_reference.as_deref(),
                    resolution_mode,
                ));
            }
            ModuleResolutionKind::NodeJs => {
                result = Some(node_module_name_resolver(
                    module_name,
                    containing_file,
                    compiler_options.clone(),
                    host,
                    cache,
                    redirected_reference.as_deref(),
                    None,
                ));
            }
            ModuleResolutionKind::Classic => {
                result = Some(classic_name_resolver(
                    module_name,
                    containing_file,
                    &compiler_options,
                    host,
                    cache,
                    redirected_reference.as_deref(),
                ));
            }
            _ => {
                Debug_.fail(Some(&format!(
                    "Unexpected moduleResolution: {:?}",
                    module_resolution
                )));
            }
        }
        if let Some(ref result_resolved_module) = result
            .as_ref()
            .and_then(|result| result.resolved_module.clone())
        {
            // perfLogger.logInfoEvent(`Module "${moduleName}" resolved to "${result.resolvedModule.resolvedFileName}"`);
        }
        // perfLogger.logStopResolvedModule((result && result.resolvedModule) ? "" + result.resolvedModule.resolvedFileName : "null");

        if let Some(per_folder_cache) = per_folder_cache.as_ref() {
            per_folder_cache.set(module_name, resolution_mode, result.clone().unwrap());
            if !is_external_module_name_relative(module_name) {
                cache
                    .unwrap()
                    .get_or_create_cache_for_module_name(
                        module_name,
                        resolution_mode,
                        redirected_reference.clone(),
                    )
                    .set(&containing_directory, result.clone().unwrap());
            }
        }
    }
    let result = result.unwrap();

    if trace_enabled {
        if let Some(result_resolved_module) = result.resolved_module.as_ref() {
            if let Some(result_resolved_module_package_id) =
                result_resolved_module.package_id.as_ref()
            {
                trace(
                    host,
                    &Diagnostics::Module_name_0_was_successfully_resolved_to_1_with_Package_ID_2,
                    Some(vec![
                        module_name.to_owned(),
                        result_resolved_module.resolved_file_name.clone(),
                        package_id_to_string(result_resolved_module_package_id),
                    ]),
                );
            } else {
                trace(
                    host,
                    &Diagnostics::Module_name_0_was_successfully_resolved_to_1,
                    Some(vec![
                        module_name.to_owned(),
                        result_resolved_module.resolved_file_name.clone(),
                    ]),
                );
            }
        } else {
            trace(
                host,
                &Diagnostics::Module_name_0_was_not_resolved,
                Some(vec![module_name.to_owned()]),
            );
        }
    }

    result
}

type ResolutionKindSpecificLoader =
    Rc<dyn Fn(Extensions, &str, bool, &ModuleResolutionState) -> Option<Resolved>>;

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

fn node12_module_name_resolver(
    module_name: &str,
    containing_file: &str,
    compiler_options: &CompilerOptions,
    host: &dyn ModuleResolutionHost,
    cache: Option<&ModuleResolutionCache>,
    redirected_reference: Option<&ResolvedProjectReference>,
    resolution_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) -> Rc<ResolvedModuleWithFailedLookupLocations> {
    unimplemented!()
}

fn node_next_module_name_resolver(
    module_name: &str,
    containing_file: &str,
    compiler_options: &CompilerOptions,
    host: &dyn ModuleResolutionHost,
    cache: Option<&ModuleResolutionCache>,
    redirected_reference: Option<&ResolvedProjectReference>,
    resolution_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) -> Rc<ResolvedModuleWithFailedLookupLocations> {
    unimplemented!()
}

lazy_static! {
    static ref ts_extensions: Vec<Extensions> =
        vec![Extensions::TypeScript, Extensions::JavaScript];
    static ref ts_plus_json_extensions: Vec<Extensions> = vec![
        Extensions::TypeScript,
        Extensions::JavaScript,
        Extensions::Json
    ];
    static ref tsconfig_extensions: Vec<Extensions> = vec![Extensions::TSConfig];
}

pub fn node_module_name_resolver(
    module_name: &str,
    containing_file: &str,
    compiler_options: Rc<CompilerOptions>,
    host: &dyn ModuleResolutionHost,
    cache: Option<&ModuleResolutionCache>,
    redirected_reference: Option<&ResolvedProjectReference>,
    lookup_config: Option<bool>,
) -> Rc<ResolvedModuleWithFailedLookupLocations> {
    node_module_name_resolver_worker(
        NodeResolutionFeatures::None,
        module_name,
        &get_directory_path(containing_file),
        compiler_options.clone(),
        host,
        cache,
        if lookup_config == Some(true) {
            &*tsconfig_extensions
        } else if compiler_options.resolve_json_module == Some(true) {
            &*ts_plus_json_extensions
        } else {
            &*ts_extensions
        },
        redirected_reference,
    )
}

fn node_module_name_resolver_worker(
    features: NodeResolutionFeatures,
    module_name: &str,
    containing_directory: &str,
    compiler_options: Rc<CompilerOptions>,
    host: &dyn ModuleResolutionHost,
    cache: Option<&ModuleResolutionCache>,
    extensions: &[Extensions],
    redirected_reference: Option<&ResolvedProjectReference>,
) -> Rc<ResolvedModuleWithFailedLookupLocations> {
    let trace_enabled = is_trace_enabled(&compiler_options, host);

    let failed_lookup_locations: Vec<String> = vec![];
    let state = ModuleResolutionState {
        compiler_options: compiler_options.clone(),
        host,
        trace_enabled,
        failed_lookup_locations: RefCell::new(failed_lookup_locations),
        package_json_info_cache: cache.map(|cache| cache.as_dyn_package_json_info_cache()),
        features,
        conditions: if features.intersects(NodeResolutionFeatures::EsmMode) {
            vec!["node".to_owned(), "import".to_owned(), "types".to_owned()]
        } else {
            vec!["node".to_owned(), "require".to_owned(), "types".to_owned()]
        },
        result_from_cache: None,
    };

    unimplemented!()
}

fn real_path(path: &str, host: &dyn ModuleResolutionHost, trace_enabled: bool) -> String {
    if !host.is_realpath_supported() {
        return path.to_owned();
    }

    let real = normalize_path(&host.realpath(path).unwrap());
    if trace_enabled {
        trace(
            host,
            &Diagnostics::Resolving_real_path_for_0_result_1,
            Some(vec![path.to_owned(), real.clone()]),
        );
    }
    Debug_.assert(
        host.file_exists(&real),
        Some(&format!("{} linked to nonexistent file {}", path, real)),
    );
    real
}

fn node_load_module_by_relative_name(
    extensions: Extensions,
    candidate: &str,
    mut only_record_failures: bool,
    state: &ModuleResolutionState,
    consider_package_json: bool,
) -> Option<Resolved> {
    if state.trace_enabled {
        trace(
            state.host,
            &Diagnostics::Loading_module_as_file_Slash_folder_candidate_module_location_0_target_file_type_1,
            Some(vec![
                candidate.to_owned(),
                format!("{:?}", extensions)
            ])
        );
    }
    if !has_trailing_directory_separator(candidate) {
        if !only_record_failures {
            let parent_of_candidate = get_directory_path(candidate);
            if !directory_probably_exists(
                &parent_of_candidate,
                |directory_name| state.host.directory_exists(directory_name),
                || state.host.is_directory_exists_supported(),
            ) {
                if state.trace_enabled {
                    trace(
                        state.host,
                        &Diagnostics::Directory_0_does_not_exist_skipping_all_lookups_in_it,
                        Some(vec![parent_of_candidate]),
                    );
                }
                only_record_failures = true;
            }
        }
        let resolved_from_file =
            load_module_from_file(extensions, candidate, only_record_failures, state);
        if let Some(resolved_from_file) = resolved_from_file.as_ref() {
            let package_directory = if consider_package_json {
                parse_node_module_from_path(&resolved_from_file.path)
            } else {
                None
            };
            let package_info = package_directory.as_ref().and_then(|package_directory| {
                get_package_json_info(package_directory, false, state)
            });
            return with_package_id(package_info.as_deref(), Some(resolved_from_file));
        }
    }
    if !only_record_failures {
        let candidate_exists = directory_probably_exists(
            candidate,
            |directory_name| state.host.directory_exists(directory_name),
            || state.host.is_directory_exists_supported(),
        );
        if !candidate_exists {
            if state.trace_enabled {
                trace(
                    state.host,
                    &Diagnostics::Directory_0_does_not_exist_skipping_all_lookups_in_it,
                    Some(vec![candidate.to_owned()]),
                );
            }
            only_record_failures = true;
        }
    }
    load_node_module_from_directory(
        extensions,
        candidate,
        only_record_failures,
        state,
        Some(consider_package_json),
    )
}

pub(crate) const node_modules_path_part: &str = "/node_modules/";

fn path_contains_node_modules(path: &str) -> bool {
    string_contains(path, node_modules_path_part)
}

pub(crate) fn parse_node_module_from_path(resolved: &str) -> Option<String> {
    unimplemented!()
}

fn load_module_from_file(
    extensions: Extensions,
    candidate: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState,
) -> Option<PathAndExtension> {
    if matches!(extensions, Extensions::Json | Extensions::TSConfig) {
        let extension_less = try_remove_extension(candidate, Extension::Json.to_str());
        let extension = if let Some(extension_less) = extension_less {
            &candidate[extension_less.len()..]
        } else {
            ""
        };
        return if extension_less.is_none() && extensions == Extensions::Json {
            None
        } else {
            try_adding_extensions(
                match extension_less {
                    None => candidate,
                    Some(extension_less) => {
                        if extension_less.is_empty() {
                            candidate
                        } else {
                            extension_less
                        }
                    }
                },
                extensions,
                extension,
                only_record_failures,
                state,
            )
        };
    }

    if !state.features.intersects(NodeResolutionFeatures::EsmMode) {
        let resolved_by_adding_extension =
            try_adding_extensions(candidate, extensions, "", only_record_failures, state);
        if resolved_by_adding_extension.is_some() {
            return resolved_by_adding_extension;
        }
    }

    load_module_from_file_no_implicit_extensions(extensions, candidate, only_record_failures, state)
}

fn load_module_from_file_no_implicit_extensions(
    extensions: Extensions,
    candidate: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState,
) -> Option<PathAndExtension> {
    unimplemented!()
}

fn try_adding_extensions(
    candidate: &str,
    extensions: Extensions,
    original_extension: &str,
    mut only_record_failures: bool,
    state: &ModuleResolutionState,
) -> Option<PathAndExtension> {
    if !only_record_failures {
        let directory = get_directory_path(candidate);
        if !directory.is_empty() {
            only_record_failures = !directory_probably_exists(
                &directory,
                |directory_name| state.host.directory_exists(directory_name),
                || state.host.is_directory_exists_supported(),
            );
        }
    }

    let mut candidate = candidate.to_owned();
    match extensions {
        Extensions::DtsOnly => {
            if original_extension == Extension::Mjs.to_str()
                || original_extension == Extension::Mts.to_str()
                || original_extension == Extension::Dmts.to_str()
            {
                try_extension(&candidate, only_record_failures, state, Extension::Dmts)
            } else if original_extension == Extension::Cjs.to_str()
                || original_extension == Extension::Cts.to_str()
                || original_extension == Extension::Dcts.to_str()
            {
                try_extension(&candidate, only_record_failures, state, Extension::Dcts)
            } else if original_extension == Extension::Json.to_str() {
                candidate = format!("{}{}", candidate, Extension::Json.to_str());
                try_extension(&candidate, only_record_failures, state, Extension::Dts)
            } else {
                try_extension(&candidate, only_record_failures, state, Extension::Dts)
            }
        }
        Extensions::TypeScript => {
            if original_extension == Extension::Mjs.to_str()
                || original_extension == Extension::Mts.to_str()
                || original_extension == Extension::Dmts.to_str()
            {
                try_extension(&candidate, only_record_failures, state, Extension::Mts).or_else(
                    || try_extension(&candidate, only_record_failures, state, Extension::Dmts),
                )
            } else if original_extension == Extension::Cjs.to_str()
                || original_extension == Extension::Cts.to_str()
                || original_extension == Extension::Dcts.to_str()
            {
                try_extension(&candidate, only_record_failures, state, Extension::Cts).or_else(
                    || try_extension(&candidate, only_record_failures, state, Extension::Dcts),
                )
            } else if original_extension == Extension::Json.to_str() {
                candidate = format!("{}{}", candidate, Extension::Json.to_str());
                try_extension(&candidate, only_record_failures, state, Extension::Dts)
            } else {
                try_extension(&candidate, only_record_failures, state, Extension::Ts)
                    .or_else(|| {
                        try_extension(&candidate, only_record_failures, state, Extension::Tsx)
                    })
                    .or_else(|| {
                        try_extension(&candidate, only_record_failures, state, Extension::Dts)
                    })
            }
        }
        Extensions::JavaScript => {
            if original_extension == Extension::Mjs.to_str()
                || original_extension == Extension::Mts.to_str()
                || original_extension == Extension::Dmts.to_str()
            {
                try_extension(&candidate, only_record_failures, state, Extension::Mjs)
            } else if original_extension == Extension::Cjs.to_str()
                || original_extension == Extension::Cts.to_str()
                || original_extension == Extension::Dcts.to_str()
            {
                try_extension(&candidate, only_record_failures, state, Extension::Cjs)
            } else if original_extension == Extension::Json.to_str() {
                try_extension(&candidate, only_record_failures, state, Extension::Json)
            } else {
                try_extension(&candidate, only_record_failures, state, Extension::Js).or_else(
                    || try_extension(&candidate, only_record_failures, state, Extension::Jsx),
                )
            }
        }
        Extensions::TSConfig | Extensions::Json => {
            try_extension(&candidate, only_record_failures, state, Extension::Json)
        }
    }
}

fn try_extension(
    candidate: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState,
    ext: Extension,
) -> Option<PathAndExtension> {
    let path = try_file(
        &format!("{}{}", candidate, ext.to_str()),
        only_record_failures,
        state,
    )?;
    Some(PathAndExtension { path, ext })
}

fn try_file(
    file_name: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState,
) -> Option<String> {
    if !only_record_failures {
        if state.host.file_exists(file_name) {
            if state.trace_enabled {
                trace(
                    state.host,
                    &Diagnostics::File_0_exist_use_it_as_a_name_resolution_result,
                    Some(vec![file_name.to_owned()]),
                );
            }
            return Some(file_name.to_owned());
        } else {
            if state.trace_enabled {
                trace(
                    state.host,
                    &Diagnostics::File_0_does_not_exist,
                    Some(vec![file_name.to_owned()]),
                );
            }
        }
    }
    state
        .failed_lookup_locations
        .borrow_mut()
        .push(file_name.to_owned());
    None
}

fn load_node_module_from_directory(
    extensions: Extensions,
    candidate: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState,
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
    state: &ModuleResolutionState,
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
                if existing && trace_enabled {
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
            version_paths: version_paths.map(Rc::new),
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
    state: &ModuleResolutionState,
    json_content: Option<&PackageJson /*PackageJsonPathFields*/>,
    version_paths: Option<&VersionPaths>,
) -> Option<PathAndExtension> {
    let mut package_file: Option<String> = None;
    if let Some(json_content) = json_content {
        match extensions {
            Extensions::JavaScript | Extensions::Json => {
                package_file = read_package_json_main_field(json_content, candidate, state);
            }
            Extensions::TypeScript => {
                package_file = read_package_json_types_field(json_content, candidate, state)
                    .or_else(|| read_package_json_main_field(json_content, candidate, state));
            }
            Extensions::DtsOnly => {
                package_file = read_package_json_types_field(json_content, candidate, state);
            }
            Extensions::TSConfig => {
                package_file = read_package_json_tsconfig_field(json_content, candidate, state);
            }
        }
    }

    let loader: ResolutionKindSpecificLoader =
        Rc::new(|extensions, candidate, only_record_failures, state| {
            let from_file = try_file(candidate, only_record_failures, state);
            if let Some(from_file) = from_file.as_ref() {
                let resolved = resolved_if_extension_matches(extensions, from_file);
                if let Some(resolved) = resolved.as_ref() {
                    return no_package_id(Some(resolved));
                }
                if state.trace_enabled {
                    trace(
                        state.host,
                        &Diagnostics::File_0_has_an_unsupported_extension_so_skipping_it,
                        Some(vec![from_file.clone()]),
                    );
                }
            }

            let next_extensions = if extensions == Extensions::DtsOnly {
                Extensions::TypeScript
            } else {
                extensions
            };
            node_load_module_by_relative_name(
                next_extensions,
                candidate,
                only_record_failures,
                state,
                false,
            )
        });

    let only_record_failures_for_package_file =
        package_file.as_ref().map_or(false, |package_file| {
            !directory_probably_exists(
                &get_directory_path(package_file),
                |directory_name| state.host.directory_exists(directory_name),
                || state.host.is_directory_exists_supported(),
            )
        });
    let only_record_failures_for_index = only_record_failures
        || !directory_probably_exists(
            candidate,
            |directory_name| state.host.directory_exists(directory_name),
            || state.host.is_directory_exists_supported(),
        );
    let index_path = combine_paths(
        candidate,
        &[Some(if extensions == Extensions::TSConfig {
            "tsconfig"
        } else {
            "index"
        })],
    );

    if let Some(version_paths) = version_paths {
        if match package_file.as_ref() {
            None => true,
            Some(package_file) => {
                contains_path(candidate, package_file, Option::<String>::None, None)
            }
        } {
            let module_name = get_relative_path_from_directory(
                candidate,
                package_file
                    .as_ref()
                    .filter(|package_file| !package_file.is_empty())
                    .unwrap_or(&index_path),
                Option::<fn(&str) -> String>::None,
                Some(false),
            );
            if state.trace_enabled {
                trace(
                    state.host,
                    &Diagnostics::package_json_has_a_typesVersions_entry_0_that_matches_compiler_version_1_looking_for_a_pattern_to_match_module_name_2,
                    Some(vec![
                        version_paths.version.clone(),
                        version.to_owned(),
                        module_name.clone(),
                    ])
                );
            }
            let result = try_load_module_using_paths(
                extensions,
                &module_name,
                candidate,
                &version_paths.paths,
                None,
                loader.clone(),
                only_record_failures_for_package_file || only_record_failures_for_index,
                state,
            );
            if let Some(result) = result.as_ref() {
                return remove_ignored_package_id(result.value.as_ref());
            }
        }
    }

    let package_file_result = package_file.as_ref().and_then(|package_file| {
        remove_ignored_package_id(
            loader(
                extensions,
                package_file,
                only_record_failures_for_package_file,
                state,
            )
            .as_ref(),
        )
    });
    if package_file_result.is_some() {
        return package_file_result;
    }

    if !state.features.intersects(NodeResolutionFeatures::EsmMode) {
        return load_module_from_file(
            extensions,
            &index_path,
            only_record_failures_for_index,
            state,
        );
    }
    None
}

fn resolved_if_extension_matches(extensions: Extensions, path: &str) -> Option<PathAndExtension> {
    let ext = try_get_extension_from_path(path);
    ext.filter(|ext| extension_is_ok(extensions, *ext))
        .map(|ext| PathAndExtension {
            path: path.to_owned(),
            ext,
        })
}

fn extension_is_ok(extensions: Extensions, extension: Extension) -> bool {
    match extensions {
        Extensions::JavaScript => matches!(extension, Extension::Js | Extension::Jsx),
        Extensions::TSConfig | Extensions::Json => extension == Extension::Json,
        Extensions::TypeScript => {
            matches!(extension, Extension::Ts | Extension::Tsx | Extension::Dts)
        }
        Extensions::DtsOnly => extension == Extension::Dts,
    }
}

#[derive(Clone)]
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

fn try_load_module_using_paths(
    extensions: Extensions,
    module_name: &str,
    base_directory: &str,
    paths: &serde_json::Value, /*MapLike<string[]>*/
    path_patterns: Option<&[StringOrPattern]>,
    loader: ResolutionKindSpecificLoader,
    only_record_failures: bool,
    state: &ModuleResolutionState,
) -> SearchResult<Resolved> {
    unimplemented!()
}

pub(crate) fn get_types_package_name(package_name: &str) -> String {
    unimplemented!()
}

pub(crate) fn mangle_scoped_package_name(package_name: &str) -> String {
    unimplemented!()
}

pub fn classic_name_resolver<TCache: NonRelativeModuleNameResolutionCache>(
    module_name: &str,
    containing_file: &str,
    compiler_options: &CompilerOptions,
    host: &dyn ModuleResolutionHost,
    cache: Option<&TCache>,
    redirected_reference: Option<&ResolvedProjectReference>,
) -> Rc<ResolvedModuleWithFailedLookupLocations> {
    unimplemented!()
}

type SearchResult<TValue> = Option<SearchResultPresent<TValue>>;

struct SearchResultPresent<TValue> {
    pub value: Option<TValue>,
}
