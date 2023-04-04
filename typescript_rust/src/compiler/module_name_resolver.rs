use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, Trace};
use std::cmp;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::rc::Rc;
use std::{borrow::Cow, cell::RefCell};

use crate::{
    combine_paths, compare_paths, contains, contains_path, create_get_canonical_file_name,
    directory_probably_exists, directory_separator, directory_separator_str, ends_with, every,
    extension_is_ts, file_extension_is, file_extension_is_one_of, filter, first_defined, for_each,
    for_each_ancestor_directory, format_message, get_base_file_name, get_directory_path,
    get_emit_module_kind, get_normalized_absolute_path, get_path_components,
    get_path_from_path_components, get_paths_base_path, get_relative_path_from_directory,
    get_root_length, has_js_file_extension, has_trailing_directory_separator,
    is_external_module_name_relative, is_rooted_disk_path, last_index_of, match_pattern_or_exact,
    matched_text, maybe_for_each, normalize_path, normalize_path_and_parts, normalize_slashes,
    options_have_module_resolution_changes, package_id_to_string, path_is_relative, pattern_text,
    read_json, remove_file_extension, remove_prefix, sort, starts_with, string_contains, to_path,
    try_get_extension_from_path, try_parse_patterns, try_remove_extension, version,
    version_major_minor, CharacterCodes, Comparison, CompilerOptions, Debug_, DiagnosticMessage,
    Diagnostics, Extension, MapLike, ModuleKind, ModuleResolutionHost, ModuleResolutionKind,
    PackageId, Path, PathAndParts, ResolvedModuleFull, ResolvedModuleWithFailedLookupLocations,
    ResolvedProjectReference, ResolvedTypeReferenceDirective,
    ResolvedTypeReferenceDirectiveWithFailedLookupLocations, StringOrBool, StringOrPattern,
    Version, VersionRange,
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

#[derive(Clone)]
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

fn create_resolved_module_with_failed_lookup_locations(
    resolved: Option<Resolved>,
    is_external_library_import: Option<bool>,
    mut failed_lookup_locations: Vec<String>,
    result_from_cache: Option<Gc<ResolvedModuleWithFailedLookupLocations>>,
) -> Gc<ResolvedModuleWithFailedLookupLocations> {
    if let Some(result_from_cache) = result_from_cache {
        result_from_cache
            .failed_lookup_locations
            .borrow_mut()
            .append(&mut failed_lookup_locations);
        return result_from_cache;
    }
    Gc::new(ResolvedModuleWithFailedLookupLocations {
        resolved_module: resolved.map(|resolved| {
            let Resolved {
                path: resolved_path,
                extension: resolved_extension,
                package_id: resolved_package_id,
                original_path: resolved_original_path,
            } = resolved;
            Gc::new(ResolvedModuleFull {
                resolved_file_name: resolved_path,
                original_path: resolved_original_path.and_then(|resolved_original_path| {
                    match resolved_original_path {
                        StringOrBool::Bool(_ /*true*/) => None,
                        StringOrBool::String(resolved_original_path) => {
                            Some(resolved_original_path)
                        }
                    }
                }),
                extension: Some(resolved_extension),
                is_external_library_import,
                package_id: resolved_package_id,
            })
        }),
        failed_lookup_locations: RefCell::new(failed_lookup_locations),
    })
}

pub(crate) struct ModuleResolutionState<'host_and_package_json_info_cache> {
    pub host: &'host_and_package_json_info_cache dyn ModuleResolutionHost,
    pub compiler_options: Gc<CompilerOptions>,
    pub trace_enabled: bool,
    pub failed_lookup_locations: RefCell<Vec<String>>,
    pub result_from_cache: RefCell<Option<Gc<ResolvedModuleWithFailedLookupLocations>>>,
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

fn version_paths_paths_to_map_like(paths: &serde_json::Value) -> MapLike<Vec<String>> {
    HashMap::from_iter(
        paths
            .as_object()
            .unwrap()
            .into_iter()
            .map(|(key, value)| (key.clone(), serde_json_value_to_vec_string(value))),
    )
}

fn serde_json_value_to_vec_string(value: &serde_json::Value) -> Vec<String> {
    value
        .as_array()
        .unwrap()
        .into_iter()
        .map(|item| item.as_str().unwrap().to_owned())
        .collect()
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
    mut options: Gc<CompilerOptions>,
    host: &dyn ModuleResolutionHost,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
    cache: Option<Gc<TypeReferenceDirectiveResolutionCache>>,
) -> Gc<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
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
            trace_result(host, type_reference_directive_name, result);
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
        result_from_cache: RefCell::new(None),
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
        resolved = secondary_lookup(
            containing_file,
            trace_enabled,
            host,
            type_reference_directive_name,
            &module_resolution_state,
        );
        primary = false;
    }

    let mut resolved_type_reference_directive: Option<Gc<ResolvedTypeReferenceDirective>> = None;
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
        resolved_type_reference_directive = Some(Gc::new(ResolvedTypeReferenceDirective {
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
    result = Some(Gc::new(
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
        trace_result(host, type_reference_directive_name, &result);
    }
    result
}

fn trace_result(
    host: &dyn ModuleResolutionHost,
    type_reference_directive_name: &str,
    result: &ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
) {
    if result
        .resolved_type_reference_directive
        .as_ref()
        .and_then(|result_resolved_type_reference_directive| {
            result_resolved_type_reference_directive
                .resolved_file_name
                .as_ref()
        })
        .is_none()
    {
        trace(
            host,
            &Diagnostics::Type_reference_directive_0_was_not_resolved,
            Some(vec![type_reference_directive_name.to_owned()]),
        );
    } else if let Some(result_resolved_type_reference_directive_package_id) = result
        .resolved_type_reference_directive
        .as_ref()
        .unwrap()
        .package_id
        .as_ref()
    {
        trace(
            host,
            &Diagnostics::Type_reference_directive_0_was_successfully_resolved_to_1_with_Package_ID_2_primary_Colon_3,
            Some(vec![
                type_reference_directive_name.to_owned(),
                result.resolved_type_reference_directive.as_ref().unwrap().resolved_file_name.clone().unwrap(),
                package_id_to_string(result_resolved_type_reference_directive_package_id),
                result.resolved_type_reference_directive.as_ref().unwrap().primary.to_string(),
            ])
        );
    } else {
        trace(
            host,
            &Diagnostics::Type_reference_directive_0_was_successfully_resolved_to_1_primary_Colon_2,
            Some(vec![
                type_reference_directive_name.to_owned(),
                result
                    .resolved_type_reference_directive
                    .as_ref()
                    .unwrap()
                    .resolved_file_name
                    .clone()
                    .unwrap(),
                result
                    .resolved_type_reference_directive
                    .as_ref()
                    .unwrap()
                    .primary
                    .to_string(),
            ]),
        );
    }
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

fn secondary_lookup(
    containing_file: Option<&str>,
    trace_enabled: bool,
    host: &dyn ModuleResolutionHost,
    type_reference_directive_name: &str,
    module_resolution_state: &ModuleResolutionState,
) -> Option<PathAndPackageId> {
    let initial_location_for_secondary_lookup =
        containing_file.map(|containing_file| get_directory_path(containing_file));

    if let Some(initial_location_for_secondary_lookup) =
        initial_location_for_secondary_lookup.as_ref()
    {
        if trace_enabled {
            trace(
                host,
                &Diagnostics::Looking_up_in_node_modules_folder_initial_location_0,
                Some(vec![initial_location_for_secondary_lookup.clone()]),
            );
        }
        let result: Option<Resolved>;
        if !is_external_module_name_relative(type_reference_directive_name) {
            let search_result = load_module_from_nearest_node_modules_directory(
                Extensions::DtsOnly,
                type_reference_directive_name,
                initial_location_for_secondary_lookup,
                module_resolution_state,
                None,
                None,
            );
            result = search_result.and_then(|search_result| search_result.value);
        } else {
            let ref candidate = normalize_path_and_parts(&combine_paths(
                initial_location_for_secondary_lookup,
                &[Some(type_reference_directive_name)],
            ))
            .path;
            result = node_load_module_by_relative_name(
                Extensions::DtsOnly,
                candidate,
                false,
                module_resolution_state,
                true,
            );
        }
        resolved_type_script_only(result.as_ref())
    } else {
        if trace_enabled {
            trace(
                host,
                &Diagnostics::Containing_file_is_not_specified_and_root_directory_cannot_be_determined_skipping_lookup_in_node_modules_folder,
                None,
            );
        }
        None
    }
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

#[derive(Trace, Finalize)]
pub struct TypeReferenceDirectiveResolutionCache {
    pub pre_directory_resolution_cache: PerDirectoryResolutionCacheConcrete<
        Gc<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>,
    >,
    pub package_json_info_cache: Gc<Box<dyn PackageJsonInfoCache>>,
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct ModeAwareCache<TValue: Trace + Finalize + 'static> {
    underlying: GcCell<HashMap<String, TValue>>,
    #[unsafe_ignore_trace]
    memoized_reverse_keys: RefCell<HashMap<String, (String, Option<ModuleKind>)>>,
}

pub trait PerDirectoryResolutionCache<TValue: Trace + Finalize> {
    fn get_or_create_cache_for_directory(
        &self,
        directory_name: &str,
        redirected_reference: Option<Gc<ResolvedProjectReference>>,
    ) -> Gc<ModeAwareCache<TValue>>;
    fn clear(&self);
    fn update(&self, options: &CompilerOptions);
}

#[derive(Trace, Finalize)]
pub struct ModuleResolutionCache {
    current_directory: String,
    get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>,
    pre_directory_resolution_cache:
        PerDirectoryResolutionCacheConcrete<Gc<ResolvedModuleWithFailedLookupLocations>>,
    module_name_to_directory_map: Gc<CacheWithRedirects<PerModuleNameCache>>,
    package_json_info_cache: Gc<Box<dyn PackageJsonInfoCache>>,
}

pub trait NonRelativeModuleNameResolutionCache: PackageJsonInfoCache {
    fn get_or_create_cache_for_module_name(
        &self,
        non_relative_module_name: &str,
        mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
        redirected_reference: Option<Gc<ResolvedProjectReference>>,
    ) -> Gc<PerModuleNameCache>;
    fn as_dyn_package_json_info_cache(&self) -> &dyn PackageJsonInfoCache;
}

pub trait PackageJsonInfoCache: Trace + Finalize {
    fn get_package_json_info(&self, package_json_path: &str) -> Option<PackageJsonInfoOrBool>;
    fn set_package_json_info(&self, package_json_path: &str, info: PackageJsonInfoOrBool);
    fn entries(&self) -> Vec<(Path, PackageJsonInfoOrBool)>;
    fn clear(&self);
}

#[derive(Trace, Finalize)]
pub struct PerModuleNameCache {
    current_directory: String,
    get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>,
    directory_path_map: GcCell<HashMap<String, Gc<ResolvedModuleWithFailedLookupLocations>>>,
}

#[derive(Trace, Finalize)]
pub struct CacheWithRedirects<TCache: Trace + Finalize + 'static> {
    options: GcCell<Option<Gc<CompilerOptions>>>,
    own_map: GcCell<Gc<GcCell<HashMap<String, Gc<TCache>>>>>,
    redirects_map: Gc<GcCell<HashMap<Path, Gc<GcCell<HashMap<String, Gc<TCache>>>>>>>,
}

impl<TCache: Trace + Finalize> CacheWithRedirects<TCache> {
    pub fn new(options: Option<Gc<CompilerOptions>>) -> Self {
        Self {
            options: GcCell::new(options),
            own_map: Default::default(),
            redirects_map: Default::default(),
        }
    }

    pub fn get_own_map(&self) -> Gc<GcCell<HashMap<String, Gc<TCache>>>> {
        self.own_map.borrow().clone()
    }

    pub fn redirects_map(
        &self,
    ) -> Gc<GcCell<HashMap<Path, Gc<GcCell<HashMap<String, Gc<TCache>>>>>>> {
        self.redirects_map.clone()
    }

    pub fn set_own_options(&self, new_options: Gc<CompilerOptions>) {
        *self.options.borrow_mut() = Some(new_options);
    }

    pub fn set_own_map(&self, new_own_map: Gc<GcCell<HashMap<String, Gc<TCache>>>>) {
        *self.own_map.borrow_mut() = new_own_map;
    }

    pub fn get_or_create_map_of_cache_redirects(
        &self,
        redirected_reference: Option<Gc<ResolvedProjectReference>>,
    ) -> Gc<GcCell<HashMap<String, Gc<TCache>>>> {
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
                    Gc::new(GcCell::new(HashMap::new()))
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

pub(crate) fn create_cache_with_redirects<TCache: Trace + Finalize>(
    options: Option<Gc<CompilerOptions>>,
) -> CacheWithRedirects<TCache> {
    CacheWithRedirects::new(options)
}

pub fn create_package_json_info_cache(
    current_directory: &str,
    get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>,
) -> PackageJsonInfoCacheConcrete {
    PackageJsonInfoCacheConcrete {
        current_directory: current_directory.to_owned(),
        cache: Default::default(),
        get_canonical_file_name,
    }
}

#[derive(Trace, Finalize)]
pub struct PackageJsonInfoCacheConcrete {
    pub current_directory: String,
    pub cache: GcCell<Option<HashMap<Path, PackageJsonInfoOrBool>>>,
    pub get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>,
}

impl PackageJsonInfoCache for PackageJsonInfoCacheConcrete {
    fn get_package_json_info(&self, package_json_path: &str) -> Option<PackageJsonInfoOrBool> {
        self.cache.borrow().as_ref().and_then(|cache| {
            cache
                .get(&to_path(
                    package_json_path,
                    Some(&self.current_directory),
                    |path| self.get_canonical_file_name.call(path),
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
                    self.get_canonical_file_name.call(path)
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

fn get_or_create_cache<TCache: Trace + Finalize, TCreate: FnMut() -> TCache>(
    cache_with_redirects: &CacheWithRedirects<TCache>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
    key: &str,
    mut create: TCreate,
) -> Gc<TCache> {
    let cache = cache_with_redirects.get_or_create_map_of_cache_redirects(redirected_reference);
    let mut result: Option<Gc<TCache>> = (*cache).borrow().get(key).cloned();
    if result.is_none() {
        result = Some(Gc::new(create()));
        cache
            .borrow_mut()
            .insert(key.to_owned(), result.clone().unwrap());
    }
    result.unwrap()
}

fn create_per_directory_resolution_cache<TValue: Trace + Finalize>(
    current_directory: &str,
    get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>,
    directory_to_module_name_map: Gc<CacheWithRedirects<ModeAwareCache<TValue>>>,
) -> PerDirectoryResolutionCacheConcrete<TValue> {
    PerDirectoryResolutionCacheConcrete {
        current_directory: current_directory.to_owned(),
        get_canonical_file_name,
        directory_to_module_name_map,
    }
}

#[derive(Trace, Finalize)]
pub struct PerDirectoryResolutionCacheConcrete<TValue: Trace + Finalize + 'static> {
    pub current_directory: String,
    pub get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>,
    pub directory_to_module_name_map: Gc<CacheWithRedirects<ModeAwareCache<TValue>>>,
}

pub trait GetCanonicalFileName: Trace + Finalize {
    fn call(&self, file_name: &str) -> String;
}

impl<TValue: Clone + Trace + Finalize> PerDirectoryResolutionCache<TValue>
    for PerDirectoryResolutionCacheConcrete<TValue>
{
    fn get_or_create_cache_for_directory(
        &self,
        directory_name: &str,
        redirected_reference: Option<Gc<ResolvedProjectReference>>,
    ) -> Gc<ModeAwareCache<TValue>> {
        let path = to_path(directory_name, Some(&self.current_directory), |path| {
            self.get_canonical_file_name.call(path)
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

pub(crate) fn create_mode_aware_cache<TValue: Clone + Trace + Finalize>() -> ModeAwareCache<TValue>
{
    ModeAwareCache::new()
}

impl<TValue: Clone + Trace + Finalize> ModeAwareCache<TValue> {
    pub fn new() -> Self {
        Self {
            underlying: Default::default(),
            memoized_reverse_keys: Default::default(),
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
    get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>,
    options: Option<Gc<CompilerOptions>>,
    directory_to_module_name_map: Option<
        Gc<CacheWithRedirects<ModeAwareCache<Gc<ResolvedModuleWithFailedLookupLocations>>>>,
    >,
    module_name_to_directory_map: Option<Gc<CacheWithRedirects<PerModuleNameCache>>>,
) -> ModuleResolutionCache {
    let directory_to_module_name_map = directory_to_module_name_map
        .unwrap_or_else(|| Gc::new(create_cache_with_redirects(options.clone())));
    let pre_directory_resolution_cache = create_per_directory_resolution_cache(
        current_directory,
        get_canonical_file_name.clone(),
        directory_to_module_name_map.clone(),
    );
    let module_name_to_directory_map = module_name_to_directory_map
        .unwrap_or_else(|| Gc::new(create_cache_with_redirects(options.clone())));
    let package_json_info_cache: Gc<Box<dyn PackageJsonInfoCache>> = Gc::new(Box::new(
        create_package_json_info_cache(current_directory, get_canonical_file_name.clone()),
    ));

    ModuleResolutionCache {
        current_directory: current_directory.to_owned(),
        get_canonical_file_name,
        pre_directory_resolution_cache,
        module_name_to_directory_map,
        package_json_info_cache,
    }
}

impl ModuleResolutionCache {
    pub fn get_package_json_info_cache(&self) -> Gc<Box<dyn PackageJsonInfoCache>> {
        self.package_json_info_cache.clone()
    }

    pub fn as_dyn_package_json_info_cache(&self) -> &dyn PackageJsonInfoCache {
        self
    }

    pub fn create_per_module_name_cache(&self) -> PerModuleNameCache {
        PerModuleNameCache::new(
            self.current_directory.clone(),
            self.get_canonical_file_name.clone(),
        )
    }
}

impl PerModuleNameCache {
    pub fn new(
        current_directory: String,
        get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>,
    ) -> Self {
        Self {
            current_directory,
            get_canonical_file_name,
            directory_path_map: Default::default(),
        }
    }

    pub fn get(&self, directory: &str) -> Option<Gc<ResolvedModuleWithFailedLookupLocations>> {
        self.directory_path_map
            .borrow()
            .get(&*to_path(
                directory,
                Some(&self.current_directory),
                |path| self.get_canonical_file_name.call(path),
            ))
            .cloned()
    }

    pub fn set(&self, directory: &str, result: Gc<ResolvedModuleWithFailedLookupLocations>) {
        let path = to_path(directory, Some(&self.current_directory), |path| {
            self.get_canonical_file_name.call(path)
        });
        if self.directory_path_map.borrow().contains_key(&*path) {
            return;
        }
        self.directory_path_map
            .borrow_mut()
            .insert(path.to_string(), result.clone());

        let resolved_file_name = result
            .resolved_module
            .as_ref()
            .map(|result_resolved_module| {
                result_resolved_module
                    .original_path
                    .clone()
                    .unwrap_or_else(|| result_resolved_module.resolved_file_name.clone())
            });
        let common_prefix = resolved_file_name
            .as_ref()
            .and_then(|resolved_file_name| self.get_common_prefix(&path, resolved_file_name));
        let mut current = path.to_string();
        while !matches!(
            common_prefix.as_ref(),
            Some(common_prefix) if &current == common_prefix
        ) {
            let parent = get_directory_path(&current);
            if parent == current || self.directory_path_map.borrow().contains_key(&parent) {
                break;
            }
            self.directory_path_map
                .borrow_mut()
                .insert(parent.clone(), result.clone());
            current = parent;
        }
    }

    fn get_common_prefix(&self, directory: &Path, resolution: &str) -> Option<String> {
        let resolution_directory = to_path(
            &get_directory_path(resolution),
            Some(&self.current_directory),
            |file_name: &str| self.get_canonical_file_name.call(file_name),
        );

        let mut i = 0;
        let directory_as_chars = directory.chars().collect::<Vec<_>>();
        let resolution_directory_as_chars = resolution_directory.chars().collect::<Vec<_>>();
        let limit = cmp::min(
            directory_as_chars.len(),
            resolution_directory_as_chars.len(),
        );
        while i < limit && directory_as_chars[i] == resolution_directory_as_chars[i] {
            i += 1;
        }
        if i == directory_as_chars.len()
            && (resolution_directory_as_chars.len() == i
                || resolution_directory_as_chars[i] == directory_separator)
        {
            return Some(directory.to_string());
        }
        let root_length = get_root_length(directory);
        if i < root_length {
            return None;
        }
        let sep = last_index_of(
            &directory_as_chars,
            &directory_separator,
            |ch1, ch2| ch1 == ch2,
            Some(i - 1),
        )?;
        Some(directory[0..cmp::max(sep, root_length)].to_owned())
    }
}

impl PerDirectoryResolutionCache<Gc<ResolvedModuleWithFailedLookupLocations>>
    for ModuleResolutionCache
{
    fn get_or_create_cache_for_directory(
        &self,
        directory_name: &str,
        redirected_reference: Option<Gc<ResolvedProjectReference>>,
    ) -> Gc<ModeAwareCache<Gc<ResolvedModuleWithFailedLookupLocations>>> {
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
        redirected_reference: Option<Gc<ResolvedProjectReference>>,
    ) -> Gc<PerModuleNameCache> {
        Debug_.assert(
            !is_external_module_name_relative(non_relative_module_name),
            None,
        );
        get_or_create_cache(
            self.module_name_to_directory_map.as_ref(),
            redirected_reference,
            &*match mode {
                None => non_relative_module_name.to_owned(),
                Some(mode) => format!("{:?}|{}", mode, non_relative_module_name),
            },
            || self.create_per_module_name_cache(),
        )
    }

    fn as_dyn_package_json_info_cache(&self) -> &dyn PackageJsonInfoCache {
        self
    }
}

impl PackageJsonInfoCache for ModuleResolutionCache {
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

pub fn create_type_reference_directive_resolution_cache(
    current_directory: &str,
    get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>,
    options: Option<Gc<CompilerOptions>>,
    package_json_info_cache: Option<Gc<Box<dyn PackageJsonInfoCache>>>,
    directory_to_module_name_map: Option<
        Gc<
            CacheWithRedirects<
                ModeAwareCache<Gc<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
            >,
        >,
    >,
) -> TypeReferenceDirectiveResolutionCache {
    let pre_directory_resolution_cache = create_per_directory_resolution_cache(
        current_directory,
        get_canonical_file_name.clone(),
        directory_to_module_name_map
            .unwrap_or_else(|| Gc::new(create_cache_with_redirects(options))),
    );
    let package_json_info_cache = package_json_info_cache.unwrap_or_else(|| {
        Gc::new(Box::new(create_package_json_info_cache(
            current_directory,
            get_canonical_file_name,
        )))
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

impl PerDirectoryResolutionCache<Gc<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>
    for TypeReferenceDirectiveResolutionCache
{
    fn get_or_create_cache_for_directory(
        &self,
        directory_name: &str,
        redirected_reference: Option<Gc<ResolvedProjectReference>>,
    ) -> Gc<ModeAwareCache<Gc<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
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
    mut compiler_options: Gc<CompilerOptions>,
    host: &dyn ModuleResolutionHost,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
    resolution_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) -> Gc<ResolvedModuleWithFailedLookupLocations> {
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
    let per_folder_cache = cache.as_ref().map(|cache| {
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
                    cache.as_deref(),
                    redirected_reference.as_deref(),
                    resolution_mode,
                ));
            }
            ModuleResolutionKind::NodeNext => {
                result = Some(node_next_module_name_resolver(
                    module_name,
                    containing_file,
                    compiler_options.clone(),
                    host,
                    cache.clone(),
                    redirected_reference.clone(),
                    resolution_mode,
                ));
            }
            ModuleResolutionKind::NodeJs => {
                result = Some(node_module_name_resolver(
                    module_name,
                    containing_file,
                    compiler_options.clone(),
                    host,
                    cache.clone(),
                    redirected_reference.clone(),
                    None,
                ));
            }
            ModuleResolutionKind::Classic => {
                result = Some(classic_name_resolver(
                    module_name,
                    containing_file,
                    compiler_options.clone(),
                    host,
                    cache.as_deref(),
                    redirected_reference.clone(),
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

fn try_load_module_using_optional_resolution_settings(
    extensions: Extensions,
    module_name: &str,
    containing_directory: &str,
    loader: ResolutionKindSpecificLoader,
    state: &ModuleResolutionState,
) -> Option<Resolved> {
    let resolved =
        try_load_module_using_paths_if_eligible(extensions, module_name, loader.clone(), state);
    if let Some(resolved) = resolved {
        return resolved.value;
    }

    if !is_external_module_name_relative(module_name) {
        try_load_module_using_base_url(extensions, module_name, loader, state)
    } else {
        try_load_module_using_root_dirs(
            extensions,
            module_name,
            containing_directory,
            loader,
            state,
        )
    }
}

fn try_load_module_using_paths_if_eligible(
    extensions: Extensions,
    module_name: &str,
    loader: ResolutionKindSpecificLoader,
    state: &ModuleResolutionState,
) -> SearchResult<Resolved> {
    let base_url = state.compiler_options.base_url.as_ref();
    let paths = state.compiler_options.paths.as_ref();
    let config_file = state.compiler_options.config_file.as_ref();
    if let Some(paths) = paths {
        if !path_is_relative(module_name) {
            if state.trace_enabled {
                if let Some(base_url) = base_url {
                    trace(
                        state.host,
                        &Diagnostics::baseUrl_option_is_set_to_0_using_this_value_to_resolve_non_relative_module_name_1,
                        Some(vec![
                            base_url.clone(),
                            module_name.to_owned(),
                        ])
                    );
                }
                trace(
                    state.host,
                    &Diagnostics::paths_option_is_specified_looking_for_a_pattern_to_match_module_name_0,
                    Some(vec![
                        module_name.to_owned(),
                    ])
                );
            }
            let base_directory = get_paths_base_path(&state.compiler_options, || {
                state.host.get_current_directory()
            })
            .unwrap();
            let path_patterns = config_file
                .and_then(|config_file| {
                    config_file
                        .as_source_file()
                        .maybe_config_file_specs()
                        .clone()
                })
                .map(|config_file_config_file_specs| {
                    config_file_config_file_specs
                        .maybe_path_patterns()
                        .get_or_insert_with(|| try_parse_patterns(paths))
                        .clone()
                });
            return try_load_module_using_paths(
                extensions,
                module_name,
                &base_directory,
                paths,
                path_patterns.as_deref(),
                loader,
                false,
                state,
            );
        }
    }
    None
}

fn try_load_module_using_root_dirs(
    extensions: Extensions,
    module_name: &str,
    containing_directory: &str,
    loader: ResolutionKindSpecificLoader,
    state: &ModuleResolutionState,
) -> Option<Resolved> {
    if state.compiler_options.root_dirs.is_none() {
        return None;
    }

    if state.trace_enabled {
        trace(
            state.host,
            &Diagnostics::rootDirs_option_is_set_using_it_to_resolve_relative_module_name_0,
            Some(vec![module_name.to_owned()]),
        );
    }

    let candidate = normalize_path(&combine_paths(containing_directory, &[Some(module_name)]));

    let mut matched_root_dir: Option<String> = None;
    let mut matched_normalized_prefix: Option<String> = None;
    for root_dir in state.compiler_options.root_dirs.as_ref().unwrap() {
        let mut normalized_root = normalize_path(root_dir);
        if !ends_with(&normalized_root, directory_separator_str) {
            normalized_root.push_str(directory_separator_str);
        }
        let is_longest_matching_prefix = starts_with(&candidate, &normalized_root)
            && match matched_normalized_prefix.as_ref() {
                None => true,
                Some(matched_normalized_prefix) => {
                    matched_normalized_prefix.len() < normalized_root.len()
                }
            };

        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::Checking_if_0_is_the_longest_matching_prefix_for_1_2,
                Some(vec![
                    normalized_root.clone(),
                    candidate.clone(),
                    is_longest_matching_prefix.to_string(),
                ]),
            );
        }

        if is_longest_matching_prefix {
            matched_normalized_prefix = Some(normalized_root);
            matched_root_dir = Some(root_dir.clone());
        }
    }
    if let Some(matched_normalized_prefix) = matched_normalized_prefix {
        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::Longest_matching_prefix_for_0_is_1,
                Some(vec![candidate.clone(), matched_normalized_prefix.clone()]),
            );
        }
        let suffix = &candidate[matched_normalized_prefix.len()..];

        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::Loading_0_from_the_root_dir_1_candidate_location_2,
                Some(vec![
                    suffix.to_owned(),
                    matched_normalized_prefix.clone(),
                    candidate.clone(),
                ]),
            );
        }
        let resolved_file_name = loader(
            extensions,
            &candidate,
            !directory_probably_exists(
                containing_directory,
                |directory_name: &str| state.host.directory_exists(directory_name),
                || state.host.is_directory_exists_supported(),
            ),
            state,
        );
        if resolved_file_name.is_some() {
            return resolved_file_name;
        }

        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::Trying_other_entries_in_rootDirs,
                None,
            );
        }
        for root_dir in state.compiler_options.root_dirs.as_ref().unwrap() {
            if matches!(
                matched_root_dir.as_ref(),
                Some(matched_root_dir) if root_dir == matched_root_dir
            ) {
                continue;
            }
            let candidate = combine_paths(&normalize_path(root_dir), &[Some(suffix)]);
            if state.trace_enabled {
                trace(
                    state.host,
                    &Diagnostics::Loading_0_from_the_root_dir_1_candidate_location_2,
                    Some(vec![suffix.to_owned(), root_dir.clone(), candidate.clone()]),
                );
            }
            let base_directory = get_directory_path(&candidate);
            let resolved_file_name = loader(
                extensions,
                &candidate,
                !directory_probably_exists(
                    &base_directory,
                    |directory_name: &str| state.host.directory_exists(directory_name),
                    || state.host.is_directory_exists_supported(),
                ),
                state,
            );
            if resolved_file_name.is_some() {
                return resolved_file_name;
            }
        }
        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::Module_resolution_using_rootDirs_has_failed,
                None,
            );
        }
    }
    None
}

fn try_load_module_using_base_url(
    extensions: Extensions,
    module_name: &str,
    loader: ResolutionKindSpecificLoader,
    state: &ModuleResolutionState,
) -> Option<Resolved> {
    let base_url = state.compiler_options.base_url.as_ref()?;
    if state.trace_enabled {
        trace(
            state.host,
            &Diagnostics::baseUrl_option_is_set_to_0_using_this_value_to_resolve_non_relative_module_name_1,
            Some(vec![
                base_url.clone(),
                module_name.to_owned(),
            ])
        );
    }
    let candidate = normalize_path(&combine_paths(base_url, &[Some(module_name)]));
    if state.trace_enabled {
        trace(
            state.host,
            &Diagnostics::Resolving_module_name_0_relative_to_base_url_1_2,
            Some(vec![
                module_name.to_owned(),
                base_url.clone(),
                candidate.clone(),
            ]),
        );
    }
    loader(
        extensions,
        &candidate,
        !directory_probably_exists(
            &get_directory_path(&candidate),
            |directory_name| state.host.directory_exists(directory_name),
            || state.host.is_directory_exists_supported(),
        ),
        state,
    )
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

fn node12_module_name_resolver(
    module_name: &str,
    containing_file: &str,
    compiler_options: &CompilerOptions,
    host: &dyn ModuleResolutionHost,
    cache: Option<&ModuleResolutionCache>,
    redirected_reference: Option<&ResolvedProjectReference>,
    resolution_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) -> Gc<ResolvedModuleWithFailedLookupLocations> {
    unimplemented!()
}

fn node_next_module_name_resolver(
    module_name: &str,
    containing_file: &str,
    compiler_options: Gc<CompilerOptions>,
    host: &dyn ModuleResolutionHost,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
    resolution_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) -> Gc<ResolvedModuleWithFailedLookupLocations> {
    node_next_module_name_resolver_worker(
        NodeResolutionFeatures::AllFeatures,
        module_name,
        containing_file,
        compiler_options,
        host,
        cache,
        redirected_reference,
        resolution_mode,
    )
}

fn node_next_module_name_resolver_worker(
    features: NodeResolutionFeatures,
    module_name: &str,
    containing_file: &str,
    compiler_options: Gc<CompilerOptions>,
    host: &dyn ModuleResolutionHost,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
    resolution_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
) -> Gc<ResolvedModuleWithFailedLookupLocations> {
    let containing_directory = get_directory_path(containing_file);

    let esm_mode = if resolution_mode == Some(ModuleKind::ESNext) {
        NodeResolutionFeatures::EsmMode
    } else {
        NodeResolutionFeatures::None
    };
    node_module_name_resolver_worker(
        features | esm_mode,
        module_name,
        &containing_directory,
        compiler_options.clone(),
        host,
        cache,
        if compiler_options.resolve_json_module == Some(true) {
            &ts_plus_json_extensions
        } else {
            &ts_extensions
        },
        redirected_reference,
    )
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
    compiler_options: Gc<CompilerOptions>,
    host: &dyn ModuleResolutionHost,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
    lookup_config: Option<bool>,
) -> Gc<ResolvedModuleWithFailedLookupLocations> {
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
    compiler_options: Gc<CompilerOptions>,
    host: &dyn ModuleResolutionHost,
    cache: Option<Gc<ModuleResolutionCache>>,
    extensions: &[Extensions],
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
) -> Gc<ResolvedModuleWithFailedLookupLocations> {
    let trace_enabled = is_trace_enabled(&compiler_options, host);

    let failed_lookup_locations: Vec<String> = vec![];
    let state = ModuleResolutionState {
        compiler_options: compiler_options.clone(),
        host,
        trace_enabled,
        failed_lookup_locations: RefCell::new(failed_lookup_locations),
        package_json_info_cache: cache
            .as_ref()
            .map(|cache| cache.as_dyn_package_json_info_cache()),
        features,
        conditions: if features.intersects(NodeResolutionFeatures::EsmMode) {
            vec!["node".to_owned(), "import".to_owned(), "types".to_owned()]
        } else {
            vec!["node".to_owned(), "require".to_owned(), "types".to_owned()]
        },
        result_from_cache: RefCell::new(None),
    };

    let result = for_each(
        extensions,
        |ext: &Extensions, _| -> SearchResult<TryResolveSearchResultValue> {
            try_resolve(
                module_name,
                containing_directory,
                &state,
                features,
                cache.clone(),
                redirected_reference.clone(),
                host,
                &compiler_options,
                trace_enabled,
                *ext,
            )
        },
    );
    let is_external_library_import = result
        .as_ref()
        .and_then(|result| result.value.as_ref())
        .map(|result_value| result_value.is_external_library_import);
    create_resolved_module_with_failed_lookup_locations(
        result
            .and_then(|result| result.value)
            .map(|result_value| result_value.resolved),
        is_external_library_import,
        {
            let failed_lookup_locations = state.failed_lookup_locations.borrow().clone();
            failed_lookup_locations
        },
        {
            let result_from_cache = state.result_from_cache.borrow().clone();
            result_from_cache
        },
    )
}

fn try_resolve(
    module_name: &str,
    containing_directory: &str,
    state: &ModuleResolutionState,
    features: NodeResolutionFeatures,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
    host: &dyn ModuleResolutionHost,
    compiler_options: &CompilerOptions,
    trace_enabled: bool,
    extensions: Extensions,
) -> SearchResult<TryResolveSearchResultValue> {
    let loader: ResolutionKindSpecificLoader =
        Rc::new(|extensions, candidate, only_record_failures, state| {
            node_load_module_by_relative_name(
                extensions,
                candidate,
                only_record_failures,
                state,
                true,
            )
        });
    let resolved = try_load_module_using_optional_resolution_settings(
        extensions,
        module_name,
        containing_directory,
        loader.clone(),
        state,
    );
    if let Some(resolved) = resolved {
        let is_external_library_import = path_contains_node_modules(&resolved.path);
        return to_search_result(Some(TryResolveSearchResultValue {
            resolved,
            is_external_library_import,
        }));
    }

    if !is_external_module_name_relative(module_name) {
        let mut resolved: SearchResult<Resolved> = None;
        if features.intersects(NodeResolutionFeatures::Imports) && starts_with(module_name, "#") {
            resolved = load_module_from_imports(
                extensions,
                module_name,
                containing_directory,
                state,
                cache.as_deref(),
                redirected_reference.as_deref(),
            );
        }
        if resolved.is_none() && features.intersects(NodeResolutionFeatures::SelfName) {
            resolved = load_module_from_self_name_reference(
                extensions,
                module_name,
                containing_directory,
                state,
                cache.clone(),
                redirected_reference.clone(),
            );
        }
        if resolved.is_none() {
            if trace_enabled {
                trace(
                    host,
                    &Diagnostics::Loading_module_0_from_node_modules_folder_target_file_type_1,
                    Some(vec![module_name.to_owned(), format!("{:?}", extensions)]),
                );
            }
            resolved = load_module_from_nearest_node_modules_directory(
                extensions,
                module_name,
                containing_directory,
                state,
                cache,
                redirected_reference.clone(),
            );
        }
        let resolved = resolved?;

        let mut resolved_value = resolved.value.clone();
        if compiler_options.preserve_symlinks != Some(true) {
            if let Some(resolved_value_present) =
                resolved_value
                    .as_ref()
                    .filter(
                        |resolved_value| !match resolved_value.original_path.as_ref() {
                            None => false,
                            Some(StringOrBool::String(resolved_value_original_path)) => {
                                !resolved_value_original_path.is_empty()
                            }
                            Some(StringOrBool::Bool(resolved_value_original_path)) => {
                                *resolved_value_original_path
                            }
                        },
                    )
            {
                let path = real_path(&resolved_value_present.path, host, trace_enabled);
                let original_path = if are_paths_equal(&path, &resolved_value_present.path, host) {
                    None
                } else {
                    Some(resolved_value_present.path.clone())
                };
                resolved_value = Some(Resolved {
                    extension: resolved_value_present.extension,
                    package_id: resolved_value_present.package_id.clone(),
                    path,
                    original_path: original_path.map(Into::into),
                });
            }
        }
        Some(SearchResultPresent {
            value: resolved_value.map(|resolved_value| TryResolveSearchResultValue {
                resolved: resolved_value,
                is_external_library_import: true,
            }),
        })
    } else {
        let PathAndParts {
            path: candidate,
            parts,
        } = normalize_path_and_parts(&combine_paths(containing_directory, &[Some(module_name)]));
        let resolved =
            node_load_module_by_relative_name(extensions, &candidate, false, state, true);
        resolved.and_then(|resolved| {
            to_search_result(Some(TryResolveSearchResultValue {
                resolved,
                is_external_library_import: contains(Some(&parts), &"node_modules".to_owned()),
            }))
        })
    }
}

struct TryResolveSearchResultValue {
    pub resolved: Resolved,
    pub is_external_library_import: bool,
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

pub(crate) fn path_contains_node_modules(path: &str) -> bool {
    string_contains(path, node_modules_path_part)
}

pub(crate) fn parse_node_module_from_path(resolved: &str) -> Option<String> {
    let path = normalize_path(resolved);
    let idx = path.rfind(node_modules_path_part)?;

    let index_after_node_modules = idx + node_modules_path_part.len();
    let mut index_after_package_name =
        move_to_next_directory_separator_if_available(&path, index_after_node_modules);
    // TODO: this should probably use character-awareness?
    if &path[index_after_node_modules..index_after_node_modules + 1] == "@" {
        index_after_package_name =
            move_to_next_directory_separator_if_available(&path, index_after_package_name);
    }
    Some(path[0..index_after_package_name].to_owned())
}

fn move_to_next_directory_separator_if_available(path: &str, prev_separator_index: usize) -> usize {
    let next_separator_index = path[prev_separator_index + 1..]
        .find(directory_separator)
        .map(|index| index + prev_separator_index + 1);
    match next_separator_index {
        None => prev_separator_index,
        Some(next_separator_index) => next_separator_index,
    }
}

fn load_module_from_file_no_package_id(
    extensions: Extensions,
    candidate: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState,
) -> Option<Resolved> {
    no_package_id(
        load_module_from_file(extensions, candidate, only_record_failures, state).as_ref(),
    )
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
    if has_js_file_extension(candidate)
        || file_extension_is(candidate, Extension::Json.to_str())
            && state.compiler_options.resolve_json_module == Some(true)
    {
        let extensionless = remove_file_extension(candidate);
        let extension = &candidate[extensionless.len()..];
        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::File_name_0_has_a_1_extension_stripping_it,
                Some(vec![candidate.to_owned(), extension.to_owned()]),
            );
        }
        return try_adding_extensions(
            extensionless,
            extensions,
            extension,
            only_record_failures,
            state,
        );
    }
    None
}

fn load_js_or_exact_ts_file_name(
    extensions: Extensions,
    candidate: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState,
) -> Option<PathAndExtension> {
    if matches!(extensions, Extensions::TypeScript | Extensions::DtsOnly)
        && file_extension_is_one_of(
            candidate,
            &[Extension::Dts, Extension::Dcts, Extension::Dmts],
        )
    {
        let result = try_file(candidate, only_record_failures, state);
        return result.map(|result| PathAndExtension {
            path: candidate.to_owned(),
            ext: for_each(
                [Extension::Dts, Extension::Dcts, Extension::Dmts],
                |e, _| {
                    if file_extension_is(candidate, e.to_str()) {
                        Some(e)
                    } else {
                        None
                    }
                },
            )
            .unwrap(),
        });
    }

    load_module_from_file_no_implicit_extensions(extensions, candidate, only_record_failures, state)
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

#[derive(Trace, Finalize)]
pub struct PackageJsonInfo {
    #[unsafe_ignore_trace]
    pub package_directory: String,
    #[unsafe_ignore_trace]
    pub package_json_content: Rc<PackageJson /*PackageJsonPathFields*/>,
    #[unsafe_ignore_trace]
    pub version_paths: Option<Rc<VersionPaths>>,
}

pub(crate) fn get_package_scope_for_path(
    file_name: &Path,
    package_json_info_cache: Option<&dyn PackageJsonInfoCache>,
    host: &dyn ModuleResolutionHost,
    options: Gc<CompilerOptions>,
) -> Option<Gc<PackageJsonInfo>> {
    let state = ModuleResolutionState {
        host,
        compiler_options: options.clone(),
        trace_enabled: is_trace_enabled(&options, host),
        failed_lookup_locations: Default::default(),
        package_json_info_cache,
        features: NodeResolutionFeatures::None,
        conditions: Default::default(),
        result_from_cache: Default::default(),
    };
    let mut parts = get_path_components(file_name, None);
    parts.pop();
    while !parts.is_empty() {
        let pkg = get_package_json_info(&get_path_from_path_components(&parts), false, &state);
        if pkg.is_some() {
            return pkg;
        }
        parts.pop();
    }
    None
}

pub(crate) fn get_package_json_info(
    package_directory: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState,
) -> Option<Gc<PackageJsonInfo>> {
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
        let result = Gc::new(PackageJsonInfo {
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
                &version_paths_paths_to_map_like(&version_paths.paths),
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

pub(crate) fn parse_package_name(module_name: &str) -> ParsedPackageName {
    let module_name_as_chars = module_name.chars().collect::<Vec<_>>();
    let mut idx = module_name_as_chars
        .iter()
        .position(|ch| *ch == directory_separator);
    if matches!(
        module_name_as_chars.get(0),
        Some(ch) if *ch == '@'
    ) {
        idx = module_name_as_chars
            .iter()
            .skip(idx.map_or(0, |idx| idx + 1))
            .position(|ch| *ch == directory_separator);
    }
    match idx {
        None => ParsedPackageName {
            package_name: module_name.to_owned(),
            rest: "".to_owned(),
        },
        Some(idx) => ParsedPackageName {
            package_name: module_name_as_chars.iter().take(idx).collect(),
            rest: module_name_as_chars.iter().skip(idx + 1).collect(),
        },
    }
}

pub(crate) struct ParsedPackageName {
    pub package_name: String,
    pub rest: String,
}

pub(crate) fn all_keys_start_with_dot<'str>(
    mut obj_keys: impl Iterator<Item = &'str String>,
) -> bool {
    obj_keys.all(|k| starts_with(k, "."))
}

pub(crate) fn no_key_starts_with_dot<'str>(
    mut obj_keys: impl Iterator<Item = &'str String>,
) -> bool {
    !obj_keys.any(|k| starts_with(k, "."))
}

fn load_module_from_self_name_reference(
    extensions: Extensions,
    module_name: &str,
    directory: &str,
    state: &ModuleResolutionState,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
) -> SearchResult<Resolved> {
    let use_case_sensitive_file_names = state.host.use_case_sensitive_file_names();
    let directory_path = to_path(
        &combine_paths(directory, &[Some("dummy")]),
        state.host.get_current_directory().as_deref(),
        create_get_canonical_file_name(use_case_sensitive_file_names.unwrap_or(true)),
    );
    let scope = get_package_scope_for_path(
        &directory_path,
        state.package_json_info_cache,
        state.host,
        state.compiler_options.clone(),
    )?;
    let scope_package_json_content = scope.package_json_content.as_object()?;
    if !scope_package_json_content.contains_key("exports") {
        return None;
    }
    let scope_package_json_name = scope_package_json_content
        .get("name")
        .and_then(|scope_package_json_name| scope_package_json_name.as_str())?;
    let parts = get_path_components(module_name, None);
    let name_parts = get_path_components(scope_package_json_name, None);
    if !every(&name_parts, |p: &String, i| {
        matches!(
            parts.get(i),
            Some(parts_i) if parts_i == p
        )
    }) {
        return None;
    }
    let trailing_parts = &parts[name_parts.len()..];
    load_module_from_exports(
        &scope,
        extensions,
        &if trailing_parts.is_empty() {
            ".".to_owned()
        } else {
            format!(
                ".{}{}",
                directory_separator_str,
                trailing_parts.join(directory_separator_str),
            )
        },
        state,
        cache,
        redirected_reference,
    )
}

fn load_module_from_exports(
    scope: &PackageJsonInfo,
    extensions: Extensions,
    subpath: &str,
    state: &ModuleResolutionState,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
) -> SearchResult<Resolved> {
    let scope_package_json_exports = scope.package_json_content.get("exports")?;

    if subpath == "." {
        let mut main_export: Option<&serde_json::Value> = None;
        if scope_package_json_exports.is_string()
            || scope_package_json_exports.is_array()
            || scope_package_json_exports
                .as_object()
                .filter(|scope_package_json_exports| {
                    no_key_starts_with_dot(scope_package_json_exports.keys())
                })
                .is_some()
        {
            main_export = Some(scope_package_json_exports);
        } else if let Some(scope_package_json_exports_dot) = scope_package_json_exports
            .as_object()
            .and_then(|scope_package_json_exports| scope_package_json_exports.get("."))
        {
            main_export = Some(scope_package_json_exports_dot);
        }
        if let Some(main_export) = main_export {
            let load_module_from_target_import_or_export =
                get_load_module_from_target_import_or_export(
                    extensions,
                    state,
                    cache,
                    redirected_reference,
                    subpath,
                    scope,
                    false,
                );
            return load_module_from_target_import_or_export.call(Some(main_export), "", false);
        }
    } else if all_keys_start_with_dot(scope_package_json_exports.as_object().unwrap().keys()) {
        if !scope_package_json_exports.is_object() {
            if state.trace_enabled {
                trace(
                    state.host,
                    &Diagnostics::Export_specifier_0_does_not_exist_in_package_json_scope_at_path_1,
                    Some(vec![subpath.to_owned(), scope.package_directory.clone()]),
                );
            }
            return to_search_result(None);
        }
        let result = load_module_from_imports_or_exports(
            extensions,
            state,
            cache,
            redirected_reference,
            subpath,
            scope_package_json_exports.as_object().unwrap(),
            scope,
            false,
        );
        if result.is_some() {
            return result;
        }
    }

    if state.trace_enabled {
        trace(
            state.host,
            &Diagnostics::Export_specifier_0_does_not_exist_in_package_json_scope_at_path_1,
            Some(vec![subpath.to_owned(), scope.package_directory.clone()]),
        );
    }
    to_search_result(None)
}

fn load_module_from_imports(
    extensions: Extensions,
    module_name: &str,
    directory: &str,
    state: &ModuleResolutionState,
    cache: Option<&ModuleResolutionCache>,
    redirected_reference: Option<&ResolvedProjectReference>,
) -> SearchResult<Resolved> {
    unimplemented!()
}

fn load_module_from_imports_or_exports(
    extensions: Extensions,
    state: &ModuleResolutionState,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
    module_name: &str,
    lookup_table: &serde_json::Map<String, serde_json::Value>,
    scope: &PackageJsonInfo,
    is_imports: bool,
) -> SearchResult<Resolved> {
    let load_module_from_target_import_or_export = get_load_module_from_target_import_or_export(
        extensions,
        state,
        cache,
        redirected_reference,
        module_name,
        scope,
        is_imports,
    );

    if !ends_with(module_name, directory_separator_str)
        && !module_name.contains("*")
        && lookup_table.contains_key(module_name)
    {
        let target = lookup_table.get(module_name);
        return load_module_from_target_import_or_export.call(target, "", false);
    }
    let expanding_keys: Vec<_> = sort(
        &filter(
            &lookup_table.keys().cloned().collect::<Vec<_>>(),
            |k: &String| k.contains("*") || ends_with(k, "/"),
        ),
        |a: &String, b: &String| match isize::try_from(a.len()).unwrap()
            - isize::try_from(b.len()).unwrap()
        {
            value if value < 0 => Comparison::LessThan,
            0 => Comparison::EqualTo,
            _ => Comparison::GreaterThan,
        },
    )
    .into();
    for potential_target in &expanding_keys {
        if state
            .features
            .intersects(NodeResolutionFeatures::ExportsPatternTrailers)
            && matches_pattern_with_trailer(potential_target, module_name)
        {
            let target = lookup_table.get(potential_target);
            let star_pos = potential_target.find("*");
            let subpath = &module_name[match star_pos {
                Some(star_pos) => potential_target[0..star_pos].len(),
                None => 0,
            }
                ..usize::try_from(
                    isize::try_from(module_name.len()).unwrap()
                        - (isize::try_from(potential_target.len()).unwrap()
                            - 1
                            - star_pos
                                .map(|star_pos| isize::try_from(star_pos).unwrap())
                                .unwrap_or(-1)),
                )
                .unwrap()];
            return load_module_from_target_import_or_export.call(target, subpath, true);
        } else if ends_with(potential_target, "*")
            && starts_with(
                module_name,
                &potential_target[0..potential_target.len() - 1],
            )
        {
            let target = lookup_table.get(potential_target);
            let subpath = &module_name[potential_target.len() - 1..];
            return load_module_from_target_import_or_export.call(target, subpath, true);
        } else if starts_with(module_name, potential_target) {
            let target = lookup_table.get(potential_target);
            let subpath = &module_name[potential_target.len()..];
            return load_module_from_target_import_or_export.call(target, subpath, false);
        }
    }
    None
}

fn matches_pattern_with_trailer(target: &str, name: &str) -> bool {
    if ends_with(target, "*") {
        return false;
    }
    let star_pos = target.find("*");
    if star_pos.is_none() {
        return false;
    }
    let star_pos = star_pos.unwrap();
    starts_with(name, &target[0..star_pos]) && ends_with(name, &target[star_pos + 1..])
}

fn get_load_module_from_target_import_or_export<'a>(
    extensions: Extensions,
    state: &'a ModuleResolutionState,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
    module_name: &'a str,
    scope: &'a PackageJsonInfo,
    is_imports: bool,
) -> LoadModuleFromTargetImportOrExport<'a> {
    LoadModuleFromTargetImportOrExport {
        extensions,
        state,
        cache,
        redirected_reference,
        module_name,
        scope,
        is_imports,
    }
}

struct LoadModuleFromTargetImportOrExport<'a> {
    extensions: Extensions,
    state: &'a ModuleResolutionState<'a>,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
    module_name: &'a str,
    scope: &'a PackageJsonInfo,
    is_imports: bool,
}

impl<'a> LoadModuleFromTargetImportOrExport<'a> {
    fn call(
        &self,
        target: Option<&serde_json::Value>,
        subpath: &str,
        pattern: bool,
    ) -> SearchResult<Resolved> {
        if let Some(target) = target.and_then(|target| target.as_str()) {
            if !pattern && !subpath.is_empty() && !ends_with(target, "/") {
                if self.state.trace_enabled {
                    trace(
                        self.state.host,
                        &Diagnostics::package_json_scope_0_has_invalid_type_for_target_of_specifier_1,
                        Some(vec![
                            self.scope.package_directory.clone(),
                            self.module_name.to_owned(),
                        ])
                    );
                }
                return to_search_result(None);
            }
            if !starts_with(target, "./") {
                if self.is_imports
                    && !starts_with(target, "../")
                    && !starts_with(target, "/")
                    && !is_rooted_disk_path(target)
                {
                    let combined_lookup = if pattern {
                        target.replace("*", subpath)
                    } else {
                        format!("{target}{subpath}")
                    };
                    let result = node_module_name_resolver_worker(
                        self.state.features,
                        &combined_lookup,
                        &format!("{}/", self.scope.package_directory),
                        self.state.compiler_options.clone(),
                        self.state.host,
                        self.cache.clone(),
                        &[self.extensions],
                        self.redirected_reference.clone(),
                    );
                    return to_search_result(result.resolved_module.as_ref().map(
                        |result_resolved_module| Resolved {
                            path: result_resolved_module.resolved_file_name.clone(),
                            extension: result_resolved_module.extension.unwrap(),
                            package_id: result_resolved_module.package_id.clone(),
                            original_path:
                                result_resolved_module.original_path.clone().map(Into::into),
                        },
                    ));
                }
                if self.state.trace_enabled {
                    trace(
                        self.state.host,
                        &Diagnostics::package_json_scope_0_has_invalid_type_for_target_of_specifier_1,
                        Some(vec![
                            self.scope.package_directory.clone(),
                            self.module_name.to_owned()
                        ])
                    );
                }
                return to_search_result(None);
            }
            let parts = if path_is_relative(target) {
                get_path_components(target, None)[1..].to_owned()
            } else {
                get_path_components(target, None)
            };
            let parts_after_first = &parts[1..];
            if parts_after_first.into_iter().any(|part| part == "..")
                || parts_after_first.into_iter().any(|part| part == ".")
                || parts_after_first
                    .into_iter()
                    .any(|part| part == "node_modules")
            {
                if self.state.trace_enabled {
                    trace(
                        self.state.host,
                        &Diagnostics::package_json_scope_0_has_invalid_type_for_target_of_specifier_1,
                        Some(vec![
                            self.scope.package_directory.clone(),
                            self.module_name.to_owned(),
                        ])
                    );
                }
                return to_search_result(None);
            }
            let resolved_target = combine_paths(&self.scope.package_directory, &[Some(target)]);
            let subpath_parts = get_path_components(subpath, None);
            if subpath_parts
                .iter()
                .any(|subpath_part| subpath_part == "..")
                || subpath_parts.iter().any(|subpath_part| subpath_part == ".")
                || subpath_parts
                    .iter()
                    .any(|subpath_part| subpath_part == "node_modules")
            {
                if self.state.trace_enabled {
                    trace(
                        self.state.host,
                        &Diagnostics::package_json_scope_0_has_invalid_type_for_target_of_specifier_1,
                        Some(vec![
                            self.scope.package_directory.clone(),
                            self.module_name.to_owned(),
                        ])
                    );
                }
                return to_search_result(None);
            }
            let final_path = get_normalized_absolute_path(
                &if pattern {
                    resolved_target.replace("*", subpath)
                } else {
                    format!("{resolved_target}{subpath}")
                },
                self.state.host.get_current_directory().as_deref(),
            );

            return to_search_result(with_package_id(
                Some(self.scope),
                load_js_or_exact_ts_file_name(self.extensions, &final_path, false, self.state)
                    .as_ref(),
            ));
        } else if let Some(target) = target.filter(|target| target.is_object() || target.is_array())
        {
            if let Some(target) = target.as_object() {
                for key in target.keys() {
                    if key == "default"
                        || self.state.conditions.contains(key)
                        || is_applicable_versioned_types_key(&self.state.conditions, key)
                    {
                        let sub_target = target.get(key);
                        let result = self.call(sub_target, subpath, pattern);
                        if result.is_some() {
                            return result;
                        }
                    }
                }
                return None;
            } else {
                let target = target.as_array().unwrap();
                if target.is_empty() {
                    if self.state.trace_enabled {
                        trace(
                            self.state.host,
                            &Diagnostics::package_json_scope_0_has_invalid_type_for_target_of_specifier_1,
                            Some(vec![
                                self.scope.package_directory.clone(),
                                self.module_name.to_owned(),
                            ])
                        );
                    }
                    return to_search_result(None);
                }
                for elem in target {
                    let result = self.call(Some(elem), subpath, pattern);
                    if result.is_some() {
                        return result;
                    }
                }
            }
        } else if target.filter(|target| target.is_null()).is_some() {
            if self.state.trace_enabled {
                trace(
                    self.state.host,
                    &Diagnostics::package_json_scope_0_explicitly_maps_specifier_1_to_null,
                    Some(vec![
                        self.scope.package_directory.clone(),
                        self.module_name.to_owned(),
                    ]),
                );
            }
            return to_search_result(None);
        }
        if self.state.trace_enabled {
            trace(
                self.state.host,
                &Diagnostics::package_json_scope_0_has_invalid_type_for_target_of_specifier_1,
                Some(vec![
                    self.scope.package_directory.clone(),
                    self.module_name.to_owned(),
                ]),
            );
        }
        to_search_result(None)
    }
}

pub(crate) fn is_applicable_versioned_types_key(conditions: &[String], key: &str) -> bool {
    unimplemented!()
}

fn load_module_from_nearest_node_modules_directory(
    extensions: Extensions,
    module_name: &str,
    directory: &str,
    state: &ModuleResolutionState,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
) -> SearchResult<Resolved> {
    load_module_from_nearest_node_modules_directory_worker(
        extensions,
        module_name,
        directory,
        state,
        false,
        cache,
        redirected_reference,
    )
}

fn load_module_from_nearest_node_modules_directory_types_scope(
    module_name: &str,
    directory: &str,
    state: &ModuleResolutionState,
) -> SearchResult<Resolved> {
    load_module_from_nearest_node_modules_directory_worker(
        Extensions::DtsOnly,
        module_name,
        directory,
        state,
        true,
        None,
        None,
    )
}

fn load_module_from_nearest_node_modules_directory_worker(
    extensions: Extensions,
    module_name: &str,
    directory: &str,
    state: &ModuleResolutionState,
    types_scope_only: bool,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
) -> SearchResult<Resolved> {
    let per_module_name_cache = cache.as_ref().map(|cache| {
        cache.get_or_create_cache_for_module_name(
            module_name,
            if state.features == NodeResolutionFeatures::None {
                None
            } else if state.features.intersects(NodeResolutionFeatures::EsmMode) {
                Some(ModuleKind::ESNext)
            } else {
                Some(ModuleKind::CommonJS)
            },
            redirected_reference.clone(),
        )
    });
    for_each_ancestor_directory(&normalize_slashes(directory).into(), |ancestor_directory| {
        if get_base_file_name(ancestor_directory, None, None) != "node_modules" {
            let resolution_from_cache = try_find_non_relative_module_name_in_cache(
                per_module_name_cache.as_deref(),
                module_name,
                ancestor_directory,
                state,
            );
            if resolution_from_cache.is_some() {
                return resolution_from_cache;
            }
            return to_search_result(load_module_from_immediate_node_modules_directory(
                extensions,
                module_name,
                ancestor_directory,
                state,
                types_scope_only,
                cache.clone(),
                redirected_reference.clone(),
            ));
        }
        None
    })
}

fn load_module_from_immediate_node_modules_directory(
    extensions: Extensions,
    module_name: &str,
    directory: &str,
    state: &ModuleResolutionState,
    types_scope_only: bool,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
) -> Option<Resolved> {
    let node_modules_folder = combine_paths(directory, &[Some("node_modules")]);
    let node_modules_folder_exists = directory_probably_exists(
        &node_modules_folder,
        |directory_name| state.host.directory_exists(directory_name),
        || state.host.is_directory_exists_supported(),
    );
    if !node_modules_folder_exists && state.trace_enabled {
        trace(
            state.host,
            &Diagnostics::Directory_0_does_not_exist_skipping_all_lookups_in_it,
            Some(vec![node_modules_folder.clone()]),
        );
    }

    let package_result = if types_scope_only {
        None
    } else {
        load_module_from_specific_node_modules_directory(
            extensions,
            module_name,
            &node_modules_folder,
            node_modules_folder_exists,
            state,
            cache.clone(),
            redirected_reference.clone(),
        )
    };
    if package_result.is_some() {
        return package_result;
    }
    if matches!(extensions, Extensions::TypeScript | Extensions::DtsOnly) {
        let node_modules_at_types_ = combine_paths(&node_modules_folder, &[Some("@types")]);
        let mut node_modules_at_types_exists = node_modules_folder_exists;
        if node_modules_folder_exists
            && !directory_probably_exists(
                &node_modules_at_types_,
                |directory_name| state.host.directory_exists(directory_name),
                || state.host.is_directory_exists_supported(),
            )
        {
            if state.trace_enabled {
                trace(
                    state.host,
                    &Diagnostics::Directory_0_does_not_exist_skipping_all_lookups_in_it,
                    Some(vec![node_modules_at_types_.clone()]),
                );
            }
            node_modules_at_types_exists = false;
        }
        return load_module_from_specific_node_modules_directory(
            Extensions::DtsOnly,
            &mangle_scoped_package_name_with_trace(module_name, state),
            &node_modules_at_types_,
            node_modules_at_types_exists,
            state,
            cache,
            redirected_reference,
        );
    }
    None
}

fn load_module_from_specific_node_modules_directory(
    extensions: Extensions,
    module_name: &str,
    node_modules_directory: &str,
    node_modules_directory_exists: bool,
    state: &ModuleResolutionState,
    cache: Option<Gc<ModuleResolutionCache>>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
) -> Option<Resolved> {
    let candidate = normalize_path(&combine_paths(node_modules_directory, &[Some(module_name)]));

    let package_info = Rc::new(RefCell::new(get_package_json_info(
        &candidate,
        !node_modules_directory_exists,
        state,
    )));
    if !state.features.intersects(NodeResolutionFeatures::Exports) {
        if let Some(package_info) = (*package_info).borrow().as_ref() {
            let from_file = load_module_from_file(
                extensions,
                &candidate,
                !node_modules_directory_exists,
                state,
            );
            if from_file.is_some() {
                return no_package_id(from_file.as_ref());
            }

            let from_directory = load_node_module_from_directory_worker(
                extensions,
                &candidate,
                !node_modules_directory_exists,
                state,
                Some(&package_info.package_json_content),
                package_info.version_paths.as_deref(),
            );
            return with_package_id(Some(&**package_info), from_directory.as_ref());
        }
    }

    let ParsedPackageName { package_name, rest } = parse_package_name(module_name);
    let loader: ResolutionKindSpecificLoader = Rc::new({
        let package_info = package_info.clone();
        let rest = rest.clone();
        let redirected_reference = redirected_reference.clone();
        let cache = cache.clone();
        move |extensions, candidate, only_record_failures, state| {
            if let Some(package_info) = (*package_info).borrow().as_ref().filter(|package_info| {
                matches!(
                    package_info.package_json_content.as_object(),
                    Some(package_info_package_json_content) if package_info_package_json_content.get("exports").and_then(|package_info_package_json_content_exports| {
                        package_info_package_json_content_exports.as_object()
                    }).is_some()
                ) && state.features.intersects(NodeResolutionFeatures::Exports)
            }) {
                return load_module_from_exports(
                    package_info,
                    extensions,
                    &combine_paths(
                        ".",
                        &[Some(&rest)],
                    ),
                    state,
                    cache.clone(),
                    redirected_reference.clone(),
                ).and_then(|search_result| search_result.value);
            }
            let path_and_extension =
                load_module_from_file(extensions, candidate, only_record_failures, state).or_else(
                    || {
                        load_node_module_from_directory_worker(
                            extensions,
                            candidate,
                            only_record_failures,
                            state,
                            (*package_info)
                                .borrow()
                                .as_ref()
                                .map(|package_info| &*package_info.package_json_content),
                            (*package_info)
                                .borrow()
                                .as_ref()
                                .and_then(|package_info| package_info.version_paths.clone())
                                .as_deref(),
                        )
                    },
                );
            with_package_id(
                (*package_info).borrow().as_deref(),
                path_and_extension.as_ref(),
            )
        }
    });

    if !rest.is_empty() {
        let package_directory = combine_paths(node_modules_directory, &[Some(&*package_name)]);

        *package_info.borrow_mut() =
            get_package_json_info(&package_directory, !node_modules_directory_exists, state);
        if let Some(package_info_version_paths) = (*package_info)
            .borrow()
            .as_ref()
            .and_then(|package_info| package_info.version_paths.clone())
            .as_ref()
        {
            if state.trace_enabled {
                trace(
                    state.host,
                    &Diagnostics::package_json_has_a_typesVersions_entry_0_that_matches_compiler_version_1_looking_for_a_pattern_to_match_module_name_2,
                    Some(vec![
                        package_info_version_paths.version.clone(),
                        version.to_owned(),
                        rest.clone(),
                    ])
                );
            }
            let package_directory_exists = node_modules_directory_exists
                && directory_probably_exists(
                    &package_directory,
                    |directory_name| state.host.directory_exists(directory_name),
                    || state.host.is_directory_exists_supported(),
                );
            let from_paths = try_load_module_using_paths(
                extensions,
                &rest,
                &package_directory,
                &version_paths_paths_to_map_like(&package_info_version_paths.paths),
                None,
                loader.clone(),
                !package_directory_exists,
                state,
            );
            if let Some(from_paths) = from_paths {
                return from_paths.value;
            }
        }
    }

    loader(
        extensions,
        &candidate,
        !node_modules_directory_exists,
        state,
    )
}

mod _PackageJsonInfoOrBoolDeriveTraceScope {
    use super::*;
    use local_macros::Trace;

    #[derive(Clone, Trace, Finalize)]
    pub enum PackageJsonInfoOrBool {
        PackageJsonInfo(Gc<PackageJsonInfo>),
        Bool(bool),
    }
}
pub use _PackageJsonInfoOrBoolDeriveTraceScope::PackageJsonInfoOrBool;

impl From<Gc<PackageJsonInfo>> for PackageJsonInfoOrBool {
    fn from(value: Gc<PackageJsonInfo>) -> Self {
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
    paths: &MapLike<Vec<String>>,
    path_patterns: Option<&[StringOrPattern]>,
    loader: ResolutionKindSpecificLoader,
    only_record_failures: bool,
    state: &ModuleResolutionState,
) -> SearchResult<Resolved> {
    let path_patterns = path_patterns
        .map(ToOwned::to_owned)
        .unwrap_or_else(|| try_parse_patterns(paths));
    let matched_pattern = match_pattern_or_exact(&path_patterns, module_name);
    if let Some(matched_pattern) = matched_pattern {
        let matched_star = match &matched_pattern {
            StringOrPattern::String(_) => None,
            StringOrPattern::Pattern(matched_pattern) => {
                Some(matched_text(matched_pattern, module_name))
            }
        };
        let matched_pattern_text = match matched_pattern {
            StringOrPattern::String(matched_pattern) => matched_pattern,
            StringOrPattern::Pattern(matched_pattern) => pattern_text(&matched_pattern),
        };
        if state.trace_enabled {
            trace(
                state.host,
                &Diagnostics::Module_name_0_matched_pattern_1,
                Some(vec![module_name.to_owned(), matched_pattern_text.clone()]),
            );
        }
        let resolved = maybe_for_each(paths.get(&matched_pattern_text), |subst: &String, _| {
            let path = if let Some(matched_star) = matched_star.as_ref() {
                subst.replace("*", matched_star)
            } else {
                subst.clone()
            };
            let candidate = normalize_path(&combine_paths(base_directory, &[Some(&path)]));
            if state.trace_enabled {
                trace(
                    state.host,
                    &Diagnostics::Trying_substitution_0_candidate_module_location_Colon_1,
                    Some(vec![subst.clone(), path.clone()]),
                );
            }
            let extension = try_get_extension_from_path(subst);
            if let Some(extension) = extension {
                let path = try_file(&candidate, only_record_failures, state);
                if let Some(path) = path {
                    return no_package_id(Some(&PathAndExtension {
                        path,
                        ext: extension,
                    }));
                }
            }
            loader(
                extensions,
                &candidate,
                only_record_failures
                    || !directory_probably_exists(
                        &get_directory_path(&candidate),
                        |directory_name: &str| state.host.directory_exists(directory_name),
                        || state.host.is_directory_exists_supported(),
                    ),
                state,
            )
        });
        return Some(SearchResultPresent { value: resolved });
    }
    None
}

static mangled_scoped_package_separator: &str = "__";

pub(crate) fn mangle_scoped_package_name_with_trace(
    package_name: &str,
    state: &ModuleResolutionState,
) -> String {
    let mangled = mangle_scoped_package_name(package_name);
    if state.trace_enabled && mangled != package_name {
        trace(
            state.host,
            &Diagnostics::Scoped_package_detected_looking_in_0,
            Some(vec![mangled.clone()]),
        );
    }
    mangled
}

pub(crate) fn get_types_package_name(package_name: &str) -> String {
    format!("@types/{}", mangle_scoped_package_name(package_name))
}

pub(crate) fn mangle_scoped_package_name(package_name: &str) -> String {
    if starts_with(package_name, "@") {
        let replace_slash =
            package_name.replace(directory_separator_str, mangled_scoped_package_separator);
        if replace_slash != package_name {
            return replace_slash[1..].to_owned();
        }
    }
    package_name.to_owned()
}

pub(crate) fn get_package_name_from_types_package_name<'arg>(
    mangled_name: &'arg str,
) -> Cow<'arg, str> {
    let without_at_type_prefix = remove_prefix(mangled_name, "@types");
    if without_at_type_prefix != mangled_name {
        return unmangle_scoped_package_name(without_at_type_prefix);
    }
    mangled_name.into()
}

pub(crate) fn unmangle_scoped_package_name<'arg>(types_package_name: &'arg str) -> Cow<'arg, str> {
    if string_contains(types_package_name, mangled_scoped_package_separator) {
        format!(
            "@{}",
            types_package_name.replace(mangled_scoped_package_separator, directory_separator_str)
        )
        .into()
    } else {
        types_package_name.into()
    }
}

fn try_find_non_relative_module_name_in_cache(
    cache: Option<&PerModuleNameCache>,
    module_name: &str,
    containing_directory: &str,
    state: &ModuleResolutionState,
) -> SearchResult<Resolved> {
    let result = cache.and_then(|cache| cache.get(containing_directory))?;
    if state.trace_enabled {
        trace(
            state.host,
            &Diagnostics::Resolution_for_module_0_was_found_in_cache_from_location_1,
            Some(vec![
                module_name.to_owned(),
                containing_directory.to_owned(),
            ]),
        );
    }
    *state.result_from_cache.borrow_mut() = Some(result.clone());
    Some(SearchResultPresent {
        value: result
            .resolved_module
            .clone()
            .map(|result_resolved_module| Resolved {
                path: result_resolved_module.resolved_file_name.clone(),
                original_path: Some(result_resolved_module.original_path.clone().map_or_else(
                    || true.into(),
                    |result_resolved_module_original_path| {
                        result_resolved_module_original_path.into()
                    },
                )),
                extension: result_resolved_module.extension(),
                package_id: result_resolved_module.package_id.clone(),
            }),
    })
}

pub fn classic_name_resolver<TCache: NonRelativeModuleNameResolutionCache>(
    module_name: &str,
    containing_file: &str,
    compiler_options: Gc<CompilerOptions>,
    host: &dyn ModuleResolutionHost,
    cache: Option<&TCache>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
) -> Gc<ResolvedModuleWithFailedLookupLocations> {
    let trace_enabled = is_trace_enabled(&compiler_options, host);
    let failed_lookup_locations: RefCell<Vec<String>> = RefCell::new(vec![]);
    let state = ModuleResolutionState {
        compiler_options: compiler_options.clone(),
        host,
        trace_enabled,
        failed_lookup_locations,
        package_json_info_cache: cache.map(|cache| cache.as_dyn_package_json_info_cache()),
        features: NodeResolutionFeatures::None,
        conditions: vec![],
        result_from_cache: RefCell::new(None),
    };
    let ref containing_directory = get_directory_path(containing_file);

    let resolved = classic_name_resolver_try_resolve(
        module_name,
        containing_directory,
        &state,
        cache,
        redirected_reference.clone(),
        Extensions::TypeScript,
    )
    .or_else(|| {
        classic_name_resolver_try_resolve(
            module_name,
            containing_directory,
            &state,
            cache,
            redirected_reference.clone(),
            Extensions::JavaScript,
        )
    });
    let ModuleResolutionState {
        failed_lookup_locations: state_failed_lookup_locations,
        result_from_cache: state_result_from_cache,
        ..
    } = state;
    create_resolved_module_with_failed_lookup_locations(
        resolved.and_then(|resolved| resolved.value),
        Some(false),
        state_failed_lookup_locations.into_inner(),
        state_result_from_cache.into_inner(),
    )
}

fn classic_name_resolver_try_resolve<TCache: NonRelativeModuleNameResolutionCache>(
    module_name: &str,
    containing_directory: &str,
    state: &ModuleResolutionState,
    cache: Option<&TCache>,
    redirected_reference: Option<Gc<ResolvedProjectReference>>,
    extensions: Extensions,
) -> SearchResult<Resolved> {
    let resolved_using_settings = try_load_module_using_optional_resolution_settings(
        extensions,
        module_name,
        containing_directory,
        Rc::new(load_module_from_file_no_package_id),
        state,
    );
    if let Some(resolved_using_settings) = resolved_using_settings {
        return Some(SearchResultPresent {
            value: Some(resolved_using_settings),
        });
    }

    if !is_external_module_name_relative(module_name) {
        let per_module_name_cache = cache.map(|cache| {
            cache.get_or_create_cache_for_module_name(
                module_name,
                None,
                redirected_reference.clone(),
            )
        });
        let resolved = for_each_ancestor_directory(
            &containing_directory.to_owned().into(),
            |directory: &Path| {
                let resolution_from_cache = try_find_non_relative_module_name_in_cache(
                    per_module_name_cache.as_deref(),
                    module_name,
                    directory,
                    state,
                );
                if resolution_from_cache.is_some() {
                    return resolution_from_cache;
                }
                let ref search_name =
                    normalize_path(&combine_paths(directory, &[Some(module_name)]));
                to_search_result(load_module_from_file_no_package_id(
                    extensions,
                    search_name,
                    false,
                    state,
                ))
            },
        );
        if resolved.is_some() {
            return resolved;
        }
        if extensions == Extensions::TypeScript {
            return load_module_from_nearest_node_modules_directory_types_scope(
                module_name,
                containing_directory,
                state,
            );
        }
    } else {
        let ref candidate =
            normalize_path(&combine_paths(containing_directory, &[Some(module_name)]));
        return to_search_result(load_module_from_file_no_package_id(
            extensions, candidate, false, state,
        ));
    }
    None
}

type SearchResult<TValue> = Option<SearchResultPresent<TValue>>;

struct SearchResultPresent<TValue> {
    pub value: Option<TValue>,
}

fn to_search_result<TValue>(value: Option<TValue>) -> SearchResult<TValue> {
    value.map(|value| SearchResultPresent { value: Some(value) })
}
