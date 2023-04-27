#![cfg(test)]

use std::{collections::HashMap, io, rc::Rc};

use derive_builder::Builder;
use gc::{Finalize, Gc, Trace};
use itertools::Itertools;
use speculoos::prelude::*;
use typescript_rust::{
    extension_from_path, get_directory_path, ModuleResolutionHost, ModuleResolutionHostOverrider,
    ResolvedModuleFull, ResolvedModuleFullBuilder, ResolvedModuleWithFailedLookupLocations,
};

pub fn check_resolved_module(
    actual: Option<&ResolvedModuleFull>,
    expected: Option<&ResolvedModuleFull>,
) -> bool {
    if expected.is_none() {
        if let Some(actual) = actual {
            panic!(
                "expected resolved module to be undefined, actual: {:?}, expected: {:?}",
                actual, expected,
            );
            // return false;
        }
        return true;
    } else if actual.is_none() {
        panic!(
            "expected resolved module to be defined, actual: {:?}, expected: {:?}",
            actual, expected,
        );
        // return false;
    }
    let actual = actual.unwrap();
    let expected = expected.unwrap();

    asserting(&format!(
        "'resolvedFileName': expected '{}' to be equal to '{}'",
        actual.resolved_file_name, expected.resolved_file_name,
    ))
    .that(&actual.resolved_file_name)
    .is_equal_to(&expected.resolved_file_name);
    asserting(&format!(
        "'ext': expected '{:?}' to be equal to '{:?}'",
        actual.extension, expected.extension,
    ))
    .that(&actual.extension)
    .is_equal_to(expected.extension);
    asserting(&format!(
        "'isExternalLibraryImport': expected '{:?}' to be equal to '{:?}'",
        actual.is_external_library_import, expected.is_external_library_import,
    ))
    .that(&actual.is_external_library_import)
    .is_equal_to(expected.is_external_library_import);
    true
}

pub fn check_resolved_module_with_failed_lookup_locations(
    actual: &ResolvedModuleWithFailedLookupLocations,
    expected_resolved_module: &ResolvedModuleFull,
    expected_failed_lookup_locations: &[&str],
) {
    asserting("module should be resolved")
        .that(&actual.resolved_module)
        .is_some();
    check_resolved_module(
        actual.resolved_module.as_deref(),
        Some(expected_resolved_module),
    );
    asserting(&format!(
        "Failed lookup locations should match - expected has {}, actual has {}",
        expected_failed_lookup_locations.len(),
        actual.failed_lookup_locations().len(),
    ))
    .that(&*actual.failed_lookup_locations())
    .is_equal_to(
        expected_failed_lookup_locations
            .into_iter()
            .map(|value| (*value).to_owned())
            .collect_vec(),
    );
}

pub fn create_resolved_module(
    resolved_file_name: &str,
    is_external_library_import: Option<bool>,
) -> ResolvedModuleFull {
    let is_external_library_import = is_external_library_import.unwrap_or(false);
    ResolvedModuleFullBuilder::default()
        .resolved_file_name(resolved_file_name)
        .extension(extension_from_path(resolved_file_name))
        .is_external_library_import(is_external_library_import)
        .build()
        .unwrap()
}

#[derive(Builder, Clone)]
#[builder(setter(into, strip_option))]
struct File {
    pub name: String,
    #[builder(default)]
    pub content: Option<String>,
    #[builder(default)]
    pub symlinks: Option<Vec<String>>,
}

fn create_module_resolution_host(
    has_directory_exists: bool,
    files: Vec<File>,
) -> Rc<dyn ModuleResolutionHost> {
    let mut map: HashMap<String, File> = Default::default();
    for file in &files {
        map.insert(file.name.clone(), file.clone());
        if let Some(file_symlinks) = file.symlinks.as_ref() {
            for symlink in file_symlinks {
                map.insert(symlink.clone(), file.clone());
            }
        }
    }

    if has_directory_exists {
        let mut directories: HashMap<String, String> = Default::default();
        for f in &files {
            let mut name = get_directory_path(&f.name);
            loop {
                directories.insert(name.clone(), name.clone());
                let base_name = get_directory_path(&name);
                if base_name == name {
                    break;
                }
                name = base_name;
            }
        }
        Rc::new(CreateModuleResolutionHostHasDirectoryExists::new(
            map,
            directories,
        ))
    } else {
        Rc::new(CreateModuleResolutionHostNoHasDirectoryExists::new(map))
    }
}

#[derive(Trace, Finalize)]
struct CreateModuleResolutionHostHasDirectoryExists {
    #[unsafe_ignore_trace]
    map: HashMap<String, File>,
    #[unsafe_ignore_trace]
    directories: HashMap<String, String>,
}

impl CreateModuleResolutionHostHasDirectoryExists {
    fn new(map: HashMap<String, File>, directories: HashMap<String, String>) -> Self {
        Self { map, directories }
    }
}

impl ModuleResolutionHost for CreateModuleResolutionHostHasDirectoryExists {
    fn read_file(&self, path: &str) -> io::Result<Option<String>> {
        let file = self.map.get(path);
        Ok(file.and_then(|file| file.content.clone()))
    }

    fn realpath(&self, path: &str) -> Option<String> {
        Some(self.map.get(path).unwrap().name.clone())
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        Some(self.directories.contains_key(path))
    }

    fn file_exists(&self, path: &str) -> bool {
        asserting(&format!(
            "'fileExists' '{path}' request in non-existing directory"
        ))
        .that(&self.directories)
        .contains_key(&get_directory_path(path));
        self.map.contains_key(path)
    }

    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        Some(true)
    }

    fn file_exists_non_overridden(&self, _file_name: &str) -> bool {
        unreachable!()
    }

    fn set_overriding_file_exists(
        &self,
        _overriding_file_exists: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn set_overriding_read_file(
        &self,
        _overriding_read_file: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn read_file_non_overridden(&self, _file_name: &str) -> io::Result<Option<String>> {
        unreachable!()
    }

    fn is_trace_supported(&self) -> bool {
        false
    }

    fn is_directory_exists_supported(&self) -> bool {
        true
    }

    fn set_overriding_directory_exists(
        &self,
        _overriding_directory_exists: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_realpath_supported(&self) -> bool {
        true
    }

    fn set_overriding_realpath(
        &self,
        _overriding_realpath: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_get_directories_supported(&self) -> bool {
        false
    }

    fn set_overriding_get_directories(
        &self,
        _overriding_get_directories: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }
}

#[derive(Trace, Finalize)]
struct CreateModuleResolutionHostNoHasDirectoryExists {
    #[unsafe_ignore_trace]
    map: HashMap<String, File>,
}

impl CreateModuleResolutionHostNoHasDirectoryExists {
    fn new(map: HashMap<String, File>) -> Self {
        Self { map }
    }
}

impl ModuleResolutionHost for CreateModuleResolutionHostNoHasDirectoryExists {
    fn read_file(&self, path: &str) -> io::Result<Option<String>> {
        let file = self.map.get(path);
        Ok(file.and_then(|file| file.content.clone()))
    }

    fn realpath(&self, path: &str) -> Option<String> {
        Some(self.map.get(path).unwrap().name.clone())
    }

    fn file_exists(&self, path: &str) -> bool {
        self.map.contains_key(path)
    }

    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        Some(true)
    }

    fn file_exists_non_overridden(&self, _file_name: &str) -> bool {
        unreachable!()
    }

    fn set_overriding_file_exists(
        &self,
        _overriding_file_exists: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn set_overriding_read_file(
        &self,
        _overriding_read_file: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn read_file_non_overridden(&self, _file_name: &str) -> io::Result<Option<String>> {
        unreachable!()
    }

    fn is_trace_supported(&self) -> bool {
        false
    }

    fn is_directory_exists_supported(&self) -> bool {
        false
    }

    fn set_overriding_directory_exists(
        &self,
        _overriding_directory_exists: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_realpath_supported(&self) -> bool {
        true
    }

    fn set_overriding_realpath(
        &self,
        _overriding_realpath: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_get_directories_supported(&self) -> bool {
        false
    }

    fn set_overriding_get_directories(
        &self,
        _overriding_get_directories: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }
}

mod node_module_resolution_relative_paths {
    use once_cell::sync::Lazy;
    use typescript_rust::{
        combine_paths, get_root_length, node_module_name_resolver, normalize_path,
        supported_ts_extensions, supported_ts_extensions_flat, Extension,
    };

    use super::*;

    static non_implicit_extensions: Lazy<Vec<Extension>> = Lazy::new(|| {
        vec![
            Extension::Mts,
            Extension::Dmts,
            Extension::Mjs,
            Extension::Cts,
            Extension::Dcts,
            Extension::Cjs,
        ]
    });

    static auto_extensions: Lazy<Vec<Extension>> = Lazy::new(|| {
        supported_ts_extensions_flat
            .iter()
            .copied()
            .filter(|e| !non_implicit_extensions.contains(e))
            .collect()
    });

    fn test_load_as_file(
        containing_file_name: &str,
        module_file_name_no_ext: &str,
        module_name: &str,
    ) {
        let test = |ext: &Extension, has_directory_exists: bool| {
            let containing_file = FileBuilder::default()
                .name(containing_file_name)
                .build()
                .unwrap();
            let module_file = FileBuilder::default()
                .name(format!("{module_file_name_no_ext}{}", ext.to_str()))
                .build()
                .unwrap();
            let resolution = node_module_name_resolver(
                module_name,
                &containing_file.name,
                Default::default(),
                &*create_module_resolution_host(
                    has_directory_exists,
                    vec![containing_file.clone(), module_file.clone()],
                ),
                None,
                None,
                None,
            );
            check_resolved_module(
                resolution.resolved_module.as_deref(),
                Some(&create_resolved_module(&module_file.name, None)),
            );

            let mut failed_lookup_locations: Vec<String> = Default::default();
            let dir = get_directory_path(containing_file_name);
            for e in &*auto_extensions {
                if e == ext {
                    break;
                } else {
                    failed_lookup_locations.push(format!(
                        "{}{}",
                        normalize_path(&if get_root_length(module_name) == 0 {
                            combine_paths(&dir, &[Some(module_name)])
                        } else {
                            module_name.to_owned()
                        }),
                        e.to_str()
                    ));
                }
            }

            assert_that(&*resolution.failed_lookup_locations())
                .is_equal_to(failed_lookup_locations);
        };

        for ext in &*auto_extensions {
            test(ext, false);
            test(ext, true);
        }
    }

    #[test]
    fn test_module_name_that_starts_with_dot_slash_resolved_as_relative_file_name() {
        test_load_as_file("/foo/bar/baz.ts", "/foo/bar/foo", "./foo");
    }

    #[test]
    fn test_module_name_that_starts_with_dot_dot_slash_resolved_as_relative_file_name() {
        test_load_as_file("/foo/bar/baz.ts", "/foo/foo", "../foo");
    }

    #[test]
    fn test_module_name_that_starts_with_slash_script_extension_resolved_as_relative_file_name() {
        test_load_as_file("/foo/bar/baz.ts", "/foo", "/foo");
    }

    #[test]
    fn test_module_name_that_starts_with_c_colon_slash_script_extension_resolved_as_relative_file_name(
    ) {
        test_load_as_file("c:/foo/bar/baz.ts", "c:/foo", "c:/foo");
    }

    fn test_loading_from_package_json(
        containing_file_name: &str,
        package_json_file_name: &str,
        field_ref: &str,
        module_file_name: &str,
        module_name: &str,
    ) {
        let test = |has_directory_exists: bool| {
            let containing_file = FileBuilder::default()
                .name(containing_file_name)
                .build()
                .unwrap();
            let package_json = FileBuilder::default()
                .name(package_json_file_name)
                .content(
                    serde_json::to_string::<HashMap<String, String>>(&HashMap::from_iter([(
                        "typings".to_owned(),
                        field_ref.to_owned(),
                    )]))
                    .unwrap(),
                )
                .build()
                .unwrap();
            let module_file = FileBuilder::default()
                .name(module_file_name)
                .build()
                .unwrap();
            let resolution = node_module_name_resolver(
                module_name,
                &containing_file.name,
                Default::default(),
                &*create_module_resolution_host(
                    has_directory_exists,
                    vec![
                        containing_file.clone(),
                        package_json.clone(),
                        module_file.clone(),
                    ],
                ),
                None,
                None,
                None,
            );
            check_resolved_module(
                resolution.resolved_module.as_deref(),
                Some(&create_resolved_module(&module_file.name, None)),
            );
            assert_that(&*resolution.failed_lookup_locations())
                .has_length(supported_ts_extensions[0].len());
        };

        test(false);
        test(true);
    }

    #[test]
    fn test_module_name_as_directory_load_from_typings() {
        test_loading_from_package_json(
            "/a/b/c/d.ts",
            "/a/b/c/bar/package.json",
            "c/d/e.d.ts",
            "/a/b/c/bar/c/d/e.d.ts",
            "./bar",
        );
        test_loading_from_package_json(
            "/a/b/c/d.ts",
            "/a/bar/package.json",
            "e.d.ts",
            "/a/bar/e.d.ts",
            "../../bar",
        );
        test_loading_from_package_json(
            "/a/b/c/d.ts",
            "/bar/package.json",
            "e.d.ts",
            "/bar/e.d.ts",
            "/bar",
        );
        test_loading_from_package_json(
            "c:/a/b/c/d.ts",
            "c:/bar/package.json",
            "e.d.ts",
            "c:/bar/e.d.ts",
            "c:/bar",
        );
    }

    fn test_typings_ignored(typings: impl Into<Option<serde_json::Value>>) {
        let typings = typings.into();
        let test = |has_directory_exists: bool| {
            let containing_file = FileBuilder::default().name("/a/b.ts").build().unwrap();
            let package_json = FileBuilder::default()
                .name("/node_modules/b/package.json")
                .content(
                    match &typings {
                        Some(typings) => serde_json::json!({
                            "typings": typings,
                        }),
                        None => serde_json::json!({}),
                    }
                    .to_string(),
                )
                .build()
                .unwrap();
            let module_file = FileBuilder::default().name("/a/b.d.ts").build().unwrap();

            let index_path = "/node_modules/b/index.d.ts";
            let index_file = FileBuilder::default().name(index_path).build().unwrap();

            let resolution = node_module_name_resolver(
                "b",
                &containing_file.name,
                Default::default(),
                &*create_module_resolution_host(
                    has_directory_exists,
                    vec![
                        containing_file.clone(),
                        package_json.clone(),
                        module_file.clone(),
                        index_file.clone(),
                    ],
                ),
                None,
                None,
                None,
            );

            check_resolved_module(
                resolution.resolved_module.as_deref(),
                Some(&create_resolved_module(index_path, Some(true))),
            );
        };

        test(false);
        test(true);
    }

    #[test]
    fn test_module_name_as_directory_handle_invalid_typings() {
        test_typings_ignored(serde_json::json!(["a", "b"]));
        test_typings_ignored(serde_json::json!({"a": "b"}));
        test_typings_ignored(serde_json::json!(true));
        test_typings_ignored(serde_json::json!(null));
        test_typings_ignored(None);
    }

    #[test]
    fn test_module_name_as_directory_load_index_d_ts() {
        let test = |has_directory_exists: bool| {
            let containing_file = FileBuilder::default().name("/a/b/c.ts").build().unwrap();
            let package_json = FileBuilder::default()
                .name("/a/b/foo/package.json")
                .content(
                    serde_json::json!({
                        "main": "/c/d"
                    })
                    .to_string(),
                )
                .build()
                .unwrap();
            let index_file = FileBuilder::default()
                .name("/a/b/foo/index.d.ts")
                .build()
                .unwrap();
            let resolution = node_module_name_resolver(
                "./foo",
                &containing_file.name,
                Default::default(),
                &*create_module_resolution_host(
                    has_directory_exists,
                    vec![
                        containing_file.clone(),
                        package_json.clone(),
                        index_file.clone(),
                    ],
                ),
                None,
                None,
                None,
            );
            check_resolved_module_with_failed_lookup_locations(
                &resolution,
                &create_resolved_module(&index_file.name, None),
                &[
                    "/a/b/foo.ts",
                    "/a/b/foo.tsx",
                    "/a/b/foo.d.ts",
                    "/c/d",
                    "/c/d.ts",
                    "/c/d.tsx",
                    "/c/d.d.ts",
                    "/c/d/index.ts",
                    "/c/d/index.tsx",
                    "/c/d/index.d.ts",
                    "/a/b/foo/index.ts",
                    "/a/b/foo/index.tsx",
                ],
            );
        };

        test(false);
        test(true);
    }
}
