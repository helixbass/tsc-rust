#![cfg(test)]

use std::{collections::HashMap, io, rc::Rc};

use derive_builder::Builder;
use gc::{Finalize, Trace};
use harness::AllArenasHarness;
use itertools::Itertools;
use speculoos::prelude::*;
use typescript_rust::{
    extension_from_path, get_directory_path, id_arena::Id, HasArena, InArena, ModuleResolutionHost,
    ModuleResolutionHostOverrider, ResolvedModuleFull, ResolvedModuleFullBuilder,
    ResolvedModuleWithFailedLookupLocations,
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
        _overriding_file_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn set_overriding_read_file(
        &self,
        _overriding_read_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
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
        _overriding_directory_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_realpath_supported(&self) -> bool {
        true
    }

    fn set_overriding_realpath(
        &self,
        _overriding_realpath: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_get_directories_supported(&self) -> bool {
        false
    }

    fn set_overriding_get_directories(
        &self,
        _overriding_get_directories: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
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
        _overriding_file_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn set_overriding_read_file(
        &self,
        _overriding_read_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
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
        _overriding_directory_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_realpath_supported(&self) -> bool {
        true
    }

    fn set_overriding_realpath(
        &self,
        _overriding_realpath: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_get_directories_supported(&self) -> bool {
        false
    }

    fn set_overriding_get_directories(
        &self,
        _overriding_get_directories: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }
}

mod node_module_resolution_relative_paths {
    use once_cell::sync::Lazy;
    use typescript_rust::{
        combine_paths, get_root_length, node_module_name_resolver, normalize_path,
        supported_ts_extensions, supported_ts_extensions_flat, Extension, Owned,
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
            )
            .unwrap();
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
                    serde_json::to_string::<HashMap<String, String>>(&HashMap::from_iter(
                        [("typings", field_ref)].owned(),
                    ))
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
            )
            .unwrap();
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
            )
            .unwrap();

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
            )
            .unwrap();
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

mod node_module_resolution_non_relative_paths {
    use typescript_rust::{
        create_module_resolution_cache, node_module_name_resolver, resolve_module_name,
        CompilerOptionsBuilder, Extension, GetCanonicalFileName, ModuleResolutionKind,
        NonRelativeModuleNameResolutionCache, Owned,
    };

    use super::*;

    #[test]
    fn test_computes_correct_common_prefix_for_module_name_cache() {
        let ref arena = AllArenasHarness::default();
        let resolution_cache = create_module_resolution_cache(
            "/",
            arena.alloc_get_canonical_file_name(Box::new(GetCanonicalFileNameNoop)),
            None,
            None,
            None,
            arena,
        );
        let mut cache = resolution_cache.get_or_create_cache_for_module_name("a", None, None);
        cache.ref_(arena).set(
            "/sub",
            arena.alloc_resolved_module_with_failed_lookup_locations(
                ResolvedModuleWithFailedLookupLocations::new(
                    Some(
                        arena.alloc_resolved_module_full(
                            ResolvedModuleFullBuilder::default()
                                .resolved_file_name("/sub/node_modules/a/index.ts")
                                .is_external_library_import(true)
                                .extension(Extension::Ts)
                                .build()
                                .unwrap(),
                        ),
                    ),
                    Default::default(),
                ),
            ),
        );
        assert_that(&cache.get("/sub")).is_some();
        assert_that(&cache.get("/")).is_none();

        cache = resolution_cache.get_or_create_cache_for_module_name("b", None, None);
        cache.set(
            "/sub/dir/foo",
            arena.alloc_resolved_module_with_failed_lookup_locations(
                ResolvedModuleWithFailedLookupLocations::new(
                    Some(
                        arena.alloc_resolved_module_full(
                            ResolvedModuleFullBuilder::default()
                                .resolved_file_name("/sub/directory/node_modules/b/index.ts")
                                .is_external_library_import(true)
                                .extension(Extension::Ts)
                                .build()
                                .unwrap(),
                        ),
                    ),
                    Default::default(),
                ),
            ),
        );
        assert_that(&cache.get("/sub/dir/foo")).is_some();
        assert_that(&cache.get("/sub/dir")).is_some();
        assert_that(&cache.get("/sub")).is_some();
        assert_that(&cache.get("/")).is_none();

        cache = resolution_cache.get_or_create_cache_for_module_name("c", None, None);
        cache.set(
            "/foo/bar",
            arena.alloc_resolved_module_with_failed_lookup_locations(
                ResolvedModuleWithFailedLookupLocations::new(
                    Some(
                        arena.alloc_resolved_module_full(
                            ResolvedModuleFullBuilder::default()
                                .resolved_file_name("/bar/node_modules/c/index.ts")
                                .is_external_library_import(true)
                                .extension(Extension::Ts)
                                .build()
                                .unwrap(),
                        ),
                    ),
                    Default::default(),
                ),
            ),
        );
        assert_that(&cache.get("/foo/bar")).is_some();
        assert_that(&cache.get("/foo")).is_some();
        assert_that(&cache.get("/")).is_some();

        cache = resolution_cache.get_or_create_cache_for_module_name("d", None, None);
        cache.ref_(arena).set(
            "/foo",
            arena.alloc_resolved_module_with_failed_lookup_locations(
                ResolvedModuleWithFailedLookupLocations::new(
                    Some(
                        arena.alloc_resolved_module_full(
                            ResolvedModuleFullBuilder::default()
                                .resolved_file_name("/foo/index.ts")
                                .is_external_library_import(true)
                                .extension(Extension::Ts)
                                .build()
                                .unwrap(),
                        ),
                    ),
                    Default::default(),
                ),
            ),
        );
        assert_that(&cache.get("/foo")).is_some();
        assert_that(&cache.get("/")).is_none();

        cache = resolution_cache.get_or_create_cache_for_module_name("e", None, None);
        cache.ref_(arena).set(
            "c:/foo",
            arena.alloc_resolved_module_with_failed_lookup_locations(
                ResolvedModuleWithFailedLookupLocations::new(
                    Some(
                        arena.alloc_resolved_module_full(
                            ResolvedModuleFullBuilder::default()
                                .resolved_file_name("d:/bar/node_modules/e/index.ts")
                                .is_external_library_import(true)
                                .extension(Extension::Ts)
                                .build()
                                .unwrap(),
                        ),
                    ),
                    Default::default(),
                ),
            ),
        );
        assert_that(&cache.get("c:/foo")).is_some();
        assert_that(&cache.get("c:/")).is_some();
        assert_that(&cache.get("d:/")).is_none();

        cache = resolution_cache.get_or_create_cache_for_module_name("f", None, None);
        cache.ref_(arena).set(
            "/foo/bar/baz",
            arena.alloc_resolved_module_with_failed_lookup_locations(
                ResolvedModuleWithFailedLookupLocations::new(None, Default::default()),
            ),
        );
        assert_that(&cache.get("/foo/bar/baz")).is_some();
        assert_that(&cache.get("/foo/bar")).is_some();
        assert_that(&cache.get("/foo")).is_some();
        assert_that!(&cache.get("/")).is_some();
    }

    #[derive(Trace, Finalize)]
    struct GetCanonicalFileNameNoop;

    impl GetCanonicalFileName for GetCanonicalFileNameNoop {
        fn call(&self, file_name: &str) -> String {
            file_name.to_owned()
        }
    }

    #[test]
    fn test_load_module_as_file_ts_files_not_loaded() {
        let test = |has_directory_exists: bool| {
            let containing_file = FileBuilder::default()
                .name("/a/b/c/d/e.ts")
                .build()
                .unwrap();
            let module_file = FileBuilder::default()
                .name("/a/b/node_modules/foo.ts")
                .build()
                .unwrap();
            let resolution = node_module_name_resolver(
                "foo",
                &containing_file.name,
                Default::default(),
                &*create_module_resolution_host(
                    has_directory_exists,
                    vec![containing_file.clone(), module_file.clone()],
                ),
                None,
                None,
                None,
            )
            .unwrap();
            check_resolved_module_with_failed_lookup_locations(
                &resolution,
                &create_resolved_module(&module_file.name, Some(true)),
                &[
                    "/a/b/c/d/node_modules/foo/package.json",
                    "/a/b/c/d/node_modules/foo.ts",
                    "/a/b/c/d/node_modules/foo.tsx",
                    "/a/b/c/d/node_modules/foo.d.ts",
                    "/a/b/c/d/node_modules/foo/index.ts",
                    "/a/b/c/d/node_modules/foo/index.tsx",
                    "/a/b/c/d/node_modules/foo/index.d.ts",
                    "/a/b/c/d/node_modules/@types/foo/package.json",
                    "/a/b/c/d/node_modules/@types/foo.d.ts",
                    "/a/b/c/d/node_modules/@types/foo/index.d.ts",
                    "/a/b/c/node_modules/foo/package.json",
                    "/a/b/c/node_modules/foo.ts",
                    "/a/b/c/node_modules/foo.tsx",
                    "/a/b/c/node_modules/foo.d.ts",
                    "/a/b/c/node_modules/foo/index.ts",
                    "/a/b/c/node_modules/foo/index.tsx",
                    "/a/b/c/node_modules/foo/index.d.ts",
                    "/a/b/c/node_modules/@types/foo/package.json",
                    "/a/b/c/node_modules/@types/foo.d.ts",
                    "/a/b/c/node_modules/@types/foo/index.d.ts",
                    "/a/b/node_modules/foo/package.json",
                ],
            );
        };

        test(false);
        test(true);
    }

    #[test]
    fn test_load_module_as_file() {
        let test = |has_directory_exists: bool| {
            let containing_file = FileBuilder::default()
                .name("/a/b/c/d/e.ts")
                .build()
                .unwrap();
            let module_file = FileBuilder::default()
                .name("/a/b/node_modules/foo.ts")
                .build()
                .unwrap();
            let resolution = node_module_name_resolver(
                "foo",
                &containing_file.name,
                Default::default(),
                &*create_module_resolution_host(
                    has_directory_exists,
                    vec![containing_file.clone(), module_file.clone()],
                ),
                None,
                None,
                None,
            )
            .unwrap();
            check_resolved_module(
                resolution.resolved_module.as_deref(),
                Some(&create_resolved_module(&module_file.name, Some(true))),
            );
        };

        test(false);
        test(true);
    }

    #[test]
    fn test_load_module_as_directory() {
        let test = |has_directory_exists: bool| {
            let containing_file = FileBuilder::default()
                .name("/a/node_modules/b/c/node_modules/d/e.ts")
                .build()
                .unwrap();
            let module_file = FileBuilder::default()
                .name("/a/node_modules/foo/index.d.ts")
                .build()
                .unwrap();
            let resolution = node_module_name_resolver(
                "foo",
                &containing_file.name,
                Default::default(),
                &*create_module_resolution_host(
                    has_directory_exists,
                    vec![containing_file.clone(), module_file.clone()],
                ),
                None,
                None,
                None,
            )
            .unwrap();
            check_resolved_module_with_failed_lookup_locations(
                &resolution,
                &create_resolved_module(&module_file.name, Some(true)),
                &[
                    "/a/node_modules/b/c/node_modules/d/node_modules/foo/package.json",
                    "/a/node_modules/b/c/node_modules/d/node_modules/foo.ts",
                    "/a/node_modules/b/c/node_modules/d/node_modules/foo.tsx",
                    "/a/node_modules/b/c/node_modules/d/node_modules/foo.d.ts",
                    "/a/node_modules/b/c/node_modules/d/node_modules/foo/index.ts",
                    "/a/node_modules/b/c/node_modules/d/node_modules/foo/index.tsx",
                    "/a/node_modules/b/c/node_modules/d/node_modules/foo/index.d.ts",
                    "/a/node_modules/b/c/node_modules/d/node_modules/@types/foo/package.json",
                    "/a/node_modules/b/c/node_modules/d/node_modules/@types/foo.d.ts",
                    "/a/node_modules/b/c/node_modules/d/node_modules/@types/foo/index.d.ts",
                    "/a/node_modules/b/c/node_modules/foo/package.json",
                    "/a/node_modules/b/c/node_modules/foo.ts",
                    "/a/node_modules/b/c/node_modules/foo.tsx",
                    "/a/node_modules/b/c/node_modules/foo.d.ts",
                    "/a/node_modules/b/c/node_modules/foo/index.ts",
                    "/a/node_modules/b/c/node_modules/foo/index.tsx",
                    "/a/node_modules/b/c/node_modules/foo/index.d.ts",
                    "/a/node_modules/b/c/node_modules/@types/foo/package.json",
                    "/a/node_modules/b/c/node_modules/@types/foo.d.ts",
                    "/a/node_modules/b/c/node_modules/@types/foo/index.d.ts",
                    "/a/node_modules/b/node_modules/foo/package.json",
                    "/a/node_modules/b/node_modules/foo.ts",
                    "/a/node_modules/b/node_modules/foo.tsx",
                    "/a/node_modules/b/node_modules/foo.d.ts",
                    "/a/node_modules/b/node_modules/foo/index.ts",
                    "/a/node_modules/b/node_modules/foo/index.tsx",
                    "/a/node_modules/b/node_modules/foo/index.d.ts",
                    "/a/node_modules/b/node_modules/@types/foo/package.json",
                    "/a/node_modules/b/node_modules/@types/foo.d.ts",
                    "/a/node_modules/b/node_modules/@types/foo/index.d.ts",
                    "/a/node_modules/foo/package.json",
                    "/a/node_modules/foo.ts",
                    "/a/node_modules/foo.tsx",
                    "/a/node_modules/foo.d.ts",
                    "/a/node_modules/foo/index.ts",
                    "/a/node_modules/foo/index.tsx",
                ],
            );
        };

        test(false);
        test(true);
    }

    fn test_preserve_symlinks(preserve_symlinks: bool) {
        let ref arena = AllArenasHarness::default();
        let real_file_name = "/linked/index.d.ts";
        let symlink_file_name = "/app/node_modules/linked/index.d.ts";
        let host = create_module_resolution_host(
            true,
            vec![
                FileBuilder::default()
                    .name(real_file_name)
                    .symlinks([symlink_file_name].owned())
                    .build()
                    .unwrap(),
                FileBuilder::default()
                    .name("/app/node_modules/linked/package.json")
                    .content("{\"version\": \"0.0.0\", \"main\": \"./index\"}")
                    .build()
                    .unwrap(),
            ],
        );
        let resolution = node_module_name_resolver(
            "linked",
            "/app/app.ts",
            arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .preserve_symlinks(preserve_symlinks)
                    .build()
                    .unwrap(),
            ),
            &*host,
            None,
            None,
            None,
            arena,
        )
        .unwrap();
        let resolved_file_name = if preserve_symlinks {
            symlink_file_name
        } else {
            real_file_name
        };
        check_resolved_module(
            resolution.resolved_module.as_deref(),
            Some(&create_resolved_module(resolved_file_name, Some(true))),
        );
    }

    #[test]
    fn test_preserve_symlinks_false() {
        test_preserve_symlinks(false);
    }

    #[test]
    fn test_preserve_symlinks_true() {
        test_preserve_symlinks(true);
    }

    #[test]
    fn test_uses_original_path_for_caching() {
        let ref arena = AllArenasHarness::default();
        let host = create_module_resolution_host(
            true,
            vec![
                FileBuilder::default()
                    .name("/modules/a.ts")
                    .symlinks(["/sub/node_modules/a/index.ts"].owned())
                    .build()
                    .unwrap(),
                FileBuilder::default()
                    .name("/sub/node_modules/a/package.json")
                    .content("{\"version\": \"0.0.0\", \"main\": \"./index\"}")
                    .build()
                    .unwrap(),
            ],
        );
        let compiler_options = arena.alloc_compiler_options(
            CompilerOptionsBuilder::default()
                .module_resolution(ModuleResolutionKind::NodeJs)
                .build()
                .unwrap(),
        );
        let cache = arena.alloc_module_resolution_cache(create_module_resolution_cache(
            "/",
            arena.alloc_get_canonical_file_name(Box::new(GetCanonicalFileNameNoop)),
            None,
            None,
            None,
        ));
        let mut resolution = resolve_module_name(
            "a",
            "/sub/dir/foo.ts",
            compiler_options.clone(),
            &*host,
            Some(cache.clone()),
            None,
            None,
        )
        .unwrap();
        check_resolved_module(
            resolution.resolved_module.as_deref(),
            Some(&create_resolved_module("/modules/a.ts", Some(true))),
        );

        resolution = resolve_module_name(
            "a",
            "/sub/foo.ts",
            compiler_options.clone(),
            &*host,
            Some(cache.clone()),
            None,
            None,
        )
        .unwrap();
        check_resolved_module(
            resolution.resolved_module.as_deref(),
            Some(&create_resolved_module("/modules/a.ts", Some(true))),
        );

        resolution = resolve_module_name(
            "a",
            "/foo.ts",
            compiler_options.clone(),
            &*host,
            Some(cache.clone()),
            None,
            None,
        )
        .unwrap();
        asserting("lookup in parent directory doesn't hit the cache")
            .that(&resolution.resolved_module)
            .is_none();
    }

    fn check_resolution(resolution: &ResolvedModuleWithFailedLookupLocations) {
        check_resolved_module(
            resolution.resolved_module.as_deref(),
            Some(&create_resolved_module("/linked/index.d.ts", Some(true))),
        );
        assert_that(
            &resolution
                .resolved_module
                .as_ref()
                .unwrap()
                .original_path
                .as_deref(),
        )
        .is_some()
        .is_equal_to("/app/node_modules/linked/index.d.ts");
    }

    #[test]
    fn test_preserves_original_path_on_cache_hit() {
        let ref arena = AllArenasHarness::default();
        let host = create_module_resolution_host(
            true,
            vec![
                FileBuilder::default()
                    .name("/linked/index.d.ts")
                    .symlinks(["/app/node_modules/linked/index.d.ts".to_owned()])
                    .build()
                    .unwrap(),
                FileBuilder::default()
                    .name("/app/node_modules/linked/package.json")
                    .content("{\"version\": \"0.0.0\", \"main\": \"./index\"}")
                    .build()
                    .unwrap(),
            ],
        );
        let cache = arena.alloc_module_resolution_cache(create_module_resolution_cache(
            "/",
            arena.alloc_get_canonical_file_name(Box::new(GetCanonicalFileNameNoop)),
            None,
            None,
            None,
            arena,
        ));
        let compiler_options = arena.alloc_compiler_options(
            CompilerOptionsBuilder::default()
                .module_resolution(ModuleResolutionKind::NodeJs)
                .build()
                .unwrap(),
        );
        check_resolution(
            &resolve_module_name(
                "linked",
                "/app/src/app.ts",
                compiler_options.clone(),
                &*host,
                Some(cache.clone()),
                None,
                None,
            )
            .unwrap(),
        );
        check_resolution(
            &resolve_module_name(
                "linked",
                "/app/lib/main.ts",
                compiler_options.clone(),
                &*host,
                Some(cache.clone()),
                None,
                None,
            )
            .unwrap(),
        );
    }
}

mod relative_imports {
    use harness::Compiler;
    use typescript_rust::{
        combine_paths, create_program, create_source_file, normalize_path, not_implemented,
        CompilerHost, CompilerOptions, CompilerOptionsBuilder, CreateProgramOptionsBuilder,
        ModuleKind, Node, Owned, ScriptReferenceHost, ScriptTarget,
    };

    use super::*;

    fn test(
        files: &HashMap<String, String>,
        current_directory: &str,
        root_files: &[&str],
        expected_files_count: usize,
        relative_names_to_check: &[&str],
    ) {
        let ref arena = AllArenasHarness::default();
        let options = arena.alloc_compiler_options(
            CompilerOptionsBuilder::default()
                .module(ModuleKind::CommonJS)
                .build()
                .unwrap(),
        );
        let host: Id<Box<dyn CompilerHost>> =
            RelativeImportsCompilerHost::new(current_directory.to_owned(), files.clone(), arena);

        let program = create_program(
            CreateProgramOptionsBuilder::default()
                .root_names(root_files.owned())
                .options(options)
                .host(host)
                .build()
                .unwrap(),
        )
        .unwrap();

        assert_that(&*program.get_source_files()).has_length(expected_files_count);
        let syntactic_diagnostics = program.get_syntactic_diagnostics(None, None);
        asserting(&format!(
            "expect no syntactic diagnostics, got: {:?}",
            Compiler::minimal_diagnostics_to_string(&syntactic_diagnostics, None,)
        ))
        .that(&syntactic_diagnostics)
        .is_empty();
        let semantic_diagnostics = program.get_semantic_diagnostics(None, None).unwrap();
        asserting(&format!(
            "expect no semantic diagnostics, got: {:?}",
            Compiler::minimal_diagnostics_to_string(&semantic_diagnostics, None,)
        ))
        .that(&semantic_diagnostics)
        .is_empty();

        for relative_file_name in relative_names_to_check {
            asserting("expected to get file by relative name, got undefined")
                .that(&program.get_source_file(relative_file_name))
                .is_some();
        }
    }

    #[derive(Trace, Finalize)]
    struct RelativeImportsCompilerHost {
        current_directory: String,
        files: HashMap<String, String>,
    }

    impl RelativeImportsCompilerHost {
        pub fn new(
            current_directory: String,
            files: HashMap<String, String>,
            arena: &impl HasArena,
        ) -> Id<Box<dyn CompilerHost>> {
            arena.alloc_compiler_host(Box::new(Self {
                current_directory,
                files,
            }))
        }
    }

    impl CompilerHost for RelativeImportsCompilerHost {
        fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
            self
        }

        fn get_source_file(
            &self,
            file_name: &str,
            language_version: ScriptTarget,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _should_create_new_source_file: Option<bool>,
        ) -> io::Result<Option<Id<Node /*SourceFile*/>>> {
            let ref path =
                normalize_path(&combine_paths(&self.current_directory, &[Some(file_name)]));
            let file = self.files.get(path);
            Ok(file.map(|file| {
                create_source_file(file_name, file.clone(), language_version, None, None, self)
                    .unwrap()
            }))
        }

        fn get_default_lib_file_name(&self, _options: &CompilerOptions) -> io::Result<String> {
            Ok("lib.d.ts".to_owned())
        }

        fn write_file(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Id<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            not_implemented()
        }

        fn write_file_non_overridden(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Id<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            unreachable!()
        }

        fn is_write_file_supported(&self) -> bool {
            true
        }

        fn set_overriding_write_file(
            &self,
            _overriding_write_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn get_current_directory(&self) -> io::Result<String> {
            Ok(self.current_directory.clone())
        }

        fn get_canonical_file_name(&self, file_name: &str) -> String {
            file_name.to_lowercase()
        }

        fn get_new_line(&self) -> String {
            "\r\n".to_owned()
        }

        fn use_case_sensitive_file_names(&self) -> bool {
            false
        }

        fn is_read_directory_implemented(&self) -> bool {
            false
        }

        fn is_resolve_module_names_supported(&self) -> bool {
            false
        }

        fn is_resolve_type_reference_directives_supported(&self) -> bool {
            false
        }

        fn is_on_release_old_source_file_supported(&self) -> bool {
            false
        }

        fn is_on_release_parsed_command_line_supported(&self) -> bool {
            false
        }

        fn is_create_directory_supported(&self) -> bool {
            false
        }

        fn set_overriding_create_directory(
            &self,
            _overriding_create_directory: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_get_source_file_by_path_supported(&self) -> bool {
            false
        }

        fn is_get_parsed_command_line_supported(&self) -> bool {
            false
        }
    }

    impl ModuleResolutionHost for RelativeImportsCompilerHost {
        fn is_get_directories_supported(&self) -> bool {
            true
        }

        fn get_directories(&self, _path: &str) -> Option<Vec<String>> {
            Some(Default::default())
        }

        fn set_overriding_get_directories(
            &self,
            _overriding_get_directories: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn file_exists(&self, file_name: &str) -> bool {
            let ref path =
                normalize_path(&combine_paths(&self.current_directory, &[Some(file_name)]));
            self.files.contains_key(path)
        }

        fn file_exists_non_overridden(&self, _file_name: &str) -> bool {
            unreachable!()
        }

        fn set_overriding_file_exists(
            &self,
            _overriding_file_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn read_file(&self, _file_name: &str) -> io::Result<Option<String>> {
            not_implemented()
        }

        fn set_overriding_read_file(
            &self,
            _overriding_read_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
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
            _overriding_directory_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_realpath_supported(&self) -> bool {
            false
        }

        fn set_overriding_realpath(
            &self,
            _overriding_realpath: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }
    }

    #[test]
    fn test_should_find_all_modules() {
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                (
                    "/a/b/c/first/shared.ts",
                    "
class A {}
export = A",
                ),
                (
                    "/a/b/c/first/second/class_a.ts",
                    "
import Shared = require('../shared');
import C = require('../../third/class_c');
class B {}
export = B;",
                ),
                (
                    "/a/b/c/third/class_c.ts",
                    "
import Shared = require('../first/shared');
class C {}
export = C;
                ",
                ),
            ]
            .owned(),
        );
        test(
            &files,
            "/a/b/c/first/second",
            &["class_a.ts"],
            3,
            &["../../../c/third/class_c.ts"],
        );
    }

    #[test]
    fn test_should_find_modules_in_node_modules() {
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("/parent/node_modules/mod/index.d.ts", "export var x"),
                ("/parent/app/myapp.ts", r#"import {x} from "mod""#),
            ]
            .owned(),
        );
        test(&files, "/parent/app", &["myapp.ts"], 2, &[]);
    }

    #[test]
    fn test_should_find_files_referenced_via_absolute_and_relative_names() {
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("/a/b/c.ts", r#"/// <reference path="b.ts"/>"#),
                ("/a/b/b.ts", "var x"),
            ]
            .owned(),
        );
        test(&files, "/a/b", &["c.ts", "/a/b/b.ts"], 2, &[]);
    }
}

mod files_with_different_casing_with_force_consistent_casing_in_file_names {
    use once_cell::unsync::OnceCell;
    use typescript_rust::{
        combine_paths, create_get_canonical_file_name, create_program, create_source_file,
        id_arena::Id, normalize_path, not_implemented, sort_and_deduplicate_diagnostics,
        CompilerHost, CompilerOptions, CompilerOptionsBuilder, CreateProgramOptionsBuilder,
        Diagnostic, DiagnosticInterface, DiagnosticRelatedInformation,
        DiagnosticRelatedInformationInterface, Diagnostics, ModuleKind, Node, Owned, Program,
        ScriptTarget, VecExt,
    };

    use super::*;
    use crate::{
        get_diagnostic_message_chain, get_diagnostic_of_file_from,
        get_diagnostic_of_file_from_program,
    };

    thread_local! {
        static library: OnceCell<Id<Node /*SourceFile*/>> = Default::default();
    }

    fn test(
        mut files: HashMap<String, String>,
        options: Id<CompilerOptions>,
        current_directory: &str,
        use_case_sensitive_file_names: bool,
        root_files: &[&str],
        mut expected_diagnostics: impl FnMut(&Program) -> Vec<Id<Diagnostic>>,
    ) {
        let get_canonical_file_name = create_get_canonical_file_name(use_case_sensitive_file_names);
        if !use_case_sensitive_file_names {
            let old_files = files.clone();
            files = Default::default();
            for (file_name, file) in old_files {
                files.insert(get_canonical_file_name(&file_name), file);
            }
        }

        let host: Id<Box<dyn CompilerHost>> = FilesWithDifferentCasingCompilerHost::new(
            current_directory.to_owned(),
            files.clone(),
            get_canonical_file_name,
            use_case_sensitive_file_names,
        );
        let program = create_program(
            CreateProgramOptionsBuilder::default()
                .root_names(root_files.owned())
                .options(options)
                .host(host)
                .build()
                .unwrap(),
        )
        .unwrap();
        let diagnostics: Vec<_> = sort_and_deduplicate_diagnostics(
            &program
                .get_semantic_diagnostics(None, None)
                .unwrap()
                .and_extend(Vec::<_>::from(program.get_options_diagnostics(None))),
        )
        .into();
        assert_that(&diagnostics).is_equal_to(Vec::<_>::from(sort_and_deduplicate_diagnostics(
            &expected_diagnostics(&program),
        )));
    }

    #[derive(Trace, Finalize)]
    struct FilesWithDifferentCasingCompilerHost {
        current_directory: String,
        files: HashMap<String, String>,
        #[unsafe_ignore_trace]
        get_canonical_file_name: fn(&str) -> String,
        use_case_sensitive_file_names: bool,
    }

    impl FilesWithDifferentCasingCompilerHost {
        pub fn new(
            current_directory: String,
            files: HashMap<String, String>,
            get_canonical_file_name: fn(&str) -> String,
            use_case_sensitive_file_names: bool,
            arena: &impl HasArena,
        ) -> Id<Box<dyn CompilerHost>> {
            arena.alloc_compiler_host(Box::new(Self {
                current_directory,
                files,
                get_canonical_file_name,
                use_case_sensitive_file_names,
            }))
        }
    }

    impl CompilerHost for FilesWithDifferentCasingCompilerHost {
        fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
            self
        }

        fn get_source_file(
            &self,
            file_name: &str,
            language_version: ScriptTarget,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _should_create_new_source_file: Option<bool>,
        ) -> io::Result<Option<Id<Node /*SourceFile*/>>> {
            if file_name == "lib.d.ts" {
                return Ok(Some(library.with(|library_| {
                    library_
                        .get_or_init(|| {
                            create_source_file(
                                "lib.d.ts",
                                "".to_owned(),
                                ScriptTarget::ES5,
                                None,
                                None,
                            )
                            .unwrap()
                        })
                        .clone()
                })));
            }
            let ref path = self.get_canonical_file_name(&normalize_path(&combine_paths(
                &self.current_directory,
                &[Some(file_name)],
            )));
            let file = self.files.get(path);
            Ok(file.map(|file| {
                create_source_file(file_name, file.clone(), language_version, None, None).unwrap()
            }))
        }

        fn get_default_lib_file_name(&self, _options: &CompilerOptions) -> io::Result<String> {
            Ok("lib.d.ts".to_owned())
        }

        fn write_file(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Id<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            not_implemented()
        }

        fn write_file_non_overridden(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Id<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            unreachable!()
        }

        fn is_write_file_supported(&self) -> bool {
            true
        }

        fn set_overriding_write_file(
            &self,
            _overriding_write_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn get_current_directory(&self) -> io::Result<String> {
            Ok(self.current_directory.clone())
        }

        fn get_canonical_file_name(&self, file_name: &str) -> String {
            (self.get_canonical_file_name)(file_name)
        }

        fn get_new_line(&self) -> String {
            "\r\n".to_owned()
        }

        fn use_case_sensitive_file_names(&self) -> bool {
            self.use_case_sensitive_file_names
        }

        fn is_read_directory_implemented(&self) -> bool {
            false
        }

        fn is_resolve_module_names_supported(&self) -> bool {
            false
        }

        fn is_resolve_type_reference_directives_supported(&self) -> bool {
            false
        }

        fn is_on_release_old_source_file_supported(&self) -> bool {
            false
        }

        fn is_on_release_parsed_command_line_supported(&self) -> bool {
            false
        }

        fn is_create_directory_supported(&self) -> bool {
            false
        }

        fn set_overriding_create_directory(
            &self,
            _overriding_create_directory: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_get_source_file_by_path_supported(&self) -> bool {
            false
        }

        fn is_get_parsed_command_line_supported(&self) -> bool {
            false
        }
    }

    impl ModuleResolutionHost for FilesWithDifferentCasingCompilerHost {
        fn is_get_directories_supported(&self) -> bool {
            true
        }

        fn get_directories(&self, _path: &str) -> Option<Vec<String>> {
            Some(Default::default())
        }

        fn set_overriding_get_directories(
            &self,
            _overriding_get_directories: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn file_exists(&self, file_name: &str) -> bool {
            let ref path = (self.get_canonical_file_name)(&normalize_path(&combine_paths(
                &self.current_directory,
                &[Some(file_name)],
            )));
            self.files.contains_key(path)
        }

        fn file_exists_non_overridden(&self, _file_name: &str) -> bool {
            unreachable!()
        }

        fn set_overriding_file_exists(
            &self,
            _overriding_file_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn read_file(&self, _file_name: &str) -> io::Result<Option<String>> {
            not_implemented()
        }

        fn set_overriding_read_file(
            &self,
            _overriding_read_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
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
            _overriding_directory_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_realpath_supported(&self) -> bool {
            false
        }

        fn set_overriding_realpath(
            &self,
            _overriding_realpath: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }
    }

    #[test]
    fn test_should_succeed_when_the_same_file_is_referenced_using_absolute_and_relative_names() {
        let ref arena = AllArenasHarness::default();
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("/a/b/c.ts", r#"/// <reference path="d.ts"/>"#),
                ("/a/b/d.ts", "var x"),
            ]
            .owned(),
        );
        test(
            files,
            arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module(ModuleKind::AMD)
                    .build()
                    .unwrap(),
            ),
            "/a/b",
            false,
            &["c.ts", "/a/b/d.ts"],
            |_| vec![],
        );
    }

    #[test]
    fn test_should_fail_when_two_files_used_in_program_differ_only_in_casing_tripleslash_references(
    ) {
        let ref arena = AllArenasHarness::default();
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("/a/b/c.ts", r#"/// <reference path="D.ts"/>"#),
                ("/a/b/d.ts", "var x"),
            ]
            .owned(),
        );
        test(
            files,
            arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module(ModuleKind::AMD)
                    .force_consistent_casing_in_file_names(true)
                    .build()
                    .unwrap(),
            ),
            "/a/b",
            false,
            &["c.ts", "d.ts"],
            |program: &Program| {
                vec![{
                    let diagnostic = get_diagnostic_of_file_from_program(
                        program,
                        "c.ts",
                        r#"/// <reference path="D.ts"/>"#.find("D.ts").unwrap().try_into().unwrap(),
                        "D.ts".len().try_into().unwrap(),
                        get_diagnostic_message_chain(
                            &Diagnostics::Already_included_file_name_0_differs_from_file_name_1_only_in_casing,
                            ["D.ts", "d.ts"].owned(),
                            vec![
                                get_diagnostic_message_chain(
                                    &Diagnostics::The_file_is_in_the_program_because_Colon,
                                    vec![],
                                    vec![
                                        get_diagnostic_message_chain(
                                            &Diagnostics::Referenced_via_0_from_file_1,
                                            ["D.ts", "c.ts"].owned(),
                                            None,
                                        ),
                                        get_diagnostic_message_chain(
                                            &Diagnostics::Root_file_specified_for_compilation,
                                            None,
                                            None,
                                        ),
                                    ]
                                )
                            ],
                        ),
                        None,
                        arena,
                    );
                    *diagnostic.maybe_related_information_mut() = None;
                    diagnostic
                }]
            },
        );
    }

    #[test]
    fn test_should_fail_when_two_files_used_in_program_differ_only_in_casing_imports() {
        let ref arena = AllArenasHarness::default();
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("/a/b/c.ts", r#"import {x} from "D""#),
                ("/a/b/d.ts", "export var x"),
            ]
            .owned(),
        );
        test(
            files,
            arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module(ModuleKind::AMD)
                    .force_consistent_casing_in_file_names(true)
                    .build()
                    .unwrap(),
            ),
            "/a/b",
            false,
            &["c.ts", "d.ts"],
            |program: &Program| {
                vec![{
                    let diagnostic = get_diagnostic_of_file_from_program(
                        program,
                        "c.ts",
                        r#"import {x} from "D""#.find(r#""D""#).unwrap().try_into().unwrap(),
                        r#""D""#.len().try_into().unwrap(),
                        get_diagnostic_message_chain(
                            &Diagnostics::Already_included_file_name_0_differs_from_file_name_1_only_in_casing,
                            ["/a/b/D.ts", "d.ts"].owned(),
                            vec![
                                get_diagnostic_message_chain(
                                    &Diagnostics::The_file_is_in_the_program_because_Colon,
                                    vec![],
                                    vec![
                                        get_diagnostic_message_chain(
                                            &Diagnostics::Imported_via_0_from_file_1,
                                            [r#""D""#, "c.ts"].owned(),
                                            None,
                                        ),
                                        get_diagnostic_message_chain(
                                            &Diagnostics::Root_file_specified_for_compilation,
                                            None,
                                            None,
                                        ),
                                    ]
                                )
                            ],
                        ),
                        None,
                        arena,
                    );
                    *diagnostic.maybe_related_information_mut() = None;
                    diagnostic
                }]
            },
        );
    }

    #[test]
    fn test_should_fail_when_two_files_used_in_program_differ_only_in_casing_imports_relative_module_names(
    ) {
        let ref arena = AllArenasHarness::default();
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("moduleA.ts", r#"import {x} from "./ModuleB""#),
                ("moduleB.ts", "export var x"),
            ]
            .owned(),
        );
        test(
            files,
            arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module(ModuleKind::CommonJS)
                    .force_consistent_casing_in_file_names(true)
                    .build()
                    .unwrap(),
            ),
            "",
            false,
            &["moduleA.ts", "moduleB.ts"],
            |program: &Program| {
                vec![{
                    let diagnostic = get_diagnostic_of_file_from_program(
                        program,
                        "moduleA.ts",
                        r#"import {x} from "./ModuleB""#.find(r#""./ModuleB""#).unwrap().try_into().unwrap(),
                        r#""./ModuleB""#.len().try_into().unwrap(),
                        get_diagnostic_message_chain(
                            &Diagnostics::Already_included_file_name_0_differs_from_file_name_1_only_in_casing,
                            ["ModuleB.ts", "moduleB.ts"].owned(),
                            vec![
                                get_diagnostic_message_chain(
                                    &Diagnostics::The_file_is_in_the_program_because_Colon,
                                    vec![],
                                    vec![
                                        get_diagnostic_message_chain(
                                            &Diagnostics::Imported_via_0_from_file_1,
                                            [r#""./ModuleB""#, "moduleA.ts"].owned(),
                                            None,
                                        ),
                                        get_diagnostic_message_chain(
                                            &Diagnostics::Root_file_specified_for_compilation,
                                            None,
                                            None,
                                        ),
                                    ]
                                )
                            ],
                        ),
                        None,
                        arena,
                    );
                    *diagnostic.maybe_related_information_mut() = None;
                    diagnostic
                }]
            },
        );
    }

    #[test]
    fn test_should_fail_when_two_files_exist_on_disk_that_differs_only_in_casing() {
        let ref arena = AllArenasHarness::default();
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("/a/b/c.ts", r#"import {x} from "D""#),
                ("/a/b/D.ts", "export var x"),
                ("/a/b/d.ts", "export var y"),
            ]
            .owned(),
        );
        test(
            files,
            arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module(ModuleKind::AMD)
                    .build()
                    .unwrap(),
            ),
            "/a/b",
            true,
            &["c.ts", "d.ts"],
            |program: &Program| {
                vec![{
                    let diagnostic = get_diagnostic_of_file_from_program(
                        program,
                        "c.ts",
                        r#"import {x} from "D""#.find(r#""D""#).unwrap().try_into().unwrap(),
                        r#""D""#.len().try_into().unwrap(),
                        get_diagnostic_message_chain(
                            &Diagnostics::Already_included_file_name_0_differs_from_file_name_1_only_in_casing,
                            ["/a/b/D.ts", "d.ts"].owned(),
                            vec![
                                get_diagnostic_message_chain(
                                    &Diagnostics::The_file_is_in_the_program_because_Colon,
                                    vec![],
                                    vec![
                                        get_diagnostic_message_chain(
                                            &Diagnostics::Imported_via_0_from_file_1,
                                            [r#""D""#, "c.ts"].owned(),
                                            None,
                                        ),
                                        get_diagnostic_message_chain(
                                            &Diagnostics::Root_file_specified_for_compilation,
                                            None,
                                            None,
                                        ),
                                    ]
                                )
                            ],
                        ),
                        None,
                        arena,
                    );
                    *diagnostic.maybe_related_information_mut() = None;
                    diagnostic
                }]
            },
        );
    }

    #[test]
    fn test_should_fail_when_module_name_in_require_calls_has_inconsistent_casing() {
        let ref arena = AllArenasHarness::default();
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("moduleA.ts", r#"import a = require("./ModuleC")"#),
                ("moduleB.ts", r#"import a = require("./moduleC")"#),
                ("moduleC.ts", "export var x"),
            ]
            .owned(),
        );
        test(
            files,
            arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module(ModuleKind::CommonJS)
                    .force_consistent_casing_in_file_names(true)
                    .build()
                    .unwrap(),
            ),
            "",
            false,
            &["moduleA.ts", "moduleB.ts", "moduleC.ts"],
            |program: &Program| {
                let import_in_a: Id<DiagnosticRelatedInformation> = arena.alloc_diagnostic_related_information((*get_diagnostic_of_file_from_program(
                    program,
                    "moduleA.ts",
                    r#"import a = require("./ModuleC")"#.find(r#""./ModuleC""#).unwrap().try_into().unwrap(),
                    r#""./ModuleC""#.len().try_into().unwrap(),
                    &*Diagnostics::File_is_included_via_import_here,
                    None,
                    arena,
                )).clone().into());
                // reportsUnnecessary: undefined,
                // reportsDeprecated: undefined,
                let import_in_b: Id<DiagnosticRelatedInformation>  = arena.alloc_diagnostic_related_information((*get_diagnostic_of_file_from_program(
                    program,
                    "moduleB.ts",
                    r#"import a = require("./moduleC")"#.find(r#""./moduleC""#).unwrap().try_into().unwrap(),
                    r#""./moduleC""#.len().try_into().unwrap(),
                    &*Diagnostics::File_is_included_via_import_here,
                    None,
                    arena,
                )).clone().into());
                // reportsUnnecessary: undefined,
                // reportsDeprecated: undefined,
                let import_here_in_a = get_diagnostic_message_chain(
                    &Diagnostics::Imported_via_0_from_file_1,
                    [r#""./ModuleC""#, "moduleA.ts"].owned(),
                    None,
                );
                let import_here_in_b = get_diagnostic_message_chain(
                    &Diagnostics::Imported_via_0_from_file_1,
                    [r#""./moduleC""#, "moduleB.ts"].owned(),
                    None,
                );
                let details = vec![get_diagnostic_message_chain(
                    &Diagnostics::The_file_is_in_the_program_because_Colon,
                    None,
                    vec![
                        import_here_in_a,
                        import_here_in_b,
                        get_diagnostic_message_chain(
                            &Diagnostics::Root_file_specified_for_compilation,
                            None,
                            None,
                        ),
                    ],
                )];
                vec![
                    {
                        let diagnostic = get_diagnostic_of_file_from(
                            import_in_a.maybe_file(),
                            Some(import_in_a.start()),
                            Some(import_in_a.length()),
                            get_diagnostic_message_chain(
                                &Diagnostics::Already_included_file_name_0_differs_from_file_name_1_only_in_casing,
                                ["ModuleC.ts", "moduleC.ts"].owned(),
                                details.clone(),
                            ),
                            None,
                            arena,
                        );
                        *diagnostic.maybe_related_information_mut() =
                            Some(vec![import_in_b.clone()]);
                        diagnostic
                    },
                    {
                        let diagnostic = get_diagnostic_of_file_from(
                            import_in_b.maybe_file(),
                            Some(import_in_b.start()),
                            Some(import_in_b.length()),
                            get_diagnostic_message_chain(
                                &Diagnostics::File_name_0_differs_from_already_included_file_name_1_only_in_casing,
                                ["moduleC.ts", "ModuleC.ts"].owned(),
                                details
                            ),
                            None,
                            arena,
                        );
                        *diagnostic.maybe_related_information_mut() = Some(vec![import_in_a]);
                        diagnostic
                    },
                ]
            },
        );
    }

    #[test]
    fn test_should_fail_when_module_name_in_require_calls_has_inconsistent_casing_and_current_directory_has_uppercase_chars(
    ) {
        let ref arena = AllArenasHarness::default();
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("/a/B/c/moduleA.ts", r#"import a = require("./ModuleC")"#),
                ("/a/B/c/moduleB.ts", r#"import a = require("./moduleC")"#),
                ("/a/B/c/moduleC.ts", "export var x"),
                (
                    "/a/B/c/moduleD.ts",
                    r#"
import a = require("./moduleA");
import b = require("./moduleB");
                "#,
                ),
            ]
            .owned(),
        );
        test(
            files,
            arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module(ModuleKind::CommonJS)
                    .force_consistent_casing_in_file_names(true)
                    .build()
                    .unwrap(),
            ),
            "/a/B/c",
            false,
            &["moduleD.ts"],
            |program: &Program| {
                vec![{
                    let diagnostic = get_diagnostic_of_file_from_program(
                        program,
                        "moduleB.ts",
                        r#"import a = require("./moduleC")"#.find(r#""./moduleC""#).unwrap().try_into().unwrap(),
                        r#""./moduleC""#.len().try_into().unwrap(),
                        get_diagnostic_message_chain(
                            &Diagnostics::File_name_0_differs_from_already_included_file_name_1_only_in_casing,
                            ["/a/B/c/moduleC.ts", "/a/B/c/ModuleC.ts"].owned(),
                            vec![
                                get_diagnostic_message_chain(
                                    &Diagnostics::The_file_is_in_the_program_because_Colon,
                                    vec![],
                                    vec![
                                        get_diagnostic_message_chain(
                                            &Diagnostics::Imported_via_0_from_file_1,
                                            [r#""./ModuleC""#, "/a/B/c/moduleA.ts"].owned(),
                                            None,
                                        ),
                                        get_diagnostic_message_chain(
                                            &Diagnostics::Imported_via_0_from_file_1,
                                            [r#""./moduleC""#, "/a/B/c/moduleB.ts"].owned(),
                                            None,
                                        ),
                                    ]
                                )
                            ],
                        ),
                        None,
                        arena,
                    );
                    *diagnostic.ref_(arena).maybe_related_information_mut() = Some(vec![
                        arena.alloc_diagnostic_related_information((*get_diagnostic_of_file_from_program(
                            program,
                            "moduleA.ts",
                            r#"import a = require("./ModuleC")"#.find(r#""./ModuleC""#).unwrap().try_into().unwrap(),
                            r#""./ModuleC""#.len().try_into().unwrap(),
                            &*Diagnostics::File_is_included_via_import_here,
                            None,
                            arena,
                        )).clone().into())
                        // reportsUnnecessary: undefined,
                        // reportsDeprecated: undefined,
                    ]);
                    diagnostic
                }]
            },
        );
    }

    #[test]
    fn test_should_not_fail_when_module_name_in_require_calls_has_consistent_casing_and_current_directory_has_uppercase_chars(
    ) {
        let ref arena = AllArenasHarness::default();
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("/a/B/c/moduleA.ts", r#"import a = require("./moduleC")"#),
                ("/a/B/c/moduleB.ts", r#"import a = require("./moduleC")"#),
                ("/a/B/c/moduleC.ts", "export var x"),
                (
                    "/a/B/c/moduleD.ts",
                    r#"
import a = require("./moduleA");
import b = require("./moduleB");
                "#,
                ),
            ]
            .owned(),
        );
        test(
            files,
            arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module(ModuleKind::CommonJS)
                    .force_consistent_casing_in_file_names(true)
                    .build()
                    .unwrap(),
            ),
            "/a/B/c",
            false,
            &["moduleD.ts"],
            |_| vec![],
        );
    }

    #[test]
    fn test_should_succeed_when_the_two_files_in_program_differ_only_in_drive_letter_in_their_names(
    ) {
        let ref arena = AllArenasHarness::default();
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                (
                    "d:/someFolder/moduleA.ts",
                    r#"import a = require("D:/someFolder/moduleC")"#,
                ),
                (
                    "d:/someFolder/moduleB.ts",
                    r#"import a = require("./moduleC")"#,
                ),
                ("D:/someFolder/moduleC.ts", "export const x = 10"),
            ]
            .owned(),
        );
        test(
            files,
            arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module(ModuleKind::CommonJS)
                    .force_consistent_casing_in_file_names(true)
                    .build()
                    .unwrap(),
            ),
            "d:/someFolder",
            false,
            &["d:/someFolder/moduleA.ts", "d:/someFolder/moduleB.ts"],
            |_| vec![],
        );
    }
}

mod base_url_augmented_module_resolution {
    use typescript_rust::{
        resolve_module_name, CompilerOptionsBuilder, JsxEmit, ModuleResolutionKind, Owned,
    };

    use super::*;

    #[test]
    fn test_module_resolution_without_path_mappings_root_dirs() {
        let ref arena = AllArenasHarness::default();
        let test = |has_directory_exists: bool| {
            let file1 = FileBuilder::default()
                .name("/root/folder1/file1.ts")
                .build()
                .unwrap();
            let file2 = FileBuilder::default()
                .name("/root/folder2/file2.ts")
                .build()
                .unwrap();
            let file3 = FileBuilder::default()
                .name("/root/folder2/file3.ts")
                .build()
                .unwrap();
            let host = create_module_resolution_host(
                has_directory_exists,
                vec![file1.clone(), file2.clone(), file3.clone()],
            );
            for module_resolution in [ModuleResolutionKind::NodeJs, ModuleResolutionKind::Classic] {
                let options = arena.alloc_compiler_options(
                    CompilerOptionsBuilder::default()
                        .module_resolution(module_resolution)
                        .base_url("/root")
                        .build()
                        .unwrap(),
                );
                let result = resolve_module_name(
                    "folder2/file2",
                    &file1.name,
                    options.clone(),
                    &*host,
                    None,
                    None,
                    None,
                    arena,
                )
                .unwrap();
                check_resolved_module_with_failed_lookup_locations(
                    &result,
                    &create_resolved_module(&file2.name, None),
                    &[],
                );
                let result = resolve_module_name(
                    "./file3",
                    &file2.name,
                    options.clone(),
                    &*host,
                    None,
                    None,
                    None,
                    arena,
                )
                .unwrap();
                check_resolved_module_with_failed_lookup_locations(
                    &result,
                    &create_resolved_module(&file3.name, None),
                    &[],
                );
                let result = resolve_module_name(
                    "/root/folder1/file1",
                    &file2.name,
                    options.clone(),
                    &*host,
                    None,
                    None,
                    None,
                    arena,
                )
                .unwrap();
                check_resolved_module_with_failed_lookup_locations(
                    &result,
                    &create_resolved_module(&file1.name, None),
                    &[],
                );
            }
        };

        test(false);
        test(true);
    }

    #[test]
    fn test_node_plus_base_url() {
        let ref arena = AllArenasHarness::default();
        let test = |has_directory_exists: bool| {
            let main = FileBuilder::default()
                .name("/root/a/b/main.ts")
                .build()
                .unwrap();
            let m1 = FileBuilder::default().name("/root/m1.ts").build().unwrap();
            let m2 = FileBuilder::default()
                .name("/root/m2/index.d.ts")
                .build()
                .unwrap();
            let m3 = FileBuilder::default()
                .name("/root/m3/package.json")
                .content(serde_json::json!({"typings": "dist/typings.d.ts"}).to_string())
                .build()
                .unwrap();
            let m3_typings = FileBuilder::default()
                .name("/root/m3/dist/typings.d.ts")
                .build()
                .unwrap();
            let m4 = FileBuilder::default()
                .name("/root/node_modules/m4.ts")
                .build()
                .unwrap();

            let options = arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module_resolution(ModuleResolutionKind::NodeJs)
                    .base_url("/root")
                    .build()
                    .unwrap(),
            );
            let host = create_module_resolution_host(
                has_directory_exists,
                vec![
                    main.clone(),
                    m1.clone(),
                    m2.clone(),
                    m3.clone(),
                    m3_typings.clone(),
                    m4.clone(),
                ],
            );

            let check = |name: &str,
                         caller: &File,
                         expected: &File,
                         is_external_library_import: Option<bool>| {
                let is_external_library_import = is_external_library_import.unwrap_or(false);
                let result = resolve_module_name(
                    name,
                    &caller.name,
                    options.clone(),
                    &*host,
                    None,
                    None,
                    None,
                )
                .unwrap();
                check_resolved_module(
                    result.resolved_module.as_deref(),
                    Some(&create_resolved_module(
                        &expected.name,
                        Some(is_external_library_import),
                    )),
                );
            };

            check("m1", &main, &m1, None);
            check("m2", &main, &m2, None);
            check("m3", &main, &m3_typings, None);
            check("m4", &main, &m4, Some(true));
        };

        test(false);
        test(true);
    }

    #[test]
    fn test_classic_plus_base_url() {
        let ref arena = AllArenasHarness::default();
        let test = |has_directory_exists: bool| {
            let main = FileBuilder::default()
                .name("/root/a/b/main.ts")
                .build()
                .unwrap();
            let m1 = FileBuilder::default()
                .name("/root/x/m1.ts")
                .build()
                .unwrap();
            let m2 = FileBuilder::default().name("/m2.ts").build().unwrap();

            let options = arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module_resolution(ModuleResolutionKind::Classic)
                    .base_url("/root/x")
                    .jsx(JsxEmit::React)
                    .build()
                    .unwrap(),
            );
            let host = create_module_resolution_host(
                has_directory_exists,
                vec![main.clone(), m1.clone(), m2.clone()],
            );

            let check = |name: &str, caller: &File, expected: &File| {
                let result = resolve_module_name(
                    name,
                    &caller.name,
                    options.clone(),
                    &*host,
                    None,
                    None,
                    None,
                )
                .unwrap();
                check_resolved_module(
                    result.resolved_module.as_deref(),
                    Some(&create_resolved_module(&expected.name, None)),
                );
            };

            check("m1", &main, &m1);
            check("m2", &main, &m2);
        };

        test(false);
        test(true);
    }

    #[test]
    fn test_node_plus_base_url_plus_path_mappings() {
        let ref arena = AllArenasHarness::default();
        let test = |has_directory_exists: bool| {
            let main = FileBuilder::default()
                .name("/root/folder1/main.ts")
                .build()
                .unwrap();
            let file1 = FileBuilder::default()
                .name("/root/folder1/file1.ts")
                .build()
                .unwrap();
            let file2 = FileBuilder::default()
                .name("/root/generated/folder1/file2.ts")
                .build()
                .unwrap();
            let file3 = FileBuilder::default()
                .name("/root/generated/folder2/file3/index.d.ts")
                .build()
                .unwrap();
            let file4_typings = FileBuilder::default()
                .name("/root/generated/folder2/file4/package.json")
                .content(serde_json::json!({"typings": "dist/types.d.ts"}).to_string())
                .build()
                .unwrap();
            let file4 = FileBuilder::default()
                .name("/root/generated/folder2/file4/dist/types.d.ts")
                .build()
                .unwrap();
            let file5 = FileBuilder::default()
                .name("/root/someanotherfolder/file5/index.d.ts")
                .build()
                .unwrap();
            let file6 = FileBuilder::default()
                .name("/root/node_modules/file6.ts")
                .build()
                .unwrap();
            let host = create_module_resolution_host(
                has_directory_exists,
                vec![
                    file1.clone(),
                    file2.clone(),
                    file3.clone(),
                    file4.clone(),
                    file4_typings.clone(),
                    file5.clone(),
                    file6.clone(),
                ],
            );

            let options = arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module_resolution(ModuleResolutionKind::NodeJs)
                    .base_url("/root")
                    .jsx(JsxEmit::React)
                    .paths(HashMap::from_iter(
                        [
                            ("*", vec!["*", "generated/*"]),
                            ("somefolder/*", vec!["someanotherfolder/*"]),
                            ("/rooted/*", vec!["generated/*"]),
                        ]
                        .owned(),
                    ))
                    .build()
                    .unwrap(),
            );

            let check = |name: &str,
                         expected: &File,
                         expected_failed_lookups: &[&str],
                         is_external_library_import: Option<bool>| {
                let is_external_library_import = is_external_library_import.unwrap_or(false);
                let result = resolve_module_name(
                    name,
                    &main.name,
                    options.clone(),
                    &*host,
                    None,
                    None,
                    None,
                )
                .unwrap();
                check_resolved_module_with_failed_lookup_locations(
                    &result,
                    &create_resolved_module(&expected.name, Some(is_external_library_import)),
                    expected_failed_lookups,
                );
            };

            check("folder1/file1", &file1, &[], None);
            check(
                "folder1/file2",
                &file2,
                &[
                    "/root/folder1/file2.ts",
                    "/root/folder1/file2.tsx",
                    "/root/folder1/file2.d.ts",
                    "/root/folder1/file2/package.json",
                    "/root/folder1/file2/index.ts",
                    "/root/folder1/file2/index.tsx",
                    "/root/folder1/file2/index.d.ts",
                ],
                None,
            );
            check("/rooted/folder1/file2", &file2, &[], None);
            check(
                "folder2/file3",
                &file3,
                &[
                    "/root/folder2/file3.ts",
                    "/root/folder2/file3.tsx",
                    "/root/folder2/file3.d.ts",
                    "/root/folder2/file3/package.json",
                    "/root/folder2/file3/index.ts",
                    "/root/folder2/file3/index.tsx",
                    "/root/folder2/file3/index.d.ts",
                    "/root/generated/folder2/file3.ts",
                    "/root/generated/folder2/file3.tsx",
                    "/root/generated/folder2/file3.d.ts",
                    "/root/generated/folder2/file3/package.json",
                    "/root/generated/folder2/file3/index.ts",
                    "/root/generated/folder2/file3/index.tsx",
                ],
                None,
            );
            check(
                "folder2/file4",
                &file4,
                &[
                    "/root/folder2/file4.ts",
                    "/root/folder2/file4.tsx",
                    "/root/folder2/file4.d.ts",
                    "/root/folder2/file4/package.json",
                    "/root/folder2/file4/index.ts",
                    "/root/folder2/file4/index.tsx",
                    "/root/folder2/file4/index.d.ts",
                    "/root/generated/folder2/file4.ts",
                    "/root/generated/folder2/file4.tsx",
                    "/root/generated/folder2/file4.d.ts",
                ],
                None,
            );
            check(
                "somefolder/file5",
                &file5,
                &[
                    "/root/someanotherfolder/file5.ts",
                    "/root/someanotherfolder/file5.tsx",
                    "/root/someanotherfolder/file5.d.ts",
                    "/root/someanotherfolder/file5/package.json",
                    "/root/someanotherfolder/file5/index.ts",
                    "/root/someanotherfolder/file5/index.tsx",
                ],
                None,
            );
            check(
                "file6",
                &file6,
                &[
                    "/root/file6.ts",
                    "/root/file6.tsx",
                    "/root/file6.d.ts",
                    "/root/file6/package.json",
                    "/root/file6/index.ts",
                    "/root/file6/index.tsx",
                    "/root/file6/index.d.ts",
                    "/root/generated/file6.ts",
                    "/root/generated/file6.tsx",
                    "/root/generated/file6.d.ts",
                    "/root/generated/file6/package.json",
                    "/root/generated/file6/index.ts",
                    "/root/generated/file6/index.tsx",
                    "/root/generated/file6/index.d.ts",
                    "/root/folder1/node_modules/file6/package.json",
                    "/root/folder1/node_modules/file6.ts",
                    "/root/folder1/node_modules/file6.tsx",
                    "/root/folder1/node_modules/file6.d.ts",
                    "/root/folder1/node_modules/file6/index.ts",
                    "/root/folder1/node_modules/file6/index.tsx",
                    "/root/folder1/node_modules/file6/index.d.ts",
                    "/root/folder1/node_modules/@types/file6/package.json",
                    "/root/folder1/node_modules/@types/file6.d.ts",
                    "/root/folder1/node_modules/@types/file6/index.d.ts",
                    "/root/node_modules/file6/package.json",
                ],
                Some(true),
            );
        };

        test(false);
        test(true);
    }

    #[test]
    fn test_classic_plus_base_url_plus_path_mappings() {
        let ref arena = AllArenasHarness::default();
        let test = |has_directory_exists: bool| {
            let main = FileBuilder::default()
                .name("/root/folder1/main.ts")
                .build()
                .unwrap();

            let file1 = FileBuilder::default()
                .name("/root/folder1/file1.ts")
                .build()
                .unwrap();
            let file2 = FileBuilder::default()
                .name("/root/generated/folder1/file2.ts")
                .build()
                .unwrap();
            let file3 = FileBuilder::default()
                .name("/folder1/file3.ts")
                .build()
                .unwrap();
            let host = create_module_resolution_host(
                has_directory_exists,
                vec![file1.clone(), file2.clone(), file3.clone()],
            );

            let options = arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module_resolution(ModuleResolutionKind::Classic)
                    .base_url("/root")
                    .jsx(JsxEmit::React)
                    .paths(HashMap::from_iter(
                        [
                            ("*", vec!["*", "generated/*"]),
                            ("somefolder/*", vec!["someanotherfolder/*"]),
                            ("/rooted/*", vec!["generated/*"]),
                        ]
                        .owned(),
                    ))
                    .build()
                    .unwrap(),
            );

            let check = |name: &str, expected: &File, expected_failed_lookups: &[&str]| {
                let result = resolve_module_name(
                    name,
                    &main.name,
                    options.clone(),
                    &*host,
                    None,
                    None,
                    None,
                )
                .unwrap();
                check_resolved_module_with_failed_lookup_locations(
                    &result,
                    &create_resolved_module(&expected.name, None),
                    expected_failed_lookups,
                );
            };

            check("folder1/file1", &file1, &[]);
            check(
                "folder1/file2",
                &file2,
                &[
                    "/root/folder1/file2.ts",
                    "/root/folder1/file2.tsx",
                    "/root/folder1/file2.d.ts",
                ],
            );
            check("/rooted/folder1/file2", &file2, &[]);
            check(
                "folder1/file3",
                &file3,
                &[
                    "/root/folder1/file3.ts",
                    "/root/folder1/file3.tsx",
                    "/root/folder1/file3.d.ts",
                    "/root/generated/folder1/file3.ts",
                    "/root/generated/folder1/file3.tsx",
                    "/root/generated/folder1/file3.d.ts",
                    "/root/folder1/folder1/file3.ts",
                    "/root/folder1/folder1/file3.tsx",
                    "/root/folder1/folder1/file3.d.ts",
                    "/root/folder1/file3.ts",
                    "/root/folder1/file3.tsx",
                    "/root/folder1/file3.d.ts",
                ],
            );
        };

        test(false);
    }

    #[test]
    fn test_node_plus_root_dirs() {
        let ref arena = AllArenasHarness::default();
        let test = |has_directory_exists: bool| {
            let file1 = FileBuilder::default()
                .name("/root/folder1/file1.ts")
                .build()
                .unwrap();
            let file1_1 = FileBuilder::default()
                .name("/root/folder1/file1_1/index.d.ts")
                .build()
                .unwrap();
            let file2 = FileBuilder::default()
                .name("/root/generated/folder1/file2.ts")
                .build()
                .unwrap();
            let file3 = FileBuilder::default()
                .name("/root/generated/folder2/file3.ts")
                .build()
                .unwrap();
            let host = create_module_resolution_host(
                has_directory_exists,
                vec![file1.clone(), file1_1.clone(), file2.clone(), file3.clone()],
            );

            let options = arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module_resolution(ModuleResolutionKind::NodeJs)
                    .root_dirs(["/root", "/root/generated/"].owned())
                    .build()
                    .unwrap(),
            );

            let check = |name: &str,
                         container: &File,
                         expected: &File,
                         expected_failed_lookups: &[&str]| {
                let result = resolve_module_name(
                    name,
                    &container.name,
                    options.clone(),
                    &*host,
                    None,
                    None,
                    None,
                )
                .unwrap();
                check_resolved_module_with_failed_lookup_locations(
                    &result,
                    &create_resolved_module(&expected.name, None),
                    expected_failed_lookups,
                );
            };

            check(
                "./file2",
                &file1,
                &file2,
                &[
                    "/root/folder1/file2.ts",
                    "/root/folder1/file2.tsx",
                    "/root/folder1/file2.d.ts",
                    "/root/folder1/file2/package.json",
                    "/root/folder1/file2/index.ts",
                    "/root/folder1/file2/index.tsx",
                    "/root/folder1/file2/index.d.ts",
                ],
            );
            check(
                "../folder1/file1",
                &file3,
                &file1,
                &[
                    "/root/generated/folder1/file1.ts",
                    "/root/generated/folder1/file1.tsx",
                    "/root/generated/folder1/file1.d.ts",
                    "/root/generated/folder1/file1/package.json",
                    "/root/generated/folder1/file1/index.ts",
                    "/root/generated/folder1/file1/index.tsx",
                    "/root/generated/folder1/file1/index.d.ts",
                ],
            );
            check(
                "../folder1/file1_1",
                &file3,
                &file1_1,
                &[
                    "/root/generated/folder1/file1_1.ts",
                    "/root/generated/folder1/file1_1.tsx",
                    "/root/generated/folder1/file1_1.d.ts",
                    "/root/generated/folder1/file1_1/package.json",
                    "/root/generated/folder1/file1_1/index.ts",
                    "/root/generated/folder1/file1_1/index.tsx",
                    "/root/generated/folder1/file1_1/index.d.ts",
                    "/root/folder1/file1_1.ts",
                    "/root/folder1/file1_1.tsx",
                    "/root/folder1/file1_1.d.ts",
                    "/root/folder1/file1_1/package.json",
                    "/root/folder1/file1_1/index.ts",
                    "/root/folder1/file1_1/index.tsx",
                ],
            );
        };

        test(false);
        test(true);
    }

    #[test]
    fn test_classic_plus_root_dirs() {
        let ref arena = AllArenasHarness::default();
        let test = |has_directory_exists: bool| {
            let file1 = FileBuilder::default()
                .name("/root/folder1/file1.ts")
                .build()
                .unwrap();
            let file2 = FileBuilder::default()
                .name("/root/generated/folder1/file2.ts")
                .build()
                .unwrap();
            let file3 = FileBuilder::default()
                .name("/root/generated/folder2/file3.ts")
                .build()
                .unwrap();
            let file4 = FileBuilder::default()
                .name("/folder1/file1_1.ts")
                .build()
                .unwrap();
            let host = create_module_resolution_host(
                has_directory_exists,
                vec![file1.clone(), file2.clone(), file3.clone(), file4.clone()],
            );
            let options = arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module_resolution(ModuleResolutionKind::Classic)
                    .jsx(JsxEmit::React)
                    .root_dirs(["/root", "/root/generated/"].owned())
                    .build()
                    .unwrap(),
            );

            let check = |name: &str,
                         container: &File,
                         expected: &File,
                         expected_failed_lookups: &[&str]| {
                let result = resolve_module_name(
                    name,
                    &container.name,
                    options.clone(),
                    &*host,
                    None,
                    None,
                    None,
                )
                .unwrap();
                check_resolved_module_with_failed_lookup_locations(
                    &result,
                    &create_resolved_module(&expected.name, None),
                    expected_failed_lookups,
                );
            };

            check(
                "./file2",
                &file1,
                &file2,
                &[
                    "/root/folder1/file2.ts",
                    "/root/folder1/file2.tsx",
                    "/root/folder1/file2.d.ts",
                ],
            );
            check(
                "../folder1/file1",
                &file3,
                &file1,
                &[
                    "/root/generated/folder1/file1.ts",
                    "/root/generated/folder1/file1.tsx",
                    "/root/generated/folder1/file1.d.ts",
                ],
            );
            check(
                "folder1/file1_1",
                &file3,
                &file4,
                &[
                    "/root/generated/folder2/folder1/file1_1.ts",
                    "/root/generated/folder2/folder1/file1_1.tsx",
                    "/root/generated/folder2/folder1/file1_1.d.ts",
                    "/root/generated/folder1/file1_1.ts",
                    "/root/generated/folder1/file1_1.tsx",
                    "/root/generated/folder1/file1_1.d.ts",
                    "/root/folder1/file1_1.ts",
                    "/root/folder1/file1_1.tsx",
                    "/root/folder1/file1_1.d.ts",
                ],
            );
        };

        test(false);
    }

    #[test]
    fn test_nested_node_module() {
        let ref arena = AllArenasHarness::default();
        let test = |has_directory_exists: bool| {
            let app = FileBuilder::default()
                .name("/root/src/app.ts")
                .build()
                .unwrap();
            let libs_package = FileBuilder::default()
                .name("/root/src/libs/guid/package.json")
                .content(serde_json::json!({"typings": "dist/guid.d.ts"}).to_string())
                .build()
                .unwrap();
            let libs_typings = FileBuilder::default()
                .name("/root/src/libs/guid/dist/guid.d.ts")
                .build()
                .unwrap();
            let host = create_module_resolution_host(
                has_directory_exists,
                vec![app.clone(), libs_package.clone(), libs_typings.clone()],
            );

            let options = arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module_resolution(ModuleResolutionKind::NodeJs)
                    .base_url("/root")
                    .paths(HashMap::from_iter(
                        [("libs/guid", ["src/libs/guid"])].owned(),
                    ))
                    .build()
                    .unwrap(),
            );

            let result = resolve_module_name(
                "libs/guid",
                &app.name,
                options.clone(),
                &*host,
                None,
                None,
                None,
            )
            .unwrap();
            check_resolved_module_with_failed_lookup_locations(
                &result,
                &create_resolved_module(&libs_typings.name, None),
                &[
                    "/root/src/libs/guid.ts",
                    "/root/src/libs/guid.tsx",
                    "/root/src/libs/guid.d.ts",
                ],
            );
        };

        test(false);
        test(true);
    }
}

mod module_resolution_host_directory_exists {
    use typescript_rust::{
        not_implemented, resolve_module_name, CompilerOptionsBuilder, ModuleResolutionKind,
    };

    use super::*;

    #[test]
    fn test_no_file_exists_calls_if_containing_directory_is_missing() {
        let ref arena = AllArenasHarness::default();
        let host = DirectoryExistsModuleResolutionHost::new();

        let result = resolve_module_name(
            "someName",
            "/a/b/c/d",
            arena.alloc_compiler_options(
                CompilerOptionsBuilder::default()
                    .module_resolution(ModuleResolutionKind::NodeJs)
                    .build()
                    .unwrap(),
            ),
            &host,
            None,
            None,
            None,
            arena,
        )
        .unwrap();
        assert_that(&result.resolved_module).is_none();
    }

    #[derive(Trace, Finalize)]
    struct DirectoryExistsModuleResolutionHost;

    impl DirectoryExistsModuleResolutionHost {
        pub fn new() -> Self {
            Self
        }
    }

    impl ModuleResolutionHost for DirectoryExistsModuleResolutionHost {
        fn read_file(&self, _file_name: &str) -> io::Result<Option<String>> {
            not_implemented()
        }

        fn set_overriding_read_file(
            &self,
            _overriding_read_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn read_file_non_overridden(&self, _file_name: &str) -> io::Result<Option<String>> {
            unreachable!()
        }

        fn file_exists(&self, _file_name: &str) -> bool {
            not_implemented()
        }

        fn file_exists_non_overridden(&self, _file_name: &str) -> bool {
            unreachable!()
        }

        fn set_overriding_file_exists(
            &self,
            _overriding_file_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn directory_exists(&self, _directory_name: &str) -> Option<bool> {
            Some(false)
        }

        fn is_directory_exists_supported(&self) -> bool {
            true
        }

        fn set_overriding_directory_exists(
            &self,
            _overriding_directory_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_get_directories_supported(&self) -> bool {
            false
        }

        fn set_overriding_get_directories(
            &self,
            _overriding_get_directories: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_trace_supported(&self) -> bool {
            false
        }

        fn is_realpath_supported(&self) -> bool {
            false
        }

        fn set_overriding_realpath(
            &self,
            _overriding_realpath: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }
    }
}

mod type_reference_directive_resolution {
    use typescript_rust::{
        array_to_map, create_program, create_source_file, not_implemented,
        resolve_type_reference_directive, CompilerHost, CompilerOptions, CompilerOptionsBuilder,
        CreateProgramOptionsBuilder, MapOrDefault, Node, Owned, ResolvedProjectReference,
        ScriptTarget, SourceFileLike, StructureIsReused, VecExt,
    };

    use super::*;

    fn test_worker(
        has_directory_exists: bool,
        types_root: Option<&str>,
        type_directive: &str,
        primary: bool,
        initial_file: &File,
        target_file: &File,
        other_files: &[&File],
    ) {
        let ref arena = AllArenasHarness::default();
        let host = create_module_resolution_host(
            has_directory_exists,
            vec![initial_file.clone(), target_file.clone()]
                .and_extend(other_files.into_iter().map(|value| (*value).clone())),
        );
        let result = resolve_type_reference_directive(
            type_directive,
            Some(&initial_file.name),
            types_root.map_or_default(|types_root| {
                arena.alloc_compiler_options(
                    CompilerOptionsBuilder::default()
                        .type_roots([types_root].owned())
                        .build()
                        .unwrap(),
                )
            }),
            &*host,
            None,
            None,
            arena,
        )
        .unwrap();
        asserting("expected type directive to be resolved")
            .that(
                &result
                    .resolved_type_reference_directive
                    .as_ref()
                    .unwrap()
                    .resolved_file_name,
            )
            .is_some();
        asserting("unexpected result of type reference resolution")
            .that(
                result
                    .resolved_type_reference_directive
                    .as_ref()
                    .unwrap()
                    .resolved_file_name
                    .as_ref()
                    .unwrap(),
            )
            .is_equal_to(&target_file.name);
        asserting("unexpected 'primary' value")
            .that(
                &result
                    .resolved_type_reference_directive
                    .as_ref()
                    .unwrap()
                    .primary,
            )
            .is_equal_to(primary);
    }

    fn test(
        types_root: &str,
        type_directive: &str,
        primary: bool,
        initial_file: &File,
        target_file: &File,
        other_files: &[&File],
    ) {
        test_worker(
            false,
            Some(types_root),
            type_directive,
            primary,
            initial_file,
            target_file,
            other_files,
        )
    }

    #[test]
    fn test_can_be_resolved_from_primary_location() {
        let f1 = FileBuilder::default()
            .name("/root/src/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/src/types/lib/index.d.ts")
            .build()
            .unwrap();
        test("/root/src/types", "lib", true, &f1, &f2, &[]);

        let f1 = FileBuilder::default()
            .name("/root/src/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/src/types/lib/typings/lib.d.ts")
            .build()
            .unwrap();
        let package_file = FileBuilder::default()
            .name("/root/src/types/lib/package.json")
            .content(serde_json::json!({"types": "typings/lib.d.ts"}).to_string())
            .build()
            .unwrap();
        test("/root/src/types", "lib", true, &f1, &f2, &[&package_file]);

        let f1 = FileBuilder::default()
            .name("/root/src/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/src/node_modules/lib/index.d.ts")
            .build()
            .unwrap();
        test("/root/src/types", "lib", false, &f1, &f2, &[]);

        let f1 = FileBuilder::default()
            .name("/root/src/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/src/node_modules/lib/typings/lib.d.ts")
            .build()
            .unwrap();
        let package_file = FileBuilder::default()
            .name("/root/src/node_modules/lib/package.json")
            .content(serde_json::json!({"types": "typings/lib.d.ts"}).to_string())
            .build()
            .unwrap();
        test("/root/src/types", "lib", false, &f1, &f2, &[&package_file]);

        let f1 = FileBuilder::default()
            .name("/root/src/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/src/node_modules/@types/lib/index.d.ts")
            .build()
            .unwrap();
        test("/root/src/types", "lib", false, &f1, &f2, &[]);

        let f1 = FileBuilder::default()
            .name("/root/src/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/src/node_modules/@types/lib/typings/lib.d.ts")
            .build()
            .unwrap();
        let package_file = FileBuilder::default()
            .name("/root/src/node_modules/@types/lib/package.json")
            .content(serde_json::json!({"types": "typings/lib.d.ts"}).to_string())
            .build()
            .unwrap();
        test("/root/src/types", "lib", false, &f1, &f2, &[&package_file]);
    }

    #[test]
    fn test_can_be_resolved_from_secondary_location() {
        let f1 = FileBuilder::default()
            .name("/root/src/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/node_modules/lib.d.ts")
            .build()
            .unwrap();
        test("/root/src/types", "lib", false, &f1, &f2, &[]);

        let f1 = FileBuilder::default()
            .name("/root/src/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/node_modules/lib/index.d.ts")
            .build()
            .unwrap();
        test("/root/src/types", "lib", false, &f1, &f2, &[]);

        let f1 = FileBuilder::default()
            .name("/root/src/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/node_modules/lib/typings/lib.d.ts")
            .build()
            .unwrap();
        let package_file = FileBuilder::default()
            .name("/root/node_modules/lib/package.json")
            .content(serde_json::json!({"typings": "typings/lib.d.ts"}).to_string())
            .build()
            .unwrap();
        test("/root/src/types", "lib", false, &f1, &f2, &[&package_file]);

        let f1 = FileBuilder::default()
            .name("/root/src/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/node_modules/@types/lib/index.d.ts")
            .build()
            .unwrap();
        test("/root/src/types", "lib", false, &f1, &f2, &[]);

        let f1 = FileBuilder::default()
            .name("/root/src/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/node_modules/@types/lib/typings/lib.d.ts")
            .build()
            .unwrap();
        let package_file = FileBuilder::default()
            .name("/root/node_modules/@types/lib/package.json")
            .content(serde_json::json!({"typings": "typings/lib.d.ts"}).to_string())
            .build()
            .unwrap();
        test("/root/src/types", "lib", false, &f1, &f2, &[&package_file]);
    }

    #[test]
    fn test_primary_resolution_overrides_secondary_resolution() {
        let f1 = FileBuilder::default()
            .name("/root/src/a/b/c/app.ts")
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/src/types/lib/index.d.ts")
            .build()
            .unwrap();
        let f3 = FileBuilder::default()
            .name("/root/src/a/b/node_modules/lib.d.ts")
            .build()
            .unwrap();
        test("/root/src/types", "lib", true, &f1, &f2, &[&f3]);
    }

    #[test]
    fn test_reused_program_keeps_errors() {
        let f1 = FileBuilder::default()
            .name("/root/src/a/b/c/d/e/app.ts")
            .content(r#"/// <reference types="lib"/>"#)
            .build()
            .unwrap();
        let f2 = FileBuilder::default()
            .name("/root/src/a/b/c/d/node_modules/lib/index.d.ts")
            .content(r#"declare var x: number;"#)
            .build()
            .unwrap();
        let f3 = FileBuilder::default()
            .name("/root/src/a/b/c/d/f/g/app.ts")
            .content(r#"/// <reference types="lib"/>"#)
            .build()
            .unwrap();
        let f4 = FileBuilder::default()
            .name("/root/src/a/b/c/d/f/node_modules/lib/index.d.ts")
            .content(r#"declare var x: number;"#)
            .build()
            .unwrap();
        let files = vec![f1, f2, f3, f4];

        let names = files.iter().map(|file| file.name.clone()).collect_vec();
        let source_files = array_to_map(
            &files
                .iter()
                .map(|f| {
                    create_source_file(
                        &f.name,
                        f.content.clone().unwrap(),
                        ScriptTarget::ES2015,
                        None,
                        None,
                    )
                    .unwrap()
                })
                .collect_vec(),
            |f: &Id<Node>| Some(f.as_source_file().file_name().clone()),
            |f: &Id<Node>| f.clone(),
        );
        let compiler_host = ReusedProgramKeepsErrorsCompilerHost::new(source_files);
        let program1 = create_program(
            CreateProgramOptionsBuilder::default()
                .root_names(names.clone())
                .host(compiler_host.clone())
                .build()
                .unwrap(),
        )
        .unwrap();
        let diagnostics1: Vec<_> = program1.get_options_diagnostics(None).into();
        asserting("expected one diagnostic")
            .that(&diagnostics1)
            .has_length(1);

        let program2 = create_program(
            CreateProgramOptionsBuilder::default()
                .root_names(names.clone())
                .host(compiler_host.clone())
                .old_program(program1)
                .build()
                .unwrap(),
        )
        .unwrap();
        assert_that!(&program2.structure_is_reused()).is_equal_to(&StructureIsReused::Completely);
        let diagnostics2: Vec<_> = program2.get_options_diagnostics(None).into();
        asserting("expected one diagnostic")
            .that(&diagnostics2)
            .has_length(1);
    }

    #[derive(Trace, Finalize)]
    struct ReusedProgramKeepsErrorsCompilerHost {
        source_files: HashMap<String, Id<Node>>,
    }

    impl ReusedProgramKeepsErrorsCompilerHost {
        pub fn new(
            source_files: HashMap<String, Id<Node>>,
            arena: &impl HasArena,
        ) -> Id<Box<dyn CompilerHost>> {
            arena.alloc_compiler_host(Box::new(Self { source_files }))
        }
    }

    impl CompilerHost for ReusedProgramKeepsErrorsCompilerHost {
        fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
            self
        }

        fn get_source_file(
            &self,
            file_name: &str,
            _language_version: ScriptTarget,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _should_create_new_source_file: Option<bool>,
        ) -> io::Result<Option<Id<Node /*SourceFile*/>>> {
            Ok(self.source_files.get(file_name).cloned())
        }

        fn get_default_lib_file_name(&self, _options: &CompilerOptions) -> io::Result<String> {
            Ok("lib.d.ts".to_owned())
        }

        fn write_file(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Id<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            not_implemented()
        }

        fn write_file_non_overridden(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Id<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            unreachable!()
        }

        fn is_write_file_supported(&self) -> bool {
            true
        }

        fn set_overriding_write_file(
            &self,
            _overriding_write_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn get_current_directory(&self) -> io::Result<String> {
            Ok("/".to_owned())
        }

        fn get_canonical_file_name(&self, f: &str) -> String {
            f.to_lowercase()
        }

        fn get_new_line(&self) -> String {
            "\r\n".to_owned()
        }

        fn use_case_sensitive_file_names(&self) -> bool {
            false
        }

        fn is_read_directory_implemented(&self) -> bool {
            false
        }

        fn is_resolve_module_names_supported(&self) -> bool {
            false
        }

        fn is_resolve_type_reference_directives_supported(&self) -> bool {
            false
        }

        fn is_on_release_old_source_file_supported(&self) -> bool {
            false
        }

        fn is_on_release_parsed_command_line_supported(&self) -> bool {
            false
        }

        fn is_create_directory_supported(&self) -> bool {
            false
        }

        fn set_overriding_create_directory(
            &self,
            _overriding_create_directory: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_get_source_file_by_path_supported(&self) -> bool {
            false
        }

        fn is_get_parsed_command_line_supported(&self) -> bool {
            false
        }
    }

    impl ModuleResolutionHost for ReusedProgramKeepsErrorsCompilerHost {
        fn file_exists(&self, file_name: &str) -> bool {
            self.source_files.contains_key(file_name)
        }

        fn file_exists_non_overridden(&self, _file_name: &str) -> bool {
            unreachable!()
        }

        fn set_overriding_file_exists(
            &self,
            _overriding_file_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_get_directories_supported(&self) -> bool {
            true
        }

        fn get_directories(&self, _path: &str) -> Option<Vec<String>> {
            Some(Default::default())
        }

        fn set_overriding_get_directories(
            &self,
            _overriding_get_directories: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
            let file = self.source_files.get(file_name);
            Ok(file.map(|file| file.as_source_file().text().clone()))
        }

        fn set_overriding_read_file(
            &self,
            _overriding_read_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
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
            _overriding_directory_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_realpath_supported(&self) -> bool {
            false
        }

        fn set_overriding_realpath(
            &self,
            _overriding_realpath: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }
    }

    #[test]
    fn test_modules_in_the_same_d_ts_file_are_preferred_to_external_files() {
        let f = FileBuilder::default()
            .name("/a/b/c/c/app.d.ts")
            .content(
                r#"
                declare module "fs" {
                    export interface Stat { id: number }
                }
                declare module "fs-client" {
                    import { Stat } from "fs";
                    export function foo(): Stat;
                }"#,
            )
            .build()
            .unwrap();
        let file = create_source_file(
            &f.name,
            f.content.clone().unwrap(),
            ScriptTarget::ES2015,
            None,
            None,
        )
        .unwrap();
        let compiler_host = ModulesInTheSameKeepsErrorsCompilerHost::new(file);
        create_program(
            CreateProgramOptionsBuilder::default()
                .root_names([&*f.name].owned())
                .host(compiler_host.clone())
                .build()
                .unwrap(),
        )
        .unwrap();
    }

    #[derive(Trace, Finalize)]
    struct ModulesInTheSameKeepsErrorsCompilerHost {
        file: Id<Node /*SourceFile*/>,
    }

    impl ModulesInTheSameKeepsErrorsCompilerHost {
        pub fn new(file: Id<Node>, arena: &impl HasArena) -> Id<Box<dyn CompilerHost>> {
            arena.alloc_compiler_host(Box::new(Self { file }))
        }
    }

    impl CompilerHost for ModulesInTheSameKeepsErrorsCompilerHost {
        fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
            self
        }

        fn get_source_file(
            &self,
            file_name: &str,
            _language_version: ScriptTarget,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _should_create_new_source_file: Option<bool>,
        ) -> io::Result<Option<Id<Node /*SourceFile*/>>> {
            Ok(if file_name == &*self.file.as_source_file().file_name() {
                Some(self.file.clone())
            } else {
                None
            })
        }

        fn get_default_lib_file_name(&self, _options: &CompilerOptions) -> io::Result<String> {
            Ok("lib.d.ts".to_owned())
        }

        fn write_file(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Id<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            not_implemented()
        }

        fn write_file_non_overridden(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Id<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            unreachable!()
        }

        fn is_write_file_supported(&self) -> bool {
            true
        }

        fn set_overriding_write_file(
            &self,
            _overriding_write_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn get_current_directory(&self) -> io::Result<String> {
            Ok("/".to_owned())
        }

        fn get_canonical_file_name(&self, f: &str) -> String {
            f.to_lowercase()
        }

        fn get_new_line(&self) -> String {
            "\r\n".to_owned()
        }

        fn use_case_sensitive_file_names(&self) -> bool {
            false
        }

        fn is_read_directory_implemented(&self) -> bool {
            false
        }

        fn resolve_module_names(
            &self,
            _module_names: &[String],
            _containing_file: &str,
            _reused_names: Option<&[String]>,
            _redirected_reference: Option<&ResolvedProjectReference>,
            _options: &CompilerOptions,
            _containing_source_file: Option<&Node /*SourceFile*/>,
        ) -> Option<Vec<Option<ResolvedModuleFull>>> {
            not_implemented()
        }

        fn is_resolve_module_names_supported(&self) -> bool {
            true
        }

        fn is_resolve_type_reference_directives_supported(&self) -> bool {
            false
        }

        fn is_on_release_old_source_file_supported(&self) -> bool {
            false
        }

        fn is_on_release_parsed_command_line_supported(&self) -> bool {
            false
        }

        fn is_create_directory_supported(&self) -> bool {
            false
        }

        fn set_overriding_create_directory(
            &self,
            _overriding_create_directory: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_get_source_file_by_path_supported(&self) -> bool {
            false
        }

        fn is_get_parsed_command_line_supported(&self) -> bool {
            false
        }
    }

    impl ModuleResolutionHost for ModulesInTheSameKeepsErrorsCompilerHost {
        fn file_exists(&self, file_name: &str) -> bool {
            file_name == &*self.file.as_source_file().file_name()
        }

        fn file_exists_non_overridden(&self, _file_name: &str) -> bool {
            unreachable!()
        }

        fn set_overriding_file_exists(
            &self,
            _overriding_file_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_get_directories_supported(&self) -> bool {
            true
        }

        fn get_directories(&self, _path: &str) -> Option<Vec<String>> {
            Some(Default::default())
        }

        fn set_overriding_get_directories(
            &self,
            _overriding_get_directories: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
            Ok(if file_name == &*self.file.as_source_file().file_name() {
                Some(self.file.as_source_file().text().clone())
            } else {
                None
            })
        }

        fn set_overriding_read_file(
            &self,
            _overriding_read_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
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
            _overriding_directory_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_realpath_supported(&self) -> bool {
            false
        }

        fn set_overriding_realpath(
            &self,
            _overriding_realpath: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }
    }

    #[test]
    fn test_modules_in_ts_file_are_not_checked_in_the_same_file() {
        let f = FileBuilder::default()
            .name("/a/b/c/c/app.ts")
            .content(
                r#"
                declare module "fs" {
                    export interface Stat { id: number }
                }
                declare module "fs-client" {
                    import { Stat } from "fs";
                    export function foo(): Stat;
                }"#,
            )
            .build()
            .unwrap();
        let file = create_source_file(
            &f.name,
            f.content.clone().unwrap(),
            ScriptTarget::ES2015,
            None,
            None,
        )
        .unwrap();
        let compiler_host = ModulesInTsFileCompilerHost::new(file);
        create_program(
            CreateProgramOptionsBuilder::default()
                .root_names([&*f.name].owned())
                .host(compiler_host.clone())
                .build()
                .unwrap(),
        )
        .unwrap();
    }

    #[derive(Trace, Finalize)]
    struct ModulesInTsFileCompilerHost {
        file: Id<Node /*SourceFile*/>,
    }

    impl ModulesInTsFileCompilerHost {
        pub fn new(file: Id<Node>, arena: &impl HasArena) -> Id<Box<dyn CompilerHost>> {
            arena.alloc_compiler_host(Box::new(Self { file }))
        }
    }

    impl CompilerHost for ModulesInTsFileCompilerHost {
        fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
            self
        }

        fn get_source_file(
            &self,
            file_name: &str,
            _language_version: ScriptTarget,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _should_create_new_source_file: Option<bool>,
        ) -> io::Result<Option<Id<Node /*SourceFile*/>>> {
            Ok(if file_name == &*self.file.as_source_file().file_name() {
                Some(self.file.clone())
            } else {
                None
            })
        }

        fn get_default_lib_file_name(&self, _options: &CompilerOptions) -> io::Result<String> {
            Ok("lib.d.ts".to_owned())
        }

        fn write_file(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Id<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            not_implemented()
        }

        fn write_file_non_overridden(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Id<Node /*SourceFile*/>]>,
        ) -> io::Result<()> {
            unreachable!()
        }

        fn is_write_file_supported(&self) -> bool {
            true
        }

        fn set_overriding_write_file(
            &self,
            _overriding_write_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn get_current_directory(&self) -> io::Result<String> {
            Ok("/".to_owned())
        }

        fn get_canonical_file_name(&self, f: &str) -> String {
            f.to_lowercase()
        }

        fn get_new_line(&self) -> String {
            "\r\n".to_owned()
        }

        fn use_case_sensitive_file_names(&self) -> bool {
            false
        }

        fn is_read_directory_implemented(&self) -> bool {
            false
        }

        fn resolve_module_names(
            &self,
            module_names: &[String],
            _containing_file: &str,
            _reused_names: Option<&[String]>,
            _redirected_reference: Option<&ResolvedProjectReference>,
            _options: &CompilerOptions,
            _containing_source_file: Option<&Node /*SourceFile*/>,
        ) -> Option<Vec<Option<ResolvedModuleFull>>> {
            assert_that!(module_names).is_equal_to(&*["fs"].owned());
            Some(vec![None])
        }

        fn is_resolve_module_names_supported(&self) -> bool {
            true
        }

        fn is_resolve_type_reference_directives_supported(&self) -> bool {
            false
        }

        fn is_on_release_old_source_file_supported(&self) -> bool {
            false
        }

        fn is_on_release_parsed_command_line_supported(&self) -> bool {
            false
        }

        fn is_create_directory_supported(&self) -> bool {
            false
        }

        fn set_overriding_create_directory(
            &self,
            _overriding_create_directory: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_get_source_file_by_path_supported(&self) -> bool {
            false
        }

        fn is_get_parsed_command_line_supported(&self) -> bool {
            false
        }
    }

    impl ModuleResolutionHost for ModulesInTsFileCompilerHost {
        fn file_exists(&self, file_name: &str) -> bool {
            file_name == &*self.file.as_source_file().file_name()
        }

        fn file_exists_non_overridden(&self, _file_name: &str) -> bool {
            unreachable!()
        }

        fn set_overriding_file_exists(
            &self,
            _overriding_file_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_get_directories_supported(&self) -> bool {
            true
        }

        fn get_directories(&self, _path: &str) -> Option<Vec<String>> {
            Some(Default::default())
        }

        fn set_overriding_get_directories(
            &self,
            _overriding_get_directories: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
            Ok(if file_name == &*self.file.as_source_file().file_name() {
                Some(self.file.as_source_file().text().clone())
            } else {
                None
            })
        }

        fn set_overriding_read_file(
            &self,
            _overriding_read_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
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
            _overriding_directory_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn is_realpath_supported(&self) -> bool {
            false
        }

        fn set_overriding_realpath(
            &self,
            _overriding_realpath: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }
    }

    mod can_be_resolved_when_type_reference_directive_is_relative_and_in_a_sibling_folder {
        use once_cell::sync::Lazy;

        use super::*;

        static initial_file: Lazy<File> = Lazy::new(|| {
            FileBuilder::default()
                .name("/root/src/background/app.ts")
                .build()
                .unwrap()
        });
        static target_file: Lazy<File> = Lazy::new(|| {
            FileBuilder::default()
                .name("/root/src/typedefs/filesystem.d.ts")
                .build()
                .unwrap()
        });

        #[test]
        fn test_when_host_doesnt_have_directory_exists() {
            test_worker(
                false,
                None,
                "../typedefs/filesystem",
                false,
                &initial_file,
                &target_file,
                &[],
            );
        }

        #[test]
        fn test_when_host_has_directory_exists() {
            test_worker(
                true,
                None,
                "../typedefs/filesystem",
                false,
                &initial_file,
                &target_file,
                &[],
            );
        }
    }
}
