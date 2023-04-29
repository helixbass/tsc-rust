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

mod node_module_resolution_non_relative_paths {
    use super::*;

    use typescript_rust::{
        create_module_resolution_cache, node_module_name_resolver, resolve_module_name,
        CompilerOptionsBuilder, Extension, GetCanonicalFileName, ModuleResolutionKind,
        NonRelativeModuleNameResolutionCache,
    };

    #[test]
    fn test_computes_correct_common_prefix_for_module_name_cache() {
        let resolution_cache = create_module_resolution_cache(
            "/",
            Gc::new(Box::new(GetCanonicalFileNameNoop)),
            None,
            None,
            None,
        );
        let mut cache = resolution_cache.get_or_create_cache_for_module_name("a", None, None);
        cache.set(
            "/sub",
            Gc::new(ResolvedModuleWithFailedLookupLocations::new(
                Some(Gc::new(
                    ResolvedModuleFullBuilder::default()
                        .resolved_file_name("/sub/node_modules/a/index.ts")
                        .is_external_library_import(true)
                        .extension(Extension::Ts)
                        .build()
                        .unwrap(),
                )),
                Default::default(),
            )),
        );
        assert_that(&cache.get("/sub")).is_some();
        assert_that(&cache.get("/")).is_none();

        cache = resolution_cache.get_or_create_cache_for_module_name("b", None, None);
        cache.set(
            "/sub/dir/foo",
            Gc::new(ResolvedModuleWithFailedLookupLocations::new(
                Some(Gc::new(
                    ResolvedModuleFullBuilder::default()
                        .resolved_file_name("/sub/directory/node_modules/b/index.ts")
                        .is_external_library_import(true)
                        .extension(Extension::Ts)
                        .build()
                        .unwrap(),
                )),
                Default::default(),
            )),
        );
        assert_that(&cache.get("/sub/dir/foo")).is_some();
        assert_that(&cache.get("/sub/dir")).is_some();
        assert_that(&cache.get("/sub")).is_some();
        assert_that(&cache.get("/")).is_none();

        cache = resolution_cache.get_or_create_cache_for_module_name("c", None, None);
        cache.set(
            "/foo/bar",
            Gc::new(ResolvedModuleWithFailedLookupLocations::new(
                Some(Gc::new(
                    ResolvedModuleFullBuilder::default()
                        .resolved_file_name("/bar/node_modules/c/index.ts")
                        .is_external_library_import(true)
                        .extension(Extension::Ts)
                        .build()
                        .unwrap(),
                )),
                Default::default(),
            )),
        );
        assert_that(&cache.get("/foo/bar")).is_some();
        assert_that(&cache.get("/foo")).is_some();
        assert_that(&cache.get("/")).is_some();

        cache = resolution_cache.get_or_create_cache_for_module_name("d", None, None);
        cache.set(
            "/foo",
            Gc::new(ResolvedModuleWithFailedLookupLocations::new(
                Some(Gc::new(
                    ResolvedModuleFullBuilder::default()
                        .resolved_file_name("/foo/index.ts")
                        .is_external_library_import(true)
                        .extension(Extension::Ts)
                        .build()
                        .unwrap(),
                )),
                Default::default(),
            )),
        );
        assert_that(&cache.get("/foo")).is_some();
        assert_that(&cache.get("/")).is_none();

        cache = resolution_cache.get_or_create_cache_for_module_name("e", None, None);
        cache.set(
            "c:/foo",
            Gc::new(ResolvedModuleWithFailedLookupLocations::new(
                Some(Gc::new(
                    ResolvedModuleFullBuilder::default()
                        .resolved_file_name("d:/bar/node_modules/e/index.ts")
                        .is_external_library_import(true)
                        .extension(Extension::Ts)
                        .build()
                        .unwrap(),
                )),
                Default::default(),
            )),
        );
        assert_that(&cache.get("c:/foo")).is_some();
        assert_that(&cache.get("c:/")).is_some();
        assert_that(&cache.get("d:/")).is_none();

        cache = resolution_cache.get_or_create_cache_for_module_name("f", None, None);
        cache.set(
            "/foo/bar/baz",
            Gc::new(ResolvedModuleWithFailedLookupLocations::new(
                None,
                Default::default(),
            )),
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
            );
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
            );
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
            );
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
        let real_file_name = "/linked/index.d.ts";
        let symlink_file_name = "/app/node_modules/linked/index.d.ts";
        let host = create_module_resolution_host(
            true,
            vec![
                FileBuilder::default()
                    .name(real_file_name)
                    .symlinks([symlink_file_name.to_owned()])
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
            Gc::new(
                CompilerOptionsBuilder::default()
                    .preserve_symlinks(preserve_symlinks)
                    .build()
                    .unwrap(),
            ),
            &*host,
            None,
            None,
            None,
        );
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
        let host = create_module_resolution_host(
            true,
            vec![
                FileBuilder::default()
                    .name("/modules/a.ts")
                    .symlinks(["/sub/node_modules/a/index.ts".to_owned()])
                    .build()
                    .unwrap(),
                FileBuilder::default()
                    .name("/sub/node_modules/a/package.json")
                    .content("{\"version\": \"0.0.0\", \"main\": \"./index\"}")
                    .build()
                    .unwrap(),
            ],
        );
        let compiler_options = Gc::new(
            CompilerOptionsBuilder::default()
                .module_resolution(ModuleResolutionKind::NodeJs)
                .build()
                .unwrap(),
        );
        let cache = Gc::new(create_module_resolution_cache(
            "/",
            Gc::new(Box::new(GetCanonicalFileNameNoop)),
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
        );
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
        );
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
        );
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
        let cache = Gc::new(create_module_resolution_cache(
            "/",
            Gc::new(Box::new(GetCanonicalFileNameNoop)),
            None,
            None,
            None,
        ));
        let compiler_options = Gc::new(
            CompilerOptionsBuilder::default()
                .module_resolution(ModuleResolutionKind::NodeJs)
                .build()
                .unwrap(),
        );
        check_resolution(&resolve_module_name(
            "linked",
            "/app/src/app.ts",
            compiler_options.clone(),
            &*host,
            Some(cache.clone()),
            None,
            None,
        ));
        check_resolution(&resolve_module_name(
            "linked",
            "/app/lib/main.ts",
            compiler_options.clone(),
            &*host,
            Some(cache.clone()),
            None,
            None,
        ));
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
        let options = Gc::new(
            CompilerOptionsBuilder::default()
                .module(ModuleKind::CommonJS)
                .build()
                .unwrap(),
        );
        let host: Gc<Box<dyn CompilerHost>> =
            RelativeImportsCompilerHost::new(current_directory.to_owned(), files.clone());

        let program = create_program(
            CreateProgramOptionsBuilder::default()
                .root_names(root_files.owned())
                .options(options)
                .host(host)
                .build()
                .unwrap(),
        );

        assert_that(&*program.get_source_files()).has_length(expected_files_count);
        let syntactic_diagnostics = program.get_syntactic_diagnostics(None, None);
        asserting(&format!(
            "expect no syntactic diagnostics, got: {:?}",
            Compiler::minimal_diagnostics_to_string(&syntactic_diagnostics, None,)
        ))
        .that(&syntactic_diagnostics)
        .is_empty();
        let semantic_diagnostics = program.get_semantic_diagnostics(None, None);
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
        ) -> Gc<Box<dyn CompilerHost>> {
            Gc::new(Box::new(Self {
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
        ) -> Option<Gc<Node /*SourceFile*/>> {
            let ref path =
                normalize_path(&combine_paths(&self.current_directory, &[Some(file_name)]));
            let file = self.files.get(path);
            file.map(|file| {
                create_source_file(file_name, file.clone(), language_version, None, None)
            })
        }

        fn get_default_lib_file_name(&self, _options: &CompilerOptions) -> String {
            "lib.d.ts".to_owned()
        }

        fn write_file(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Gc<Node /*SourceFile*/>]>,
        ) {
            not_implemented()
        }

        fn write_file_non_overridden(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Gc<Node /*SourceFile*/>]>,
        ) {
            unreachable!()
        }

        fn is_write_file_supported(&self) -> bool {
            true
        }

        fn set_overriding_write_file(
            &self,
            _overriding_write_file: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn get_current_directory(&self) -> String {
            self.current_directory.clone()
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
            _overriding_create_directory: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
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
            _overriding_get_directories: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
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
            _overriding_file_exists: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn read_file(&self, _file_name: &str) -> io::Result<Option<String>> {
            not_implemented()
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
            false
        }

        fn set_overriding_realpath(
            &self,
            _overriding_realpath: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
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
        normalize_path, not_implemented, sort_and_deduplicate_diagnostics, CompilerHost,
        CompilerOptions, CompilerOptionsBuilder, CreateProgramOptionsBuilder, Diagnostic,
        DiagnosticInterface, Diagnostics, ModuleKind, Node, Owned, Program, ScriptTarget, VecExt,
    };

    use super::*;
    use crate::{get_diagnostic_message_chain, get_diagnostic_of_file_from_program};

    thread_local! {
        static library: OnceCell<Gc<Node /*SourceFile*/>> = Default::default();
    }

    fn test(
        mut files: HashMap<String, String>,
        options: Gc<CompilerOptions>,
        current_directory: &str,
        use_case_sensitive_file_names: bool,
        root_files: &[&str],
        mut expected_diagnostics: impl FnMut(&Program) -> Vec<Gc<Diagnostic>>,
    ) {
        let get_canonical_file_name = create_get_canonical_file_name(use_case_sensitive_file_names);
        if !use_case_sensitive_file_names {
            let old_files = files.clone();
            files = Default::default();
            for (file_name, file) in old_files {
                files.insert(get_canonical_file_name(&file_name), file);
            }
        }

        let host: Gc<Box<dyn CompilerHost>> = FilesWithDifferentCasingCompilerHost::new(
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
        );
        let diagnostics: Vec<_> = sort_and_deduplicate_diagnostics(
            &program
                .get_semantic_diagnostics(None, None)
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
        ) -> Gc<Box<dyn CompilerHost>> {
            Gc::new(Box::new(Self {
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
        ) -> Option<Gc<Node /*SourceFile*/>> {
            if file_name == "lib.d.ts" {
                return Some(library.with(|library_| {
                    library_
                        .get_or_init(|| {
                            create_source_file(
                                "lib.d.ts",
                                "".to_owned(),
                                ScriptTarget::ES5,
                                None,
                                None,
                            )
                        })
                        .clone()
                }));
            }
            let ref path = self.get_canonical_file_name(&normalize_path(&combine_paths(
                &self.current_directory,
                &[Some(file_name)],
            )));
            let file = self.files.get(path);
            file.map(|file| {
                create_source_file(file_name, file.clone(), language_version, None, None)
            })
        }

        fn get_default_lib_file_name(&self, _options: &CompilerOptions) -> String {
            "lib.d.ts".to_owned()
        }

        fn write_file(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Gc<Node /*SourceFile*/>]>,
        ) {
            not_implemented()
        }

        fn write_file_non_overridden(
            &self,
            _file_name: &str,
            _data: &str,
            _write_byte_order_mark: bool,
            _on_error: Option<&mut dyn FnMut(&str)>,
            _source_files: Option<&[Gc<Node /*SourceFile*/>]>,
        ) {
            unreachable!()
        }

        fn is_write_file_supported(&self) -> bool {
            true
        }

        fn set_overriding_write_file(
            &self,
            _overriding_write_file: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn get_current_directory(&self) -> String {
            self.current_directory.clone()
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
            _overriding_create_directory: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
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
            _overriding_get_directories: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
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
            _overriding_file_exists: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }

        fn read_file(&self, _file_name: &str) -> io::Result<Option<String>> {
            not_implemented()
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
            false
        }

        fn set_overriding_realpath(
            &self,
            _overriding_realpath: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
        ) {
            unreachable!()
        }
    }

    #[test]
    fn test_should_succeed_when_the_same_file_is_referenced_using_absolute_and_relative_names() {
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("/a/b/c.ts", r#"/// <reference path="d.ts"/>"#),
                ("/a/b/d.ts", "var x"),
            ]
            .owned(),
        );
        test(
            files,
            Gc::new(
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
        let files: HashMap<String, String> = HashMap::from_iter(
            [
                ("/a/b/c.ts", r#"/// <reference path="D.ts"/>"#),
                ("/a/b/d.ts", "var x"),
            ]
            .owned(),
        );
        test(
            files,
            Gc::new(
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
                    );
                    *diagnostic.maybe_related_information_mut() = None;
                    diagnostic
                }]
            },
        );
    }
}
