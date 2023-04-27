#![cfg(test)]

use std::{collections::HashMap, io, rc::Rc};

use derive_builder::Builder;
use gc::{Finalize, Gc, Trace};
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
    expected_failed_lookup_locations: &[String],
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
    .is_equal_to(expected_failed_lookup_locations.to_owned());
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
        supported_ts_extensions_flat, Extension,
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
}
