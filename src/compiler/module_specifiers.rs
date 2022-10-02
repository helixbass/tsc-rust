use std::rc::Rc;

use crate::{
    starts_with, CharacterCodes, CompilerOptions, ModulePath, ModuleSpecifierCache,
    ModuleSpecifierResolutionHost, Node, Path, Symbol, TypeChecker, UserPreferences,
};

fn try_get_module_specifiers_from_cache_worker(
    module_symbol: &Symbol,
    importing_source_file: &Node, /*SourceFile*/
    host: &dyn ModuleSpecifierResolutionHost,
    user_preferences: &UserPreferences,
) -> (
    Option<Vec<String>>,
    Option<Rc<Node>>,
    Option<Vec<ModulePath>>,
    Option<Rc<dyn ModuleSpecifierCache>>,
) {
    unimplemented!()
}

pub fn get_module_specifiers(
    module_symbol: &Symbol,
    checker: &TypeChecker,
    compiler_options: &CompilerOptions,
    importing_source_file: &Node, /*SourceFile*/
    host: &dyn ModuleSpecifierResolutionHost,
    user_preferences: &UserPreferences,
) -> Vec<String> {
    get_module_specifiers_with_cache_info(
        module_symbol,
        checker,
        compiler_options,
        importing_source_file, /*SourceFile*/
        host,
        user_preferences,
    )
    .module_specifiers
}

pub fn get_module_specifiers_with_cache_info(
    module_symbol: &Symbol,
    checker: &TypeChecker,
    compiler_options: &CompilerOptions,
    importing_source_file: &Node, /*SourceFile*/
    host: &dyn ModuleSpecifierResolutionHost,
    user_preferences: &UserPreferences,
) -> ModuleSpecifiersWithCacheInfo {
    let mut computed_without_cache = false;
    let ambient = try_get_module_name_from_ambient_module(module_symbol, checker);
    if let Some(ambient) = ambient {
        return ModuleSpecifiersWithCacheInfo {
            module_specifiers: vec![ambient],
            computed_without_cache,
        };
    }

    let (specifiers, module_source_file, mut module_paths, cache) =
        try_get_module_specifiers_from_cache_worker(
            module_symbol,
            importing_source_file,
            host,
            user_preferences,
        );
    if let Some(specifiers) = specifiers {
        return ModuleSpecifiersWithCacheInfo {
            module_specifiers: specifiers,
            computed_without_cache,
        };
    }
    if module_source_file.is_none() {
        return ModuleSpecifiersWithCacheInfo {
            module_specifiers: vec![],
            computed_without_cache,
        };
    }
    let module_source_file = module_source_file.unwrap();

    computed_without_cache = true;
    let importing_source_file_as_source_file = importing_source_file.as_source_file();
    let module_source_file_as_source_file = module_source_file.as_source_file();
    let module_paths = module_paths.unwrap_or_else(|| {
        get_all_module_paths_worker(
            &importing_source_file_as_source_file.path(),
            &module_source_file_as_source_file.original_file_name(),
            host,
        )
    });
    let result = compute_module_specifiers(
        &module_paths,
        compiler_options,
        importing_source_file,
        host,
        user_preferences,
    );
    if let Some(cache) = cache {
        cache.set(
            &importing_source_file_as_source_file.path(),
            &module_source_file_as_source_file.path(),
            user_preferences,
            &module_paths,
            &result,
        );
    }
    ModuleSpecifiersWithCacheInfo {
        module_specifiers: result,
        computed_without_cache,
    }
}

fn compute_module_specifiers(
    module_paths: &[ModulePath],
    compiler_options: &CompilerOptions,
    importing_source_file: &Node, /*SourceFile*/
    host: &dyn ModuleSpecifierResolutionHost,
    user_preferences: &UserPreferences,
) -> Vec<String> {
    unimplemented!()
}

pub struct ModuleSpecifiersWithCacheInfo {
    pub module_specifiers: Vec<String>,
    pub computed_without_cache: bool,
}

pub fn count_path_components(path: &str) -> usize {
    let mut count = 0;
    for ch in path
        .chars()
        .skip(if starts_with(path, "./") { 2 } else { 0 })
    {
        if ch == CharacterCodes::slash {
            count += 1;
        }
    }
    count
}

fn get_all_module_paths_worker(
    importing_file_name: &Path,
    imported_file_name: &str,
    host: &dyn ModuleSpecifierResolutionHost,
) -> Vec<ModulePath> {
    unimplemented!()
}

fn try_get_module_name_from_ambient_module(
    module_symbol: &Symbol,
    checker: &TypeChecker,
) -> Option<String> {
    unimplemented!()
}
