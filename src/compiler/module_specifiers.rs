use crate::{
    starts_with, CharacterCodes, CompilerOptions, ModuleSpecifierResolutionHost, Node, Symbol,
    TypeChecker, UserPreferences,
};

pub fn get_module_specifiers(
    module_symbol: &Symbol,
    checker: &TypeChecker,
    compiler_options: &CompilerOptions,
    importing_source_file: &Node, /*SourceFile*/
    host: &dyn ModuleSpecifierResolutionHost,
    user_preferences: UserPreferences,
) -> Vec<String> {
    unimplemented!()
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
