use crate::{
    CompilerOptions, ModuleSpecifierResolutionHost, Node, Symbol, TypeChecker, UserPreferences,
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
    unimplemented!()
}
