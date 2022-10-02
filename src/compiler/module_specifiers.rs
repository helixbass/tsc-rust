use std::rc::Rc;

use crate::{
    is_external_module_augmentation, is_non_global_ambient_module, starts_with, CharacterCodes,
    CompilerOptions, ModulePath, ModuleSpecifierCache, ModuleSpecifierResolutionHost, Node,
    NodeFlags, NodeInterface, Path, Symbol, SymbolFlags, SymbolInterface, TypeChecker,
    UserPreferences, __String, get_text_of_identifier_or_literal, is_ambient_module,
    is_external_module_name_relative, is_module_block, is_module_declaration, is_source_file,
    map_defined, LiteralLikeNodeInterface,
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
    let decl = module_symbol
        .maybe_declarations()
        .as_ref()
        .and_then(|module_symbol_declarations| {
            module_symbol_declarations
                .into_iter()
                .find(|d| {
                    is_non_global_ambient_module(d)
                        && (!is_external_module_augmentation(d)
                            || !is_external_module_name_relative(
                                &get_text_of_identifier_or_literal(&d.as_module_declaration().name),
                            ))
                })
                .cloned()
        });
    if let Some(decl) = decl {
        return Some(
            decl.as_module_declaration()
                .name
                .as_string_literal()
                .text()
                .clone(),
        );
    }

    let ambient_module_declare_candidates = map_defined(
        module_symbol.maybe_declarations().as_ref(),
        |d: &Rc<Node>, _| -> Option<Rc<Node>> {
            if !is_module_declaration(d) {
                return None;
            }
            let top_namespace = get_top_namespace(d);
            if !(matches!(
                top_namespace.maybe_parent().as_ref(),
                Some(top_namespace_parent) if matches!(
                    top_namespace_parent.maybe_parent().as_ref(),
                    Some(top_namespace_parent_parent) if is_module_block(top_namespace_parent) &&
                        is_ambient_module(top_namespace_parent_parent) &&
                        is_source_file(&top_namespace_parent_parent.parent())
                )
            )) {
                return None;
            }
            let ref export_assignment = top_namespace
                .parent()
                .parent()
                .symbol()
                .maybe_exports()
                .as_ref()
                .and_then(|top_namespace_parent_parent_symbol_exports| {
                    (**top_namespace_parent_parent_symbol_exports)
                        .borrow()
                        .get(&__String::new("export=".to_owned()))
                        .cloned()
                })
                .and_then(|top_namespace_parent_parent_symbol_exports_got| {
                    top_namespace_parent_parent_symbol_exports_got.maybe_value_declaration()
                })
                .map(
                    |top_namespace_parent_parent_symbol_exports_got_value_declaration| {
                        top_namespace_parent_parent_symbol_exports_got_value_declaration
                            .as_export_assignment()
                            .expression
                            .clone()
                    },
                )?;
            let ref export_symbol = checker.get_symbol_at_location(export_assignment)?;
            let ref original_export_symbol = if export_symbol.flags().intersects(SymbolFlags::Alias)
            {
                checker.get_aliased_symbol(export_symbol)
            } else {
                export_symbol.clone()
            };
            if matches!(
                d.maybe_symbol().as_ref(),
                Some(d_symbol) if Rc::ptr_eq(
                    original_export_symbol,
                    d_symbol
                )
            ) {
                return Some(top_namespace.parent().parent());
            }
            None
        },
    );
    let ambient_module_declare = ambient_module_declare_candidates.get(0);
    if let Some(ambient_module_declare) = ambient_module_declare {
        return Some(
            ambient_module_declare
                .as_module_declaration()
                .name
                .as_string_literal()
                .text()
                .clone(),
        );
    }
    None
}

fn get_top_namespace(namespace_declaration: &Node /*ModuleDeclaration*/) -> Rc<Node> {
    let mut namespace_declaration = namespace_declaration.node_wrapper();
    while namespace_declaration
        .flags()
        .intersects(NodeFlags::NestedNamespace)
    {
        namespace_declaration = namespace_declaration.parent();
    }
    namespace_declaration
}
