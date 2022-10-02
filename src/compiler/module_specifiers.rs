use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    append, comparison_to_ordering, contains_ignored_path, create_get_canonical_file_name,
    ensure_trailing_directory_separator, every, first_defined, for_each,
    for_each_ancestor_directory, get_directory_path, get_emit_module_resolution_kind,
    get_module_name_string_literal_at, get_normalized_absolute_path,
    get_relative_path_from_directory, get_source_file_of_module, has_js_file_extension,
    host_get_canonical_file_name, is_external_module_augmentation, is_non_global_ambient_module,
    maybe_for_each, path_contains_node_modules, path_is_bare_specifier, path_is_relative,
    resolve_path, some, starts_with, starts_with_directory, to_path, CharacterCodes, Comparison,
    CompilerOptions, Debug_, FileIncludeKind, FileIncludeReason, ModulePath, ModuleResolutionKind,
    ModuleSpecifierCache, ModuleSpecifierResolutionHost, Node, NodeFlags, NodeInterface, Path,
    Symbol, SymbolFlags, SymbolInterface, TypeChecker, UserPreferences, __String,
    get_text_of_identifier_or_literal, is_ambient_module, is_external_module_name_relative,
    is_module_block, is_module_declaration, is_source_file, map_defined, LiteralLikeNodeInterface,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum RelativePreference {
    Relative,
    NonRelative,
    Shortest,
    ExternalNonRelative,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Ending {
    Minimal,
    Index,
    JsExtension,
}

struct Preferences {
    pub relative_preference: RelativePreference,
    pub ending: Ending,
}

fn get_preferences(
    host: &dyn ModuleSpecifierResolutionHost,
    user_preferences: &UserPreferences,
    compiler_options: &CompilerOptions,
    importing_source_file: &Node, /*SourceFile*/
) -> Preferences {
    let import_module_specifier_preference = user_preferences
        .import_module_specifier_preference
        .as_deref();
    let import_module_specifier_ending = user_preferences.import_module_specifier_ending.as_deref();
    Preferences {
        relative_preference: match import_module_specifier_preference {
            Some("relative") => RelativePreference::Relative,
            Some("non-relative") => RelativePreference::NonRelative,
            Some("project-relative") => RelativePreference::ExternalNonRelative,
            _ => RelativePreference::Shortest,
        },
        ending: get_ending(
            import_module_specifier_ending,
            importing_source_file,
            compiler_options,
            host,
        ),
    }
}

fn get_ending(
    import_module_specifier_ending: Option<&str>,
    importing_source_file: &Node, /*SourceFile*/
    compiler_options: &CompilerOptions,
    host: &dyn ModuleSpecifierResolutionHost,
) -> Ending {
    match import_module_specifier_ending {
        Some("minimal") => Ending::Minimal,
        Some("index") => Ending::Index,
        Some("js") => Ending::JsExtension,
        _ => {
            if uses_js_extensions_on_imports(importing_source_file)
                || is_format_requiring_extensions(
                    compiler_options,
                    &importing_source_file.as_source_file().path(),
                    host,
                )
            {
                Ending::JsExtension
            } else if get_emit_module_resolution_kind(compiler_options)
                != ModuleResolutionKind::NodeJs
            {
                Ending::Index
            } else {
                Ending::Minimal
            }
        }
    }
}

fn is_format_requiring_extensions(
    compiler_options: &CompilerOptions,
    importing_source_file_name: &Path,
    host: &dyn ModuleSpecifierResolutionHost,
) -> bool {
    unimplemented!()
}

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
    let module_source_file = get_source_file_of_module(module_symbol);
    if module_source_file.is_none() {
        return (None, None, None, None);
    }
    let module_source_file = module_source_file.unwrap();

    let cache = host.get_module_specifier_cache();
    let cached = cache.as_ref().and_then(|cache| {
        cache.get(
            &importing_source_file.as_source_file().path(),
            &module_source_file.as_source_file().path(),
            user_preferences,
        )
    });
    (
        cached
            .as_ref()
            .and_then(|cached| cached.module_specifiers.clone()),
        Some(module_source_file),
        cached
            .as_ref()
            .and_then(|cached| cached.module_paths.clone()),
        cache,
    )
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
    let importing_source_file_as_source_file = importing_source_file.as_source_file();
    let info = get_info(&importing_source_file_as_source_file.path(), host);
    let preferences = get_preferences(
        host,
        user_preferences,
        compiler_options,
        importing_source_file,
    );
    let existing_specifier = for_each(module_paths, |module_path: &ModulePath, _| {
        maybe_for_each(
            host.get_file_include_reasons().get(&to_path(
                &module_path.path,
                Some(&host.get_current_directory()),
                info.get_canonical_file_name,
            )),
            |reason: &FileIncludeReason, _| {
                if reason.kind() != FileIncludeKind::Import
                    || reason.as_referenced_file().file
                        != *importing_source_file_as_source_file.path()
                {
                    return None;
                }
                let specifier = get_module_name_string_literal_at(
                    importing_source_file_as_source_file,
                    reason.as_referenced_file().index,
                )
                .as_literal_like_node()
                .text()
                .clone();
                if preferences.relative_preference != RelativePreference::NonRelative
                    || !path_is_relative(&specifier)
                {
                    Some(specifier)
                } else {
                    None
                }
            },
        )
    });

    let imported_file_is_in_node_modules = some(
        Some(module_paths),
        Some(|p: &ModulePath| p.is_in_node_modules),
    );

    let mut node_modules_specifiers: Option<Vec<String>> = None;
    let mut paths_specifiers: Option<Vec<String>> = None;
    let mut relative_specifiers: Option<Vec<String>> = None;
    for module_path in module_paths {
        let specifier =
            try_get_module_name_as_node_module(module_path, &info, host, compiler_options, None);
        if let Some(specifier) = specifier.as_ref() {
            append(
                node_modules_specifiers.get_or_insert_with(|| vec![]),
                Some(specifier.clone()),
            );
        }
        if let Some(specifier) = specifier.as_ref() {
            if module_path.is_redirect {
                return node_modules_specifiers.unwrap();
            }
        }

        if specifier.is_none() && !module_path.is_redirect {
            let local = get_local_module_specifier(
                &module_path.path,
                &info,
                compiler_options,
                host,
                &preferences,
            );
            if path_is_bare_specifier(&local) {
                append(paths_specifiers.get_or_insert_with(|| vec![]), Some(local));
            } else if !imported_file_is_in_node_modules || module_path.is_in_node_modules {
                append(
                    relative_specifiers.get_or_insert_with(|| vec![]),
                    Some(local),
                );
            }
        }
    }

    paths_specifiers
        .filter(|paths_specifiers| !paths_specifiers.is_empty())
        .or_else(|| {
            node_modules_specifiers
                .filter(|node_modules_specifiers| !node_modules_specifiers.is_empty())
        })
        .unwrap_or_else(|| Debug_.check_defined(relative_specifiers, None))
}

struct Info {
    pub get_canonical_file_name: fn(&str) -> String,
    pub importing_source_file_name: Path,
    pub source_directory: Path,
}

fn get_info(importing_source_file_name: &Path, host: &dyn ModuleSpecifierResolutionHost) -> Info {
    let get_canonical_file_name =
        create_get_canonical_file_name(host.use_case_sensitive_file_names().unwrap_or(true));
    let source_directory: Path = get_directory_path(importing_source_file_name).into();
    Info {
        get_canonical_file_name,
        importing_source_file_name: importing_source_file_name.clone(),
        source_directory,
    }
}

fn get_local_module_specifier(
    module_file_name: &str,
    info: &Info,
    compiler_options: &CompilerOptions,
    host: &dyn ModuleSpecifierResolutionHost,
    preferences: &Preferences,
) -> String {
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

fn uses_js_extensions_on_imports(node: &Node /*SourceFile*/) -> bool {
    let imports = node.as_source_file().maybe_imports();
    imports
        .as_ref()
        .and_then(|imports| {
            first_defined(imports, |node: &Rc<Node>, _| {
                let text = node.as_literal_like_node().text();
                if path_is_relative(&text) {
                    Some(has_js_file_extension(&text))
                } else {
                    None
                }
            })
        })
        .unwrap_or(false)
}

fn compare_paths_by_redirect_and_number_of_directory_separators(
    a: &ModulePath,
    b: &ModulePath,
) -> Comparison {
    unimplemented!()
}

pub fn for_each_file_name_of_module<TReturn, TCallback: FnMut(&str, bool) -> Option<TReturn>>(
    importing_file_name: &str,
    imported_file_name: &str,
    host: &dyn ModuleSpecifierResolutionHost,
    prefer_sym_links: bool,
    mut cb: TCallback,
) -> Option<TReturn> {
    let get_canonical_file_name =
        host_get_canonical_file_name(|| host.use_case_sensitive_file_names());
    let cwd = host.get_current_directory();
    let reference_redirect = if host.is_source_of_project_reference_redirect(imported_file_name) {
        host.get_project_reference_redirect(imported_file_name)
    } else {
        None
    };
    let imported_path = to_path(imported_file_name, Some(&cwd), get_canonical_file_name);
    let mut redirects = (*host.redirect_targets_map())
        .borrow()
        .get(&imported_path)
        .cloned()
        .unwrap_or_else(|| vec![]);
    let mut imported_file_names = if let Some(reference_redirect) = reference_redirect.as_ref() {
        vec![reference_redirect.clone()]
    } else {
        vec![]
    };
    imported_file_names.push(imported_file_name.to_owned());
    imported_file_names.append(&mut redirects);
    let targets = imported_file_names
        .iter()
        .map(|f| get_normalized_absolute_path(f, Some(&cwd)))
        .collect::<Vec<_>>();
    let mut should_filter_ignored_paths =
        !every(&targets, |target: &String, _| contains_ignored_path(target));

    if !prefer_sym_links {
        let result = for_each(&targets, |p: &String, _| {
            if !(should_filter_ignored_paths && contains_ignored_path(p)) {
                cb(
                    p,
                    matches!(
                        reference_redirect.as_ref(),
                        Some(reference_redirect) if reference_redirect == p
                    ),
                )
            } else {
                None
            }
        });
        if result.is_some() {
            return result;
        }
    }

    let symlink_cache = host.get_symlink_cache();
    let symlinked_directories = symlink_cache
        .as_ref()
        .map(|symlink_cache| symlink_cache.get_symlinked_directories_by_realpath());
    let ref full_imported_file_name = get_normalized_absolute_path(imported_file_name, Some(&cwd));
    let result = symlinked_directories.and_then(|symlinked_directories| {
        symlinked_directories
            .as_ref()
            .and_then(|symlinked_directories| {
                for_each_ancestor_directory(
                    &get_directory_path(full_imported_file_name).into(),
                    |real_path_directory: &Path| -> Option<Option<TReturn>> {
                        let symlink_directories = symlinked_directories.get(
                            &Into::<Path>::into(ensure_trailing_directory_separator(&to_path(
                                real_path_directory,
                                Some(&cwd),
                                get_canonical_file_name,
                            ))),
                        )?;

                        if starts_with_directory(
                            importing_file_name,
                            real_path_directory,
                            get_canonical_file_name,
                        ) {
                            return Some(None);
                        }

                        for_each(&targets, |target: &String, _| -> Option<Option<TReturn>> {
                            if !starts_with_directory(
                                target,
                                real_path_directory,
                                get_canonical_file_name,
                            ) {
                                return None;
                            }

                            let relative = get_relative_path_from_directory(
                                real_path_directory,
                                target,
                                Some(get_canonical_file_name),
                                None,
                            );
                            for symlink_directory in symlink_directories {
                                let option = resolve_path(symlink_directory, &[Some(&relative)]);
                                let result = cb(
                                    &option,
                                    matches!(
                                        reference_redirect.as_ref(),
                                        Some(reference_redirect) if target == reference_redirect
                                    ),
                                );
                                should_filter_ignored_paths = true;
                                if result.is_some() {
                                    return Some(result);
                                }
                            }
                            None
                        })
                    },
                )
                .flatten()
            })
    });
    result.or_else(|| {
        if prefer_sym_links {
            for_each(&targets, |p: &String, _| {
                if should_filter_ignored_paths && contains_ignored_path(p) {
                    None
                } else {
                    cb(
                        p,
                        matches!(
                            reference_redirect.as_ref(),
                            Some(reference_redirect) if p == reference_redirect
                        ),
                    )
                }
            })
        } else {
            None
        }
    })
}

fn get_all_module_paths_worker(
    importing_file_name: &Path,
    imported_file_name: &str,
    host: &dyn ModuleSpecifierResolutionHost,
) -> Vec<ModulePath> {
    let get_canonical_file_name =
        host_get_canonical_file_name(|| host.use_case_sensitive_file_names());
    let mut all_file_names: HashMap<String, ModulePath> = HashMap::new();
    let mut imported_file_from_node_modules = false;
    for_each_file_name_of_module(
        importing_file_name,
        imported_file_name,
        host,
        true,
        |path: &str, is_redirect| -> Option<()> {
            let is_in_node_modules = path_contains_node_modules(path);
            all_file_names.insert(
                path.to_owned(),
                ModulePath {
                    path: get_canonical_file_name(path),
                    is_redirect,
                    is_in_node_modules,
                },
            );
            imported_file_from_node_modules = imported_file_from_node_modules || is_in_node_modules;
            None
        },
    );

    let mut sorted_paths: Vec<ModulePath> = vec![];
    let mut directory = get_directory_path(importing_file_name);
    while !all_file_names.is_empty() {
        let directory_start = ensure_trailing_directory_separator(&directory);
        let mut paths_in_directory: Option<Vec<ModulePath>> = None;
        let mut keys_to_remove: Vec<String> = vec![];
        for (file_name, value) in &all_file_names {
            let path = &value.path;
            let is_redirect = value.is_redirect;
            let is_in_node_modules = value.is_in_node_modules;
            if starts_with(path, &directory_start) {
                paths_in_directory
                    .get_or_insert_with(|| vec![])
                    .push(ModulePath {
                        path: file_name.clone(),
                        is_in_node_modules,
                        is_redirect,
                    });
                keys_to_remove.push(file_name.clone());
            }
        }
        for key_to_remove in &keys_to_remove {
            all_file_names.remove(key_to_remove);
        }
        if let Some(mut paths_in_directory) = paths_in_directory {
            if paths_in_directory.len() > 1 {
                paths_in_directory.sort_by(|a, b| {
                    comparison_to_ordering(
                        compare_paths_by_redirect_and_number_of_directory_separators(a, b),
                    )
                });
            }
            sorted_paths.append(&mut paths_in_directory);
        }
        let new_directory = get_directory_path(&directory);
        if new_directory == directory {
            break;
        }
        directory = new_directory;
    }
    if !all_file_names.is_empty() {
        let mut remaining_paths = all_file_names.into_values().collect::<Vec<_>>();
        if remaining_paths.len() > 1 {
            remaining_paths.sort_by(|a, b| {
                comparison_to_ordering(
                    compare_paths_by_redirect_and_number_of_directory_separators(a, b),
                )
            });
        }
        sorted_paths.append(&mut remaining_paths);
    }

    sorted_paths
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

fn try_get_module_name_as_node_module(
    module_path: &ModulePath,
    info: &Info,
    host: &dyn ModuleSpecifierResolutionHost,
    options: &CompilerOptions,
    package_name_only: Option<bool>,
) -> Option<String> {
    unimplemented!()
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
