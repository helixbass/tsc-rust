use std::{collections::HashMap, convert::TryInto, io, rc::Rc};

use gc::Gc;
use id_arena::Id;

use crate::{
    append, combine_paths, compare_booleans, compare_number_of_directory_separators,
    comparison_to_ordering, contains_ignored_path, create_get_canonical_file_name,
    directory_separator_str, ends_with, ensure_path_is_non_module_name,
    ensure_trailing_directory_separator, every, extension_from_path, file_extension_is_one_of,
    first_defined, flatten, for_each, for_each_ancestor_directory,
    for_each_ancestor_directory_str_bool, get_directory_path, get_emit_module_resolution_kind,
    get_implied_node_format_for_file, get_module_name_string_literal_at,
    get_normalized_absolute_path, get_package_json_types_version_paths,
    get_package_name_from_types_package_name, get_paths_base_path,
    get_relative_path_from_directory, get_relative_path_to_directory_or_url,
    get_source_file_of_module, get_supported_extensions, get_text_of_identifier_or_literal,
    has_js_file_extension, has_ts_file_extension, host_get_canonical_file_name, is_ambient_module,
    is_external_module_augmentation, is_external_module_name_relative, is_module_block,
    is_module_declaration, is_non_global_ambient_module, is_rooted_disk_path, is_source_file,
    maybe_for_each, node_modules_path_part, normalize_path, path_contains_node_modules,
    path_is_bare_specifier, path_is_relative, remove_file_extension, remove_suffix,
    remove_trailing_directory_separator, resolve_path, return_ok_default_if_none, some,
    starts_with, starts_with_directory, to_path, try_get_extension_from_path, try_map_defined,
    CharacterCodes, Comparison, CompilerOptions, CompilerOptionsBuilder, Debug_, Extension,
    FileExtensionInfo, FileIncludeKind, FileIncludeReason, GetOrInsertDefault, HasArena, JsxEmit,
    LiteralLikeNodeInterface, ModuleKind, ModulePath, ModuleResolutionHost,
    ModuleResolutionHostOverrider, ModuleResolutionKind, ModuleSpecifierCache,
    ModuleSpecifierResolutionHost, Node, NodeFlags, NodeInterface, NonEmpty, OptionTry, Path,
    ScriptKind, StringOrBool, Symbol, SymbolFlags, SymbolInterface, TypeChecker, UserPreferences,
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

#[derive(Copy, Clone)]
struct Preferences {
    pub relative_preference: RelativePreference,
    pub ending: Ending,
}

fn get_preferences(
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
    user_preferences: &UserPreferences,
    compiler_options: Gc<CompilerOptions>,
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
    compiler_options: Gc<CompilerOptions>,
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
) -> Ending {
    match import_module_specifier_ending {
        Some("minimal") => Ending::Minimal,
        Some("index") => Ending::Index,
        Some("js") => Ending::JsExtension,
        _ => {
            if uses_js_extensions_on_imports(importing_source_file)
                || is_format_requiring_extensions(
                    compiler_options.clone(),
                    &importing_source_file.as_source_file().path(),
                    host,
                )
            {
                Ending::JsExtension
            } else if get_emit_module_resolution_kind(&compiler_options)
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
    compiler_options: Gc<CompilerOptions>,
    importing_source_file_name: &Path,
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
) -> bool {
    if !matches!(
        get_emit_module_resolution_kind(&compiler_options),
        ModuleResolutionKind::Node12 | ModuleResolutionKind::NodeNext
    ) {
        return false;
    }
    get_implied_node_format_for_file(
        importing_source_file_name,
        None,
        &get_module_resolution_host(host),
        compiler_options,
    ) != Some(ModuleKind::CommonJS)
}

fn get_module_resolution_host<'host, THost: ModuleSpecifierResolutionHost + ?Sized>(
    host: &'host THost,
) -> ModuleResolutionHostFromModuleSpecifierResolutionHost<'host, THost> {
    ModuleResolutionHostFromModuleSpecifierResolutionHost::new(host)
}

struct ModuleResolutionHostFromModuleSpecifierResolutionHost<
    'host,
    THost: ModuleSpecifierResolutionHost + ?Sized,
> {
    pub host: &'host THost,
}

impl<'host, THost: ModuleSpecifierResolutionHost + ?Sized>
    ModuleResolutionHostFromModuleSpecifierResolutionHost<'host, THost>
{
    pub fn new(host: &'host THost) -> Self {
        Self { host }
    }
}

impl<THost: ModuleSpecifierResolutionHost + ?Sized> ModuleResolutionHost
    for ModuleResolutionHostFromModuleSpecifierResolutionHost<'_, THost>
{
    fn file_exists(&self, file_name: &str) -> bool {
        self.host.file_exists(file_name)
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

    fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
        self.host.read_file(file_name).unwrap()
    }

    fn read_file_non_overridden(&self, _file_name: &str) -> io::Result<Option<String>> {
        unreachable!()
    }

    fn set_overriding_read_file(
        &self,
        _overriding_read_file: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_trace_supported(&self) -> bool {
        unreachable!()
    }

    fn directory_exists(&self, directory_name: &str) -> Option<bool> {
        self.host.directory_exists(directory_name)
    }

    fn is_directory_exists_supported(&self) -> bool {
        unreachable!()
    }

    fn directory_exists_non_overridden(&self, _directory_name: &str) -> Option<bool> {
        unreachable!()
    }

    fn set_overriding_directory_exists(
        &self,
        _overriding_directory_exists: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn realpath(&self, path: &str) -> Option<String> {
        self.host.realpath(path)
    }

    fn realpath_non_overridden(&self, _path: &str) -> Option<String> {
        unreachable!()
    }

    fn is_realpath_supported(&self) -> bool {
        unreachable!()
    }

    fn set_overriding_realpath(
        &self,
        _overriding_realpath: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn get_current_directory(&self) -> Option<io::Result<String>> {
        Some(Ok(self.host.get_current_directory()))
    }

    fn get_directories(&self, _path: &str) -> Option<Vec<String>> {
        unreachable!()
    }

    fn is_get_directories_supported(&self) -> bool {
        unreachable!()
    }

    fn get_directories_non_overridden(&self, _path: &str) -> Option<Vec<String>> {
        unreachable!()
    }

    fn set_overriding_get_directories(
        &self,
        _overriding_get_directories: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        self.host.use_case_sensitive_file_names()
    }
}

pub fn get_module_specifier(
    compiler_options: Gc<CompilerOptions>,
    importing_source_file: &Node, /*SourceFile*/
    importing_source_file_name: &Path,
    to_file_name: &str,
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
) -> io::Result<String> {
    get_module_specifier_worker(
        &compiler_options,
        importing_source_file_name,
        to_file_name,
        host,
        get_preferences(
            host,
            &Default::default(),
            compiler_options.clone(),
            importing_source_file,
        ),
        &Default::default(),
    )
}

fn get_module_specifier_worker(
    compiler_options: &CompilerOptions,
    importing_source_file_name: &Path,
    to_file_name: &str,
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
    preferences: Preferences,
    user_preferences: &UserPreferences,
) -> io::Result<String> {
    let info = get_info(importing_source_file_name, host);
    let module_paths = get_all_module_paths(
        importing_source_file_name,
        to_file_name,
        host,
        user_preferences,
        None,
    );
    first_defined(&module_paths, |module_path: &ModulePath, _| {
        try_get_module_name_as_node_module(module_path, &info, host, compiler_options, None)
    })
    .non_empty()
    .try_unwrap_or_else(|| {
        get_local_module_specifier(to_file_name, &info, compiler_options, host, preferences)
    })
}

fn try_get_module_specifiers_from_cache_worker(
    checker: &TypeChecker,
    module_symbol: Id<Symbol>,
    importing_source_file: &Node, /*SourceFile*/
    host: &dyn ModuleSpecifierResolutionHost,
    user_preferences: &UserPreferences,
) -> (
    Option<Vec<String>>,
    Option<Id<Node>>,
    Option<Vec<ModulePath>>,
    Option<Rc<dyn ModuleSpecifierCache>>,
) {
    let module_source_file = get_source_file_of_module(&checker.symbol(module_symbol));
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
    module_symbol: Id<Symbol>,
    checker: &TypeChecker,
    compiler_options: Gc<CompilerOptions>,
    importing_source_file: &Node, /*SourceFile*/
    host: &dyn ModuleSpecifierResolutionHost,
    user_preferences: &UserPreferences,
) -> io::Result<Vec<String>> {
    Ok(get_module_specifiers_with_cache_info(
        module_symbol,
        checker,
        compiler_options,
        importing_source_file, /*SourceFile*/
        host,
        user_preferences,
    )?
    .module_specifiers)
}

pub fn get_module_specifiers_with_cache_info(
    module_symbol: Id<Symbol>,
    checker: &TypeChecker,
    compiler_options: Gc<CompilerOptions>,
    importing_source_file: &Node, /*SourceFile*/
    host: &dyn ModuleSpecifierResolutionHost,
    user_preferences: &UserPreferences,
) -> io::Result<ModuleSpecifiersWithCacheInfo> {
    let mut computed_without_cache = false;
    let ambient = try_get_module_name_from_ambient_module(module_symbol, checker)?;
    if let Some(ambient) = ambient {
        return Ok(ModuleSpecifiersWithCacheInfo {
            module_specifiers: vec![ambient],
            computed_without_cache,
        });
    }

    let (specifiers, module_source_file, module_paths, cache) =
        try_get_module_specifiers_from_cache_worker(
            checker,
            module_symbol,
            importing_source_file,
            host,
            user_preferences,
        );
    if let Some(specifiers) = specifiers {
        return Ok(ModuleSpecifiersWithCacheInfo {
            module_specifiers: specifiers,
            computed_without_cache,
        });
    }
    if module_source_file.is_none() {
        return Ok(ModuleSpecifiersWithCacheInfo {
            module_specifiers: vec![],
            computed_without_cache,
        });
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
    )?;
    if let Some(cache) = cache {
        cache.set(
            &importing_source_file_as_source_file.path(),
            &module_source_file_as_source_file.path(),
            user_preferences,
            &module_paths,
            &result,
        );
    }
    Ok(ModuleSpecifiersWithCacheInfo {
        module_specifiers: result,
        computed_without_cache,
    })
}

fn compute_module_specifiers(
    module_paths: &[ModulePath],
    compiler_options: Gc<CompilerOptions>,
    importing_source_file: &Node, /*SourceFile*/
    host: &dyn ModuleSpecifierResolutionHost,
    user_preferences: &UserPreferences,
) -> io::Result<Vec<String>> {
    let importing_source_file_as_source_file = importing_source_file.as_source_file();
    let info = get_info(&importing_source_file_as_source_file.path(), host);
    let preferences = get_preferences(
        host,
        user_preferences,
        compiler_options.clone(),
        importing_source_file,
    );
    let existing_specifier = for_each(module_paths, |module_path: &ModulePath, _| {
        maybe_for_each(
            (*host.get_file_include_reasons()).borrow().get(&to_path(
                &module_path.path,
                Some(&host.get_current_directory()),
                info.get_canonical_file_name,
            )),
            |reason: &Gc<FileIncludeReason>, _| {
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
    if let Some(existing_specifier) = existing_specifier {
        let module_specifiers = vec![existing_specifier];
        return Ok(module_specifiers);
    }

    let imported_file_is_in_node_modules = some(
        Some(module_paths),
        Some(|p: &ModulePath| p.is_in_node_modules),
    );

    let mut node_modules_specifiers: Option<Vec<String>> = None;
    let mut paths_specifiers: Option<Vec<String>> = None;
    let mut relative_specifiers: Option<Vec<String>> = None;
    for module_path in module_paths {
        let specifier =
            try_get_module_name_as_node_module(module_path, &info, host, &compiler_options, None);
        if let Some(specifier) = specifier.as_ref() {
            append(
                node_modules_specifiers.get_or_insert_default_(),
                Some(specifier.clone()),
            );
        }
        if specifier.is_some() {
            if module_path.is_redirect {
                return Ok(node_modules_specifiers.unwrap());
            }
        }

        if specifier.is_none() && !module_path.is_redirect {
            let local = get_local_module_specifier(
                &module_path.path,
                &info,
                &compiler_options,
                host,
                preferences,
            )?;
            if path_is_bare_specifier(&local) {
                append(paths_specifiers.get_or_insert_default_(), Some(local));
            } else if !imported_file_is_in_node_modules || module_path.is_in_node_modules {
                append(relative_specifiers.get_or_insert_default_(), Some(local));
            }
        }
    }

    Ok(paths_specifiers
        .filter(|paths_specifiers| !paths_specifiers.is_empty())
        .or_else(|| {
            node_modules_specifiers
                .filter(|node_modules_specifiers| !node_modules_specifiers.is_empty())
        })
        .unwrap_or_else(|| Debug_.check_defined(relative_specifiers, None)))
}

struct Info {
    pub get_canonical_file_name: fn(&str) -> String,
    #[allow(dead_code)]
    pub importing_source_file_name: Path,
    pub source_directory: Path,
}

fn get_info(
    importing_source_file_name: &Path,
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
) -> Info {
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
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
    preferences: Preferences,
) -> io::Result<String> {
    let ending = preferences.ending;
    let relative_preference = preferences.relative_preference;
    let base_url = compiler_options.base_url.as_ref();
    let paths = compiler_options.paths.as_ref();
    let root_dirs = compiler_options.root_dirs.as_ref();
    let source_directory = &info.source_directory;
    let get_canonical_file_name = info.get_canonical_file_name;
    let relative_path = if let Some(root_dirs) = root_dirs {
        try_get_module_name_from_root_dirs(
            root_dirs,
            module_file_name,
            source_directory,
            get_canonical_file_name,
            ending,
            compiler_options,
        )
    } else {
        None
    }
    .unwrap_or_else(|| {
        remove_extension_and_index_post_fix(
            &ensure_path_is_non_module_name(&get_relative_path_from_directory(
                source_directory,
                module_file_name,
                Some(get_canonical_file_name),
                None,
            )),
            ending,
            compiler_options,
        )
    });
    if match base_url {
        None => true,
        Some(base_url) => base_url.is_empty(),
    } && paths.is_none()
        || relative_preference == RelativePreference::Relative
    {
        return Ok(relative_path);
    }

    let base_directory = get_normalized_absolute_path(
        &get_paths_base_path(compiler_options, || Some(Ok(host.get_current_directory())))?
            .unwrap_or_else(|| base_url.unwrap().clone()),
        Some(&host.get_current_directory()),
    );
    let relative_to_base_url = get_relative_path_if_in_directory(
        module_file_name,
        &base_directory,
        get_canonical_file_name,
    );
    if relative_to_base_url.is_none() {
        return Ok(relative_path);
    }
    let relative_to_base_url = relative_to_base_url.unwrap();
    if relative_to_base_url.is_empty() {
        return Ok(relative_path);
    }

    let import_relative_to_base_url =
        remove_extension_and_index_post_fix(&relative_to_base_url, ending, compiler_options);
    let from_paths = paths.and_then(|paths| {
        try_get_module_name_from_paths(
            remove_file_extension(&relative_to_base_url),
            &import_relative_to_base_url,
            paths,
        )
    });
    let non_relative = if from_paths.is_none() && base_url.is_some() {
        Some(import_relative_to_base_url)
    } else {
        from_paths
    };
    if non_relative.is_none() {
        return Ok(relative_path);
    }
    let non_relative = non_relative.unwrap();
    if non_relative.is_empty() {
        return Ok(relative_path);
    }

    if relative_preference == RelativePreference::NonRelative {
        return Ok(non_relative);
    }

    if relative_preference == RelativePreference::ExternalNonRelative {
        let project_directory = if let Some(compiler_options_config_file_path) =
            compiler_options.config_file_path.as_ref().non_empty()
        {
            to_path(
                &get_directory_path(compiler_options_config_file_path),
                Some(&host.get_current_directory()),
                |file_name: &str| (info.get_canonical_file_name)(file_name),
            )
            .to_string()
        } else {
            (info.get_canonical_file_name)(&host.get_current_directory())
        };
        let module_path = to_path(
            module_file_name,
            Some(&project_directory),
            get_canonical_file_name,
        );
        let source_is_internal = starts_with(source_directory, &project_directory);
        let target_is_internal = starts_with(&module_path, &project_directory);
        if source_is_internal && !target_is_internal || !source_is_internal && target_is_internal {
            return Ok(non_relative);
        }

        let nearest_target_package_json = get_nearest_ancestor_directory_with_package_json(
            host,
            &get_directory_path(&module_path),
        );
        let nearest_source_package_json =
            get_nearest_ancestor_directory_with_package_json(host, source_directory);
        if nearest_source_package_json != nearest_target_package_json {
            return Ok(non_relative);
        }

        return Ok(relative_path);
    }

    if relative_preference != RelativePreference::Shortest {
        Debug_.assert_never(relative_preference, None);
    }

    Ok(
        if is_path_relative_to_parent(&non_relative)
            || count_path_components(&relative_path) < count_path_components(&non_relative)
        {
            relative_path
        } else {
            non_relative
        },
    )
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
            first_defined(imports, |node: &Id<Node>, _| {
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
    let result = compare_booleans(b.is_redirect, a.is_redirect);
    if result != Comparison::EqualTo {
        return result;
    }
    compare_number_of_directory_separators(&a.path, &b.path)
}

fn get_nearest_ancestor_directory_with_package_json(
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
    file_name: &str,
) -> Option<StringOrBool> {
    if host.is_get_nearest_ancestor_directory_with_package_json_supported() {
        return host
            .get_nearest_ancestor_directory_with_package_json(file_name, None)
            .map(Into::into);
    }
    Some(
        for_each_ancestor_directory_str_bool(file_name, |directory: &str| {
            host.file_exists(&combine_paths(directory, &[Some("package.json")]))
        })
        .into(),
    )
}

pub fn for_each_file_name_of_module<TReturn, TCallback: FnMut(&str, bool) -> Option<TReturn>>(
    importing_file_name: &str,
    imported_file_name: &str,
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
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

fn get_all_module_paths(
    importing_file_path: &Path,
    imported_file_name: &str,
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
    preferences: &UserPreferences,
    imported_file_path: Option<Path>,
) -> Vec<ModulePath> {
    let imported_file_path = imported_file_path.unwrap_or_else(|| {
        to_path(
            imported_file_name,
            Some(&host.get_current_directory()),
            host_get_canonical_file_name(|| host.use_case_sensitive_file_names()),
        )
    });
    let cache = host.get_module_specifier_cache();
    if let Some(cache) = cache.as_ref() {
        let cached = cache.get(importing_file_path, &imported_file_path, preferences);
        // TODO: should this be rc-wrapped at the Vec<ModulePath> and/or ModulePath level?
        if let Some(cached_module_paths) = cached.and_then(|cached| cached.module_paths.clone()) {
            return cached_module_paths;
        }
    }
    let module_paths = get_all_module_paths_worker(importing_file_path, imported_file_name, host);
    if let Some(cache) = cache.as_ref() {
        cache.set_module_paths(
            importing_file_path,
            &imported_file_path,
            preferences,
            &module_paths,
        );
    }
    module_paths
}

fn get_all_module_paths_worker(
    importing_file_name: &Path,
    imported_file_name: &str,
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
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
                    .get_or_insert_default_()
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
    module_symbol: Id<Symbol>,
    checker: &TypeChecker,
) -> io::Result<Option<String>> {
    let decl = checker
        .symbol(module_symbol)
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
        return Ok(Some(
            decl.as_module_declaration()
                .name
                .as_string_literal()
                .text()
                .clone(),
        ));
    }

    let ambient_module_declare_candidates = try_map_defined(
        checker.symbol(module_symbol).maybe_declarations().as_ref(),
        |d: &Id<Node>, _| -> io::Result<Option<Id<Node>>> {
            if !is_module_declaration(d) {
                return Ok(None);
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
                return Ok(None);
            }
            let ref export_assignment = return_ok_default_if_none!(checker
                .symbol(top_namespace.parent().parent().symbol())
                .maybe_exports()
                .as_ref()
                .and_then(|top_namespace_parent_parent_symbol_exports| {
                    (**top_namespace_parent_parent_symbol_exports)
                        .borrow()
                        .get("export=")
                        .cloned()
                })
                .and_then(|top_namespace_parent_parent_symbol_exports_got| {
                    checker
                        .symbol(top_namespace_parent_parent_symbol_exports_got)
                        .maybe_value_declaration()
                })
                .map(
                    |top_namespace_parent_parent_symbol_exports_got_value_declaration| {
                        top_namespace_parent_parent_symbol_exports_got_value_declaration
                            .as_export_assignment()
                            .expression
                            .clone()
                    },
                ));
            let export_symbol =
                return_ok_default_if_none!(checker.get_symbol_at_location(export_assignment)?);
            let original_export_symbol = if checker
                .symbol(export_symbol)
                .flags()
                .intersects(SymbolFlags::Alias)
            {
                checker.get_aliased_symbol(export_symbol)?
            } else {
                export_symbol.clone()
            };
            if matches!(
                d.maybe_symbol(),
                Some(d_symbol) if original_export_symbol == d_symbol
            ) {
                return Ok(Some(top_namespace.parent().parent()));
            }
            Ok(None)
        },
    )?;
    let ambient_module_declare = ambient_module_declare_candidates.get(0);
    if let Some(ambient_module_declare) = ambient_module_declare {
        return Ok(Some(
            ambient_module_declare
                .as_module_declaration()
                .name
                .as_string_literal()
                .text()
                .clone(),
        ));
    }
    Ok(None)
}

fn try_get_module_name_from_paths(
    relative_to_base_url_with_index: &str,
    relative_to_base_url: &str,
    paths: &HashMap<String, Vec<String>>,
) -> Option<String> {
    for (key, pattern_texts) in paths {
        for pattern_text in pattern_texts {
            let normalized = normalize_path(pattern_text);
            let pattern = remove_file_extension(&normalized);
            let index_of_star = pattern.find("*");
            if let Some(index_of_star) = index_of_star {
                let prefix = &pattern[0..index_of_star];
                let suffix = &pattern[index_of_star + 1..];
                if relative_to_base_url.len() >= prefix.len() + suffix.len()
                    && starts_with(relative_to_base_url, prefix)
                    && ends_with(relative_to_base_url, suffix)
                    || suffix.is_empty()
                        && relative_to_base_url == &*remove_trailing_directory_separator(prefix)
                {
                    let matched_star = &relative_to_base_url[prefix.len()
                        ..prefix.len() + relative_to_base_url.len() - suffix.len() - prefix.len()];
                    return Some(key.replace("*", matched_star));
                }
            } else if pattern == relative_to_base_url || pattern == relative_to_base_url_with_index
            {
                return Some(key.clone());
            }
        }
    }
    None
}

fn get_top_namespace(namespace_declaration: &Node /*ModuleDeclaration*/) -> Id<Node> {
    let mut namespace_declaration = namespace_declaration.node_wrapper();
    while namespace_declaration
        .flags()
        .intersects(NodeFlags::NestedNamespace)
    {
        namespace_declaration = namespace_declaration.parent();
    }
    namespace_declaration
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum MatchingMode {
    Exact,
    Directory,
    Pattern,
}

fn try_get_module_name_from_exports(
    _options: &CompilerOptions,
    _target_file_path: &str,
    _package_directory: &str,
    _package_name: &str,
    _exports: &serde_json::Value,
    _conditions: &[&str],
    mode: Option<MatchingMode>,
) -> Option<TryGetModuleNameFromExportsReturn> {
    let _mode = mode.unwrap_or(MatchingMode::Exact);
    unimplemented!()
}

struct TryGetModuleNameFromExportsReturn {
    pub module_file_to_try: String,
}

fn try_get_module_name_from_root_dirs(
    _root_dirs: &[String],
    _module_file_name: &str,
    _source_directory: &str,
    _get_canonical_file_name: fn(&str) -> String,
    _ending: Ending,
    _compiler_options: &CompilerOptions,
) -> Option<String> {
    unimplemented!()
}

fn try_get_module_name_as_node_module(
    module_path: &ModulePath,
    info: &Info,
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
    options: &CompilerOptions,
    package_name_only: Option<bool>,
) -> Option<String> {
    let path = &module_path.path;
    let is_redirect = module_path.is_redirect;
    let get_canonical_file_name = info.get_canonical_file_name;
    let source_directory = &info.source_directory;
    if
    /* !host.fileExists ||*/
    !host.is_read_file_supported() {
        return None;
    }
    let parts = get_node_module_path_parts(path)?;

    let mut module_specifier = path.clone();
    let mut is_package_root_path = false;
    if package_name_only != Some(true) {
        let mut package_root_index = parts.package_root_index;
        let mut module_file_name_for_extensionless: Option<String> = None;
        loop {
            let TryDirectoryWithPackageJsonReturn {
                module_file_to_try,
                package_root_path,
                blocked_by_exports,
                verbatim_from_exports,
            } = try_directory_with_package_json(
                path,
                host,
                options,
                get_canonical_file_name,
                package_root_index,
            );
            if get_emit_module_resolution_kind(options) != ModuleResolutionKind::Classic {
                if blocked_by_exports == Some(true) {
                    return None;
                }
                if verbatim_from_exports == Some(true) {
                    return Some(module_file_to_try);
                }
            }
            if let Some(package_root_path) = package_root_path {
                module_specifier = package_root_path;
                is_package_root_path = true;
                break;
            }
            if module_file_name_for_extensionless.is_none() {
                module_file_name_for_extensionless = Some(module_file_to_try);
            }

            let maybe_package_root_index = path
                [TryInto::<usize>::try_into(package_root_index + 1).unwrap()..]
                .find(directory_separator_str)
                .map(|index| index + TryInto::<usize>::try_into(package_root_index + 1).unwrap());
            match maybe_package_root_index {
                None => {
                    module_specifier = get_extensionless_file_name(
                        get_canonical_file_name,
                        &parts,
                        host,
                        module_file_name_for_extensionless.as_ref().unwrap(),
                    );
                    break;
                }
                Some(maybe_package_root_index) => {
                    package_root_index = maybe_package_root_index.try_into().unwrap();
                }
            }
        }
    }

    if is_redirect && !is_package_root_path {
        return None;
    }

    let global_typings_cache_location = host.get_global_typings_cache_location();
    let path_to_top_level_node_modules =
        get_canonical_file_name(&module_specifier[0..parts.top_level_node_modules_index]);
    if !(starts_with(source_directory, &path_to_top_level_node_modules)
        || matches!(
            global_typings_cache_location.as_ref(),
            Some(global_typings_cache_location) if starts_with(
                &get_canonical_file_name(global_typings_cache_location),
                &path_to_top_level_node_modules
            )
        ))
    {
        return None;
    }

    let node_modules_directory_name = &module_specifier[parts.top_level_package_name_index + 1..];
    let package_name = get_package_name_from_types_package_name(node_modules_directory_name);
    if get_emit_module_resolution_kind(options) == ModuleResolutionKind::Classic
        && package_name == node_modules_directory_name
    {
        None
    } else {
        Some(package_name.into_owned())
    }
}

fn get_extensionless_file_name(
    get_canonical_file_name: fn(&str) -> String,
    parts: &NodeModulePathParts,
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
    path: &str,
) -> String {
    let full_module_path_without_extension = remove_file_extension(path);

    if get_canonical_file_name(&full_module_path_without_extension[parts.file_name_index..])
        == "/index"
        && match try_get_any_file_from_path(
            host,
            &full_module_path_without_extension[0..parts.file_name_index],
        ) {
            None => true,
            Some(value) => value.is_empty(),
        }
    {
        return full_module_path_without_extension[0..parts.file_name_index].to_owned();
    }

    full_module_path_without_extension.to_owned()
}

fn try_get_any_file_from_path(
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
    path: &str,
) -> Option<String> {
    // if (!host.fileExists) return;
    let extensions = flatten(&get_supported_extensions(
        Some(
            &CompilerOptionsBuilder::default()
                .allow_js(true)
                .build()
                .unwrap(),
        ),
        Some(&[
            FileExtensionInfo {
                extension: "node".to_owned(),
                is_mixed_content: false,
                script_kind: None,
            },
            FileExtensionInfo {
                extension: "json".to_owned(),
                is_mixed_content: false,
                script_kind: Some(ScriptKind::JSON),
            },
        ]),
    ));
    for e in extensions {
        let full_path = format!("{path}{}", e.to_str());
        if host.file_exists(&full_path) {
            return Some(full_path);
        }
    }
    None
}

fn try_directory_with_package_json(
    path: &String,
    host: &(impl ModuleSpecifierResolutionHost + ?Sized),
    options: &CompilerOptions,
    get_canonical_file_name: fn(&str) -> String,
    package_root_index: isize,
) -> TryDirectoryWithPackageJsonReturn {
    let package_root_path = if package_root_index == -1 {
        ""
    } else {
        let package_root_index: usize = package_root_index.try_into().unwrap();
        &path[0..package_root_index]
    };
    let package_json_path = combine_paths(package_root_path, &[Some("package.json")]);
    let mut module_file_to_try = path.clone();
    if host.file_exists(&package_json_path) {
        let package_json_content: serde_json::Value = serde_json::from_str(
            &host
                .read_file(&package_json_path)
                .unwrap()
                .unwrap()
                .unwrap(),
        )
        .unwrap();
        let package_json_content = package_json_content.as_object().unwrap();
        if matches!(
            get_emit_module_resolution_kind(options),
            ModuleResolutionKind::Node12 | ModuleResolutionKind::NodeNext
        ) {
            let from_exports = if package_json_content.get("exports").is_some()
                && package_json_content
                    .get("name")
                    .map(|value| value.as_str())
                    .is_some()
            {
                try_get_module_name_from_exports(
                    options,
                    path,
                    package_root_path,
                    package_json_content.get("name").unwrap().as_str().unwrap(),
                    package_json_content.get("exports").unwrap(),
                    &["node", "types"],
                    None,
                )
            } else {
                None
            };
            if let Some(from_exports) = from_exports {
                let with_js_extension = if !has_ts_file_extension(&from_exports.module_file_to_try)
                {
                    from_exports
                } else {
                    TryGetModuleNameFromExportsReturn {
                        module_file_to_try: format!(
                            "{}{}",
                            remove_file_extension(&from_exports.module_file_to_try),
                            match try_get_js_extension_for_file(
                                &from_exports.module_file_to_try,
                                options,
                            ) {
                                Some(extension) => extension.to_str(),
                                None => "undefined",
                            }
                        ),
                    }
                };
                return TryDirectoryWithPackageJsonReturn {
                    module_file_to_try: with_js_extension.module_file_to_try,
                    package_root_path: None,
                    blocked_by_exports: None,
                    verbatim_from_exports: Some(true),
                };
            }
            if package_json_content.get("exports").is_some() {
                return TryDirectoryWithPackageJsonReturn {
                    module_file_to_try: path.clone(),
                    package_root_path: None,
                    blocked_by_exports: Some(true),
                    verbatim_from_exports: None,
                };
            }
        }
        let version_paths = package_json_content
            .get("typesVersions")
            .and_then(|value| value.as_object())
            .and_then(|value| get_package_json_types_version_paths(value));
        if let Some(version_paths) = version_paths {
            let sub_module_name = &path[package_root_path.len() + 1..];
            let from_paths = try_get_module_name_from_paths(
                remove_file_extension(sub_module_name),
                &remove_extension_and_index_post_fix(sub_module_name, Ending::Minimal, options),
                &serde_json_value_to_map_like_vec_string(&version_paths.paths),
            );
            if let Some(from_paths) = from_paths {
                module_file_to_try = combine_paths(package_root_path, &[Some(&from_paths)]);
            }
        }
        let main_file_relative = package_json_content
            .get("typings")
            .or_else(|| package_json_content.get("types"))
            .or_else(|| package_json_content.get("main"));
        if let Some(main_file_relative) =
            main_file_relative.and_then(|main_file_relative| main_file_relative.as_str())
        {
            let main_export_file = to_path(
                main_file_relative,
                Some(package_root_path),
                get_canonical_file_name,
            );
            if remove_file_extension(&main_export_file)
                == remove_file_extension(&get_canonical_file_name(&module_file_to_try))
            {
                return TryDirectoryWithPackageJsonReturn {
                    module_file_to_try,
                    package_root_path: Some(package_root_path.to_owned()),
                    blocked_by_exports: None,
                    verbatim_from_exports: None,
                };
            }
        }
    }
    TryDirectoryWithPackageJsonReturn {
        module_file_to_try,
        package_root_path: None,
        blocked_by_exports: None,
        verbatim_from_exports: None,
    }
}

fn serde_json_value_to_map_like_vec_string(
    _value: &serde_json::Value,
) -> HashMap<String, Vec<String>> {
    unimplemented!()
}

struct TryDirectoryWithPackageJsonReturn {
    pub module_file_to_try: String,
    pub package_root_path: Option<String>,
    pub blocked_by_exports: Option<bool>,
    pub verbatim_from_exports: Option<bool>,
}

struct NodeModulePathParts {
    pub top_level_node_modules_index: usize,
    pub top_level_package_name_index: usize,
    pub package_root_index: isize,
    pub file_name_index: usize,
}

fn get_node_module_path_parts(full_path: &str) -> Option<NodeModulePathParts> {
    let mut top_level_node_modules_index = 0;
    let mut top_level_package_name_index = 0;
    let mut package_root_index: isize = 0;
    let file_name_index: usize/* = 0*/;

    let mut part_start = 0;
    let mut part_end = Some(0);
    let mut state = States::BeforeNodeModules;

    while let Some(part_end_present) = part_end {
        part_start = part_end_present;
        part_end = full_path[part_start + 1..]
            .find("/")
            .map(|index| index + part_start + 1);
        match state {
            States::BeforeNodeModules => {
                if full_path[part_start..]
                    .find(node_modules_path_part)
                    .map(|index| index + part_start)
                    == Some(part_start)
                {
                    top_level_node_modules_index = part_start;
                    match part_end {
                        None => break,
                        Some(part_end) => {
                            top_level_package_name_index = part_end;
                        }
                    }
                    state = States::NodeModules;
                }
            }
            States::NodeModules | States::Scope => {
                if state == States::NodeModules && &full_path[part_start + 1..part_start + 2] == "@"
                {
                    state = States::Scope;
                } else {
                    package_root_index = match part_end {
                        None => -1,
                        Some(part_end) => part_end.try_into().unwrap(),
                    };
                    state = States::PackageContent;
                }
            }
            States::PackageContent => {
                if full_path[part_start..]
                    .find(node_modules_path_part)
                    .map(|index| index + part_start)
                    == Some(part_start)
                {
                    state = States::NodeModules;
                } else {
                    state = States::PackageContent;
                }
            }
        }
    }

    file_name_index = part_start;

    if matches!(state, States::Scope | States::PackageContent) {
        Some(NodeModulePathParts {
            top_level_node_modules_index,
            top_level_package_name_index,
            package_root_index,
            file_name_index,
        })
    } else {
        None
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum States {
    BeforeNodeModules,
    NodeModules,
    Scope,
    PackageContent,
}

fn remove_extension_and_index_post_fix(
    file_name: &str,
    ending: Ending,
    options: &CompilerOptions,
) -> String {
    if file_extension_is_one_of(
        file_name,
        &[Extension::Json, Extension::Mjs, Extension::Cjs],
    ) {
        return file_name.to_owned();
    }
    let no_extension = remove_file_extension(file_name);
    if file_extension_is_one_of(
        file_name,
        &[
            Extension::Dmts,
            Extension::Mts,
            Extension::Dcts,
            Extension::Cts,
        ],
    ) {
        return format!(
            "{}{}",
            no_extension,
            get_js_extension_for_file(file_name, options).to_str()
        );
    }
    match ending {
        Ending::Minimal => remove_suffix(no_extension, "/index").to_owned(),
        Ending::Index => no_extension.to_owned(),
        Ending::JsExtension => format!(
            "{}{}",
            no_extension,
            get_js_extension_for_file(file_name, options).to_str()
        ),
    }
}

fn get_js_extension_for_file(file_name: &str, options: &CompilerOptions) -> Extension {
    try_get_js_extension_for_file(file_name, options).unwrap_or_else(|| {
        Debug_.fail(Some(&format!(
            "Extension {} is unsupported:: FileName:: {}",
            extension_from_path(file_name).to_str(),
            file_name
        )))
    })
}

fn try_get_js_extension_for_file(file_name: &str, options: &CompilerOptions) -> Option<Extension> {
    let ext = try_get_extension_from_path(file_name)?;
    match ext {
        Extension::Ts | Extension::Dts => Some(Extension::Js),
        Extension::Tsx => Some(if options.jsx == Some(JsxEmit::Preserve) {
            Extension::Jsx
        } else {
            Extension::Js
        }),
        Extension::Js | Extension::Jsx | Extension::Json => Some(ext),
        Extension::Dmts | Extension::Mts | Extension::Mjs => Some(Extension::Mjs),
        Extension::Dcts | Extension::Cts | Extension::Cjs => Some(Extension::Cjs),
        _ => None,
    }
}

fn get_relative_path_if_in_directory(
    path: &str,
    directory_path: &str,
    get_canonical_file_name: fn(&str) -> String,
) -> Option<String> {
    let relative_path = get_relative_path_to_directory_or_url(
        directory_path,
        path,
        directory_path,
        get_canonical_file_name,
        false,
    );
    if is_rooted_disk_path(&relative_path) {
        None
    } else {
        Some(relative_path)
    }
}

fn is_path_relative_to_parent(path: &str) -> bool {
    starts_with(path, "..")
}
