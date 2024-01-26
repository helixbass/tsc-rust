use std::{
    borrow::Borrow,
    cell::{Cell, Ref, RefCell, RefMut},
    cmp,
    collections::HashMap,
    convert::TryInto,
    fmt,
    iter::FromIterator,
    rc::Rc,
};

use fancy_regex::{Captures, Regex};
use gc::{Finalize, Gc, Trace};
use id_arena::Id;
use local_macros::enum_unwrapped;

use crate::{
    change_any_extension, combine_paths, compare_strings_case_sensitive,
    compare_strings_case_sensitive_maybe, compare_values, comparison_to_ordering,
    contains_ignored_path, contains_path, create_get_canonical_file_name, create_multi_map,
    directory_separator, ensure_trailing_directory_separator, every, file_extension_is,
    file_extension_is_one_of, find, find_best_pattern_match, find_index, flat_map, flatten,
    for_each, format_string_from_args, get_directory_path, get_locale_specific_message,
    get_normalized_absolute_path, get_normalized_path_components, get_path_components,
    get_path_from_path_components, get_string_comparer, get_token_pos_of_node, has_extension,
    index_of, index_of_any_char_code, is_rooted_disk_path, last, map_defined, maybe_map,
    normalize_path, remove_trailing_directory_separator, skip_trivia, some, sort, starts_with,
    to_path, BaseDiagnostic, BaseDiagnosticRelatedInformation, BaseTextRange, CharacterCodes,
    CommandLineOption, CommandLineOptionInterface, CommandLineOptionMapTypeValue,
    CommandLineOptionType, Comparison, CompilerOptions, CompilerOptionsValue, Debug_, Diagnostic,
    DiagnosticInterface, DiagnosticMessage, DiagnosticMessageChain, DiagnosticMessageText,
    DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface, Extension,
    FileExtensionInfo, GetCanonicalFileName, GetOrInsertDefault, HasArena, InArena, JsxEmit,
    LanguageVariant, MapLike, Matches, ModuleKind, ModuleResolutionKind, MultiMap, Node, NodeArray,
    NodeInterface, Path, Pattern, PluginImport, PragmaArgumentName, PragmaName, ReadonlyTextRange,
    ResolvedModuleFull, ResolvedTypeReferenceDirective, ScriptKind, ScriptTarget, SourceFileLike,
    TypeAcquisition, WatchOptions, AllArenas,
};

pub fn create_compiler_diagnostic_from_message_chain(
    chain: DiagnosticMessageChain,
    related_information: Option<Vec<Id<DiagnosticRelatedInformation>>>,
) -> BaseDiagnostic {
    BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            chain.category,
            chain.code,
            None,
            None,
            None,
            if chain.next.is_some() {
                Into::<DiagnosticMessageText>::into(chain)
            } else {
                Into::<DiagnosticMessageText>::into(chain.message_text)
            },
        ),
        related_information,
    )
}

pub fn chain_diagnostic_messages(
    details: Option<DiagnosticMessageChain>,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticMessageChain {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        text = format_string_from_args(&text, args);
    }
    DiagnosticMessageChain::new(
        text,
        message.category,
        message.code,
        details.map(|details| vec![details]),
    )
}

pub fn chain_diagnostic_messages_multiple(
    details: Vec<DiagnosticMessageChain>,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticMessageChain {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        text = format_string_from_args(&text, args);
    }
    DiagnosticMessageChain::new(text, message.category, message.code, Some(details))
}

pub fn concatenate_diagnostic_message_chains(
    head_chain: &mut DiagnosticMessageChain,
    tail_chain: DiagnosticMessageChain,
) {
    let mut next = &mut head_chain.next;
    loop {
        if next.is_none() {
            *next = Some(vec![tail_chain]);
            return;
        }
        next = &mut next.as_mut().unwrap()[0].next;
    }
}

fn get_diagnostic_file_path(
    diagnostic: &impl DiagnosticRelatedInformationInterface,
    arena: &impl HasArena,
) -> Option<String> {
    diagnostic.maybe_file().and_then(|file| {
        file.ref_(arena).as_source_file()
            .maybe_path()
            .as_ref()
            .map(|path| path.to_string())
    })
}

pub fn compare_diagnostics(
    d1: &impl DiagnosticRelatedInformationInterface,
    d2: &impl DiagnosticRelatedInformationInterface,
    arena: &impl HasArena,
) -> Comparison {
    let mut compared = compare_diagnostics_skip_related_information(d1, d2, arena);
    if compared != Comparison::EqualTo {
        return compared;
    }
    if let Some(d1) = d1.maybe_as_diagnostic() {
        if let Some(d2) = d2.maybe_as_diagnostic() {
            compared = compare_related_information(d1, d2, arena);
            if compared != Comparison::EqualTo {
                return compared;
            }
        }
    }
    Comparison::EqualTo
}

pub fn compare_diagnostics_skip_related_information(
    d1: &impl DiagnosticRelatedInformationInterface,
    d2: &impl DiagnosticRelatedInformationInterface,
    arena: &impl HasArena,
) -> Comparison {
    let mut compared = compare_strings_case_sensitive_maybe(
        get_diagnostic_file_path(d1, arena).as_deref(),
        get_diagnostic_file_path(d2, arena).as_deref(),
    );
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(d1.maybe_start(), d2.maybe_start());
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(d1.maybe_length(), d2.maybe_length());
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.code()), Some(d2.code()));
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_message_text(d1.message_text(), d2.message_text());
    if compared != Comparison::EqualTo {
        return compared;
    }
    Comparison::EqualTo
}

fn compare_related_information(d1: &Diagnostic, d2: &Diagnostic, arena: &impl HasArena) -> Comparison {
    if d1.maybe_related_information().is_none() && d2.maybe_related_information().is_none() {
        return Comparison::EqualTo;
    }
    if let Some(d1_related_information) = d1.maybe_related_information().as_ref() {
        if let Some(d2_related_information) = d2.maybe_related_information().as_ref() {
            let compared = compare_values(
                Some(d1_related_information.len()),
                Some(d2_related_information.len()),
            );
            if compared != Comparison::EqualTo {
                return compared;
            }
            let compared_maybe = for_each(d1_related_information, |d1i, index| {
                let d2i = &d2_related_information[index];
                let compared = compare_diagnostics(&**d1i, &**d2i, arena);
                match compared {
                    Comparison::EqualTo => None,
                    compared => Some(compared),
                }
            });
            if let Some(compared) = compared_maybe {
                if compared != Comparison::EqualTo {
                    return compared;
                }
            }
            return Comparison::EqualTo;
        }
    }
    if d1.maybe_related_information().is_some() {
        Comparison::LessThan
    } else {
        Comparison::GreaterThan
    }
}

fn compare_message_text(t1: &DiagnosticMessageText, t2: &DiagnosticMessageText) -> Comparison {
    if let DiagnosticMessageText::String(t1) = t1 {
        if let DiagnosticMessageText::String(t2) = t2 {
            return compare_strings_case_sensitive(t1, t2);
        }
    }
    if matches!(t1, DiagnosticMessageText::String(_)) {
        return Comparison::LessThan;
    }
    if matches!(t2, DiagnosticMessageText::String(_)) {
        return Comparison::GreaterThan;
    }
    let t1 = enum_unwrapped!(t1, [DiagnosticMessageText, DiagnosticMessageChain]);
    let t2 = enum_unwrapped!(t2, [DiagnosticMessageText, DiagnosticMessageChain]);
    let mut res = compare_strings_case_sensitive(&t1.message_text, &t2.message_text);
    if res != Comparison::EqualTo {
        return res;
    }
    if t1.next.is_none() && t2.next.is_none() {
        return Comparison::EqualTo;
    }
    if t1.next.is_none() {
        return Comparison::LessThan;
    }
    if t2.next.is_none() {
        return Comparison::GreaterThan;
    }
    let t1_next = t1.next.as_ref().unwrap();
    let t2_next = t2.next.as_ref().unwrap();
    let len = cmp::min(t1_next.len(), t2_next.len());
    for i in 0..len {
        res = compare_message_text(&t1_next[i].clone().into(), &t2_next[i].clone().into());
        if res != Comparison::EqualTo {
            return res;
        }
    }
    if t1_next.len() < t2_next.len() {
        return Comparison::LessThan;
    }
    if t1_next.len() > t2_next.len() {
        return Comparison::GreaterThan;
    }
    Comparison::EqualTo
}

pub fn get_language_variant(script_kind: ScriptKind) -> LanguageVariant {
    match script_kind {
        ScriptKind::TSX | ScriptKind::JSX | ScriptKind::JS | ScriptKind::JSON => {
            LanguageVariant::JSX
        }
        _ => LanguageVariant::Standard,
    }
}

pub fn get_emit_script_target(compiler_options: &CompilerOptions) -> ScriptTarget {
    get_emit_script_target_from_module_and_target(compiler_options.module, compiler_options.target)
}

pub fn get_emit_script_target_from_module_and_target(
    module: Option<ModuleKind>,
    target: Option<ScriptTarget>,
) -> ScriptTarget {
    target.unwrap_or_else(|| {
        if matches!(module, Some(ModuleKind::Node12)) {
            ScriptTarget::ES2020
        } else if matches!(module, Some(ModuleKind::NodeNext)) {
            ScriptTarget::ESNext
        } else {
            ScriptTarget::ES3
        }
    })
}

pub fn get_emit_module_kind(compiler_options: &CompilerOptions) -> ModuleKind {
    get_emit_module_kind_from_module_and_target(compiler_options.module, compiler_options.target)
}

pub fn get_emit_module_kind_from_module_and_target(
    module: Option<ModuleKind>,
    target: Option<ScriptTarget>,
) -> ModuleKind {
    module.unwrap_or_else(|| {
        if get_emit_script_target_from_module_and_target(module, target) >= ScriptTarget::ES2015 {
            ModuleKind::ES2015
        } else {
            ModuleKind::CommonJS
        }
    })
}

pub fn get_emit_module_resolution_kind(compiler_options: &CompilerOptions) -> ModuleResolutionKind {
    let mut module_resolution = compiler_options.module_resolution;
    if module_resolution.is_none() {
        match get_emit_module_kind(compiler_options) {
            ModuleKind::CommonJS => {
                module_resolution = Some(ModuleResolutionKind::NodeJs);
            }
            ModuleKind::Node12 => {
                module_resolution = Some(ModuleResolutionKind::Node12);
            }
            ModuleKind::NodeNext => {
                module_resolution = Some(ModuleResolutionKind::NodeNext);
            }
            _ => {
                module_resolution = Some(ModuleResolutionKind::Classic);
            }
        }
    }
    module_resolution.unwrap()
}

pub fn has_json_module_emit_enabled(options: &CompilerOptions) -> bool {
    matches!(
        get_emit_module_kind(options),
        ModuleKind::CommonJS
            | ModuleKind::AMD
            | ModuleKind::ES2015
            | ModuleKind::ES2020
            | ModuleKind::ES2022
            | ModuleKind::ESNext
            | ModuleKind::Node12
            | ModuleKind::NodeNext
    )
}

pub fn unreachable_code_is_error(options: &CompilerOptions) -> bool {
    matches!(options.allow_unreachable_code, Some(false))
}

pub fn unused_label_is_error(options: &CompilerOptions) -> bool {
    matches!(options.allow_unused_labels, Some(false))
}

// TODO: should remove these if unused (I don't remember what they were for)?
#[allow(dead_code)]
fn json_value_to_bool(value: Option<serde_json::Value>) -> Option<bool> {
    value.and_then(|value| match value {
        serde_json::Value::Null => None,
        serde_json::Value::Bool(value) => Some(value),
        _ => panic!("Expected bool"),
    })
}

#[allow(dead_code)]
fn json_value_to_string(value: Option<serde_json::Value>) -> Option<String> {
    value.and_then(|value| match value {
        serde_json::Value::Null => None,
        serde_json::Value::String(value) => Some(value),
        _ => panic!("Expected string"),
    })
}

#[allow(dead_code)]
fn json_value_to_map_value<TReturn, TCallback: Fn(&CommandLineOptionMapTypeValue) -> TReturn>(
    value: Option<serde_json::Value>,
    option: &CommandLineOption,
    callback: TCallback,
) -> Option<TReturn> {
    value.and_then(|value| match value {
        serde_json::Value::Null => None,
        serde_json::Value::String(value) => match option.type_() {
            CommandLineOptionType::Map(map) => map.get(&&*value).map(callback),
            _ => panic!("Expected map"),
        },
        _ => panic!("Expected string"),
    })
}

#[allow(dead_code)]
fn json_value_to_vec_string(value: Option<serde_json::Value>) -> Option<Vec<String>> {
    value.and_then(|value| match value {
        serde_json::Value::Null => None,
        serde_json::Value::Array(value) => Some(
            value
                .into_iter()
                .map(|item| match item {
                    serde_json::Value::String(item) => item,
                    _ => panic!("Expected string"),
                })
                .collect::<Vec<_>>(),
        ),
        _ => panic!("Expected array"),
    })
}

#[allow(dead_code)]
fn json_value_to_usize(value: Option<serde_json::Value>) -> Option<usize> {
    value.and_then(|value| match value {
        serde_json::Value::Null => None,
        serde_json::Value::Number(value) => Some(
            value
                .as_u64()
                .expect("Couldn't convert JSON number to u64")
                .try_into()
                .expect("Couldn't convert u64 to usize"),
        ),
        _ => panic!("Expected number"),
    })
}

#[allow(dead_code)]
fn json_value_to_map_like_vec_string(
    value: Option<serde_json::Value>,
) -> Option<MapLike<Vec<String>>> {
    value.and_then(|value| match value {
        serde_json::Value::Null => None,
        serde_json::Value::Object(value) => Some({
            let mut map: HashMap<String, Vec<String>> = HashMap::new();
            for (key, map_value) in value {
                map.insert(
                    key,
                    match map_value {
                        serde_json::Value::Array(map_value) => map_value
                            .into_iter()
                            .map(|item| match item {
                                serde_json::Value::String(item) => item,
                                _ => panic!("Expected string"),
                            })
                            .collect::<Vec<_>>(),
                        _ => panic!("Expected array"),
                    },
                );
            }
            map
        }),
        _ => panic!("Expected object"),
    })
}

#[allow(dead_code)]
fn json_value_to_vec_plugin_import(value: Option<serde_json::Value>) -> Option<Vec<PluginImport>> {
    value.and_then(|value| match value {
        serde_json::Value::Null => None,
        serde_json::Value::Array(value) => Some(
            value
                .into_iter()
                .map(|item| match item {
                    serde_json::Value::Object(item) => PluginImport {
                        name: match item.get("name").expect("Expected name") {
                            serde_json::Value::String(name) => name.clone(),
                            _ => panic!("Expected string"),
                        },
                    },
                    _ => panic!("Expected object"),
                })
                .collect::<Vec<_>>(),
        ),
        _ => panic!("Expected array"),
    })
}

pub(crate) fn set_type_acquisition_value(
    options: &mut TypeAcquisition,
    option: &CommandLineOption,
    value: CompilerOptionsValue,
) {
    match option.name() {
        "enableAutoDiscovery" => {
            options.enable_auto_discovery = value.into_option_bool();
        }
        "enable" => {
            options.enable = value.into_option_bool();
        }
        "include" => {
            options.include = value.into_option_vec_string();
        }
        "exclude" => {
            options.exclude = value.into_option_vec_string();
        }
        "disableFilenameBasedTypeAcquisition" => {
            options.disable_filename_based_type_acquisition = value.into_option_bool();
        }
        _ => panic!("Unknown type acquisition field: {:?}", option.name()),
    }
}

// json_value_to_map_value(value, option, |map_value| match map_value {
//     CommandLineOptionMapTypeValue::WatchDirectoryKind(map_value) => *map_value,
//     _ => panic!("Expected WatchDirectoryKind"),
// });

pub(crate) fn set_watch_option_value(
    options: &mut WatchOptions,
    option: &CommandLineOption,
    value: CompilerOptionsValue,
) {
    match option.name() {
        "watchFile" => {
            options.watch_file = value.into_option_watch_file_kind();
        }
        "watchDirectory" => {
            options.watch_directory = value.into_option_watch_directory_kind();
        }
        "fallbackPolling" => {
            options.fallback_polling = value.into_option_polling_watch_kind();
        }
        "synchronousWatchDirectory" => {
            options.synchronous_watch_directory = value.into_option_bool();
        }
        "excludeDirectories" => {
            options.exclude_directories = value.into_option_vec_string();
        }
        "excludeFiles" => {
            options.exclude_files = value.into_option_vec_string();
        }
        _ => panic!("Unknown watch option: {:?}", option.name()),
    }
}

fn lookup_compiler_option_value(options: &CompilerOptions, name: &str) -> CompilerOptionsValue {
    match name {
        "all" => CompilerOptionsValue::Bool(options.all.clone()),
        "allowJs" => CompilerOptionsValue::Bool(options.allow_js.clone()),
        "allowNonTsExtensions" => {
            CompilerOptionsValue::Bool(options.allow_non_ts_extensions.clone())
        }
        "allowSyntheticDefaultImports" => {
            CompilerOptionsValue::Bool(options.allow_synthetic_default_imports.clone())
        }
        "allowUmdGlobalAccess" => {
            CompilerOptionsValue::Bool(options.allow_umd_global_access.clone())
        }
        "allowUnreachableCode" => {
            CompilerOptionsValue::Bool(options.allow_unreachable_code.clone())
        }
        "allowUnusedLabels" => CompilerOptionsValue::Bool(options.allow_unused_labels.clone()),
        "alwaysStrict" => CompilerOptionsValue::Bool(options.always_strict.clone()),
        "baseUrl" => CompilerOptionsValue::String(options.base_url.clone()),
        "build" => CompilerOptionsValue::Bool(options.build.clone()),
        "charset" => CompilerOptionsValue::String(options.charset.clone()),
        "checkJs" => CompilerOptionsValue::Bool(options.check_js.clone()),
        "configFilePath" => CompilerOptionsValue::String(options.config_file_path.clone()),
        "configFile" => CompilerOptionsValue::SourceFile(options.config_file.clone()),
        "declaration" => CompilerOptionsValue::Bool(options.declaration.clone()),
        "declarationMap" => CompilerOptionsValue::Bool(options.declaration_map.clone()),
        "emitDeclarationOnly" => CompilerOptionsValue::Bool(options.emit_declaration_only.clone()),
        "declarationDir" => CompilerOptionsValue::String(options.declaration_dir.clone()),
        "diagnostics" => CompilerOptionsValue::Bool(options.diagnostics.clone()),
        "extendedDiagnostics" => CompilerOptionsValue::Bool(options.extended_diagnostics.clone()),
        "disableSizeLimit" => CompilerOptionsValue::Bool(options.disable_size_limit.clone()),
        "disableSourceOfProjectReferenceRedirect" => {
            CompilerOptionsValue::Bool(options.disable_source_of_project_reference_redirect.clone())
        }
        "disableSolutionSearching" => {
            CompilerOptionsValue::Bool(options.disable_solution_searching.clone())
        }
        "disableReferencedProjectLoad" => {
            CompilerOptionsValue::Bool(options.disable_referenced_project_load.clone())
        }
        "downlevelIteration" => CompilerOptionsValue::Bool(options.downlevel_iteration.clone()),
        "emitBom" => CompilerOptionsValue::Bool(options.emit_bom.clone()),
        "emitDecoratorMetadata" => {
            CompilerOptionsValue::Bool(options.emit_decorator_metadata.clone())
        }
        "exactOptionalPropertyTypes" => {
            CompilerOptionsValue::Bool(options.exact_optional_property_types.clone())
        }
        "experimentalDecorators" => {
            CompilerOptionsValue::Bool(options.experimental_decorators.clone())
        }
        "forceConsistentCasingInFileNames" => {
            CompilerOptionsValue::Bool(options.force_consistent_casing_in_file_names.clone())
        }
        "generateCpuProfile" => CompilerOptionsValue::String(options.generate_cpu_profile.clone()),
        "generateTrace" => CompilerOptionsValue::String(options.generate_trace.clone()),
        "help" => CompilerOptionsValue::Bool(options.help.clone()),
        "importHelpers" => CompilerOptionsValue::Bool(options.import_helpers.clone()),
        "importsNotUsedAsValues" => {
            CompilerOptionsValue::ImportsNotUsedAsValues(options.imports_not_used_as_values.clone())
        }
        "init" => CompilerOptionsValue::Bool(options.init.clone()),
        "inlineSourceMap" => CompilerOptionsValue::Bool(options.inline_source_map.clone()),
        "inlineSources" => CompilerOptionsValue::Bool(options.inline_sources.clone()),
        "isolatedModules" => CompilerOptionsValue::Bool(options.isolated_modules.clone()),
        "jsx" => CompilerOptionsValue::JsxEmit(options.jsx.clone()),
        "keyofStringsOnly" => CompilerOptionsValue::Bool(options.keyof_strings_only.clone()),
        "lib" => CompilerOptionsValue::VecString(options.lib.clone()),
        "listEmittedFiles" => CompilerOptionsValue::Bool(options.list_emitted_files.clone()),
        "listFiles" => CompilerOptionsValue::Bool(options.list_files.clone()),
        "explainFiles" => CompilerOptionsValue::Bool(options.explain_files.clone()),
        "listFilesOnly" => CompilerOptionsValue::Bool(options.list_files_only.clone()),
        "locale" => CompilerOptionsValue::String(options.locale.clone()),
        "mapRoot" => CompilerOptionsValue::String(options.map_root.clone()),
        "maxNodeModuleJsDepth" => {
            CompilerOptionsValue::Usize(options.max_node_module_js_depth.clone())
        }
        "module" => CompilerOptionsValue::ModuleKind(options.module.clone()),
        "moduleResolution" => {
            CompilerOptionsValue::ModuleResolutionKind(options.module_resolution.clone())
        }
        "newLine" => CompilerOptionsValue::NewLineKind(options.new_line.clone()),
        "noEmit" => CompilerOptionsValue::Bool(options.no_emit.clone()),
        "noEmitForJsFiles" => CompilerOptionsValue::Bool(options.no_emit_for_js_files.clone()),
        "noEmitHelpers" => CompilerOptionsValue::Bool(options.no_emit_helpers.clone()),
        "noEmitOnError" => CompilerOptionsValue::Bool(options.no_emit_on_error.clone()),
        "noErrorTruncation" => CompilerOptionsValue::Bool(options.no_error_truncation.clone()),
        "noFallthroughCasesInSwitch" => {
            CompilerOptionsValue::Bool(options.no_fallthrough_cases_in_switch.clone())
        }
        "noImplicitAny" => CompilerOptionsValue::Bool(options.no_implicit_any.clone()),
        "noImplicitReturns" => CompilerOptionsValue::Bool(options.no_implicit_returns.clone()),
        "noImplicitThis" => CompilerOptionsValue::Bool(options.no_implicit_this.clone()),
        "noStrictGenericChecks" => {
            CompilerOptionsValue::Bool(options.no_strict_generic_checks.clone())
        }
        "noUnusedLocals" => CompilerOptionsValue::Bool(options.no_unused_locals.clone()),
        "noUnusedParameters" => CompilerOptionsValue::Bool(options.no_unused_parameters.clone()),
        "noImplicitUseStrict" => CompilerOptionsValue::Bool(options.no_implicit_use_strict.clone()),
        "noPropertyAccessFromIndexSignature" => {
            CompilerOptionsValue::Bool(options.no_property_access_from_index_signature.clone())
        }
        "assumeChangesOnlyAffectDirectDependencies" => CompilerOptionsValue::Bool(
            options
                .assume_changes_only_affect_direct_dependencies
                .clone(),
        ),
        "noLib" => CompilerOptionsValue::Bool(options.no_lib.clone()),
        "noResolve" => CompilerOptionsValue::Bool(options.no_resolve.clone()),
        "noUncheckedIndexedAccess" => {
            CompilerOptionsValue::Bool(options.no_unchecked_indexed_access.clone())
        }
        "out" => CompilerOptionsValue::String(options.out.clone()),
        "outDir" => CompilerOptionsValue::String(options.out_dir.clone()),
        "outFile" => CompilerOptionsValue::String(options.out_file.clone()),
        "paths" => CompilerOptionsValue::MapLikeVecString(options.paths.clone()),
        "pathsBasePath" => CompilerOptionsValue::String(options.paths_base_path.clone()),
        "plugins" => CompilerOptionsValue::VecPluginImport(options.plugins.clone()),
        "preserveConstEnums" => CompilerOptionsValue::Bool(options.preserve_const_enums.clone()),
        "noImplicitOverride" => CompilerOptionsValue::Bool(options.no_implicit_override.clone()),
        "preserveSymlinks" => CompilerOptionsValue::Bool(options.preserve_symlinks.clone()),
        "preserveValueImports" => {
            CompilerOptionsValue::Bool(options.preserve_value_imports.clone())
        }
        "preserveWatchOutput" => CompilerOptionsValue::Bool(options.preserve_watch_output.clone()),
        "project" => CompilerOptionsValue::String(options.project.clone()),
        "pretty" => CompilerOptionsValue::Bool(options.pretty.clone()),
        "reactNamespace" => CompilerOptionsValue::String(options.react_namespace.clone()),
        "jsxFactory" => CompilerOptionsValue::String(options.jsx_factory.clone()),
        "jsxFragmentFactory" => CompilerOptionsValue::String(options.jsx_fragment_factory.clone()),
        "jsxImportSource" => CompilerOptionsValue::String(options.jsx_import_source.clone()),
        "composite" => CompilerOptionsValue::Bool(options.composite.clone()),
        "incremental" => CompilerOptionsValue::Bool(options.incremental.clone()),
        "tsBuildInfoFile" => CompilerOptionsValue::String(options.ts_build_info_file.clone()),
        "removeComments" => CompilerOptionsValue::Bool(options.remove_comments.clone()),
        "rootDir" => CompilerOptionsValue::String(options.root_dir.clone()),
        "rootDirs" => CompilerOptionsValue::VecString(options.root_dirs.clone()),
        "skipLibCheck" => CompilerOptionsValue::Bool(options.skip_lib_check.clone()),
        "skipDefaultLibCheck" => CompilerOptionsValue::Bool(options.skip_default_lib_check.clone()),
        "sourceMap" => CompilerOptionsValue::Bool(options.source_map.clone()),
        "sourceRoot" => CompilerOptionsValue::String(options.source_root.clone()),
        "strict" => CompilerOptionsValue::Bool(options.strict.clone()),
        "strictFunctionTypes" => CompilerOptionsValue::Bool(options.strict_function_types.clone()),
        "strictBindCallApply" => CompilerOptionsValue::Bool(options.strict_bind_call_apply.clone()),
        "strictNullChecks" => CompilerOptionsValue::Bool(options.strict_null_checks.clone()),
        "strictPropertyInitialization" => {
            CompilerOptionsValue::Bool(options.strict_property_initialization.clone())
        }
        "stripInternal" => CompilerOptionsValue::Bool(options.strip_internal.clone()),
        "suppressExcessPropertyErrors" => {
            CompilerOptionsValue::Bool(options.suppress_excess_property_errors.clone())
        }
        "suppressImplicitAnyIndexErrors" => {
            CompilerOptionsValue::Bool(options.suppress_implicit_any_index_errors.clone())
        }
        "suppressOutputPathCheck" => {
            CompilerOptionsValue::Bool(options.suppress_output_path_check.clone())
        }
        "target" => CompilerOptionsValue::ScriptTarget(options.target.clone()),
        "traceResolution" => CompilerOptionsValue::Bool(options.trace_resolution.clone()),
        "useUnknownInCatchVariables" => {
            CompilerOptionsValue::Bool(options.use_unknown_in_catch_variables.clone())
        }
        "resolveJsonModule" => CompilerOptionsValue::Bool(options.resolve_json_module.clone()),
        "types" => CompilerOptionsValue::VecString(options.types.clone()),
        "typeRoots" => CompilerOptionsValue::VecString(options.type_roots.clone()),
        "version" => CompilerOptionsValue::Bool(options.version.clone()),
        "watch" => CompilerOptionsValue::Bool(options.watch.clone()),
        "esModuleInterop" => CompilerOptionsValue::Bool(options.es_module_interop.clone()),
        "showConfig" => CompilerOptionsValue::Bool(options.show_config.clone()),
        "useDefineForClassFields" => {
            CompilerOptionsValue::Bool(options.use_define_for_class_fields.clone())
        }
        _ => panic!("Unknown compiler option: {:?}", name),
    }
}

pub fn get_are_declaration_maps_enabled(options: &CompilerOptions) -> bool {
    get_emit_declarations(options) && options.declaration_map == Some(true)
}

pub fn get_es_module_interop(compiler_options: &CompilerOptions) -> Option<bool> {
    if compiler_options.es_module_interop.is_some() {
        return compiler_options.es_module_interop;
    }
    match get_emit_module_kind(compiler_options) {
        ModuleKind::Node12 | ModuleKind::NodeNext => {
            return Some(true);
        }
        _ => (),
    }
    None
}

pub fn get_allow_synthetic_default_imports(compiler_options: &CompilerOptions) -> bool {
    let module_kind = get_emit_module_kind(compiler_options);
    if compiler_options.allow_synthetic_default_imports.is_some() {
        compiler_options.allow_synthetic_default_imports.unwrap()
    } else {
        matches!(get_es_module_interop(compiler_options), Some(true))
            || module_kind == ModuleKind::System
    }
}

pub fn get_emit_declarations(compiler_options: &CompilerOptions) -> bool {
    compiler_options.declaration == Some(true) || compiler_options.composite == Some(true)
}

pub fn should_preserve_const_enums(compiler_options: &CompilerOptions) -> bool {
    matches!(compiler_options.preserve_const_enums, Some(true))
        || matches!(compiler_options.isolated_modules, Some(true))
}

pub fn is_incremental_compilation(compiler_options: &CompilerOptions) -> bool {
    matches!(compiler_options.incremental, Some(true))
        || matches!(compiler_options.composite, Some(true))
}

pub fn get_strict_option_value(
    compiler_options: &CompilerOptions,
    flag: &str, /*StrictOptionName*/
) -> bool {
    match lookup_compiler_option_value(compiler_options, flag).as_option_bool() {
        None => compiler_options.strict.unwrap_or(false),
        Some(bool_) => bool_,
    }
}

pub fn get_allow_js_compiler_option(compiler_options: &CompilerOptions) -> bool {
    compiler_options
        .allow_js
        .unwrap_or_else(|| matches!(compiler_options.check_js, Some(true)))
}

pub fn get_use_define_for_class_fields(compiler_options: &CompilerOptions) -> bool {
    if compiler_options.use_define_for_class_fields.is_none() {
        get_emit_script_target(compiler_options) == ScriptTarget::ESNext
    } else {
        compiler_options.use_define_for_class_fields.unwrap()
    }
}

pub fn get_compiler_option_value(
    options: &CompilerOptions,
    option: &CommandLineOption,
) -> CompilerOptionsValue {
    if option.strict_flag() {
        CompilerOptionsValue::Bool(Some(get_strict_option_value(options, option.name())))
    } else {
        lookup_compiler_option_value(options, option.name())
    }
}

pub fn get_jsx_implicit_import_base(
    compiler_options: &CompilerOptions,
    file: Option<&Node /*SourceFile*/>,
) -> Option<String> {
    let jsx_import_source_pragmas = file.and_then(|file| {
        file.as_source_file()
            .pragmas()
            .get(&PragmaName::Jsximportsource)
            .cloned()
    });
    let jsx_import_source_pragma = jsx_import_source_pragmas
        .as_ref()
        .filter(|jsx_import_source_pragmas| !jsx_import_source_pragmas.is_empty())
        .map(|jsx_import_source_pragmas| {
            jsx_import_source_pragmas[jsx_import_source_pragmas.len() - 1].clone()
        });
    if matches!(
        compiler_options.jsx,
        Some(JsxEmit::ReactJSX) | Some(JsxEmit::ReactJSXDev)
    ) || compiler_options.jsx_import_source.is_some()
        || jsx_import_source_pragma.is_some()
    {
        Some(
            jsx_import_source_pragma
                .as_ref()
                .map(|jsx_import_source_pragma| {
                    jsx_import_source_pragma
                        .arguments
                        .get(&PragmaArgumentName::Factory)
                        .unwrap()
                        .as_without_captured_span()
                        .clone()
                })
                .or_else(|| compiler_options.jsx_import_source.clone())
                .unwrap_or_else(|| "react".to_owned()),
        )
    } else {
        None
    }
}

pub fn get_jsx_runtime_import(base: Option<&str>, options: &CompilerOptions) -> Option<String> {
    base.map(|base| {
        format!(
            "{}/{}",
            base,
            if options.jsx == Some(JsxEmit::ReactJSXDev) {
                "jsx-dev-runtime"
            } else {
                "jsx-runtime"
            }
        )
    })
}

pub fn get_jsx_transform_enabled(options: &CompilerOptions) -> bool {
    let jsx = options.jsx;
    matches!(
        jsx,
        Some(JsxEmit::React | JsxEmit::ReactJSX | JsxEmit::ReactJSXDev)
    )
}

pub fn has_zero_or_one_asterisk_character(str_: &str) -> bool {
    let mut seen_asterisk = false;
    for ch in str_.chars() {
        if ch == CharacterCodes::asterisk {
            if !seen_asterisk {
                seen_asterisk = true;
            } else {
                return false;
            }
        }
    }
    true
}

#[derive(Debug)]
pub struct SymlinkedDirectory {
    pub real: String,
    pub real_path: Path,
}

#[derive(Trace, Finalize)]
pub struct SymlinkCache {
    cwd: String,
    get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>,
    #[unsafe_ignore_trace]
    symlinked_files: RefCell<Option<HashMap<Path, String>>>,
    #[unsafe_ignore_trace]
    symlinked_directories: RefCell<Option<HashMap<Path, Option<SymlinkedDirectory>>>>,
    #[unsafe_ignore_trace]
    symlinked_directories_by_realpath: RefCell<Option<MultiMap<Path, String>>>,
    #[unsafe_ignore_trace]
    has_processed_resolutions: Cell<bool>,
}

impl fmt::Debug for SymlinkCache {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SymlinkCache").finish()
    }
}

impl SymlinkCache {
    pub fn new(cwd: &str, get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>) -> Self {
        Self {
            cwd: cwd.to_owned(),
            get_canonical_file_name,
            symlinked_files: RefCell::new(None),
            symlinked_directories: RefCell::new(None),
            symlinked_directories_by_realpath: RefCell::new(None),
            has_processed_resolutions: Cell::new(false),
        }
    }

    fn symlinked_files(&self) -> RefMut<Option<HashMap<Path, String>>> {
        self.symlinked_files.borrow_mut()
    }

    fn set_has_processed_resolutions(&self, has_processed_resolutions: bool) {
        self.has_processed_resolutions
            .set(has_processed_resolutions);
    }

    pub fn get_symlinked_files(&self) -> Ref<Option<HashMap<Path, String>>> {
        self.symlinked_files.borrow()
    }

    pub fn get_symlinked_directories(
        &self,
    ) -> Ref<Option<HashMap<Path, Option<SymlinkedDirectory>>>> {
        self.symlinked_directories.borrow()
    }

    pub fn get_symlinked_directories_by_realpath(&self) -> Ref<Option<MultiMap<Path, String>>> {
        self.symlinked_directories_by_realpath.borrow()
    }

    pub fn set_symlinked_file(&self, path: &Path, real: &str) {
        let mut symlinked_files = self.symlinked_files();
        if symlinked_files.is_none() {
            *symlinked_files = Some(HashMap::new());
        }
        symlinked_files
            .as_mut()
            .unwrap()
            .insert(path.clone(), real.to_owned());
    }

    pub fn set_symlinked_directory(&self, symlink: &str, real: Option<SymlinkedDirectory>) {
        let mut symlink_path = to_path(symlink, Some(&self.cwd), |file_name| {
            self.get_canonical_file_name.call(file_name)
        });
        if !contains_ignored_path(&symlink_path) {
            symlink_path = ensure_trailing_directory_separator(&symlink_path).into();
            if let Some(real) = real.as_ref() {
                if !matches!(
                    &*self.symlinked_directories.borrow(),
                    Some(symlinked_directories) if symlinked_directories.contains_key(&symlink_path)
                ) {
                    let mut symlinked_directories_by_realpath =
                        self.symlinked_directories_by_realpath.borrow_mut();
                    if symlinked_directories_by_realpath.is_none() {
                        *symlinked_directories_by_realpath = Some(create_multi_map());
                    }
                    symlinked_directories_by_realpath.as_mut().unwrap().add(
                        ensure_trailing_directory_separator(&real.real_path).into(),
                        symlink.to_owned(),
                    );
                }
            }
            self.symlinked_directories
                .borrow_mut()
                .get_or_insert_default_()
                .insert(symlink_path, real);
        }
    }

    pub fn set_symlinked_directory_from_symlinked_file(&self, symlink: &str, real: &str) {
        self.set_symlinked_file(
            &to_path(symlink, Some(&self.cwd), |file_name| {
                self.get_canonical_file_name.call(file_name)
            }),
            real,
        );
        let guessed = guess_directory_symlink(real, symlink, &self.cwd, |file_name| {
            self.get_canonical_file_name.call(file_name)
        });
        if let Some((common_resolved, common_original)) = guessed {
            if !common_resolved.is_empty() && !common_original.is_empty() {
                self.set_symlinked_directory(
                    &common_original,
                    Some(SymlinkedDirectory {
                        real: common_resolved.clone(),
                        real_path: to_path(&common_resolved, Some(&self.cwd), |file_name| {
                            self.get_canonical_file_name.call(file_name)
                        }),
                    }),
                );
            }
        }
    }

    pub fn set_symlinks_from_resolutions(
        &self,
        files: &[Id<Node /*SourceFile*/>],
        type_reference_directives: Option<
            &HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>,
        >,
    ) {
        Debug_.assert(!self.has_processed_resolutions(), None);
        self.set_has_processed_resolutions(true);
        for file in files {
            if let Some(file_resolved_modules) =
                file.ref_(self).as_source_file().maybe_resolved_modules().as_ref()
            {
                file_resolved_modules.for_each(|resolution, _, _| {
                    self.process_resolution(resolution.clone().map(Into::into))
                });
            }
        }
        if let Some(type_reference_directives) = type_reference_directives {
            for resolution in type_reference_directives.values() {
                self.process_resolution(resolution.clone().map(Into::into));
            }
        }
    }

    pub fn has_processed_resolutions(&self) -> bool {
        self.has_processed_resolutions.get()
    }

    fn process_resolution(
        &self,
        resolution: Option<ResolvedModuleFullOrResolvedTypeReferenceDirective>,
    ) {
        if resolution.is_none() {
            return;
        }
        let resolution = resolution.unwrap();
        if resolution.maybe_original_path(self).is_none()
            || resolution.maybe_resolved_file_name(self).is_none()
        {
            return;
        }
        let ref resolved_file_name = resolution.maybe_resolved_file_name(self).unwrap();
        let ref original_path = resolution.maybe_original_path(self).unwrap();
        self.set_symlinked_file(
            &to_path(original_path, Some(&self.cwd), |file_name| {
                self.get_canonical_file_name.call(file_name)
            }),
            resolved_file_name,
        );
        let guessed =
            guess_directory_symlink(resolved_file_name, original_path, &self.cwd, |file_name| {
                self.get_canonical_file_name.call(file_name)
            });
        if let Some((common_resolved, common_original)) = guessed {
            if !common_resolved.is_empty() && !common_original.is_empty() {
                self.set_symlinked_directory(
                    &common_original,
                    Some(SymlinkedDirectory {
                        real: common_resolved.clone(),
                        real_path: to_path(&common_resolved, Some(&self.cwd), |file_name| {
                            self.get_canonical_file_name.call(file_name)
                        }),
                    }),
                );
            }
        }
    }
}

impl HasArena for SymlinkCache {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

enum ResolvedModuleFullOrResolvedTypeReferenceDirective {
    ResolvedModuleFull(Gc<ResolvedModuleFull>),
    ResolvedTypeReferenceDirective(Id<ResolvedTypeReferenceDirective>),
}

impl ResolvedModuleFullOrResolvedTypeReferenceDirective {
    pub fn maybe_original_path(&self, arena: &impl HasArena) -> Option<String> {
        match self {
            Self::ResolvedModuleFull(value) => value.original_path.clone(),
            Self::ResolvedTypeReferenceDirective(value) => value.ref_(arena).original_path.clone(),
        }
    }

    pub fn maybe_resolved_file_name(&self, arena: &impl HasArena) -> Option<String> {
        match self {
            Self::ResolvedModuleFull(value) => Some(value.resolved_file_name.clone()),
            Self::ResolvedTypeReferenceDirective(value) => value.ref_(arena).resolved_file_name.clone(),
        }
    }
}

impl From<Gc<ResolvedModuleFull>> for ResolvedModuleFullOrResolvedTypeReferenceDirective {
    fn from(value: Gc<ResolvedModuleFull>) -> Self {
        Self::ResolvedModuleFull(value)
    }
}

impl From<Id<ResolvedTypeReferenceDirective>>
    for ResolvedModuleFullOrResolvedTypeReferenceDirective
{
    fn from(value: Id<ResolvedTypeReferenceDirective>) -> Self {
        Self::ResolvedTypeReferenceDirective(value)
    }
}

pub fn create_symlink_cache(
    cwd: &str,
    get_canonical_file_name: Gc<Box<dyn GetCanonicalFileName>>,
) -> SymlinkCache {
    SymlinkCache::new(cwd, get_canonical_file_name)
}

fn guess_directory_symlink(
    a: &str,
    b: &str,
    cwd: &str,
    get_canonical_file_name: impl Fn(&str) -> String,
) -> Option<(String, String)> {
    let mut a_parts = get_path_components(&get_normalized_absolute_path(a, Some(cwd)), None);
    let mut b_parts = get_path_components(&get_normalized_absolute_path(b, Some(cwd)), None);
    let mut is_directory = false;
    while a_parts.len() >= 2
        && b_parts.len() >= 2
        && !is_node_modules_or_scoped_package_directory(
            Some(&a_parts[a_parts.len() - 2]),
            |file_name| get_canonical_file_name(file_name),
        )
        && !is_node_modules_or_scoped_package_directory(
            Some(&b_parts[b_parts.len() - 2]),
            |file_name| get_canonical_file_name(file_name),
        )
        && get_canonical_file_name(&a_parts[a_parts.len() - 1])
            == get_canonical_file_name(&b_parts[b_parts.len() - 1])
    {
        a_parts.pop();
        b_parts.pop();
        is_directory = true;
    }
    is_directory.then(|| {
        (
            get_path_from_path_components(&a_parts),
            get_path_from_path_components(&b_parts),
        )
    })
}

fn is_node_modules_or_scoped_package_directory(
    s: Option<&str>,
    get_canonical_file_name: impl Fn(&str) -> String,
) -> bool {
    s.matches(|s| get_canonical_file_name(s) == "node_modules" || starts_with(s, "@"))
}

lazy_static! {
    static ref reserved_character_pattern: Regex = Regex::new(r"[^\w\s/]").unwrap();
}

pub fn reg_exp_escape(text: &str) -> String {
    regex::escape(text)
    // reserved_character_pattern.replace_all(text, |captures: &Captures| {
    //     escape_reg_exp_character(&captures[0])
    // })
}

#[allow(dead_code)]
fn escape_reg_exp_character(match_: &str) -> String {
    format!(r#"\{match_}"#)
}

lazy_static! {
    pub static ref common_package_folders: Vec<&'static str> =
        vec!["node_modules", "bower_components", "jspm_packages",];
}

lazy_static! {
    static ref wildcard_char_codes: Vec<char> =
        vec![CharacterCodes::asterisk, CharacterCodes::question];
    static ref implicit_exclude_path_regex_pattern: String =
        format!("(?!({})(/|$))", common_package_folders.join("|"));
}

#[derive(Clone)]
pub struct WildcardMatcher {
    pub single_asterisk_regex_fragment: String,
    pub double_asterisk_regex_fragment: String,
    pub replace_wildcard_character: fn(&str) -> String,
}

lazy_static! {
    static ref files_matcher: WildcardMatcher = WildcardMatcher {
        single_asterisk_regex_fragment: "([^./]|(\\.(?!min\\.js$))?)*".to_owned(),
        double_asterisk_regex_fragment: format!(
            "(/{}[^/.][^/]*)*?",
            &*implicit_exclude_path_regex_pattern
        ),
        replace_wildcard_character: files_matcher_replace_wildcard_character
    };
}

fn files_matcher_replace_wildcard_character(match_: &str) -> String {
    replace_wildcard_character(match_, &files_matcher.single_asterisk_regex_fragment)
}

lazy_static! {
    static ref directories_matcher: WildcardMatcher = WildcardMatcher {
        single_asterisk_regex_fragment: "[^/]*".to_owned(),
        double_asterisk_regex_fragment: format!(
            "(/{}[^/.][^/]*)*?",
            &*implicit_exclude_path_regex_pattern
        ),
        replace_wildcard_character: directories_matcher_replace_wildcard_character
    };
}

fn directories_matcher_replace_wildcard_character(match_: &str) -> String {
    replace_wildcard_character(match_, &directories_matcher.single_asterisk_regex_fragment)
}

lazy_static! {
    static ref exclude_matcher: WildcardMatcher = WildcardMatcher {
        single_asterisk_regex_fragment: "[^/]*".to_owned(),
        double_asterisk_regex_fragment: "(/.+?)?".to_owned(),
        replace_wildcard_character: exclude_matcher_replace_wildcard_character
    };
}

fn exclude_matcher_replace_wildcard_character(match_: &str) -> String {
    replace_wildcard_character(match_, &exclude_matcher.single_asterisk_regex_fragment)
}

lazy_static! {
    static ref wildcard_matchers: HashMap<&'static str, WildcardMatcher> =
        HashMap::from_iter(IntoIterator::into_iter([
            ("files", files_matcher.clone()),
            ("directories", directories_matcher.clone()),
            ("exclude", exclude_matcher.clone()),
        ]));
}

pub fn get_regular_expression_for_wildcard<TSpec: AsRef<str>>(
    specs: Option<&[TSpec]>,
    base_path: &str,
    usage: &str, /*"files" | "directories" | "exclude"*/
) -> Option<String> {
    let patterns = get_regular_expressions_for_wildcards(specs, base_path, usage)?;
    if patterns.is_empty() {
        return None;
    }

    let pattern = patterns
        .iter()
        .map(|pattern| format!("({})", pattern))
        .collect::<Vec<_>>()
        .join("|");
    let terminator = if usage == "exclude" { "($|/)" } else { "$" };
    Some(format!("^({}){}", pattern, terminator))
}

pub fn get_regular_expressions_for_wildcards<TSpec: AsRef<str>>(
    specs: Option<&[TSpec]>,
    base_path: &str,
    usage: &str, /*"files" | "directories" | "exclude"*/
) -> Option<Vec<String>> {
    let specs = specs?;
    if specs.is_empty() {
        return None;
    }

    Some(flat_map(Some(specs), |spec, _| {
        let spec = spec.as_ref();
        if !spec.is_empty() {
            get_sub_pattern_from_spec(
                spec,
                base_path,
                usage,
                wildcard_matchers.get(usage).unwrap(),
            )
        } else {
            None
        }
        .map_or_else(|| vec![], |sub_pattern| vec![sub_pattern])
    }))
}

pub fn is_implicit_glob(last_path_component: &str) -> bool {
    lazy_static! {
        static ref regex: Regex = Regex::new("[.*?]").unwrap();
    }
    !regex.is_match(last_path_component).unwrap()
}

pub fn get_pattern_from_spec(
    spec: &str,
    base_path: &str,
    usage: &str, /*"files" | "directories" | "exclude"*/
) -> Option<String> {
    let pattern = if !spec.is_empty() {
        get_sub_pattern_from_spec(
            spec,
            base_path,
            usage,
            wildcard_matchers.get(usage).unwrap(),
        )
    } else {
        None
    };
    pattern.map(|pattern| {
        format!(
            "^({}){}",
            pattern,
            if usage == "exclude" { "($|/)" } else { "$" }
        )
    })
}

pub fn get_sub_pattern_from_spec(
    spec: &str,
    base_path: &str,
    usage: &str, /*"files" | "directories" | "exclude"*/
    wildcard_matcher: &WildcardMatcher,
) -> Option<String> {
    let single_asterisk_regex_fragment = &wildcard_matcher.single_asterisk_regex_fragment;
    let double_asterisk_regex_fragment = &wildcard_matcher.double_asterisk_regex_fragment;
    let replace_wildcard_character = wildcard_matcher.replace_wildcard_character;
    let mut subpattern = "".to_owned();
    let mut has_written_component = false;
    let mut components = get_normalized_path_components(spec, Some(base_path));
    let last_component = last(&components).clone();
    if usage != "exclude" && last_component == "**" {
        return None;
    }

    components[0] = remove_trailing_directory_separator(&components[0]);

    if is_implicit_glob(&last_component) {
        components.push("**".to_owned());
        components.push("*".to_owned());
    }

    let mut optional_count = 0;
    for component in &components {
        let mut component = component.clone();
        if component == "**" {
            subpattern.push_str(double_asterisk_regex_fragment);
        } else {
            if usage == "directories" {
                subpattern.push_str("(");
                optional_count += 1;
            }

            if has_written_component {
                subpattern.push(directory_separator);
            }

            if usage != "exclude" {
                let mut component_pattern = "".to_owned();
                let component_char_code_at_0 = component.chars().next();
                if component_char_code_at_0 == Some(CharacterCodes::asterisk) {
                    component_pattern
                        .push_str(&format!("([^./]{})?", single_asterisk_regex_fragment));
                    component = component[1..].to_owned();
                } else if component_char_code_at_0 == Some(CharacterCodes::question) {
                    component_pattern.push_str("[^./]");
                    component = component[1..].to_owned();
                }

                component_pattern.push_str(
                    &reserved_character_pattern.replace_all(&component, |captures: &Captures| {
                        replace_wildcard_character(&captures[0])
                    }),
                );

                if component_pattern != component {
                    subpattern.push_str(&implicit_exclude_path_regex_pattern);
                }

                subpattern.push_str(&component_pattern);
            } else {
                subpattern.push_str(
                    &reserved_character_pattern.replace_all(&component, |captures: &Captures| {
                        replace_wildcard_character(&captures[0])
                    }),
                );
            }
        }

        has_written_component = true;
    }

    while optional_count > 0 {
        subpattern.push_str(")?");
        optional_count -= 1;
    }

    Some(subpattern)
}

pub fn replace_wildcard_character(match_: &str, single_asterisk_regex_fragment: &str) -> String {
    if match_ == "*" {
        single_asterisk_regex_fragment.to_owned()
    } else if match_ == "?" {
        "[^/]".to_owned()
    } else {
        format!("\\{}", match_)
    }
}

#[derive(Clone)]
pub struct FileSystemEntries {
    pub files: Vec<String>,
    pub directories: Vec<String>,
}

#[derive(Debug)]
pub struct FileMatcherPatterns {
    pub include_file_patterns: Option<Vec<String>>,
    pub include_file_pattern: Option<String>,
    pub include_directory_pattern: Option<String>,
    pub exclude_pattern: Option<String>,
    pub base_paths: Vec<String>,
}

pub fn get_file_matcher_patterns<TExclude: AsRef<str>, TInclude: AsRef<str>>(
    path: &str,
    excludes: Option<&[TExclude]>,
    includes: Option<&[TInclude]>,
    use_case_sensitive_file_names: bool,
    current_directory: &str,
) -> FileMatcherPatterns {
    let path = normalize_path(path);
    let current_directory = normalize_path(current_directory);
    let absolute_path = combine_paths(&current_directory, &[Some(&*path)]);

    FileMatcherPatterns {
        include_file_patterns: maybe_map(
            get_regular_expressions_for_wildcards(includes, &absolute_path, "files").as_ref(),
            |pattern: &String, _| format!("^{}$", pattern),
        ),
        include_file_pattern: get_regular_expression_for_wildcard(
            includes,
            &absolute_path,
            "files",
        ),
        include_directory_pattern: get_regular_expression_for_wildcard(
            includes,
            &absolute_path,
            "directories",
        ),
        exclude_pattern: get_regular_expression_for_wildcard(excludes, &absolute_path, "exclude"),
        base_paths: get_base_paths(&path, includes, use_case_sensitive_file_names),
    }
}

pub fn get_regex_from_pattern(pattern: &str, use_case_sensitive_file_names: bool) -> Regex {
    Regex::new(&if use_case_sensitive_file_names {
        pattern.to_owned()
    } else {
        format!("(?i){}", pattern)
    })
    .unwrap()
}

pub fn match_files(
    path: &str,
    extensions: Option<&[impl AsRef<str>]>,
    excludes: Option<&[impl AsRef<str>]>,
    includes: Option<&[impl AsRef<str>]>,
    use_case_sensitive_file_names: bool,
    current_directory: &str,
    mut depth: Option<usize>,
    mut get_file_system_entries: impl FnMut(&str) -> FileSystemEntries,
    mut realpath: impl FnMut(&str) -> String,
) -> Vec<String> {
    let path = normalize_path(path);
    let current_directory = normalize_path(current_directory);

    let patterns = get_file_matcher_patterns(
        &path,
        excludes,
        includes,
        use_case_sensitive_file_names,
        &current_directory,
    );

    let include_file_regexes =
        patterns
            .include_file_patterns
            .as_ref()
            .map(|patterns_include_file_patterns| {
                patterns_include_file_patterns
                    .into_iter()
                    .map(|pattern| get_regex_from_pattern(pattern, use_case_sensitive_file_names))
                    .collect::<Vec<_>>()
            });
    let include_directory_regex =
        patterns
            .include_directory_pattern
            .as_ref()
            .map(|patterns_include_directory_pattern| {
                get_regex_from_pattern(
                    patterns_include_directory_pattern,
                    use_case_sensitive_file_names,
                )
            });
    let exclude_regex = patterns
        .exclude_pattern
        .as_ref()
        .map(|patterns_exclude_pattern| {
            get_regex_from_pattern(patterns_exclude_pattern, use_case_sensitive_file_names)
        });

    let mut results: Vec<Vec<String>> =
        if let Some(include_file_regexes) = include_file_regexes.as_ref() {
            include_file_regexes.into_iter().map(|_| vec![]).collect()
        } else {
            vec![vec![]]
        };
    let mut visited: HashMap<String, bool /*true*/> = HashMap::new();
    let to_canonical = create_get_canonical_file_name(use_case_sensitive_file_names);
    for base_path in &patterns.base_paths {
        visit_directory(
            &to_canonical,
            &mut realpath,
            &mut visited,
            &mut get_file_system_entries,
            extensions,
            exclude_regex.as_ref(),
            include_file_regexes.as_deref(),
            &mut results,
            include_directory_regex.as_ref(),
            base_path,
            &combine_paths(&current_directory, &[Some(base_path)]),
            &mut depth,
        );
    }

    flatten(&results)
}

fn visit_directory<
    TRealpath: FnMut(&str) -> String,
    TGetFileSystemEntries: FnMut(&str) -> FileSystemEntries,
    TExtension: AsRef<str>,
    TToCanonical: Fn(&str) -> String,
>(
    to_canonical: &TToCanonical,
    realpath: &mut TRealpath,
    visited: &mut HashMap<String, bool>,
    get_file_system_entries: &mut TGetFileSystemEntries,
    extensions: Option<&[TExtension]>,
    exclude_regex: Option<&Regex>,
    include_file_regexes: Option<&[Regex]>,
    results: &mut Vec<Vec<String>>,
    include_directory_regex: Option<&Regex>,
    path: &str,
    absolute_path: &str,
    depth: &mut Option<usize>,
) {
    let canonical_path = to_canonical(&realpath(absolute_path));
    if visited.contains_key(&canonical_path) {
        return;
    }
    visited.insert(canonical_path.clone(), true);
    let FileSystemEntries { files, directories } = get_file_system_entries(path);

    for current in &*sort(&files, |a, b| compare_strings_case_sensitive(a, b)) {
        let name = combine_paths(path, &[Some(current)]);
        let absolute_name = combine_paths(absolute_path, &[Some(current)]);
        if matches!(
            extensions,
            Some(extensions) if !file_extension_is_one_of(&name, extensions)
        ) {
            continue;
        }
        if matches!(
            exclude_regex,
            Some(exclude_regex) if exclude_regex.is_match(&absolute_name).unwrap()
        ) {
            continue;
        }
        match include_file_regexes {
            None => {
                results[0].push(name);
            }
            Some(include_file_regexes) => {
                let include_index = find_index(
                    include_file_regexes,
                    |re: &Regex, _| re.is_match(&absolute_name).unwrap(),
                    None,
                );
                if let Some(include_index) = include_index {
                    results[include_index].push(name);
                }
            }
        }
    }

    if depth.is_some() {
        *depth = Some(depth.unwrap() - 1);
        if depth.unwrap() == 0 {
            return;
        }
    }

    for current in &*sort(&directories, |a, b| compare_strings_case_sensitive(a, b)) {
        let name = combine_paths(path, &[Some(current)]);
        let absolute_name = combine_paths(absolute_path, &[Some(current)]);
        if match include_directory_regex {
            None => true,
            Some(include_directory_regex) => {
                include_directory_regex.is_match(&absolute_name).unwrap()
            }
        } && match exclude_regex {
            None => true,
            Some(exclude_regex) => !exclude_regex.is_match(&absolute_name).unwrap(),
        } {
            visit_directory(
                to_canonical,
                realpath,
                visited,
                get_file_system_entries,
                extensions,
                exclude_regex,
                include_file_regexes,
                results,
                include_directory_regex,
                &name,
                &absolute_name,
                depth,
            );
        }
    }
}

fn get_base_paths<TInclude: AsRef<str>>(
    path: &str,
    includes: Option<&[TInclude]>,
    use_case_sensitive_file_names: bool,
) -> Vec<String> {
    let mut base_paths: Vec<String> = vec![path.to_owned()];

    if let Some(includes) = includes {
        let mut include_base_paths: Vec<String> = vec![];
        for include in includes {
            let include = include.as_ref();
            let absolute = if is_rooted_disk_path(include) {
                include.to_owned()
            } else {
                normalize_path(&combine_paths(path, &[Some(include)]))
            };
            include_base_paths.push(get_include_base_path(&absolute));
        }

        let string_comparer = get_string_comparer(Some(!use_case_sensitive_file_names));
        include_base_paths.sort_by(|a, b| comparison_to_ordering(string_comparer(a, b)));

        for include_base_path in include_base_paths {
            if every(&base_paths, |base_path: &String, _| {
                !contains_path(
                    base_path,
                    &include_base_path,
                    Some(path.to_owned()),
                    Some(!use_case_sensitive_file_names),
                )
            }) {
                base_paths.push(include_base_path);
            }
        }
    }

    base_paths
}

fn get_include_base_path(absolute: &str) -> String {
    let wildcard_offset = index_of_any_char_code(
        &absolute.chars().collect::<Vec<_>>(),
        &wildcard_char_codes,
        None,
    );
    if wildcard_offset.is_none() {
        return if !has_extension(absolute) {
            absolute.to_owned()
        } else {
            remove_trailing_directory_separator(&get_directory_path(absolute))
        };
    }
    let wildcard_offset = wildcard_offset.unwrap();
    absolute[0..absolute[0..wildcard_offset]
        .rfind(directory_separator)
        .unwrap_or(0)]
        .to_owned()
}

pub fn ensure_script_kind(file_name: &str, script_kind: Option<ScriptKind>) -> ScriptKind {
    script_kind.unwrap_or_else(|| {
        let script_kind = get_script_kind_from_file_name(file_name);
        if script_kind == ScriptKind::Unknown {
            ScriptKind::TS
        } else {
            script_kind
        }
    })
}

pub fn get_script_kind_from_file_name(file_name: &str) -> ScriptKind {
    let ext = file_name
        .rfind('.')
        .map(|extension_index| file_name[extension_index..].to_owned())
        .and_then(|ext| Extension::maybe_from_str(&ext));
    match ext {
        Some(Extension::Js) | Some(Extension::Cjs) | Some(Extension::Mjs) => ScriptKind::JS,
        Some(Extension::Jsx) => ScriptKind::JSX,
        Some(Extension::Ts) | Some(Extension::Cts) | Some(Extension::Mts) => ScriptKind::TS,
        Some(Extension::Tsx) => ScriptKind::TSX,
        Some(Extension::Json) => ScriptKind::JSON,
        _ => ScriptKind::Unknown,
    }
}

lazy_static! {
    pub static ref supported_ts_extensions: Vec<Vec<Extension>> = vec![
        vec![Extension::Ts, Extension::Tsx, Extension::Dts],
        vec![Extension::Cts, Extension::Dcts],
        vec![Extension::Mts, Extension::Dmts]
    ];
}
lazy_static! {
    pub static ref supported_ts_extensions_flat: Vec<Extension> = flatten(&supported_ts_extensions);
}
lazy_static! {
    pub static ref supported_ts_extensions_with_json: Vec<Vec<Extension>> = {
        let mut ret = supported_ts_extensions.clone();
        ret.append(&mut vec![vec![Extension::Json]]);
        ret
    };
}
lazy_static! {
    pub(super) static ref supported_ts_extensions_for_extract_extension: Vec<Extension> = vec![
        Extension::Dts,
        Extension::Dcts,
        Extension::Dmts,
        Extension::Cts,
        Extension::Mts,
        Extension::Ts,
        Extension::Tsx,
        Extension::Cts,
        Extension::Mts,
    ];
}
lazy_static! {
    pub static ref supported_js_extensions: Vec<Vec<Extension>> = vec![
        vec![Extension::Js, Extension::Jsx],
        vec![Extension::Mjs],
        vec![Extension::Cjs]
    ];
}
lazy_static! {
    pub static ref supported_js_extensions_flat: Vec<Extension> = flatten(&supported_js_extensions);
}
lazy_static! {
    pub static ref all_supported_extensions: Vec<Vec<Extension>> = vec![
        vec![
            Extension::Ts,
            Extension::Tsx,
            Extension::Dts,
            Extension::Js,
            Extension::Jsx
        ],
        vec![Extension::Cts, Extension::Dcts, Extension::Cjs],
        vec![Extension::Mts, Extension::Dmts, Extension::Mjs]
    ];
}
lazy_static! {
    pub static ref all_supported_extensions_with_json: Vec<Vec<Extension>> = vec![
        vec![
            Extension::Ts,
            Extension::Tsx,
            Extension::Dts,
            Extension::Js,
            Extension::Jsx
        ],
        vec![Extension::Cts, Extension::Dcts, Extension::Cjs],
        vec![Extension::Mts, Extension::Dmts, Extension::Mjs],
        vec![Extension::Json],
    ];
}

pub fn get_supported_extensions(
    options: Option<&CompilerOptions>,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
) -> Vec<Vec<Extension>> {
    let need_js_extensions = matches!(
        options,
        Some(options) if get_allow_js_compiler_option(options)
    );

    if match extra_file_extensions {
        None => true,
        Some(extra_file_extensions) => extra_file_extensions.is_empty(),
    } {
        return if need_js_extensions {
            all_supported_extensions.clone()
        } else {
            supported_ts_extensions.clone()
        };
    }

    let builtins = if need_js_extensions {
        all_supported_extensions.clone()
    } else {
        supported_ts_extensions.clone()
    };
    let flat_builtins = flatten(&builtins);
    let mut extensions = builtins;
    extensions.append(&mut map_defined(
        extra_file_extensions,
        |x: &FileExtensionInfo, _| {
            if x.script_kind == Some(ScriptKind::Deferred)
                || need_js_extensions
                    && is_js_like(x.script_kind)
                    && flat_builtins
                        .iter()
                        .position(|flat_builtin| flat_builtin.to_str() == &x.extension)
                        .is_none()
            {
                Some(vec![Extension::maybe_from_str(&x.extension).unwrap()])
            } else {
                None
            }
        },
    ));

    extensions
}

pub fn get_supported_extensions_with_json_if_resolve_json_module(
    options: Option<&CompilerOptions>,
    supported_extensions: &[Vec<Extension>],
) -> Vec<Vec<Extension>> {
    if match options {
        None => true,
        Some(options) => options.resolve_json_module != Some(true),
    } {
        return supported_extensions.to_owned();
    }
    if supported_extensions == &**all_supported_extensions {
        return all_supported_extensions_with_json.clone();
    }
    if supported_extensions == &**supported_ts_extensions {
        return supported_ts_extensions_with_json.clone();
    }
    let mut ret = supported_extensions.to_owned();
    ret.push(vec![Extension::Json]);
    ret
}

pub fn is_js_like(script_kind: Option<ScriptKind>) -> bool {
    matches!(script_kind, Some(ScriptKind::JS) | Some(ScriptKind::JSX))
}

pub fn has_js_file_extension(file_name: &str) -> bool {
    some(
        Some(&**supported_js_extensions_flat),
        Some(|extension: &Extension| file_extension_is(file_name, extension.to_str())),
    )
}

pub fn has_ts_file_extension(file_name: &str) -> bool {
    some(
        Some(&**supported_ts_extensions_flat),
        Some(|extension: &Extension| file_extension_is(file_name, extension.to_str())),
    )
}

fn number_of_directory_separators(str_: &str) -> usize {
    lazy_static! {
        static ref directory_separator_regex: Regex = Regex::new(r"/").unwrap();
    }
    directory_separator_regex.find_iter(str_).count()
}

pub fn compare_number_of_directory_separators(path1: &str, path2: &str) -> Comparison {
    compare_values(
        Some(number_of_directory_separators(path1)),
        Some(number_of_directory_separators(path2)),
    )
}

lazy_static! {
    static ref extensions_to_remove: Vec<Extension> = vec![
        Extension::Dts,
        Extension::Dmts,
        Extension::Dcts,
        Extension::Mjs,
        Extension::Mts,
        Extension::Cjs,
        Extension::Cts,
        Extension::Ts,
        Extension::Js,
        Extension::Tsx,
        Extension::Jsx,
        Extension::Json,
    ];
}
pub fn remove_file_extension(path: &str) -> &str {
    for ext in &*extensions_to_remove {
        let extensionless = try_remove_extension(path, ext.to_str());
        if let Some(extensionless) = extensionless {
            return extensionless;
        }
    }
    path
}

pub fn try_remove_extension<'path>(path: &'path str, extension: &str) -> Option<&'path str> {
    if file_extension_is(path, extension) {
        Some(remove_extension(path, extension))
    } else {
        None
    }
}

pub fn remove_extension<'path>(path: &'path str, extension: &str) -> &'path str {
    &path[0..path.len() - extension.len()]
}

pub fn change_extension(path: &str, new_extension: &str) -> String {
    change_any_extension(
        path,
        new_extension,
        Some(&extensions_to_remove),
        Some(false),
    )
}

#[derive(Clone, Debug)]
pub enum StringOrPattern {
    String(String),
    Pattern(Rc<Pattern>),
}

pub fn try_parse_pattern(pattern: &str) -> Option<StringOrPattern> {
    let index_of_star = pattern.find("*");
    if index_of_star.is_none() {
        return Some(StringOrPattern::String(pattern.to_owned()));
    }
    let index_of_star = index_of_star.unwrap();
    if pattern[index_of_star + 1..].find("*").is_some() {
        None
    } else {
        Some(StringOrPattern::Pattern(Rc::new(Pattern {
            prefix: pattern[0..index_of_star].to_owned(),
            suffix: pattern[index_of_star + 1..].to_owned(),
        })))
    }
}

pub fn try_parse_patterns(paths: &MapLike<Vec<String>>) -> Vec<StringOrPattern> {
    map_defined(Some(paths.keys()), |path: &String, _| {
        try_parse_pattern(path)
    })
}

pub fn position_is_synthesized(pos: isize) -> bool {
    !(pos >= 0)
}

pub fn extension_is_ts(ext: Extension) -> bool {
    matches!(
        ext,
        Extension::Ts
            | Extension::Tsx
            | Extension::Dts
            | Extension::Cts
            | Extension::Mts
            | Extension::Dmts
            | Extension::Dcts
    )
}

pub fn resolution_extension_is_ts_or_json(ext: Extension) -> bool {
    extension_is_ts(ext) || ext == Extension::Json
}

pub fn extension_from_path(path: &str) -> Extension {
    let ext = try_get_extension_from_path(path);
    ext.unwrap_or_else(|| Debug_.fail(Some(&format!("File {path} has unknown extension."))))
}

pub fn try_get_extension_from_path(path: &str) -> Option<Extension> {
    find(&*extensions_to_remove, |e: &Extension, _| {
        file_extension_is(path, e.to_str())
    })
    .copied()
}

pub fn is_check_js_enabled_for_file(
    source_file: &Node, /*SourceFile*/
    compiler_options: &CompilerOptions,
) -> bool {
    source_file
        .as_source_file()
        .maybe_check_js_directive()
        .as_ref()
        .map_or_else(
            || compiler_options.check_js == Some(true),
            |source_file_check_js_directive| source_file_check_js_directive.enabled,
        )
}

lazy_static! {
    pub static ref empty_file_system_entries: FileSystemEntries = FileSystemEntries {
        files: vec![],
        directories: vec![],
    };
}

pub fn match_pattern_or_exact(
    pattern_or_strings: &[StringOrPattern],
    candidate: &str,
) -> Option<StringOrPattern> {
    let mut patterns: Vec<Rc<Pattern>> = vec![];
    for pattern_or_string in pattern_or_strings {
        if matches!(
            pattern_or_string,
            StringOrPattern::String(pattern_or_string) if *pattern_or_string == candidate
        ) {
            return Some(StringOrPattern::String(candidate.to_owned()));
        }

        if let StringOrPattern::Pattern(pattern_or_string) = pattern_or_string {
            patterns.push(pattern_or_string.clone());
        }
    }

    find_best_pattern_match(&patterns, |pattern: &Rc<Pattern>| &**pattern, candidate)
        .map(|value| StringOrPattern::Pattern(value.clone()))
}

pub fn slice_after<'a, TItem>(
    arr: &'a [TItem],
    value: &TItem,
    comparer: impl FnMut(&TItem, &TItem) -> bool,
) -> &'a [TItem] {
    let index = index_of(arr, value, comparer);
    Debug_.assert(index != -1, None);
    let index: usize = index.try_into().unwrap();
    &arr[index..]
}

pub fn slice_after_eq<'a, TItem: PartialEq>(
    arr: &'a [TItem],
    value: &TItem,
) -> &'a [TItem] {
    slice_after(arr, value, |a, b| a == b)
}

pub fn add_related_info(
    diagnostic: &Diagnostic,
    related_information: Vec<Id<DiagnosticRelatedInformation>>,
) {
    if related_information.is_empty() {
        return /*diagnostic*/;
    }
    let mut diagnostic_related_information = diagnostic.maybe_related_information_mut();
    if diagnostic_related_information.is_none() {
        *diagnostic_related_information = Some(vec![]);
    }
    // Debug.assert(diagnostic.relatedInformation !== emptyArray, "Diagnostic had empty array singleton for related info, but is still being constructed!");
    diagnostic_related_information
        .as_mut()
        .unwrap()
        .extend(related_information);
    // return diagnostic;
}

pub fn add_related_info_rc(
    diagnostic: Id<Diagnostic>,
    related_information: Vec<Id<DiagnosticRelatedInformation>>,
    arena: &impl HasArena,
) -> Id<Diagnostic> {
    add_related_info(&diagnostic.ref_(arena), related_information);
    diagnostic
}

pub fn min_and_max<TItem, TGetValue: FnMut(&TItem) -> usize>(
    arr: &[TItem],
    mut get_value: TGetValue,
) -> MinAndMax {
    Debug_.assert(!arr.is_empty(), None);
    let mut min = get_value(&arr[0]);
    let mut max = min;
    for i in 1..arr.len() {
        let value = get_value(&arr[i]);
        if value < min {
            min = value;
        } else if value > max {
            max = value;
        }
    }
    MinAndMax { min, max }
}

pub struct MinAndMax {
    pub min: usize,
    pub max: usize,
}

pub fn range_of_node(node: Id<Node>, arena: &impl HasArena) -> BaseTextRange {
    BaseTextRange::new(
        get_token_pos_of_node(node, Option::<Id<Node>>::None, None, arena),
        node.ref_(arena).end(),
    )
}

pub fn range_of_type_parameters(
    source_file: &Node,       /*SourceFile*/
    type_parameters: &NodeArray, /*<TypeParameterDeclaration>*/
) -> BaseTextRange {
    let pos = type_parameters.pos() - 1;
    let end = skip_trivia(
        &source_file.as_source_file().text_as_chars(),
        type_parameters.end(),
        None,
        None,
        None,
    ) + 1;
    BaseTextRange::new(pos, end)
}
