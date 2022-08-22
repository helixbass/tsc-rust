#![allow(non_upper_case_globals)]

use regex::Regex;
use std::borrow::{Borrow, Cow};
use std::cmp;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    combine_paths, compare_strings_case_sensitive, compare_strings_case_sensitive_maybe,
    compare_values, flatten, for_each, format_string_from_args, get_locale_specific_message,
    index_of, normalize_path, CommandLineOption, CommandLineOptionInterface,
    CommandLineOptionMapTypeValue, CommandLineOptionType, Comparison, CompilerOptions,
    CompilerOptionsValue, Debug_, Diagnostic, DiagnosticInterface, DiagnosticMessage,
    DiagnosticMessageChain, DiagnosticMessageText, DiagnosticRelatedInformation,
    DiagnosticRelatedInformationInterface, Extension, FileExtensionInfo, JsxEmit, LanguageVariant,
    MapLike, ModuleKind, ModuleResolutionKind, Node, Pattern, PluginImport, ScriptKind,
    ScriptTarget, TypeAcquisition, WatchOptions,
};
use local_macros::enum_unwrapped;

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
    unimplemented!()
}

fn get_diagnostic_file_path<
    TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface,
>(
    diagnostic: &TDiagnosticRelatedInformation,
) -> Option<String> {
    diagnostic.maybe_file().and_then(|file| {
        file.as_source_file()
            .maybe_path()
            .as_ref()
            .map(|path| path.to_string())
    })
}

pub fn compare_diagnostics<TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface>(
    d1: &TDiagnosticRelatedInformation,
    d2: &TDiagnosticRelatedInformation,
) -> Comparison {
    let mut compared = compare_diagnostics_skip_related_information(d1, d2);
    if compared != Comparison::EqualTo {
        return compared;
    }
    if let Some(d1) = d1.maybe_as_diagnostic() {
        if let Some(d2) = d2.maybe_as_diagnostic() {
            compared = compare_related_information(d1, d2);
            if compared != Comparison::EqualTo {
                return compared;
            }
        }
    }
    Comparison::EqualTo
}

pub fn compare_diagnostics_skip_related_information<
    TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface,
>(
    d1: &TDiagnosticRelatedInformation,
    d2: &TDiagnosticRelatedInformation,
) -> Comparison {
    let mut compared = compare_strings_case_sensitive_maybe(
        get_diagnostic_file_path(d1).as_deref(),
        get_diagnostic_file_path(d2).as_deref(),
    );
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.start()), Some(d2.start()));
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.length()), Some(d2.length()));
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

fn compare_related_information(d1: &Diagnostic, d2: &Diagnostic) -> Comparison {
    if d1.related_information().is_none() && d2.related_information().is_none() {
        return Comparison::EqualTo;
    }
    if let Some(d1_related_information) = d1.related_information().as_ref() {
        if let Some(d2_related_information) = d2.related_information().as_ref() {
            let compared = compare_values(
                Some(d1_related_information.len()),
                Some(d2_related_information.len()),
            );
            if compared != Comparison::EqualTo {
                return compared;
            }
            let compared_maybe = for_each(d1_related_information, |d1i, index| {
                let d2i = &d2_related_information[index];
                let compared = compare_diagnostics(&**d1i, &**d2i);
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
    if d1.related_information().is_some() {
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
    compiler_options.target.unwrap_or_else(|| {
        if matches!(compiler_options.module, Some(ModuleKind::Node12)) {
            ScriptTarget::ES2020
        } else if matches!(compiler_options.module, Some(ModuleKind::NodeNext)) {
            ScriptTarget::ESNext
        } else {
            ScriptTarget::ES3
        }
    })
}

pub fn get_emit_module_kind(compiler_options: &CompilerOptions) -> ModuleKind {
    compiler_options.module.unwrap_or_else(|| {
        if get_emit_script_target(compiler_options) >= ScriptTarget::ES2015 {
            ModuleKind::ES2015
        } else {
            ModuleKind::CommonJS
        }
    })
}

pub fn get_emit_module_resolution_kind(compiler_options: &CompilerOptions) -> ModuleResolutionKind {
    unimplemented!()
}

pub fn has_json_module_emit_enabled(compiler_options: &CompilerOptions) -> bool {
    unimplemented!()
}

pub fn unreachable_code_is_error(options: &CompilerOptions) -> bool {
    matches!(options.allow_unreachable_code, Some(false))
}

pub fn unused_label_is_error(options: &CompilerOptions) -> bool {
    matches!(options.allow_unused_labels, Some(false))
}

fn json_value_to_bool(value: Option<serde_json::Value>) -> Option<bool> {
    value.and_then(|value| match value {
        serde_json::Value::Null => None,
        serde_json::Value::Bool(value) => Some(value),
        _ => panic!("Expected bool"),
    })
}

fn json_value_to_string(value: Option<serde_json::Value>) -> Option<String> {
    value.and_then(|value| match value {
        serde_json::Value::Null => None,
        serde_json::Value::String(value) => Some(value),
        _ => panic!("Expected string"),
    })
}

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

pub fn get_jsx_implicit_import_base<TFile: Borrow<Node>>(
    compiler_options: &CompilerOptions,
    file: Option<TFile /*SourceFile*/>,
) -> Option<String> {
    unimplemented!()
}

pub fn get_jsx_runtime_import(base: Option<&str>, options: &CompilerOptions) -> Option<String> {
    unimplemented!()
}

pub fn get_jsx_transform_enabled(options: &CompilerOptions) -> bool {
    let jsx = options.jsx;
    matches!(
        jsx,
        Some(JsxEmit::React | JsxEmit::ReactJSX | JsxEmit::ReactJSXDev)
    )
}

pub struct SymlinkCache {}

pub fn get_regular_expression_for_wildcard(
    specs: Option<&[String]>,
    base_path: &str,
    usage: &str, /*"files" | "directories" | "exclude"*/
) -> Option<String> {
    unimplemented!()
}

pub fn get_regular_expressions_for_wildcards<TSpec: AsRef<str>>(
    specs: Option<&[TSpec]>,
    base_path: &str,
    usage: &str, /*"files" | "directories" | "exclude"*/
) -> Option<Vec<String>> {
    unimplemented!()
}

pub fn is_implicit_glob(last_path_component: &str) -> bool {
    unimplemented!()
}

pub fn get_pattern_from_spec(
    spec: &str,
    base_path: &str,
    usage: &str, /*"files" | "directories" | "exclude"*/
) -> Option<String> {
    unimplemented!()
}

pub struct FileMatcherPatterns {
    pub include_file_patterns: Option<Vec<String>>,
    pub include_file_pattern: Option<String>,
    pub include_directory_pattern: Option<String>,
    pub exclude_pattern: Option<String>,
    pub base_paths: Vec<String>,
}

pub fn get_file_matcher_patterns(
    path: &str,
    excludes: Option<&[String]>,
    includes: Option<&[String]>,
    use_case_sensitive_file_names: bool,
    current_directory: &str,
) -> FileMatcherPatterns {
    let path = normalize_path(path);
    let current_directory = normalize_path(current_directory);
    let absolute_path = combine_paths(&current_directory, &*vec![Some(&*path)]);

    unimplemented!()
    // FileMatcherPatterns {

    // }
}

pub fn get_regex_from_pattern(pattern: &str, use_case_sensitive_file_names: bool) -> Regex {
    Regex::new(&if use_case_sensitive_file_names {
        pattern.to_owned()
    } else {
        format!("(?i){}", pattern)
    })
    .unwrap()
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

pub fn get_supported_extensions(
    options: Option<&CompilerOptions>,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
) -> Vec<Vec<String>> {
    unimplemented!()
}

pub fn get_supported_extensions_with_json_if_resolve_json_module(
    options: Option<&CompilerOptions>,
    supported_extensions: &[Vec<String>],
) -> Vec<Vec<String>> {
    unimplemented!()
}

pub fn remove_file_extension<'path>(path: &'path str) -> Cow<'path, str> {
    unimplemented!()
}

pub fn remove_extension<'path>(path: &'path str, extension: &str) -> &'path str {
    &path[0..path.len() - extension.len()]
}

pub fn change_extension(path: &str, new_extension: &str) -> String {
    unimplemented!()
}

#[derive(Debug)]
pub enum StringOrPattern {
    String(String),
    Pattern(Pattern),
}

pub fn try_parse_pattern(pattern: &str) -> Option<StringOrPattern> {
    unimplemented!()
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

pub fn is_check_js_enabled_for_file(
    source_file: &Node, /*SourceFile*/
    compiler_options: &CompilerOptions,
) -> bool {
    unimplemented!()
}

pub fn slice_after<'arr, TItem, TComparer: FnMut(&TItem, &TItem) -> bool>(
    arr: &'arr [TItem],
    value: &TItem,
    comparer: TComparer,
) -> &'arr [TItem] {
    let index = index_of(arr, value, comparer);
    Debug_.assert(index != -1, None);
    let index: usize = index.try_into().unwrap();
    &arr[index..]
}

pub fn add_related_info(
    diagnostic: &Diagnostic,
    related_information: Vec<Rc<DiagnosticRelatedInformation>>,
) {
    if related_information.is_empty() {
        return /*diagnostic*/;
    }
    let mut diagnostic_related_information = diagnostic.related_information();
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
