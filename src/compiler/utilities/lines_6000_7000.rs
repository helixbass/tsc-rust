#![allow(non_upper_case_globals)]

use std::borrow::Cow;
use std::cmp;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    compare_strings_case_sensitive, compare_strings_case_sensitive_maybe, compare_values, flatten,
    for_each, format_string_from_args, get_locale_specific_message, index_of, CommandLineOption,
    CommandLineOptionInterface, CommandLineOptionMapTypeValue, CommandLineOptionType, Comparison,
    CompilerOptions, CompilerOptionsValue, Debug_, Diagnostic, DiagnosticInterface,
    DiagnosticMessage, DiagnosticMessageChain, DiagnosticMessageText, DiagnosticRelatedInformation,
    DiagnosticRelatedInformationInterface, Extension, JsxEmit, LanguageVariant, MapLike,
    ModuleKind, Pattern, PluginImport, ScriptKind, ScriptTarget,
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

pub(crate) fn set_compiler_option_value(
    options: &mut CompilerOptions,
    option: &CommandLineOption,
    value: Option<serde_json::Value>,
) {
    match option.name() {
        "all" => {
            options.all = json_value_to_bool(value);
        }
        "allowJs" => {
            options.allow_js = json_value_to_bool(value);
        }
        "allowNonTsExtensions" => {
            options.allow_non_ts_extensions = json_value_to_bool(value);
        }
        "allowSyntheticDefaultImports" => {
            options.allow_synthetic_default_imports = json_value_to_bool(value);
        }
        "allowUmdGlobalAccess" => {
            options.allow_umd_global_access = json_value_to_bool(value);
        }
        "allowUnreachableCode" => {
            options.allow_unreachable_code = json_value_to_bool(value);
        }
        "allowUnusedLabels" => {
            options.allow_unused_labels = json_value_to_bool(value);
        }
        "alwaysStrict" => {
            options.always_strict = json_value_to_bool(value);
        }
        "baseUrl" => {
            options.base_url = json_value_to_string(value);
        }
        "build" => {
            options.build = json_value_to_bool(value);
        }
        "charset" => {
            options.charset = json_value_to_string(value);
        }
        "checkJs" => {
            options.check_js = json_value_to_bool(value);
        }
        "configFilePath" => {
            options.config_file_path = json_value_to_string(value);
        }
        "configFile" => {
            panic!("Can't set configFile from JSON");
        }
        "declaration" => {
            options.declaration = json_value_to_bool(value);
        }
        "declarationMap" => {
            options.declaration_map = json_value_to_bool(value);
        }
        "emitDeclarationOnly" => {
            options.emit_declaration_only = json_value_to_bool(value);
        }
        "declarationDir" => {
            options.declaration_dir = json_value_to_string(value);
        }
        "diagnostics" => {
            options.diagnostics = json_value_to_bool(value);
        }
        "extendedDiagnostics" => {
            options.extended_diagnostics = json_value_to_bool(value);
        }
        "disableSizeLimit" => {
            options.disable_size_limit = json_value_to_bool(value);
        }
        "disableSourceOfProjectReferenceRedirect" => {
            options.disable_source_of_project_reference_redirect = json_value_to_bool(value);
        }
        "disableSolutionSearching" => {
            options.disable_solution_searching = json_value_to_bool(value);
        }
        "disableReferencedProjectLoad" => {
            options.disable_referenced_project_load = json_value_to_bool(value);
        }
        "downlevelIteration" => {
            options.downlevel_iteration = json_value_to_bool(value);
        }
        "emitBom" => {
            options.emit_bom = json_value_to_bool(value);
        }
        "emitDecoratorMetadata" => {
            options.emit_decorator_metadata = json_value_to_bool(value);
        }
        "exactOptionalPropertyTypes" => {
            options.exact_optional_property_types = json_value_to_bool(value);
        }
        "experimentalDecorators" => {
            options.experimental_decorators = json_value_to_bool(value);
        }
        "forceConsistentCasingInFileNames" => {
            options.force_consistent_casing_in_file_names = json_value_to_bool(value);
        }
        "generateCpuProfile" => {
            options.generate_cpu_profile = json_value_to_string(value);
        }
        "generateTrace" => {
            options.generate_trace = json_value_to_string(value);
        }
        "help" => {
            options.help = json_value_to_bool(value);
        }
        "importHelpers" => {
            options.import_helpers = json_value_to_bool(value);
        }
        "importsNotUsedAsValues" => {
            options.imports_not_used_as_values =
                json_value_to_map_value(value, option, |map_value| match map_value {
                    CommandLineOptionMapTypeValue::ImportsNotUsedAsValues(map_value) => *map_value,
                    _ => panic!("Expected ImportsNotUsedAsValues"),
                });
        }
        "init" => {
            options.init = json_value_to_bool(value);
        }
        "inlineSourceMap" => {
            options.inline_source_map = json_value_to_bool(value);
        }
        "inlineSources" => {
            options.inline_sources = json_value_to_bool(value);
        }
        "isolatedModules" => {
            options.isolated_modules = json_value_to_bool(value);
        }
        "jsx" => {
            options.jsx = json_value_to_map_value(value, option, |map_value| match map_value {
                CommandLineOptionMapTypeValue::JsxEmit(map_value) => *map_value,
                _ => panic!("Expected JsxEmit"),
            });
        }
        "keyofStringsOnly" => {
            options.keyof_strings_only = json_value_to_bool(value);
        }
        "lib" => {
            options.lib = json_value_to_vec_string(value);
        }
        "listEmittedFiles" => {
            options.list_emitted_files = json_value_to_bool(value);
        }
        "listFiles" => {
            options.list_files = json_value_to_bool(value);
        }
        "explainFiles" => {
            options.explain_files = json_value_to_bool(value);
        }
        "listFilesOnly" => {
            options.list_files_only = json_value_to_bool(value);
        }
        "locale" => {
            options.locale = json_value_to_string(value);
        }
        "mapRoot" => {
            options.map_root = json_value_to_string(value);
        }
        "maxNodeModuleJsDepth" => {
            options.max_node_module_js_depth = json_value_to_usize(value);
        }
        "module" => {
            options.module = json_value_to_map_value(value, option, |map_value| match map_value {
                CommandLineOptionMapTypeValue::ModuleKind(map_value) => *map_value,
                _ => panic!("Expected ModuleKind"),
            });
        }
        "moduleResolution" => {
            options.module_resolution =
                json_value_to_map_value(value, option, |map_value| match map_value {
                    CommandLineOptionMapTypeValue::ModuleResolutionKind(map_value) => *map_value,
                    _ => panic!("Expected ModuleResolutionKind"),
                });
        }
        "newLine" => {
            options.new_line =
                json_value_to_map_value(value, option, |map_value| match map_value {
                    CommandLineOptionMapTypeValue::NewLineKind(map_value) => *map_value,
                    _ => panic!("Expected NewLineKind"),
                });
        }
        "noEmit" => {
            options.no_emit = json_value_to_bool(value);
        }
        "noEmitForJsFiles" => {
            options.no_emit_for_js_files = json_value_to_bool(value);
        }
        "noEmitHelpers" => {
            options.no_emit_helpers = json_value_to_bool(value);
        }
        "noEmitOnError" => {
            options.no_emit_on_error = json_value_to_bool(value);
        }
        "noErrorTruncation" => {
            options.no_error_truncation = json_value_to_bool(value);
        }
        "noFallthroughCasesInSwitch" => {
            options.no_fallthrough_cases_in_switch = json_value_to_bool(value);
        }
        "noImplicitAny" => {
            options.no_implicit_any = json_value_to_bool(value);
        }
        "noImplicitReturns" => {
            options.no_implicit_returns = json_value_to_bool(value);
        }
        "noImplicitThis" => {
            options.no_implicit_this = json_value_to_bool(value);
        }
        "noStrictGenericChecks" => {
            options.no_strict_generic_checks = json_value_to_bool(value);
        }
        "noUnusedLocals" => {
            options.no_unused_locals = json_value_to_bool(value);
        }
        "noUnusedParameters" => {
            options.no_unused_parameters = json_value_to_bool(value);
        }
        "noImplicitUseStrict" => {
            options.no_implicit_use_strict = json_value_to_bool(value);
        }
        "noPropertyAccessFromIndexSignature" => {
            options.no_property_access_from_index_signature = json_value_to_bool(value);
        }
        "assumeChangesOnlyAffectDirectDependencies" => {
            options.assume_changes_only_affect_direct_dependencies = json_value_to_bool(value);
        }
        "noLib" => {
            options.no_lib = json_value_to_bool(value);
        }
        "noResolve" => {
            options.no_resolve = json_value_to_bool(value);
        }
        "noUncheckedIndexedAccess" => {
            options.no_unchecked_indexed_access = json_value_to_bool(value);
        }
        "out" => {
            options.out = json_value_to_string(value);
        }
        "outDir" => {
            options.out_dir = json_value_to_string(value);
        }
        "outFile" => {
            options.out_file = json_value_to_string(value);
        }
        "paths" => {
            options.paths = json_value_to_map_like_vec_string(value);
        }
        "pathsBasePath" => {
            options.paths_base_path = json_value_to_string(value);
        }
        "plugins" => {
            options.plugins = json_value_to_vec_plugin_import(value);
        }
        "preserveConstEnums" => {
            options.preserve_const_enums = json_value_to_bool(value);
        }
        "noImplicitOverride" => {
            options.no_implicit_override = json_value_to_bool(value);
        }
        "preserveSymlinks" => {
            options.preserve_symlinks = json_value_to_bool(value);
        }
        "preserveValueImports" => {
            options.preserve_value_imports = json_value_to_bool(value);
        }
        "preserveWatchOutput" => {
            options.preserve_watch_output = json_value_to_bool(value);
        }
        "project" => {
            options.project = json_value_to_string(value);
        }
        "pretty" => {
            options.pretty = json_value_to_bool(value);
        }
        "reactNamespace" => {
            options.react_namespace = json_value_to_string(value);
        }
        "jsxFactory" => {
            options.jsx_factory = json_value_to_string(value);
        }
        "jsxFragmentFactory" => {
            options.jsx_fragment_factory = json_value_to_string(value);
        }
        "jsxImportSource" => {
            options.jsx_import_source = json_value_to_string(value);
        }
        "composite" => {
            options.composite = json_value_to_bool(value);
        }
        "incremental" => {
            options.incremental = json_value_to_bool(value);
        }
        "tsBuildInfoFile" => {
            options.ts_build_info_file = json_value_to_string(value);
        }
        "removeComments" => {
            options.remove_comments = json_value_to_bool(value);
        }
        "rootDir" => {
            options.root_dir = json_value_to_string(value);
        }
        "rootDirs" => {
            options.root_dirs = json_value_to_vec_string(value);
        }
        "skipLibCheck" => {
            options.skip_lib_check = json_value_to_bool(value);
        }
        "skipDefaultLibCheck" => {
            options.skip_default_lib_check = json_value_to_bool(value);
        }
        "sourceMap" => {
            options.source_map = json_value_to_bool(value);
        }
        "sourceRoot" => {
            options.source_root = json_value_to_string(value);
        }
        "strict" => {
            options.strict = json_value_to_bool(value);
        }
        "strictFunctionTypes" => {
            options.strict_function_types = json_value_to_bool(value);
        }
        "strictBindCallApply" => {
            options.strict_bind_call_apply = json_value_to_bool(value);
        }
        "strictNullChecks" => {
            options.strict_null_checks = json_value_to_bool(value);
        }
        "strictPropertyInitialization" => {
            options.strict_property_initialization = json_value_to_bool(value);
        }
        "stripInternal" => {
            options.strip_internal = json_value_to_bool(value);
        }
        "suppressExcessPropertyErrors" => {
            options.suppress_excess_property_errors = json_value_to_bool(value);
        }
        "suppressImplicitAnyIndexErrors" => {
            options.suppress_implicit_any_index_errors = json_value_to_bool(value);
        }
        "suppressOutputPathCheck" => {
            options.suppress_output_path_check = json_value_to_bool(value);
        }
        "target" => {
            options.target = json_value_to_map_value(value, option, |map_value| match map_value {
                CommandLineOptionMapTypeValue::ScriptTarget(map_value) => *map_value,
                _ => panic!("Expected ScriptTarget"),
            });
        }
        "traceResolution" => {
            options.trace_resolution = json_value_to_bool(value);
        }
        "useUnknownInCatchVariables" => {
            options.use_unknown_in_catch_variables = json_value_to_bool(value);
        }
        "resolveJsonModule" => {
            options.resolve_json_module = json_value_to_bool(value);
        }
        "types" => {
            options.types = json_value_to_vec_string(value);
        }
        "typeRoots" => {
            options.type_roots = json_value_to_vec_string(value);
        }
        "version" => {
            options.version = json_value_to_bool(value);
        }
        "watch" => {
            options.watch = json_value_to_bool(value);
        }
        "esModuleInterop" => {
            options.es_module_interop = json_value_to_bool(value);
        }
        "showConfig" => {
            options.show_config = json_value_to_bool(value);
        }
        "useDefineForClassFields" => {
            options.use_define_for_class_fields = json_value_to_bool(value);
        }
        _ => panic!("Unknown compiler option: {:?}", option.name()),
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

pub fn get_jsx_transform_enabled(options: &CompilerOptions) -> bool {
    let jsx = options.jsx;
    matches!(
        jsx,
        Some(JsxEmit::React | JsxEmit::ReactJSX | JsxEmit::ReactJSXDev)
    )
}

pub struct SymlinkCache {}

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
    pub static ref supported_js_extensions: Vec<Vec<Extension>> = vec![
        vec![Extension::Js, Extension::Jsx],
        vec![Extension::Mjs],
        vec![Extension::Cjs]
    ];
}
lazy_static! {
    pub static ref supported_js_extensions_flat: Vec<Extension> = flatten(&supported_js_extensions);
}

pub fn remove_file_extension<'path>(path: &'path str) -> Cow<'path, str> {
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
