use fancy_regex::Regex;
use gc::Gc;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    compile_on_save_command_line_option, compiler_options_did_you_mean_diagnostics,
    convert_enable_auto_discovery_to_enable, create_compiler_diagnostic_for_invalid_custom_type,
    create_unknown_option_error, get_command_line_compiler_options_map,
    get_command_line_watch_options_map, get_compiler_option_value_type_string,
    get_option_from_name, is_compiler_options_value, parse_config, read_json_config_file,
    type_acquisition_did_you_mean_diagnostics, watch_options_did_you_mean_diagnostics,
    ParsedTsconfig,
};
use crate::{
    change_extension, combine_paths, contains_path, create_compiler_diagnostic,
    create_diagnostic_for_node_in_source_file, create_get_canonical_file_name, directory_separator,
    ends_with, ensure_trailing_directory_separator, file_extension_is, file_extension_is_one_of,
    filter, find_index, flatten, for_each, for_each_entry, get_base_file_name, get_directory_path,
    get_normalized_absolute_path, get_regex_from_pattern, get_regular_expression_for_wildcard,
    get_regular_expressions_for_wildcards, get_supported_extensions,
    get_supported_extensions_with_json_if_resolve_json_module,
    get_ts_config_prop_array_element_value, has_extension, is_implicit_glob, length, map,
    maybe_filter, maybe_map, normalize_path, normalize_slashes, set_type_acquisition_value,
    set_watch_option_value, starts_with, to_file_name_lower_case, CommandLineOption,
    CommandLineOptionInterface, CommandLineOptionType, CompilerOptions, CompilerOptionsValue,
    ConfigFileSpecs, Diagnostic, DiagnosticMessage, Diagnostics, DidYouMeanOptionsDiagnostics,
    Extension, FileExtensionInfo, Node, ParseConfigHost, ToHashMapOfCompilerOptionsValues,
    TypeAcquisition, WatchDirectoryFlags, WatchOptions,
};

pub struct ExtendedConfigCacheEntry {
    pub extended_result: Gc<Node /*TsConfigSourceFile*/>,
    pub extended_config: Option<Rc<ParsedTsconfig>>,
}

pub(crate) fn get_extended_config<TSourceFile: Borrow<Node>, THost: ParseConfigHost>(
    source_file: Option<TSourceFile>,
    extended_config_path: &str,
    host: &THost,
    resolution_stack: &[&str],
    errors: &mut Vec<Gc<Diagnostic>>,
    extended_config_cache: &mut Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
) -> Option<Rc<ParsedTsconfig>> {
    let path = if host.use_case_sensitive_file_names() {
        extended_config_path.to_owned()
    } else {
        to_file_name_lower_case(extended_config_path)
    };
    let mut extended_result: Option<Gc<Node /*TsConfigSourceFile*/>> = None;
    let mut extended_config: Option<Rc<ParsedTsconfig>> = None;
    if let Some(extended_config_cache) = extended_config_cache.as_ref() {
        let value = extended_config_cache.get(&path);
        if let Some(value) = value {
            extended_result = Some(value.extended_result.clone());
            extended_config = value.extended_config.clone();
        }
    }
    if extended_result.is_none() {
        extended_result = Some(read_json_config_file(extended_config_path, |path| {
            host.read_file(path)
        }));
        if extended_result
            .as_ref()
            .unwrap()
            .as_source_file()
            .parse_diagnostics()
            .is_empty()
        {
            extended_config = Some(Rc::new(parse_config(
                None,
                extended_result.as_deref(),
                host,
                &get_directory_path(extended_config_path),
                Some(&get_base_file_name(extended_config_path, None, None)),
                resolution_stack,
                errors,
                extended_config_cache,
            )));
        }
        if let Some(extended_config_cache) = extended_config_cache {
            extended_config_cache.insert(
                path,
                ExtendedConfigCacheEntry {
                    extended_result: extended_result.clone().unwrap(),
                    extended_config: extended_config.clone(),
                },
            );
        }
    }
    let extended_result = extended_result.unwrap();
    let extended_result_as_source_file = extended_result.as_source_file();
    if let Some(source_file) = source_file {
        let source_file = source_file.borrow();
        let mut source_file_extended_source_files =
            source_file.as_source_file().maybe_extended_source_files();
        *source_file_extended_source_files =
            Some(vec![extended_result_as_source_file.file_name().clone()]);
        if let Some(extended_result_extended_source_files) =
            &*extended_result_as_source_file.maybe_extended_source_files()
        {
            source_file_extended_source_files
                .as_mut()
                .unwrap()
                .append(&mut extended_result_extended_source_files.clone());
        }
    }
    if !extended_result_as_source_file
        .parse_diagnostics()
        .is_empty()
    {
        errors.append(&mut extended_result_as_source_file.parse_diagnostics().clone());
        return None;
    }
    extended_config
}

pub(super) fn convert_compile_on_save_option_from_json(
    json_option: &serde_json::Map<String, serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Gc<Diagnostic>>,
) -> bool {
    let compile_on_save_command_line_option = compile_on_save_command_line_option();
    if !json_option.contains_key(compile_on_save_command_line_option.name()) {
        return false;
    }
    let result = convert_json_option(
        &compile_on_save_command_line_option,
        json_option.get(compile_on_save_command_line_option.name()),
        base_path,
        errors,
    );
    match result {
        CompilerOptionsValue::Bool(Some(true)) => true,
        _ => false,
    }
}

pub fn convert_compiler_options_from_json(
    json_options: Option<&serde_json::Value>,
    base_path: &str,
    config_file_name: Option<&str>,
) -> CompilerOptionsAndErrors {
    let mut errors: Vec<Gc<Diagnostic>> = vec![];
    let options = convert_compiler_options_from_json_worker(
        json_options,
        base_path,
        &mut errors,
        config_file_name,
    );
    CompilerOptionsAndErrors { options, errors }
}

pub struct CompilerOptionsAndErrors {
    pub options: CompilerOptions,
    pub errors: Vec<Gc<Diagnostic>>,
}

pub fn convert_type_acquisition_from_json(
    json_options: Option<&serde_json::Value>,
    base_path: &str,
    config_file_name: Option<&str>,
) -> TypeAcquisitionAndErrors {
    let mut errors: Vec<Gc<Diagnostic>> = vec![];
    let options = convert_type_acquisition_from_json_worker(
        json_options,
        base_path,
        &mut errors,
        config_file_name,
    );
    TypeAcquisitionAndErrors { options, errors }
}

pub struct TypeAcquisitionAndErrors {
    pub options: TypeAcquisition,
    pub errors: Vec<Gc<Diagnostic>>,
}

pub(super) fn get_default_compiler_options(config_file_name: Option<&str>) -> CompilerOptions {
    let mut options: CompilerOptions = Default::default();
    if matches!(config_file_name, Some(config_file_name) if get_base_file_name(config_file_name, None, None) == "jsconfig.json")
    {
        options.allow_js = Some(true);
        options.max_node_module_js_depth = Some(2);
        options.allow_synthetic_default_imports = Some(true);
        options.skip_lib_check = Some(true);
        options.no_emit = Some(true);
    }
    options
}

pub(crate) fn convert_compiler_options_from_json_worker(
    json_options: Option<&serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Gc<Diagnostic>>,
    config_file_name: Option<&str>,
) -> CompilerOptions {
    let mut options = get_default_compiler_options(config_file_name);
    convert_options_from_json_compiler_options(
        &get_command_line_compiler_options_map(),
        json_options,
        base_path,
        &mut options,
        compiler_options_did_you_mean_diagnostics().as_did_you_mean_options_diagnostics(),
        errors,
    );
    if let Some(config_file_name) = config_file_name {
        options.config_file_path = Some(normalize_slashes(config_file_name));
    }
    options
}

pub(super) fn get_default_type_acquisition(config_file_name: Option<&str>) -> TypeAcquisition {
    TypeAcquisition {
        enable_auto_discovery: None,
        enable: Some(
            matches!(config_file_name, Some(config_file_name) if get_base_file_name(config_file_name, None, None) == "jsconfig.json"),
        ),
        include: Some(vec![]),
        exclude: Some(vec![]),
        disable_filename_based_type_acquisition: None,
    }
}

pub(crate) fn convert_type_acquisition_from_json_worker(
    json_options: Option<&serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Gc<Diagnostic>>,
    config_file_name: Option<&str>,
) -> TypeAcquisition {
    let mut options = get_default_type_acquisition(config_file_name);
    let type_acquisition = convert_enable_auto_discovery_to_enable(json_options);

    convert_options_from_json_type_acquisition(
        &get_command_line_watch_options_map(),
        type_acquisition.as_ref(),
        base_path,
        &mut options,
        &*type_acquisition_did_you_mean_diagnostics(),
        errors,
    );
    options
}

pub(crate) fn convert_watch_options_from_json_worker(
    json_options: Option<&serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Gc<Diagnostic>>,
) -> Option<WatchOptions> {
    convert_options_from_json_watch_options(
        &get_command_line_watch_options_map(),
        json_options,
        base_path,
        watch_options_did_you_mean_diagnostics().as_did_you_mean_options_diagnostics(),
        errors,
    )
}

pub(super) fn convert_options_from_json_compiler_options(
    options_name_map: &HashMap<String, Rc<CommandLineOption>>,
    json_options: Option<&serde_json::Value>,
    base_path: &str,
    default_options: &mut CompilerOptions,
    diagnostics: &dyn DidYouMeanOptionsDiagnostics,
    errors: &mut Vec<Gc<Diagnostic>>,
) {
    if json_options.is_none() {
        return;
    }
    let json_options = json_options.unwrap();

    match json_options {
        serde_json::Value::Object(json_options) => {
            for (id, map_value) in json_options {
                let opt = options_name_map.get(id);
                if let Some(opt) = opt {
                    default_options.set_value_from_command_line_option(
                        opt,
                        convert_json_option(opt, Some(map_value), base_path, errors),
                    );
                } else {
                    errors.push(create_unknown_option_error(
                        id,
                        diagnostics,
                        |message, args| Rc::new(create_compiler_diagnostic(message, args).into()),
                        None,
                    ));
                }
            }
        }
        _ => (),
    }
    // return defaultOptions;
}

pub(super) fn convert_options_from_json_type_acquisition(
    options_name_map: &HashMap<String, Rc<CommandLineOption>>,
    json_options: Option<&serde_json::Value>,
    base_path: &str,
    default_options: &mut TypeAcquisition,
    diagnostics: &dyn DidYouMeanOptionsDiagnostics,
    errors: &mut Vec<Gc<Diagnostic>>,
) {
    if json_options.is_none() {
        return;
    }
    let json_options = json_options.unwrap();

    match json_options {
        serde_json::Value::Object(json_options) => {
            for (id, map_value) in json_options {
                let opt = options_name_map.get(id);
                if let Some(opt) = opt {
                    set_type_acquisition_value(
                        default_options,
                        opt,
                        convert_json_option(opt, Some(map_value), base_path, errors),
                    );
                } else {
                    errors.push(create_unknown_option_error(
                        id,
                        diagnostics,
                        |message, args| Rc::new(create_compiler_diagnostic(message, args).into()),
                        None,
                    ));
                }
            }
        }
        _ => (),
    }
    // return defaultOptions;
}

pub(super) fn convert_options_from_json_watch_options(
    options_name_map: &HashMap<String, Rc<CommandLineOption>>,
    json_options: Option<&serde_json::Value>,
    base_path: &str,
    // default_options: &mut WatchOptions,
    diagnostics: &dyn DidYouMeanOptionsDiagnostics,
    errors: &mut Vec<Gc<Diagnostic>>,
) -> Option<WatchOptions> {
    if json_options.is_none() {
        return None;
    }
    let json_options = json_options.unwrap();

    let mut default_options: WatchOptions = Default::default();
    let mut did_set_any_options = false;
    match json_options {
        serde_json::Value::Object(json_options) => {
            for (id, map_value) in json_options {
                let opt = options_name_map.get(id);
                if let Some(opt) = opt {
                    did_set_any_options = true;
                    set_watch_option_value(
                        &mut default_options,
                        opt,
                        convert_json_option(opt, Some(map_value), base_path, errors),
                    );
                } else {
                    errors.push(create_unknown_option_error(
                        id,
                        diagnostics,
                        |message, args| Rc::new(create_compiler_diagnostic(message, args).into()),
                        None,
                    ));
                }
            }
        }
        _ => (),
    }
    if did_set_any_options {
        Some(default_options)
    } else {
        None
    }
}

pub(crate) fn convert_json_option(
    opt: &CommandLineOption,
    value: Option<&serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Gc<Diagnostic>>,
) -> CompilerOptionsValue {
    if is_compiler_options_value(Some(opt), value) {
        let opt_type = opt.type_();
        if matches!(opt_type, CommandLineOptionType::List)
            && matches!(value, Some(serde_json::Value::Array(_)))
        {
            return convert_json_option_of_list_type(
                opt,
                match value {
                    Some(serde_json::Value::Array(value)) => value,
                    _ => panic!("Expected array"),
                },
                base_path,
                errors,
            );
        } else if matches!(opt_type, CommandLineOptionType::Map(_)) {
            return convert_json_option_of_custom_type(
                opt,
                value.map(|value| match value {
                    serde_json::Value::String(value) => &**value,
                    _ => panic!("Expected string"),
                }),
                errors,
            );
        }
        let validated_value = validate_json_option_value(opt, value, errors);
        return if !validated_value.is_some() {
            validated_value
        } else {
            normalize_non_list_option_value_compiler_options_value(opt, base_path, validated_value)
        };
    } else {
        errors.push(Rc::new(
            create_compiler_diagnostic(
                &Diagnostics::Compiler_option_0_requires_a_value_of_type_1,
                Some(vec![
                    opt.name().to_owned(),
                    get_compiler_option_value_type_string(opt).to_owned(),
                ]),
            )
            .into(),
        ));
    }
    opt.to_compiler_options_value_none()
}

pub(super) fn normalize_option_value(
    option: &CommandLineOption,
    base_path: &str,
    value: Option<&serde_json::Value>,
) -> CompilerOptionsValue {
    if value.is_none() {
        return option.to_compiler_options_value_none();
    }
    let value = value.unwrap();
    if matches!(value, serde_json::Value::Null) {
        return option.to_compiler_options_value_none();
    }
    match option.type_() {
        CommandLineOptionType::List => {
            let list_option = option.as_command_line_option_of_list_type();
            if list_option.element.is_file_path()
                || matches!(list_option.element.type_(), CommandLineOptionType::Map(_))
            {
                return match value {
                    serde_json::Value::Array(value) => CompilerOptionsValue::VecString(Some(
                        filter(
                            &map(value, |v, _| {
                                normalize_option_value(&list_option.element, base_path, Some(v))
                            }),
                            |v| v.is_some(),
                        )
                        .into_iter()
                        .map(|item| match item {
                            CompilerOptionsValue::String(Some(item)) => item,
                            _ => panic!("Expected only vec of strings?"),
                        })
                        .collect::<Vec<String>>(),
                    )),
                    _ => panic!("Expected array"),
                };
            }
            match value {
                serde_json::Value::Array(value) => CompilerOptionsValue::VecString(Some(
                    value
                        .into_iter()
                        .map(|item| match item {
                            serde_json::Value::String(item) => item.clone(),
                            _ => panic!("Expected only vec of strings?"),
                        })
                        .collect::<Vec<String>>(),
                )),
                _ => panic!("Expected array"),
            }
        }
        CommandLineOptionType::Map(map) => match map.get(&&*match value {
            serde_json::Value::String(value) => value.to_lowercase(),
            _ => panic!("Expected string"),
        }) {
            None => option.to_compiler_options_value_none(),
            Some(map_value) => map_value.as_compiler_options_value(),
        },
        _ => normalize_non_list_option_value(option, base_path, value),
    }
}

pub(super) fn normalize_non_list_option_value(
    option: &CommandLineOption,
    base_path: &str,
    value: &serde_json::Value,
) -> CompilerOptionsValue {
    if option.is_file_path() {
        let mut value_string = get_normalized_absolute_path(
            match value {
                serde_json::Value::String(value) => value,
                _ => panic!("Expected string"),
            },
            Some(base_path),
        );
        if value_string == "" {
            value_string = ".".to_owned();
        }
        return option.to_compiler_options_value(&serde_json::Value::String(value_string));
    }
    option.to_compiler_options_value(value)
}

pub(super) fn normalize_non_list_option_value_compiler_options_value(
    option: &CommandLineOption,
    base_path: &str,
    value: CompilerOptionsValue,
) -> CompilerOptionsValue {
    if option.is_file_path() {
        let mut value_string = get_normalized_absolute_path(
            &match value {
                CompilerOptionsValue::String(Some(value)) => value,
                _ => panic!("Expected string"),
            },
            Some(base_path),
        );
        if value_string == "" {
            value_string = ".".to_owned();
        }
        return CompilerOptionsValue::String(Some(value_string));
    }
    value
}

pub(super) fn validate_json_option_value_compiler_options_value(
    opt: &CommandLineOption,
    value: CompilerOptionsValue,
    errors: &mut Vec<Gc<Diagnostic>>,
) -> CompilerOptionsValue {
    if !value.is_some() {
        return opt.to_compiler_options_value_none();
    }
    let d = opt
        .maybe_extra_validation_compiler_options_value()
        .and_then(|extra_validation| extra_validation(&value));
    if d.is_none() {
        return value;
    }
    let d = d.unwrap();
    let (diagnostic_message, args) = d;
    errors.push(Rc::new(
        create_compiler_diagnostic(diagnostic_message, args).into(),
    ));
    opt.to_compiler_options_value_none()
}

pub(super) fn validate_json_option_value(
    opt: &CommandLineOption,
    value: Option<&serde_json::Value>,
    errors: &mut Vec<Gc<Diagnostic>>,
) -> CompilerOptionsValue {
    if value.is_none() {
        return opt.to_compiler_options_value_none();
    }
    let value = value.unwrap();
    let d = opt
        .maybe_extra_validation()
        .and_then(|extra_validation| extra_validation(Some(value)));
    if d.is_none() {
        return opt.to_compiler_options_value(value);
    }
    let d = d.unwrap();
    let (diagnostic_message, args) = d;
    errors.push(Rc::new(
        create_compiler_diagnostic(diagnostic_message, args).into(),
    ));
    opt.to_compiler_options_value_none()
}

pub(super) fn convert_json_option_of_custom_type(
    opt: &CommandLineOption, /*CommandLineOptionOfCustomType*/
    value: Option<&str>,
    errors: &mut Vec<Gc<Diagnostic>>,
) -> CompilerOptionsValue {
    if value.is_none() {
        return opt
            .type_()
            .as_map()
            .values()
            .next()
            .unwrap()
            .as_compiler_options_value()
            .as_none();
    }
    let value = value.unwrap();
    let key = value.to_lowercase();
    let val = opt.type_().as_map().get(&&*key);
    if let Some(val) = val {
        return validate_json_option_value_compiler_options_value(
            opt,
            val.as_compiler_options_value(),
            errors,
        );
    } else {
        errors.push(create_compiler_diagnostic_for_invalid_custom_type(opt));
    }
    opt.type_()
        .as_map()
        .values()
        .next()
        .unwrap()
        .as_compiler_options_value()
        .as_none()
}

pub(super) fn convert_json_option_of_list_type(
    option: &CommandLineOption, /*CommandLineOptionOfListType*/
    values: &[serde_json::Value],
    base_path: &str,
    errors: &mut Vec<Gc<Diagnostic>>,
) -> CompilerOptionsValue {
    let option_as_command_line_option_of_list_type = option.as_command_line_option_of_list_type();
    CompilerOptionsValue::VecString(Some(
        values
            .into_iter()
            .filter_map(|v| {
                match convert_json_option(
                    &option_as_command_line_option_of_list_type.element,
                    Some(v),
                    base_path,
                    errors,
                ) {
                    CompilerOptionsValue::String(v) => v,
                    _ => panic!("Expected only vec of strings?"),
                }
            })
            .collect::<Vec<String>>(),
    ))
}

lazy_static! {
    pub(super) static ref invalid_trailing_recursion_pattern: Regex =
        Regex::new(r#"(^|/)\*\*/?$"#).unwrap();
}

lazy_static! {
    pub(super) static ref wildcard_directory_pattern: Regex =
        Regex::new(r#"^([^*?]*)/[^/]*[*?]"#).unwrap();
}

pub(crate) fn get_file_names_from_config_specs<THost: ParseConfigHost>(
    config_file_specs: &ConfigFileSpecs,
    base_path: &str,
    options: &CompilerOptions,
    host: &THost,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
) -> Vec<String> {
    let base_path = normalize_path(base_path);

    let key_mapper = create_get_canonical_file_name(host.use_case_sensitive_file_names());

    let mut literal_file_map: HashMap<String, String> = HashMap::new();

    let mut wildcard_file_map: HashMap<String, String> = HashMap::new();

    let mut wild_card_json_file_map: HashMap<String, String> = HashMap::new();
    let validated_files_spec = config_file_specs.validated_files_spec.as_ref();
    let validated_include_specs = config_file_specs.validated_include_specs.as_deref();
    let validated_exclude_specs = config_file_specs.validated_exclude_specs.as_deref();

    let supported_extensions = get_supported_extensions(Some(options), extra_file_extensions);
    let supported_extensions_with_json_if_resolve_json_module =
        get_supported_extensions_with_json_if_resolve_json_module(
            Some(options),
            &supported_extensions,
        );

    if let Some(validated_files_spec) = validated_files_spec {
        for file_name in validated_files_spec {
            let file = get_normalized_absolute_path(file_name, Some(&base_path));
            literal_file_map.insert(key_mapper(&file), file);
        }
    }

    let mut json_only_include_regexes: Option<Vec<Regex>> = None;
    if let Some(validated_include_specs) = validated_include_specs {
        if !validated_include_specs.is_empty() {
            for file in host.read_directory(
                &base_path,
                &flatten(&supported_extensions_with_json_if_resolve_json_module)
                    .iter()
                    .map(|string| string.to_str())
                    .collect::<Vec<_>>(),
                validated_exclude_specs,
                validated_include_specs,
                None,
            ) {
                if file_extension_is(&file, Extension::Json.to_str()) {
                    if json_only_include_regexes.is_none() {
                        let includes = validated_include_specs
                            .iter()
                            .filter(|s| ends_with(s, Extension::Json.to_str()))
                            .collect::<Vec<_>>();
                        let include_file_patterns = maybe_map(
                            get_regular_expressions_for_wildcards(
                                Some(&includes),
                                &base_path,
                                "files",
                            ),
                            |pattern, _| format!("^{}$", pattern),
                        );
                        json_only_include_regexes =
                            Some(if let Some(include_file_patterns) = include_file_patterns {
                                include_file_patterns
                                    .iter()
                                    .map(|pattern| {
                                        get_regex_from_pattern(
                                            pattern,
                                            host.use_case_sensitive_file_names(),
                                        )
                                    })
                                    .collect::<Vec<_>>()
                            } else {
                                vec![]
                            });
                    }
                    let include_index = find_index(
                        json_only_include_regexes.as_ref().unwrap(),
                        |re, _| re.is_match(&file).unwrap(),
                        None,
                    );
                    if include_index.is_some() {
                        let key = key_mapper(&file);
                        if !literal_file_map.contains_key(&key)
                            && !wild_card_json_file_map.contains_key(&key)
                        {
                            wild_card_json_file_map.insert(key, file.clone());
                        }
                    }
                    continue;
                }
                if has_file_with_higher_priority_extension(
                    &file,
                    &literal_file_map,
                    &wildcard_file_map,
                    &supported_extensions,
                    key_mapper,
                ) {
                    continue;
                }

                remove_wildcard_files_with_lower_priority_extension(
                    &file,
                    &mut wildcard_file_map,
                    &supported_extensions,
                    key_mapper,
                );

                let key = key_mapper(&file);
                if !literal_file_map.contains_key(&key) && !wildcard_file_map.contains_key(&key) {
                    wildcard_file_map.insert(key, file);
                }
            }
        }
    }

    let mut literal_files: Vec<String> = literal_file_map.into_values().collect::<Vec<_>>();
    let mut wildcard_files = wildcard_file_map.into_values().collect::<Vec<_>>();

    literal_files.append(&mut wildcard_files);
    literal_files.append(&mut wild_card_json_file_map.into_values().collect::<Vec<_>>());
    literal_files
}

pub(crate) fn is_excluded_file(
    path_to_check: &str,
    spec: &ConfigFileSpecs,
    base_path: &str,
    use_case_sensitive_file_names: bool,
    current_directory: &str,
) -> bool {
    let validated_files_spec = spec.validated_files_spec.as_deref();
    let validated_include_specs = spec.validated_include_specs.as_deref();
    let validated_exclude_specs = spec.validated_exclude_specs.as_deref();
    if length(validated_include_specs) == 0 || length(validated_exclude_specs) == 0 {
        return false;
    }

    let base_path = normalize_path(base_path);

    let key_mapper = create_get_canonical_file_name(use_case_sensitive_file_names);
    if let Some(validated_files_spec) = validated_files_spec {
        for file_name in validated_files_spec {
            if key_mapper(&get_normalized_absolute_path(file_name, Some(&base_path)))
                == path_to_check
            {
                return false;
            }
        }
    }

    matches_exclude_worker(
        path_to_check,
        validated_exclude_specs,
        use_case_sensitive_file_names,
        current_directory,
        Some(&base_path),
    )
}

pub(super) fn invalid_dot_dot_after_recursive_wildcard(s: &str) -> bool {
    let wildcard_index = if starts_with(s, "**/") {
        Some(0)
    } else {
        s.find("/**/")
    };
    if wildcard_index.is_none() {
        return false;
    }
    let wildcard_index = wildcard_index.unwrap();
    let last_dot_index = if ends_with(s, "/..") {
        Some(s.len())
    } else {
        s.rfind("/../")
    };
    if last_dot_index.is_none() {
        return false;
    }
    let last_dot_index = last_dot_index.unwrap();
    last_dot_index > wildcard_index
}

pub(crate) fn matches_exclude(
    path_to_check: &str,
    exclude_specs: Option<&[String]>,
    use_case_sensitive_file_names: bool,
    current_directory: &str,
) -> bool {
    matches_exclude_worker(
        path_to_check,
        maybe_filter(exclude_specs, |spec| {
            !invalid_dot_dot_after_recursive_wildcard(spec)
        })
        .as_deref(),
        use_case_sensitive_file_names,
        current_directory,
        None,
    )
}

pub(super) fn matches_exclude_worker(
    path_to_check: &str,
    exclude_specs: Option<&[String]>,
    use_case_sensitive_file_names: bool,
    current_directory: &str,
    base_path: Option<&str>,
) -> bool {
    let exclude_pattern = get_regular_expression_for_wildcard(
        exclude_specs,
        &combine_paths(&normalize_path(current_directory), &vec![base_path]),
        "exclude",
    );
    let exclude_regex = exclude_pattern.map(|exclude_pattern| {
        get_regex_from_pattern(&exclude_pattern, use_case_sensitive_file_names)
    });
    if exclude_regex.is_none() {
        return false;
    }
    let exclude_regex = exclude_regex.unwrap();
    if exclude_regex.is_match(path_to_check).unwrap() {
        return true;
    }
    !has_extension(path_to_check)
        && exclude_regex
            .is_match(&ensure_trailing_directory_separator(path_to_check))
            .unwrap()
}

pub(super) fn validate_specs<TJsonSourceFile: Borrow<Node> + Clone>(
    specs: &[String],
    errors: &mut Vec<Gc<Diagnostic>>,
    disallow_trailing_recursion: bool,
    json_source_file: Option<TJsonSourceFile /*TsConfigSourceFile*/>,
    spec_key: &str,
) -> Vec<String> {
    specs
        .into_iter()
        .filter(|spec| {
            // if (!isString(spec)) return false;
            let diag = spec_to_diagnostic(spec, Some(disallow_trailing_recursion));
            let diag_is_none = diag.is_none();
            if let Some((message, spec)) = diag {
                errors.push(create_diagnostic(
                    json_source_file.clone(),
                    spec_key,
                    message,
                    spec,
                ));
            }
            diag_is_none
        })
        .map(Clone::clone)
        .collect()
}

fn create_diagnostic<TJsonSourceFile: Borrow<Node> + Clone>(
    json_source_file: Option<TJsonSourceFile /*TsConfigSourceFile*/>,
    spec_key: &str,
    message: &DiagnosticMessage,
    spec: String,
) -> Gc<Diagnostic> {
    let element = get_ts_config_prop_array_element_value(json_source_file.clone(), spec_key, &spec);
    Rc::new(if let Some(element) = element {
        create_diagnostic_for_node_in_source_file(
            json_source_file.unwrap().borrow(),
            &element,
            message,
            Some(vec![spec]),
        )
        .into()
    } else {
        create_compiler_diagnostic(message, Some(vec![spec])).into()
    })
}

pub(super) fn spec_to_diagnostic(
    spec: &str,
    disallow_trailing_recursion: Option<bool>,
) -> Option<(&'static DiagnosticMessage, String)> {
    if matches!(disallow_trailing_recursion, Some(true))
        && invalid_trailing_recursion_pattern.is_match(spec).unwrap()
    {
        Some((&Diagnostics::File_specification_cannot_end_in_a_recursive_directory_wildcard_Asterisk_Asterisk_Colon_0, spec.to_owned()))
    } else if invalid_dot_dot_after_recursive_wildcard(spec) {
        Some((&Diagnostics::File_specification_cannot_contain_a_parent_directory_that_appears_after_a_recursive_directory_wildcard_Asterisk_Asterisk_Colon_0, spec.to_owned()))
    } else {
        None
    }
}

pub(super) fn get_wildcard_directories(
    config_file_specs: &ConfigFileSpecs,
    path: &str,
    use_case_sensitive_file_names: bool,
) -> HashMap<String, WatchDirectoryFlags> {
    let include = config_file_specs.validated_include_specs.as_deref();
    let exclude = config_file_specs.validated_exclude_specs.as_deref();
    let raw_exclude_regex = get_regular_expression_for_wildcard(exclude, path, "exclude");
    let exclude_regex = raw_exclude_regex.map(|raw_exclude_regex| {
        Regex::new(&if use_case_sensitive_file_names {
            raw_exclude_regex
        } else {
            format!("(?i){}", &raw_exclude_regex)
        })
        .unwrap()
    });
    let mut wildcard_directories: HashMap<String, WatchDirectoryFlags> = HashMap::new();
    if let Some(include) = include {
        let mut recursive_keys: Vec<String> = vec![];
        for file in include {
            let spec = normalize_path(&combine_paths(path, &vec![Some(&**file)]));
            if matches!(exclude_regex.as_ref(), Some(exclude_regex) if exclude_regex.is_match(&spec).unwrap())
            {
                continue;
            }

            let match_ = get_wildcard_directory_from_spec(&spec, use_case_sensitive_file_names);
            if let Some(match_) = match_ {
                let GetWildcardDirectoryFromSpecReturn { key, flags } = match_;
                let existing_flags = wildcard_directories.get(&key).copied();
                if match existing_flags {
                    None => true,
                    Some(existing_flags) => existing_flags < flags,
                } {
                    wildcard_directories.insert(key.clone(), flags);
                    if flags == WatchDirectoryFlags::Recursive {
                        recursive_keys.push(key);
                    }
                }
            }
        }

        for key in wildcard_directories
            .keys()
            .map(Clone::clone)
            .collect::<Vec<_>>()
        {
            for recursive_key in &recursive_keys {
                if &key != recursive_key
                    && contains_path(
                        &recursive_key,
                        &key,
                        Some(path.to_owned()),
                        Some(!use_case_sensitive_file_names),
                    )
                {
                    wildcard_directories.remove(&key);
                }
            }
        }
    }

    wildcard_directories
}

pub(super) fn get_wildcard_directory_from_spec(
    spec: &str,
    use_case_sensitive_file_names: bool,
) -> Option<GetWildcardDirectoryFromSpecReturn> {
    let match_ = wildcard_directory_pattern.captures(spec);
    if let Ok(Some(match_)) = match_ {
        let question_wildcard_index = spec.find("?");
        let star_wildcard_index = spec.find("*");
        let last_directory_separator_index = spec.rfind(directory_separator);
        return Some(GetWildcardDirectoryFromSpecReturn {
            key: if use_case_sensitive_file_names {
                match_[1].to_owned()
            } else {
                to_file_name_lower_case(&match_[1])
            },
            flags: if matches!(question_wildcard_index, Some(question_wildcard_index) if matches!(last_directory_separator_index, Some(last_directory_separator_index) if question_wildcard_index < last_directory_separator_index))
                || matches!(star_wildcard_index, Some(star_wildcard_index) if matches!(last_directory_separator_index, Some(last_directory_separator_index) if star_wildcard_index < last_directory_separator_index))
            {
                WatchDirectoryFlags::Recursive
            } else {
                WatchDirectoryFlags::None
            },
        });
    }
    if is_implicit_glob(spec) {
        return Some(GetWildcardDirectoryFromSpecReturn {
            key: if use_case_sensitive_file_names {
                spec.to_owned()
            } else {
                to_file_name_lower_case(spec)
            },
            flags: WatchDirectoryFlags::Recursive,
        });
    }
    None
}

pub(super) struct GetWildcardDirectoryFromSpecReturn {
    pub key: String,
    pub flags: WatchDirectoryFlags,
}

pub(super) fn has_file_with_higher_priority_extension(
    file: &str,
    literal_files: &HashMap<String, String>,
    wildcard_files: &HashMap<String, String>,
    extensions: &[Vec<Extension>],
    key_mapper: fn(&str) -> String,
) -> bool {
    let extension_group = for_each(extensions, |group, _| {
        if file_extension_is_one_of(file, group) {
            Some(group)
        } else {
            None
        }
    });
    if extension_group.is_none() {
        return false;
    }
    let extension_group = extension_group.unwrap();
    for ext in extension_group {
        if file_extension_is(file, ext.to_str()) {
            return false;
        }
        let higher_priority_path = key_mapper(&change_extension(file, ext.to_str()));
        if literal_files.contains_key(&higher_priority_path)
            || wildcard_files.contains_key(&higher_priority_path)
        {
            if *ext == Extension::Dts
                && (file_extension_is(file, Extension::Js.to_str())
                    || file_extension_is(file, Extension::Jsx.to_str()))
            {
                continue;
            }
            return true;
        }
    }

    false
}

pub(super) fn remove_wildcard_files_with_lower_priority_extension(
    file: &str,
    wildcard_files: &mut HashMap<String, String>,
    extensions: &[Vec<Extension>],
    key_mapper: fn(&str) -> String,
) {
    let extension_group = for_each(extensions, |group, _| {
        if file_extension_is_one_of(file, group) {
            Some(group)
        } else {
            None
        }
    });
    if extension_group.is_none() {
        return;
    }
    let extension_group = extension_group.unwrap();
    for ext in extension_group.iter().rev() {
        if file_extension_is(file, ext.to_str()) {
            return;
        }
        let lower_priority_path = key_mapper(&change_extension(file, ext.to_str()));
        wildcard_files.remove(&lower_priority_path);
    }
}

pub(crate) fn convert_compiler_options_for_telemetry(
    opts: &CompilerOptions,
) -> HashMap<String, CompilerOptionsValue> {
    // didn't return CompilerOptions because get_option_value_with_empty_strings() seems to violate the expected per-field types
    let mut out: HashMap<String, CompilerOptionsValue> = HashMap::new();
    for (key, value) in opts.to_hash_map_of_compiler_options_values() {
        let type_ = get_option_from_name(key, None);
        if let Some(type_) = type_ {
            out.insert(
                key.to_owned(),
                get_option_value_with_empty_strings(&value, &type_),
            );
        }
    }
    out
}

pub(super) fn get_option_value_with_empty_strings(
    value: &CompilerOptionsValue,
    option: &CommandLineOption,
) -> CompilerOptionsValue {
    match option.type_() {
        CommandLineOptionType::Object => CompilerOptionsValue::String(Some("".to_owned())),
        CommandLineOptionType::String => CompilerOptionsValue::String(Some("".to_owned())),
        CommandLineOptionType::Number => match value {
            CompilerOptionsValue::Usize(Some(value)) => CompilerOptionsValue::Usize(Some(*value)),
            _ => CompilerOptionsValue::String(Some("".to_owned())),
        },
        CommandLineOptionType::Boolean => match value {
            CompilerOptionsValue::Bool(Some(value)) => CompilerOptionsValue::Bool(Some(*value)),
            _ => CompilerOptionsValue::String(Some("".to_owned())),
        },
        CommandLineOptionType::List => {
            let element_type = &option.as_command_line_option_of_list_type().element;
            match value {
                CompilerOptionsValue::VecString(Some(value)) => {
                    CompilerOptionsValue::VecString(Some(
                        value
                            .iter()
                            .map(|v| {
                                match get_option_value_with_empty_strings(
                                    &CompilerOptionsValue::String(Some(v.clone())),
                                    element_type,
                                ) {
                                    CompilerOptionsValue::String(Some(string)) => string,
                                    _ => panic!("Expected string"),
                                }
                            })
                            .collect::<Vec<String>>(),
                    ))
                }
                _ => CompilerOptionsValue::String(Some("".to_owned())),
            }
        }
        CommandLineOptionType::Map(map) => CompilerOptionsValue::String(for_each_entry(
            map,
            |option_enum_value, option_string_value| {
                if &option_enum_value.as_compiler_options_value() == value {
                    Some(option_string_value.to_string())
                } else {
                    None
                }
            },
        )),
    }
}

pub(super) fn get_default_value_for_option(option: &CommandLineOption) -> CompilerOptionsValue {
    match option.type_() {
        CommandLineOptionType::Number => CompilerOptionsValue::Usize(Some(1)),
        CommandLineOptionType::Boolean => CompilerOptionsValue::Bool(Some(true)),
        CommandLineOptionType::String => CompilerOptionsValue::String(Some(
            if option.is_file_path() { "./" } else { "" }.to_owned(),
        )),
        CommandLineOptionType::Object => panic!("Not sure what the default should be"),
        CommandLineOptionType::List => CompilerOptionsValue::VecString(Some(vec![])),
        CommandLineOptionType::Map(map) => CompilerOptionsValue::String(Some(
            map.keys()
                .next()
                .expect("Expected 'option.type' to have entries.")
                .to_string(),
        )),
    }
}
