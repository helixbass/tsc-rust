use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    convert_enable_auto_discovery_to_enable, create_compiler_diagnostic_for_invalid_custom_type,
    create_unknown_option_error, get_command_line_watch_options_map,
    get_compiler_option_value_type_string, is_compiler_options_value,
    type_acquisition_did_you_mean_diagnostics, ParsedTsconfig,
};
use crate::{
    create_compiler_diagnostic, filter, filter_owning, get_normalized_absolute_path, map,
    set_type_acquisition_value, CommandLineOption, CommandLineOptionInterface,
    CommandLineOptionType, CompilerOptions, CompilerOptionsValue, ConfigFileSpecs, Diagnostic,
    Diagnostics, DidYouMeanOptionsDiagnostics, FileExtensionInfo, Node, ParseConfigHost,
    TypeAcquisition, WatchDirectoryFlags, WatchOptions,
};
use local_macros::enum_unwrapped;

pub struct ExtendedConfigCacheEntry {
    pub extended_result: Rc<Node /*TsConfigSourceFile*/>,
    pub extended_config: Option<ParsedTsconfig>,
}

pub(crate) fn get_extended_config<TSourceFile: Borrow<Node>, THost: ParseConfigHost>(
    source_file: Option<TSourceFile>,
    extended_config_path: &str,
    host: &THost,
    resolution_stack: &[&str],
    errors: &mut Vec<Rc<Diagnostic>>,
    extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
) -> Option<ParsedTsconfig> {
    unimplemented!()
}

pub(super) fn convert_compile_on_save_option_from_json(
    json_option: &serde_json::Map<String, serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Rc<Diagnostic>>,
) -> bool {
    unimplemented!()
}

pub(super) fn get_default_compiler_options(config_file_name: Option<&str>) -> CompilerOptions {
    unimplemented!()
}

pub(crate) fn convert_compiler_options_from_json_worker(
    json_options: Option<&serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Rc<Diagnostic>>,
    config_file_name: Option<&str>,
) -> CompilerOptions {
    unimplemented!()
}

pub(super) fn get_default_type_acquisition(config_file_name: Option<&str>) -> TypeAcquisition {
    unimplemented!()
}

pub(crate) fn convert_type_acquisition_from_json_worker(
    json_options: Option<&serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Rc<Diagnostic>>,
    config_file_name: Option<&str>,
) -> TypeAcquisition {
    let mut options = get_default_type_acquisition(config_file_name);
    let type_acquisition = convert_enable_auto_discovery_to_enable(json_options);

    convert_options_from_json_type_acquisition(
        &get_command_line_watch_options_map(),
        json_options,
        base_path,
        &mut options,
        &*type_acquisition_did_you_mean_diagnostics(),
        errors,
    );
    options
}

pub(crate) fn convert_watch_options_from_json_worker(
    json_option: Option<&serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Rc<Diagnostic>>,
) -> Option<WatchOptions> {
    unimplemented!()
}

pub(super) fn convert_options_from_json_type_acquisition(
    options_name_map: &HashMap<String, Rc<CommandLineOption>>,
    json_options: Option<&serde_json::Value>,
    base_path: &str,
    default_options: &mut TypeAcquisition,
    diagnostics: &dyn DidYouMeanOptionsDiagnostics,
    errors: &mut Vec<Rc<Diagnostic>>,
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

pub(crate) fn convert_json_option(
    opt: &CommandLineOption,
    value: Option<&serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Rc<Diagnostic>>,
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
                            Some(
                                &map(Some(value), |v, _| {
                                    normalize_option_value(&list_option.element, base_path, Some(v))
                                })
                                .unwrap(),
                            ),
                            |v| v.is_some(),
                        )
                        .unwrap()
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
    errors: &mut Vec<Rc<Diagnostic>>,
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
    errors: &mut Vec<Rc<Diagnostic>>,
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
    errors: &mut Vec<Rc<Diagnostic>>,
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
    errors: &mut Vec<Rc<Diagnostic>>,
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

pub(crate) fn get_file_names_from_config_specs<THost: ParseConfigHost>(
    config_file_specs: &ConfigFileSpecs,
    base_path: &str,
    options: &CompilerOptions,
    host: &THost,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
) -> Vec<String> {
    unimplemented!()
}

pub(super) fn validate_specs<TJsonSourceFile: Borrow<Node>>(
    specs: &[String],
    errors: &mut Vec<Rc<Diagnostic>>,
    disallow_trailing_recursion: bool,
    json_source_file: Option<TJsonSourceFile /*TsConfigSourceFile*/>,
    spec_key: &str,
) -> Vec<String> {
    unimplemented!()
}

pub(super) fn get_wildcard_directories(
    config_file_specs: &ConfigFileSpecs,
    path: &str,
    use_case_sensitive_file_names: bool,
) -> HashMap<String, WatchDirectoryFlags> {
    unimplemented!()
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
