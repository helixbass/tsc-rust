use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use super::ParsedTsconfig;
use crate::{
    filter, get_normalized_absolute_path, map, CommandLineOption, CommandLineOptionInterface,
    CommandLineOptionType, CompilerOptions, ConfigFileSpecs, Diagnostic, FileExtensionInfo, Node,
    ParseConfigHost, TypeAcquisition, WatchDirectoryFlags, WatchOptions,
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
    json_option: Option<&serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Rc<Diagnostic>>,
    config_file_name: Option<&str>,
) -> Option<TypeAcquisition> {
    unimplemented!()
}

pub(crate) fn convert_watch_options_from_json_worker(
    json_option: Option<&serde_json::Value>,
    base_path: &str,
    errors: &mut Vec<Rc<Diagnostic>>,
) -> Option<WatchOptions> {
    unimplemented!()
}

pub(super) fn normalize_option_value(
    option: &CommandLineOption,
    base_path: &str,
    value: Option<&serde_json::Value>,
) -> Option<serde_json::Value> {
    let value = value?;
    if matches!(value, serde_json::Value::Null) {
        return Some(serde_json::Value::Null);
    }
    match option.type_() {
        CommandLineOptionType::List => {
            let list_option = option.as_command_line_option_of_list_type();
            if list_option.element.is_file_path()
                || matches!(list_option.element.type_(), CommandLineOptionType::Map(_))
            {
                return match value {
                    serde_json::Value::Array(value) => Some(serde_json::Value::Array(
                        filter(
                            Some(
                                &map(Some(value), |v, _| {
                                    normalize_option_value(&list_option.element, base_path, Some(v))
                                })
                                .unwrap(),
                            ),
                            |v| match v {
                                None => false,
                                Some(value) => match value {
                                    serde_json::Value::Null => false,
                                    serde_json::Value::String(value) => !value.is_empty(),
                                    _ => true,
                                },
                            },
                        )
                        .unwrap()
                        .into_iter()
                        .map(|option| option.unwrap())
                        .collect::<Vec<_>>(),
                    )),
                    _ => panic!("Expected array"),
                };
            }
            Some(value.clone())
        }
        // CommandLineOptionType::Map(map) => map
        //     .get(match value {
        //         serde_json::Value::String(value) => &value.to_lowercase(),
        //         _ => panic!("Expected string"),
        //     })
        //     .map(Clone::clone),
        CommandLineOptionType::Map(_) => Some(value.clone()),
        _ => Some(normalize_non_list_option_value(option, base_path, value)),
    }
}

pub(super) fn normalize_non_list_option_value(
    option: &CommandLineOption,
    base_path: &str,
    value: &serde_json::Value,
) -> serde_json::Value {
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
        return serde_json::Value::String(value_string);
    }
    value.clone()
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
