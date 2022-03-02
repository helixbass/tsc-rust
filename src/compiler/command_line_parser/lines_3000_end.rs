use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use super::ParsedTsconfig;
use crate::{
    CompilerOptions, ConfigFileSpecs, Diagnostic, FileExtensionInfo, Node, ParseConfigHost,
    TypeAcquisition, WatchDirectoryFlags, WatchOptions,
};

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
