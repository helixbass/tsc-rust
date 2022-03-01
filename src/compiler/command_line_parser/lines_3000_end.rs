use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use super::ParsedTsconfig;
use crate::{
    CompilerOptions, ConfigFileSpecs, Diagnostic, FileExtensionInfo, Node, ParseConfigHost,
    TypeAcquisition, WatchDirectoryFlags,
};

pub struct ExtendedConfigCacheEntry {
    pub extended_result: Rc<Node /*TsConfigSourceFile*/>,
    pub extended_config: Option<ParsedTsconfig>,
}

pub(super) fn get_default_type_acquisition(config_file_name: Option<&str>) -> TypeAcquisition {
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
