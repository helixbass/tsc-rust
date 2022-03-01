use std::collections::HashMap;
use std::rc::Rc;

use super::ParsedTsconfig;
use crate::{ConfigFileSpecs, Node, TypeAcquisition, WatchDirectoryFlags};

pub struct ExtendedConfigCacheEntry {
    pub extended_result: Rc<Node /*TsConfigSourceFile*/>,
    pub extended_config: Option<ParsedTsconfig>,
}

pub(super) fn get_default_type_acquisition(config_file_name: Option<&str>) -> TypeAcquisition {
    unimplemented!()
}

pub(super) fn get_wildcard_directories(
    config_file_specs: &ConfigFileSpecs,
    path: &str,
    use_case_sensitive_file_names: bool,
) -> HashMap<String, WatchDirectoryFlags> {
    unimplemented!()
}
