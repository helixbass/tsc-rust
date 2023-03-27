use gc::Gc;

use crate::{EmitHost, RawSourceMap, SourceMapGenerator, SourceMapOptions};

pub struct SourceMapGeneratorOptions {
    extended_diagnostics: Option<bool>,
}

impl From<&SourceMapOptions> for SourceMapGeneratorOptions {
    fn from(value: &SourceMapOptions) -> Self {
        Self {
            extended_diagnostics: value.extended_diagnostics,
        }
    }
}

pub fn create_source_map_generator(
    host: Gc<Box<dyn EmitHost>>,
    file: &str,
    source_root: &str,
    sources_directory_path: &str,
    generator_options: &SourceMapGeneratorOptions,
) -> Gc<Box<dyn SourceMapGenerator>> {
    unimplemented!()
}

pub fn try_parse_raw_source_map(text: &str) -> Option<RawSourceMap> {
    unimplemented!()
}
