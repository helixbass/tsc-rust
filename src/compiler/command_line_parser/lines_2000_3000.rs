use serde::Serialize;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    command_options_without_build, common_options_with_build, get_default_type_acquisition,
    get_wildcard_directories,
};
use crate::{
    create_compiler_diagnostic, for_each, normalize_path, normalize_slashes,
    AlternateModeDiagnostics, BuildOptions, CommandLineOption, CommandLineOptionBase,
    CommandLineOptionInterface, CommandLineOptionOfBooleanType, CommandLineOptionOfListType,
    CommandLineOptionOfStringType, CommandLineOptionType, CompilerOptions, CompilerOptionsBuilder,
    Debug_, Diagnostic, DiagnosticMessage, DiagnosticRelatedInformationInterface, Diagnostics,
    FileExtensionInfo, ModuleKind, Node, ParseConfigHost, ParsedCommandLine, Path, Push,
    ScriptTarget, StringOrDiagnosticMessage, System, WatchOptions,
};
use local_macros::enum_unwrapped;

#[derive(Serialize)]
pub(crate) struct TSConfig {}

pub(crate) fn convert_to_tsconfig(
    config_parse_result: &ParsedCommandLine,
    config_file_name: &str,
    host: &dyn System, /*ConvertToTSConfigHost*/
) -> TSConfig {
    unimplemented!()
}

pub(crate) fn convert_to_options_with_absolute_paths<TToAbsolutePath: FnMut(&str) -> String>(
    options: Rc<CompilerOptions>,
    to_absolute_path: TToAbsolutePath,
) -> Rc<CompilerOptions> {
    options
}

// pub(super) fn parse_json_config_file_content_worker<
//     TSourceFile: Borrow<Node>,
//     THost: ParseConfigHost,
// >(
//     json: Option<serde_json::Value>,
//     source_file: Option<TSourceFile /*TsConfigSourceFile*/>,
//     host: &THost,
//     base_path: &str,
//     existing_options: Option<Rc<CompilerOptions>>,
//     existing_watch_options: Option<Rc<WatchOptions>>,
//     config_file_name: Option<&str>,
//     resolution_stack: Option<&[Path]>,
//     extra_file_extensions: Option<&[FileExtensionInfo]>,
//     extended_config_cache: Option<&HashMap<String, ExtendedConfigCacheEntry>>,
// ) -> ParsedCommandLine {
//     let existing_options = existing_options.unwrap_or_else(|| Rc::new(Default::default()));
//     let resolution_stack_default = vec![];
//     let resolution_stack = resolution_stack.unwrap_or(&resolution_stack_default);
//     let extra_file_extensions_default = vec![];
//     let extra_file_extensions = extra_file_extensions.unwrap_or(&extra_file_extensions_default);
//     Debug_.assert(
//         json.is_none() && source_file.is_some() || json.is_some() && source_file.is_none(),
//         None,
//     );
//     let mut errors: Vec<Rc<Diagnostic>> = vec![];

//     let parsed_config = parse_config(
//         json,
//         source_file,
//         host,
//         base_path,
//         config_file_name,
//         &resolution_stack,
//         &mut errors,
//         extended_config_cache,
//     );
//     let raw = parsed_config.raw.as_ref();
//     let mut options: CompilerOptions = extend_compiler_options(
//         &existing_options,
//         &parsed_config
//             .options
//             .map_or_else(|| Rc::new(Default::default()), |options| options.clone()),
//     );
//     let watch_options: Option<Rc<WatchOptions>> =
//         if existing_watch_options.is_some() && parsed_config.watch_options.is_some() {
//             Some(Rc::new(extend_watch_options(
//                 existing_watch_options.as_ref().unwrap(),
//                 parsed_config.watch_options.as_ref().unwrap(),
//             )))
//         } else {
//             parsed_config
//                 .watch_options
//                 .clone()
//                 .or_else(|| existing_watch_options.clone())
//         };

//     options.config_file_path = config_file_name
//         .as_ref()
//         .map(|config_file_name| normalize_slashes(config_file_name));
//     let config_file_specs = get_config_file_specs();
//     if let Some(source_file) = source_file.as_ref() {
//         let source_file = source_file.borrow();
//         source_file
//             .as_source_file()
//             .set_config_file_specs(config_file_specs);
//     }
//     set_config_file_in_options(&mut options, source_file);

//     let base_path_for_file_names =
//         normalize_path(if let Some(config_file_name) = config_file_name.as_ref() {
//             &directory_of_combined_path(config_file_name, base_path)
//         } else {
//             base_path
//         });
//     ParsedCommandLine {
//         options: Rc::new(options),
//         watch_options,
//         file_names: get_file_names(&base_path_for_file_names),
//         project_references: get_project_references(&base_path_for_file_names),
//         type_acquisition: Some(
//             parsed_config
//                 .type_acquisition
//                 .unwrap_or_else(|| get_default_type_acquisition()),
//         ),
//         raw,
//         errors,
//         wildcard_directories: get_wildcard_directories(
//             config_file_specs,
//             base_path_for_file_names,
//             host.use_case_sensitive_file_names(),
//         ),
//         compile_on_save: Some(match raw {
//             Some(serde_json::Value::Object(map)) => match map.get("compileOnSave") {
//                 Some(value) => match value {
//                     serde_json::Value::Bool(compile_on_save) => *compile_on_save,
//                     _ => false,
//                 },
//                 None => false,
//             },
//             _ => false,
//         }),
//     }
// }

pub struct ParsedTsconfig {}
