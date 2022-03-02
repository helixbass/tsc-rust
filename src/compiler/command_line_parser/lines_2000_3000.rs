use derive_builder::Builder;
use serde::Serialize;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    convert_compile_on_save_option_from_json, convert_compiler_options_from_json_worker,
    convert_to_object, convert_type_acquisition_from_json_worker,
    convert_watch_options_from_json_worker, get_default_type_acquisition, get_extended_config,
    get_file_names_from_config_specs, get_wildcard_directories, validate_specs,
};
use crate::{
    combine_paths, convert_to_relative_path, create_compiler_diagnostic,
    create_diagnostic_for_node_in_source_file, create_get_canonical_file_name, every,
    extend_compiler_options, extend_watch_options, filter_mutate, first_defined,
    get_directory_path, get_normalized_absolute_path, get_ts_config_prop_array, index_of,
    is_rooted_disk_path, map, maybe_extend_compiler_options, normalize_path, normalize_slashes,
    CompilerOptions, ConfigFileSpecs, Debug_, Diagnostic, DiagnosticMessage,
    DiagnosticRelatedInformationInterface, Diagnostics, ExtendedConfigCacheEntry,
    FileExtensionInfo, HasInitializerInterface, Node, NodeInterface, ParseConfigHost,
    ParsedCommandLine, Path, ProjectReference, System, TypeAcquisition, WatchOptions,
};

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

pub(crate) fn set_config_file_in_options<TConfigFile: Borrow<Node>>(
    options: &mut CompilerOptions,
    config_file: Option<TConfigFile /*TsConfigSourceFile*/>,
) {
    if let Some(config_file) = config_file {
        let config_file = config_file.borrow();
        options.config_file = Some(config_file.node_wrapper());
    }
}

pub(super) fn directory_of_combined_path(file_name: &str, base_path: &str) -> String {
    get_directory_path(&get_normalized_absolute_path(file_name, Some(base_path)))
}

pub(super) fn parse_json_config_file_content_worker<
    TSourceFile: Borrow<Node> + Clone,
    THost: ParseConfigHost,
>(
    json: Option<serde_json::Value>,
    source_file: Option<TSourceFile /*TsConfigSourceFile*/>,
    host: &THost,
    base_path: &str,
    existing_options: Option<Rc<CompilerOptions>>,
    existing_watch_options: Option<Rc<WatchOptions>>,
    config_file_name: Option<&str>,
    resolution_stack: Option<&[Path]>,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
    extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
) -> ParsedCommandLine {
    let existing_options = existing_options.unwrap_or_else(|| Rc::new(Default::default()));
    let resolution_stack_default = vec![];
    let resolution_stack = resolution_stack.unwrap_or(&resolution_stack_default);
    let extra_file_extensions_default = vec![];
    let extra_file_extensions = extra_file_extensions.unwrap_or(&extra_file_extensions_default);
    Debug_.assert(
        json.is_none() && source_file.is_some() || json.is_some() && source_file.is_none(),
        None,
    );
    let mut errors: Vec<Rc<Diagnostic>> = vec![];

    let parsed_config = parse_config(
        json,
        source_file.clone(),
        host,
        base_path,
        config_file_name,
        &*resolution_stack
            .into_iter()
            .map(|path| &**path)
            .collect::<Vec<_>>(),
        &mut errors,
        extended_config_cache,
    );
    let raw = parsed_config.raw.as_ref();
    let mut options: CompilerOptions = extend_compiler_options(
        &existing_options,
        &parsed_config
            .options
            .map_or_else(|| Rc::new(Default::default()), |options| options.clone()),
    );
    let watch_options: Option<Rc<WatchOptions>> =
        if existing_watch_options.is_some() && parsed_config.watch_options.is_some() {
            Some(Rc::new(extend_watch_options(
                existing_watch_options.as_ref().unwrap(),
                parsed_config.watch_options.as_ref().unwrap(),
            )))
        } else {
            parsed_config
                .watch_options
                .clone()
                .or_else(|| existing_watch_options.clone())
        };

    options.config_file_path = config_file_name
        .as_ref()
        .map(|config_file_name| normalize_slashes(config_file_name));
    let config_file_specs: Rc<ConfigFileSpecs> = Rc::new(get_config_file_specs(
        raw,
        source_file.clone(),
        &mut errors,
        config_file_name,
    ));
    if let Some(source_file) = source_file.as_ref() {
        let source_file = source_file.borrow();
        source_file
            .as_source_file()
            .set_config_file_specs(Some(config_file_specs.clone()));
    }
    set_config_file_in_options(&mut options, source_file.clone());

    let base_path_for_file_names =
        normalize_path(&if let Some(config_file_name) = config_file_name.as_ref() {
            directory_of_combined_path(config_file_name, base_path)
        } else {
            base_path.to_owned()
        });
    let options = Rc::new(options);
    ParsedCommandLine {
        options: options.clone(),
        watch_options,
        file_names: get_file_names(
            &config_file_specs,
            &options,
            host,
            extra_file_extensions,
            raw,
            resolution_stack,
            &mut errors,
            config_file_name,
            &base_path_for_file_names,
        ),
        project_references: get_project_references(
            raw,
            source_file,
            &mut errors,
            &base_path_for_file_names,
        ),
        type_acquisition: Some(
            parsed_config
                .type_acquisition
                .unwrap_or_else(|| Rc::new(get_default_type_acquisition(None))),
        ),
        raw: raw.map(Clone::clone),
        errors,
        wildcard_directories: Some(get_wildcard_directories(
            &config_file_specs,
            &base_path_for_file_names,
            host.use_case_sensitive_file_names(),
        )),
        compile_on_save: Some(match raw {
            Some(serde_json::Value::Object(map)) => match map.get("compileOnSave") {
                Some(value) => match value {
                    serde_json::Value::Bool(compile_on_save) => *compile_on_save,
                    _ => false,
                },
                None => false,
            },
            _ => false,
        }),
    }
}

fn get_config_file_specs<TSourceFile: Borrow<Node> + Clone>(
    raw: Option<&serde_json::Value>,
    source_file: Option<TSourceFile /*TsConfigSourceFile*/>,
    errors: &mut Vec<Rc<Diagnostic>>,
    config_file_name: Option<&str>,
) -> ConfigFileSpecs {
    let references_of_raw = get_prop_from_raw(
        raw,
        source_file.clone(),
        errors,
        "references",
        |element| matches!(element, serde_json::Value::Object(_)),
        "object",
    );
    let files_specs = to_prop_value(get_specs_from_raw(
        raw,
        source_file.clone(),
        errors,
        "files",
    ));
    if let Some(files_specs) = files_specs.as_ref() {
        let has_zero_or_no_references = match references_of_raw {
            PropOfRaw::NoProp => true,
            PropOfRaw::Array(references_of_raw) => references_of_raw.is_empty(),
            _ => false,
        };
        let has_extends = match raw {
            Some(serde_json::Value::Object(raw)) => raw.contains_key("extends"),
            _ => false,
        };
        if files_specs.is_empty() && has_zero_or_no_references && !has_extends {
            if let Some(source_file) = source_file.as_ref() {
                let source_file = source_file.borrow();
                let file_name = config_file_name.unwrap_or("tsconfig.json");
                let diagnostic_message = &Diagnostics::The_files_list_in_config_file_0_is_empty;
                let node_value = first_defined(
                    get_ts_config_prop_array(Some(source_file), "files"),
                    |property, _| property.as_property_assignment().maybe_initializer(),
                );
                let error: Rc<Diagnostic> = Rc::new(if let Some(node_value) = node_value {
                    create_diagnostic_for_node_in_source_file(
                        source_file,
                        &node_value,
                        diagnostic_message,
                        Some(vec![file_name.to_owned()]),
                    )
                    .into()
                } else {
                    create_compiler_diagnostic(diagnostic_message, Some(vec![file_name.to_owned()]))
                        .into()
                });
                errors.push(error);
            } else {
                create_compiler_diagnostic_only_if_json(
                    source_file.clone(),
                    errors,
                    &Diagnostics::The_files_list_in_config_file_0_is_empty,
                    Some(vec![config_file_name.unwrap_or("tsconfig.json").to_owned()]),
                );
            }
        }
    }

    let mut include_specs = to_prop_value(get_specs_from_raw(
        raw,
        source_file.clone(),
        errors,
        "include",
    ));

    let exclude_of_raw = get_specs_from_raw(raw, source_file.clone(), errors, "exclude");
    let is_exclude_of_raw_no_prop = matches!(exclude_of_raw, PropOfRaw::NoProp);
    let mut exclude_specs = to_prop_value(exclude_of_raw);
    if is_exclude_of_raw_no_prop {
        if let Some(serde_json::Value::Object(raw)) = raw {
            if let Some(serde_json::Value::Object(raw_compiler_options)) =
                raw.get("compilerOptions")
            {
                let out_dir =
                    raw_compiler_options
                        .get("outDir")
                        .and_then(|out_dir| match out_dir {
                            serde_json::Value::String(out_dir) if !out_dir.is_empty() => {
                                Some(out_dir)
                            }
                            _ => None,
                        });
                let declaration_dir =
                    raw_compiler_options
                        .get("declarationDir")
                        .and_then(|declaration_dir| match declaration_dir {
                            serde_json::Value::String(declaration_dir)
                                if !declaration_dir.is_empty() =>
                            {
                                Some(declaration_dir)
                            }
                            _ => None,
                        });

                if out_dir.is_some() || declaration_dir.is_some() {
                    exclude_specs = Some(
                        vec![out_dir, declaration_dir]
                            .into_iter()
                            .filter(|option| option.is_some())
                            .map(|option| option.unwrap().clone())
                            .collect(),
                    );
                }
            }
        }
    }

    if files_specs.is_none() && include_specs.is_none() {
        include_specs = Some(vec!["**/*".to_owned()]);
    }
    let mut validated_include_specs: Option<Vec<String>> = None;
    let mut validated_exclude_specs: Option<Vec<String>> = None;

    if let Some(include_specs) = include_specs.as_ref() {
        validated_include_specs = Some(validate_specs(
            include_specs,
            errors,
            true,
            source_file.clone(),
            "include",
        ));
    }

    if let Some(exclude_specs) = exclude_specs.as_ref() {
        validated_exclude_specs = Some(validate_specs(
            exclude_specs,
            errors,
            false,
            source_file,
            "exclude",
        ));
    }

    let validated_files_spec = files_specs.clone(); // filter(filesSpecs, isString);
    ConfigFileSpecs {
        files_specs,
        include_specs,
        exclude_specs,
        validated_files_spec,
        validated_include_specs,
        validated_exclude_specs,
        path_patterns: None,
    }
}

fn get_file_names<THost: ParseConfigHost>(
    config_file_specs: &ConfigFileSpecs,
    options: &CompilerOptions,
    host: &THost,
    extra_file_extensions: &[FileExtensionInfo],
    raw: Option<&serde_json::Value>,
    resolution_stack: &[Path],
    errors: &mut Vec<Rc<Diagnostic>>,
    config_file_name: Option<&str>,
    base_path: &str,
) -> Vec<String> {
    let file_names: Vec<String> = get_file_names_from_config_specs(
        config_file_specs,
        base_path,
        options,
        host,
        Some(extra_file_extensions),
    );
    if should_report_no_input_files(
        &file_names,
        can_json_report_no_input_files(raw),
        Some(resolution_stack),
    ) {
        errors.push(get_error_for_no_input_files(
            config_file_specs,
            config_file_name,
        ));
    }
    file_names
}

fn get_project_references<TSourceFile: Borrow<Node> + Clone>(
    raw: Option<&serde_json::Value>,
    source_file: Option<TSourceFile /*TsConfigSourceFile*/>,
    errors: &mut Vec<Rc<Diagnostic>>,
    base_path: &str,
) -> Option<Vec<Rc<ProjectReference>>> {
    let mut project_references: Option<Vec<Rc<ProjectReference>>> = None;
    let references_of_raw = get_prop_from_raw(
        raw,
        source_file.clone(),
        errors,
        "references",
        |element| matches!(element, serde_json::Value::Object(_)),
        "object",
    );
    if let PropOfRaw::Array(references_of_raw) = references_of_raw {
        for ref_ in references_of_raw {
            let ref_ = match ref_ {
                serde_json::Value::Object(ref_) => ref_,
                _ => panic!("Expected object"),
            };
            match ref_.get("path") {
                Some(ref_path) if matches!(ref_path, serde_json::Value::String(_)) => {
                    if project_references.is_none() {
                        project_references = Some(vec![]);
                    }
                    let ref_path = match ref_path {
                        serde_json::Value::String(ref_path) => ref_path,
                        _ => panic!("Expected string"),
                    };
                    project_references
                        .as_mut()
                        .unwrap()
                        .push(Rc::new(ProjectReference {
                            path: get_normalized_absolute_path(&ref_path, Some(base_path)),
                            original_path: Some(ref_path.clone()),
                            prepend: ref_.get("prepend").and_then(
                                |ref_prepend| match ref_prepend {
                                    serde_json::Value::Bool(ref_prepend) => Some(*ref_prepend),
                                    _ => None,
                                },
                            ),
                            circular: ref_.get("circular").and_then(|ref_circular| {
                                match ref_circular {
                                    serde_json::Value::Bool(ref_circular) => Some(*ref_circular),
                                    _ => None,
                                }
                            }),
                        }));
                }
                _ => {
                    create_compiler_diagnostic_only_if_json(
                        source_file.clone(),
                        errors,
                        &Diagnostics::Compiler_option_0_requires_a_value_of_type_1,
                        Some(vec!["reference.path".to_owned(), "string".to_owned()]),
                    );
                }
            }
        }
    }
    project_references
}

enum PropOfRaw {
    Array(Vec<serde_json::Value>),
    NotArray,
    NoProp,
}

fn to_prop_value(spec_result: PropOfRaw) -> Option<Vec<String>> {
    match spec_result {
        PropOfRaw::Array(vec) => Some(
            vec.into_iter()
                .map(|item| match item {
                    serde_json::Value::String(item) => item.to_owned(),
                    _ => panic!("Expected string"),
                })
                .collect(),
        ),
        _ => None,
    }
}

fn get_specs_from_raw<TSourceFile: Borrow<Node>>(
    raw: Option<&serde_json::Value>,
    source_file: Option<TSourceFile /*TsConfigSourceFile*/>,
    errors: &mut Vec<Rc<Diagnostic>>,
    prop: &str, /*"files" | "include" | "exclude"*/
) -> PropOfRaw {
    get_prop_from_raw(
        raw,
        source_file,
        errors,
        prop,
        |value| matches!(value, serde_json::Value::String(_)),
        "string",
    )
}

fn get_prop_from_raw<
    TValidateElement: Fn(&serde_json::Value) -> bool,
    TSourceFile: Borrow<Node>,
>(
    raw: Option<&serde_json::Value>,
    source_file: Option<TSourceFile /*TsConfigSourceFile*/>,
    errors: &mut Vec<Rc<Diagnostic>>,
    prop: &str, /*"files" | "include" | "exclude" | "references"*/
    validate_element: TValidateElement,
    element_type_name: &str,
) -> PropOfRaw {
    match raw {
        Some(serde_json::Value::Object(map)) => match map.get(prop) {
            Some(value) => match value {
                serde_json::Value::Null => PropOfRaw::NoProp,
                serde_json::Value::Array(result) => {
                    if source_file.is_none() && !every(result, |item, _| validate_element(item)) {
                        errors.push(Rc::new(
                            create_compiler_diagnostic(
                                &Diagnostics::Compiler_option_0_requires_a_value_of_type_1,
                                Some(vec![prop.to_owned(), element_type_name.to_owned()]),
                            )
                            .into(),
                        ));
                    }
                    PropOfRaw::Array(result.clone())
                }
                _ => {
                    create_compiler_diagnostic_only_if_json(
                        source_file,
                        errors,
                        &Diagnostics::Compiler_option_0_requires_a_value_of_type_1,
                        Some(vec![prop.to_owned(), "Array".to_owned()]),
                    );
                    PropOfRaw::NotArray
                }
            },
            _ => PropOfRaw::NoProp,
        },
        _ => PropOfRaw::NoProp,
    }
}

fn create_compiler_diagnostic_only_if_json<TSourceFile: Borrow<Node>>(
    source_file: Option<TSourceFile /*TsConfigSourceFile*/>,
    errors: &mut Vec<Rc<Diagnostic>>,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) {
    if source_file.is_none() {
        errors.push(Rc::new(create_compiler_diagnostic(message, args).into()));
    }
}

pub(super) fn is_error_no_input_files(error: &Diagnostic) -> bool {
    error.code() == Diagnostics::No_inputs_were_found_in_config_file_0_Specified_include_paths_were_1_and_exclude_paths_were_2.code
}

pub(super) fn get_error_for_no_input_files(
    config_file_specs: &ConfigFileSpecs,
    config_file_name: Option<&str>,
) -> Rc<Diagnostic> {
    let include_specs = config_file_specs.include_specs.as_ref();
    let exclude_specs = config_file_specs.exclude_specs.as_ref();
    let default_specs_vec = vec![];
    Rc::new(
        create_compiler_diagnostic(
            &Diagnostics::No_inputs_were_found_in_config_file_0_Specified_include_paths_were_1_and_exclude_paths_were_2,
            Some(vec![
                 config_file_name.unwrap_or("tsconfig.json").to_owned(),
                 serde_json::to_string(include_specs.unwrap_or(&default_specs_vec)).unwrap(),
                 serde_json::to_string(exclude_specs.unwrap_or(&default_specs_vec)).unwrap(),
            ])
        ).into()
    )
}

pub(super) fn should_report_no_input_files(
    file_names: &[String],
    can_json_report_no_input_files: bool,
    resolution_stack: Option<&[Path]>,
) -> bool {
    file_names.is_empty()
        && can_json_report_no_input_files
        && !matches!(resolution_stack, Some(resolution_stack) if !resolution_stack.is_empty())
}

pub(crate) fn can_json_report_no_input_files(raw: Option<&serde_json::Value>) -> bool {
    match raw {
        Some(serde_json::Value::Object(map)) => {
            !map.contains_key("files") && !map.contains_key("references")
        }
        _ => true,
    }
}

pub(crate) fn update_error_for_no_input_files(
    file_names: &[String],
    config_file_name: &str,
    config_file_specs: &ConfigFileSpecs,
    config_parse_diagnostics: &mut Vec<Rc<Diagnostic>>,
    can_json_report_no_input_files: bool,
) -> bool {
    let existing_errors = config_parse_diagnostics.len();
    if should_report_no_input_files(file_names, can_json_report_no_input_files, None) {
        config_parse_diagnostics.push(get_error_for_no_input_files(
            config_file_specs,
            Some(config_file_name),
        ));
    } else {
        filter_mutate(config_parse_diagnostics, |error| {
            !is_error_no_input_files(error)
        });
    }
    existing_errors != config_parse_diagnostics.len()
}

#[derive(Builder)]
pub struct ParsedTsconfig {
    pub raw: Option<serde_json::Value>,
    pub options: Option<Rc<CompilerOptions>>,
    pub watch_options: Option<Rc<WatchOptions>>,
    pub type_acquisition: Option<Rc<TypeAcquisition>>,
    pub extended_config_path: Option<String>,
}

pub(super) fn is_successful_parsed_tsconfig(value: &ParsedTsconfig) -> bool {
    value.options.is_some()
}

pub(super) fn parse_config<TSourceFile: Borrow<Node> + Clone, THost: ParseConfigHost>(
    json: Option<serde_json::Value>,
    source_file: Option<TSourceFile /*TsConfigSourceFile*/>,
    host: &THost,
    base_path: &str,
    config_file_name: Option<&str>,
    resolution_stack: &[&str],
    errors: &mut Vec<Rc<Diagnostic>>,
    extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
) -> ParsedTsconfig {
    let base_path = normalize_slashes(base_path);
    let resolved_path =
        get_normalized_absolute_path(config_file_name.unwrap_or(""), Some(&base_path));

    if index_of(resolution_stack, &&*resolved_path, |a, b| a == b) >= 0 {
        errors.push(Rc::new(
            create_compiler_diagnostic(
                &Diagnostics::Circularity_detected_while_resolving_configuration_Colon_0,
                Some(vec![[resolution_stack, &*vec![&*resolved_path]]
                    .concat()
                    .join(" -> ")]),
            )
            .into(),
        ));
        return ParsedTsconfigBuilder::default()
            .raw(json.or_else(|| convert_to_object(source_file.unwrap().borrow(), errors)))
            .build()
            .unwrap();
    }

    let mut own_config: ParsedTsconfig = if let Some(json) = json {
        parse_own_config_of_json(json, host, &base_path, config_file_name, errors)
    } else {
        parse_own_config_of_json_source_file(
            source_file.clone(),
            host,
            &base_path,
            config_file_name,
            errors,
        )
    };

    if own_config
        .options
        .as_ref()
        .and_then(|options| options.paths.as_ref())
        .is_some()
    {
        // own_config.options.as_mut().unwrap().paths_base_path = Some(base_path.clone());
        own_config.options = {
            let mut options = maybe_extend_compiler_options(None, own_config.options.as_deref());
            options.paths_base_path = Some(base_path.clone());
            Some(Rc::new(options))
        };
    }
    if let Some(own_config_extended_config_path) = own_config.extended_config_path.as_ref() {
        let resolution_stack = [resolution_stack, &*vec![&*resolved_path]].concat();
        let extended_config: Option<ParsedTsconfig> = get_extended_config(
            source_file,
            own_config_extended_config_path,
            host,
            &resolution_stack,
            errors,
            extended_config_cache,
        );
        if let Some(extended_config) = extended_config
            .filter(|extended_config| is_successful_parsed_tsconfig(&extended_config))
        {
            let base_raw = match extended_config.raw.as_ref().unwrap() {
                serde_json::Value::Object(map) => map,
                _ => panic!("Expected object"),
            };
            let mut raw = match own_config.raw.as_mut().unwrap() {
                serde_json::Value::Object(map) => map,
                _ => panic!("Expected object"),
            };
            let mut relative_difference: Option<String> = None;
            let mut set_property_in_raw_if_not_undefined = |property_name: &str| {
                let base_raw_property = base_raw.get(property_name);
                if let Some(serde_json::Value::Array(base_raw_property)) = base_raw_property {
                    let mut raw_property = raw.entry(property_name);
                    raw_property.or_insert_with(|| {
                        serde_json::Value::Array(
                            map(Some(base_raw_property), |path, _| {
                                let path = match path {
                                    serde_json::Value::String(path) => path,
                                    _ => panic!("Expected string"),
                                };
                                serde_json::Value::String(if is_rooted_disk_path(path) {
                                    path.to_owned()
                                } else {
                                    if relative_difference.is_none() {
                                        relative_difference = Some(convert_to_relative_path(
                                            &get_directory_path(own_config_extended_config_path),
                                            &base_path,
                                            create_get_canonical_file_name(
                                                host.use_case_sensitive_file_names(),
                                            ),
                                        ));
                                    }
                                    combine_paths(
                                        relative_difference.as_ref().unwrap(),
                                        &*vec![Some(&**path)],
                                    )
                                })
                            })
                            .unwrap(),
                        )
                    });
                }
            };
            set_property_in_raw_if_not_undefined("include");
            set_property_in_raw_if_not_undefined("exclude");
            set_property_in_raw_if_not_undefined("files");
            if let Some(base_raw_compile_on_save) = base_raw.get("compileOnSave") {
                raw.entry("compileOnSave")
                    .or_insert_with(|| base_raw_compile_on_save.clone());
            }
            own_config.options = Some(Rc::new(maybe_extend_compiler_options(
                extended_config.options.as_deref(),
                own_config.options.as_deref(),
            )));
            own_config.watch_options =
                if own_config.watch_options.is_some() && extended_config.watch_options.is_some() {
                    Some(Rc::new(extend_watch_options(
                        extended_config.watch_options.as_ref().unwrap(),
                        own_config.watch_options.as_ref().unwrap(),
                    )))
                } else {
                    own_config
                        .watch_options
                        .clone()
                        .or_else(|| extended_config.watch_options.clone())
                };
        }
    }

    own_config
}

pub(super) fn parse_own_config_of_json<THost: ParseConfigHost>(
    json: serde_json::Value,
    host: &THost,
    base_path: &str,
    config_file_name: Option<&str>,
    errors: &mut Vec<Rc<Diagnostic>>,
) -> ParsedTsconfig {
    let mut json = match json {
        serde_json::Value::Object(json) => json,
        _ => panic!("Expected object"),
    };
    if json.contains_key("excludes") {
        errors.push(Rc::new(
            create_compiler_diagnostic(
                &Diagnostics::Unknown_option_excludes_Did_you_mean_exclude,
                None,
            )
            .into(),
        ));
    }

    let options = convert_compiler_options_from_json_worker(
        json.get("compilerOptions"),
        base_path,
        errors,
        config_file_name,
    );
    let type_acquisition = convert_type_acquisition_from_json_worker(
        json.get("typeAcquisition")
            .or_else(|| json.get("typingOptions")),
        base_path,
        errors,
        config_file_name,
    );
    let watch_options =
        convert_watch_options_from_json_worker(json.get("watchOptions"), base_path, errors);
    json.insert(
        "compileOnSave".to_owned(),
        serde_json::Value::Bool(convert_compile_on_save_option_from_json(
            &json, base_path, errors,
        )),
    );
    let mut extended_config_path: Option<String> = None;

    if let Some(json_extends) = json.get("extends") {
        match json_extends {
            serde_json::Value::String(json_extends) => {
                let new_base = if let Some(config_file_name) = config_file_name {
                    directory_of_combined_path(config_file_name, base_path)
                } else {
                    base_path.to_owned()
                };
                extended_config_path = get_extends_config_path(
                    json_extends,
                    host,
                    &new_base,
                    errors,
                    |message, args| Rc::new(create_compiler_diagnostic(message, args).into()),
                );
            }
            _ => {
                errors.push(Rc::new(
                    create_compiler_diagnostic(
                        &Diagnostics::Compiler_option_0_requires_a_value_of_type_1,
                        Some(vec!["extends".to_owned(), "string".to_owned()]),
                    )
                    .into(),
                ));
            }
        }
    }
    ParsedTsconfig {
        raw: Some(serde_json::Value::Object(json)),
        options: Some(Rc::new(options)),
        watch_options: watch_options.map(|watch_options| Rc::new(watch_options)),
        type_acquisition: type_acquisition.map(|type_acquisition| Rc::new(type_acquisition)),
        extended_config_path,
    }
}

pub(super) fn parse_own_config_of_json_source_file<
    TSourceFile: Borrow<Node>,
    THost: ParseConfigHost,
>(
    source_file: Option<TSourceFile /*TsConfigSourceFile*/>,
    host: &THost,
    base_path: &str,
    config_file_name: Option<&str>,
    errors: &mut Vec<Rc<Diagnostic>>,
) -> ParsedTsconfig {
    unimplemented!()
}

pub(super) fn get_extends_config_path<
    THost: ParseConfigHost,
    TCreateDiagnostic: FnMut(&DiagnosticMessage, Option<Vec<String>>) -> Rc<Diagnostic>,
>(
    extended_config: &str,
    host: &THost,
    base_path: &str,
    errors: &mut Vec<Rc<Diagnostic>>,
    create_diagnostic: TCreateDiagnostic,
) -> Option<String> {
    unimplemented!()
}
