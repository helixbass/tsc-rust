use std::{borrow::Borrow, cell::RefCell, collections::HashMap, io, rc::Rc};

use gc::{Gc, GcCell};

use super::{
    can_json_report_no_input_files, create_compiler_diagnostic_only_if_json,
    get_default_type_acquisition, get_error_for_no_input_files, get_file_names_from_config_specs,
    get_options_name_map, get_wildcard_directories, parse_config, should_report_no_input_files,
    validate_specs,
};
use crate::{
    create_compiler_diagnostic, create_diagnostic_for_node_in_source_file, every,
    extend_compiler_options, extend_watch_options, first_defined, get_directory_path,
    get_normalized_absolute_path, get_ts_config_prop_array, normalize_path, normalize_slashes,
    CommandLineOption, CommandLineOptionInterface, CommandLineOptionType, CompilerOptions,
    CompilerOptionsValue, ConfigFileSpecs, Debug_, Diagnostic, Diagnostics,
    ExtendedConfigCacheEntry, FileExtensionInfo, HasArena, HasInitializerInterface, Node,
    NodeInterface, ParseConfigHost, ParsedCommandLine, Path, ProjectReference,
    ToHashMapOfCompilerOptionsValues, WatchOptions,
};

pub(crate) fn convert_to_options_with_absolute_paths<TToAbsolutePath: Fn(&str) -> String>(
    options: Gc<CompilerOptions>,
    to_absolute_path: TToAbsolutePath,
) -> Gc<CompilerOptions> {
    let mut result: CompilerOptions = Default::default();
    let options_name_map = &get_options_name_map().options_name_map;

    for (name, value) in options.to_hash_map_of_compiler_options_values() {
        result.set_value(
            name,
            convert_to_option_value_with_absolute_paths(
                options_name_map
                    .get(&name.to_lowercase())
                    .map(|option| &**option),
                value,
                &to_absolute_path,
            ),
        );
    }
    if let Some(result_config_file_path) = result.config_file_path.as_ref() {
        result.config_file_path = Some(to_absolute_path(result_config_file_path));
    }
    Gc::new(result)
}

pub(super) fn convert_to_option_value_with_absolute_paths<TToAbsolutePath: Fn(&str) -> String>(
    option: Option<&CommandLineOption>,
    value: CompilerOptionsValue,
    to_absolute_path: &TToAbsolutePath,
) -> CompilerOptionsValue {
    if let Some(option) = option {
        if value.is_some() {
            if matches!(option.type_(), CommandLineOptionType::List) {
                let values = match &value {
                    CompilerOptionsValue::VecString(Some(value)) => value,
                    _ => panic!("Expected vec of strings"),
                };
                if option
                    .as_command_line_option_of_list_type()
                    .element
                    .is_file_path()
                    && !values.is_empty()
                {
                    return CompilerOptionsValue::VecString(Some(
                        values
                            .into_iter()
                            .map(|value| to_absolute_path(value))
                            .collect(),
                    ));
                }
            } else if option.is_file_path() {
                return CompilerOptionsValue::String(Some(to_absolute_path(&match value {
                    CompilerOptionsValue::String(Some(value)) => value,
                    _ => panic!("Expected string"),
                })));
            }
        }
    }
    value
}

pub fn parse_json_config_file_content(
    json: Option<serde_json::Value>,
    host: &impl ParseConfigHost,
    base_path: &str,
    existing_options: Option<Gc<CompilerOptions>>,
    config_file_name: Option<&str>,
    resolution_stack: Option<&[Path]>,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
    extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
    existing_watch_options: Option<Rc<WatchOptions>>,
) -> io::Result<ParsedCommandLine> {
    parse_json_config_file_content_worker(
        json,
        Option::<&Node>::None,
        host,
        base_path,
        existing_options,
        existing_watch_options,
        config_file_name,
        resolution_stack,
        extra_file_extensions,
        extended_config_cache,
    )
}

pub fn parse_json_source_file_config_file_content(
    source_file: &Node, /*TsConfigSourceFile*/
    host: &(impl ParseConfigHost + ?Sized),
    base_path: &str,
    existing_options: Option<Gc<CompilerOptions>>,
    config_file_name: Option<&str>,
    resolution_stack: Option<&[Path]>,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
    extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
    existing_watch_options: Option<Rc<WatchOptions>>,
) -> io::Result<ParsedCommandLine> {
    parse_json_config_file_content_worker(
        None,
        Some(source_file),
        host,
        base_path,
        existing_options,
        existing_watch_options,
        config_file_name,
        resolution_stack,
        extra_file_extensions,
        extended_config_cache,
    )
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

// function isNullOrUndefined(x: any) -> x is null | undefined {
//     return x === undefined || x === null;
// }

pub(super) fn directory_of_combined_path(file_name: &str, base_path: &str) -> String {
    get_directory_path(&get_normalized_absolute_path(file_name, Some(base_path)))
}

pub(super) fn parse_json_config_file_content_worker(
    json: Option<serde_json::Value>,
    source_file: Option<
        impl Borrow<Node> + Clone,
        /*TsConfigSourceFile*/
    >,
    host: &(impl ParseConfigHost + ?Sized),
    base_path: &str,
    existing_options: Option<Gc<CompilerOptions>>,
    existing_watch_options: Option<Rc<WatchOptions>>,
    config_file_name: Option<&str>,
    resolution_stack: Option<&[Path]>,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
    mut extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
) -> io::Result<ParsedCommandLine> {
    let existing_options = existing_options.unwrap_or_else(|| Gc::new(Default::default()));
    let resolution_stack_default = vec![];
    let resolution_stack = resolution_stack.unwrap_or(&resolution_stack_default);
    let extra_file_extensions_default = vec![];
    let extra_file_extensions = extra_file_extensions.unwrap_or(&extra_file_extensions_default);
    Debug_.assert(
        json.is_none() && source_file.is_some() || json.is_some() && source_file.is_none(),
        None,
    );
    let errors: Gc<GcCell<Vec<Gc<Diagnostic>>>> = Default::default();

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
        errors.clone(),
        &mut extended_config_cache,
    )?;
    let raw = parsed_config.raw.as_ref();
    let mut options: CompilerOptions = extend_compiler_options(
        &existing_options,
        &parsed_config
            .options
            .map_or_else(|| Gc::new(Default::default()), |options| options.clone()),
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
        &mut errors.borrow_mut(),
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
    let options = Gc::new(options);
    Ok(ParsedCommandLine {
        options: options.clone(),
        watch_options,
        file_names: {
            let value = get_file_names(
                &config_file_specs,
                &options,
                host,
                extra_file_extensions,
                raw,
                resolution_stack,
                &mut errors.clone().borrow_mut(),
                config_file_name,
                &base_path_for_file_names,
            )?;
            value
        },
        project_references: get_project_references(
            raw,
            source_file,
            &mut errors.clone().borrow_mut(),
            &base_path_for_file_names,
        ),
        type_acquisition: Some(
            parsed_config
                .type_acquisition
                .unwrap_or_else(|| Rc::new(get_default_type_acquisition(None))),
        ),
        raw: raw.map(Clone::clone),
        errors: errors.clone(),
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
    })
}

pub(super) fn get_config_file_specs(
    raw: Option<&serde_json::Value>,
    source_file: Option<impl Borrow<Node> + Clone /*TsConfigSourceFile*/>,
    errors: &mut Vec<Gc<Diagnostic>>,
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
                let error: Gc<Diagnostic> = Gc::new(if let Some(node_value) = node_value {
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
        path_patterns: RefCell::new(None),
    }
}

pub(super) fn get_file_names(
    config_file_specs: &ConfigFileSpecs,
    options: &CompilerOptions,
    host: &(impl ParseConfigHost + ?Sized),
    extra_file_extensions: &[FileExtensionInfo],
    raw: Option<&serde_json::Value>,
    resolution_stack: &[Path],
    errors: &mut Vec<Gc<Diagnostic>>,
    config_file_name: Option<&str>,
    base_path: &str,
) -> io::Result<Vec<String>> {
    let file_names: Vec<String> = get_file_names_from_config_specs(
        config_file_specs,
        base_path,
        options,
        host,
        Some(extra_file_extensions),
    )?;
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
    Ok(file_names)
}

pub(super) fn get_project_references(
    raw: Option<&serde_json::Value>,
    source_file: Option<impl Borrow<Node> + Clone /*TsConfigSourceFile*/>,
    errors: &mut Vec<Gc<Diagnostic>>,
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

pub(super) enum PropOfRaw {
    Array(Vec<serde_json::Value>),
    NotArray,
    NoProp,
}

pub(super) fn to_prop_value(spec_result: PropOfRaw) -> Option<Vec<String>> {
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

pub(super) fn get_specs_from_raw<TSourceFile: Borrow<Node>>(
    raw: Option<&serde_json::Value>,
    source_file: Option<TSourceFile /*TsConfigSourceFile*/>,
    errors: &mut Vec<Gc<Diagnostic>>,
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

pub(super) fn get_prop_from_raw<
    TValidateElement: Fn(&serde_json::Value) -> bool,
    TSourceFile: Borrow<Node>,
>(
    raw: Option<&serde_json::Value>,
    source_file: Option<TSourceFile /*TsConfigSourceFile*/>,
    errors: &mut Vec<Gc<Diagnostic>>,
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
                        errors.push(Gc::new(
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
