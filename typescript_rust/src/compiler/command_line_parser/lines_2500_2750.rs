use std::{cell::RefCell, collections::HashMap, io, rc::Rc};

use id_arena::Id;

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
    ExtendedConfigCacheEntry, FileExtensionInfo, HasArena, HasInitializerInterface, InArena, Node,
    NodeInterface, OptionInArena, ParseConfigHost, ParsedCommandLine, Path, ProjectReference,
    ToHashMapOfCompilerOptionsValues, WatchOptions,
};

pub(crate) fn convert_to_options_with_absolute_paths(
    options: Id<CompilerOptions>,
    to_absolute_path: impl Fn(&str) -> String,
    arena: &impl HasArena,
) -> Id<CompilerOptions> {
    let mut result: CompilerOptions = Default::default();
    let options_name_map_ref = get_options_name_map(arena).ref_(arena);
    let options_name_map = &options_name_map_ref.options_name_map;

    for (name, value) in options.ref_(arena).to_hash_map_of_compiler_options_values() {
        result.set_value(
            name,
            convert_to_option_value_with_absolute_paths(
                options_name_map
                    .get(&name.to_lowercase())
                    .copied()
                    .refed(arena)
                    .as_deref(),
                value,
                &to_absolute_path,
                arena,
            ),
        );
    }
    if let Some(result_config_file_path) = result.config_file_path.as_ref() {
        result.config_file_path = Some(to_absolute_path(result_config_file_path));
    }
    arena.alloc_compiler_options(result)
}

pub(super) fn convert_to_option_value_with_absolute_paths(
    option: Option<&CommandLineOption>,
    value: CompilerOptionsValue,
    to_absolute_path: &impl Fn(&str) -> String,
    arena: &impl HasArena,
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
                    .ref_(arena)
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
    existing_options: Option<Id<CompilerOptions>>,
    config_file_name: Option<&str>,
    resolution_stack: Option<&[Path]>,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
    extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
    existing_watch_options: Option<Rc<WatchOptions>>,
    arena: &impl HasArena,
) -> io::Result<ParsedCommandLine> {
    parse_json_config_file_content_worker(
        json,
        Option::<Id<Node>>::None,
        host,
        base_path,
        existing_options,
        existing_watch_options,
        config_file_name,
        resolution_stack,
        extra_file_extensions,
        extended_config_cache,
        arena,
    )
}

pub fn parse_json_source_file_config_file_content(
    source_file: Id<Node>, /*TsConfigSourceFile*/
    host: &(impl ParseConfigHost + ?Sized),
    base_path: &str,
    existing_options: Option<Id<CompilerOptions>>,
    config_file_name: Option<&str>,
    resolution_stack: Option<&[Path]>,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
    extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
    existing_watch_options: Option<Rc<WatchOptions>>,
    arena: &impl HasArena,
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
        arena,
    )
}

pub(crate) fn set_config_file_in_options(
    options: &mut CompilerOptions,
    config_file: Option<Id<Node> /*TsConfigSourceFile*/>,
) {
    if let Some(config_file) = config_file {
        options.config_file = Some(config_file);
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
    source_file: Option<Id<Node> /*TsConfigSourceFile*/>,
    host: &(impl ParseConfigHost + ?Sized),
    base_path: &str,
    existing_options: Option<Id<CompilerOptions>>,
    existing_watch_options: Option<Rc<WatchOptions>>,
    config_file_name: Option<&str>,
    resolution_stack: Option<&[Path]>,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
    mut extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
    arena: &impl HasArena,
) -> io::Result<ParsedCommandLine> {
    let existing_options =
        existing_options.unwrap_or_else(|| arena.alloc_compiler_options(Default::default()));
    let resolution_stack_default = vec![];
    let resolution_stack = resolution_stack.unwrap_or(&resolution_stack_default);
    let extra_file_extensions_default = vec![];
    let extra_file_extensions = extra_file_extensions.unwrap_or(&extra_file_extensions_default);
    Debug_.assert(
        json.is_none() && source_file.is_some() || json.is_some() && source_file.is_none(),
        None,
    );
    let errors: Id<Vec<Id<Diagnostic>>> = arena.alloc_vec_diagnostic(Default::default());

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
        arena,
    )?;
    let raw = parsed_config.raw.as_ref();
    let mut options: CompilerOptions = extend_compiler_options(
        &existing_options.ref_(arena),
        &parsed_config
            .options
            .map_or_else(
                || arena.alloc_compiler_options(Default::default()),
                |options| options.clone(),
            )
            .ref_(arena),
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
        &mut errors.ref_mut(arena),
        config_file_name,
        arena,
    ));
    if let Some(source_file) = source_file {
        source_file
            .ref_(arena)
            .as_source_file()
            .set_config_file_specs(Some(config_file_specs.clone()));
    }
    set_config_file_in_options(&mut options, source_file);

    let base_path_for_file_names =
        normalize_path(&if let Some(config_file_name) = config_file_name.as_ref() {
            directory_of_combined_path(config_file_name, base_path)
        } else {
            base_path.to_owned()
        });
    let options = arena.alloc_compiler_options(options);
    Ok(ParsedCommandLine {
        options: options.clone(),
        watch_options,
        file_names: {
            let value = get_file_names(
                &config_file_specs,
                &options.ref_(arena),
                host,
                extra_file_extensions,
                raw,
                resolution_stack,
                &mut errors.clone().ref_mut(arena),
                config_file_name,
                &base_path_for_file_names,
                arena,
            )?;
            value
        },
        project_references: get_project_references(
            raw,
            source_file,
            &mut errors.clone().ref_mut(arena),
            &base_path_for_file_names,
            arena,
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
    source_file: Option<Id<Node> /*TsConfigSourceFile*/>,
    errors: &mut Vec<Id<Diagnostic>>,
    config_file_name: Option<&str>,
    arena: &impl HasArena,
) -> ConfigFileSpecs {
    let references_of_raw = get_prop_from_raw(
        raw,
        source_file.clone(),
        errors,
        "references",
        |element| matches!(element, serde_json::Value::Object(_)),
        "object",
        arena,
    );
    let files_specs = to_prop_value(get_specs_from_raw(
        raw,
        source_file.clone(),
        errors,
        "files",
        arena,
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
            if let Some(source_file) = source_file {
                let file_name = config_file_name.unwrap_or("tsconfig.json");
                let diagnostic_message = &Diagnostics::The_files_list_in_config_file_0_is_empty;
                let node_value = first_defined(
                    get_ts_config_prop_array(Some(source_file), "files", arena),
                    |property, _| {
                        property
                            .ref_(arena)
                            .as_property_assignment()
                            .maybe_initializer()
                    },
                );
                let error: Id<Diagnostic> =
                    arena.alloc_diagnostic(if let Some(node_value) = node_value {
                        create_diagnostic_for_node_in_source_file(
                            source_file,
                            node_value,
                            diagnostic_message,
                            Some(vec![file_name.to_owned()]),
                            arena,
                        )
                        .into()
                    } else {
                        create_compiler_diagnostic(
                            diagnostic_message,
                            Some(vec![file_name.to_owned()]),
                        )
                        .into()
                    });
                errors.push(error);
            } else {
                create_compiler_diagnostic_only_if_json(
                    source_file.clone(),
                    errors,
                    &Diagnostics::The_files_list_in_config_file_0_is_empty,
                    Some(vec![config_file_name.unwrap_or("tsconfig.json").to_owned()]),
                    arena,
                );
            }
        }
    }

    let mut include_specs = to_prop_value(get_specs_from_raw(
        raw,
        source_file.clone(),
        errors,
        "include",
        arena,
    ));

    let exclude_of_raw = get_specs_from_raw(raw, source_file.clone(), errors, "exclude", arena);
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
            arena,
        ));
    }

    if let Some(exclude_specs) = exclude_specs.as_ref() {
        validated_exclude_specs = Some(validate_specs(
            exclude_specs,
            errors,
            false,
            source_file,
            "exclude",
            arena,
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
    errors: &mut Vec<Id<Diagnostic>>,
    config_file_name: Option<&str>,
    base_path: &str,
    arena: &impl HasArena,
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
            arena,
        ));
    }
    Ok(file_names)
}

pub(super) fn get_project_references(
    raw: Option<&serde_json::Value>,
    source_file: Option<Id<Node> /*TsConfigSourceFile*/>,
    errors: &mut Vec<Id<Diagnostic>>,
    base_path: &str,
    arena: &impl HasArena,
) -> Option<Vec<Rc<ProjectReference>>> {
    let mut project_references: Option<Vec<Rc<ProjectReference>>> = None;
    let references_of_raw = get_prop_from_raw(
        raw,
        source_file.clone(),
        errors,
        "references",
        |element| matches!(element, serde_json::Value::Object(_)),
        "object",
        arena,
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
                        arena,
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

pub(super) fn get_specs_from_raw(
    raw: Option<&serde_json::Value>,
    source_file: Option<Id<Node> /*TsConfigSourceFile*/>,
    errors: &mut Vec<Id<Diagnostic>>,
    prop: &str, /*"files" | "include" | "exclude"*/
    arena: &impl HasArena,
) -> PropOfRaw {
    get_prop_from_raw(
        raw,
        source_file,
        errors,
        prop,
        |value| matches!(value, serde_json::Value::String(_)),
        "string",
        arena,
    )
}

pub(super) fn get_prop_from_raw(
    raw: Option<&serde_json::Value>,
    source_file: Option<Id<Node> /*TsConfigSourceFile*/>,
    errors: &mut Vec<Id<Diagnostic>>,
    prop: &str, /*"files" | "include" | "exclude" | "references"*/
    validate_element: impl Fn(&serde_json::Value) -> bool,
    element_type_name: &str,
    arena: &impl HasArena,
) -> PropOfRaw {
    match raw {
        Some(serde_json::Value::Object(map)) => match map.get(prop) {
            Some(value) => match value {
                serde_json::Value::Null => PropOfRaw::NoProp,
                serde_json::Value::Array(result) => {
                    if source_file.is_none() && !every(result, |item, _| validate_element(item)) {
                        errors.push(
                            arena.alloc_diagnostic(
                                create_compiler_diagnostic(
                                    &Diagnostics::Compiler_option_0_requires_a_value_of_type_1,
                                    Some(vec![prop.to_owned(), element_type_name.to_owned()]),
                                )
                                .into(),
                            ),
                        );
                    }
                    PropOfRaw::Array(result.clone())
                }
                _ => {
                    create_compiler_diagnostic_only_if_json(
                        source_file,
                        errors,
                        &Diagnostics::Compiler_option_0_requires_a_value_of_type_1,
                        Some(vec![prop.to_owned(), "Array".to_owned()]),
                        arena,
                    );
                    PropOfRaw::NotArray
                }
            },
            _ => PropOfRaw::NoProp,
        },
        _ => PropOfRaw::NoProp,
    }
}
