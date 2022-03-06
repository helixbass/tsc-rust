use derive_builder::Builder;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    command_options_without_build, convert_compile_on_save_option_from_json,
    convert_compiler_options_from_json_worker, convert_config_file_to_object, convert_to_object,
    convert_type_acquisition_from_json_worker, convert_watch_options_from_json_worker,
    directory_of_combined_path, get_default_compiler_options, get_default_type_acquisition,
    get_extended_config, normalize_option_value,
};
use crate::{
    append, combine_paths, convert_to_relative_path, create_compiler_diagnostic,
    create_diagnostic_for_node_in_source_file, create_get_canonical_file_name,
    extend_watch_options, filter_mutate, find, get_directory_path, get_normalized_absolute_path,
    get_text_of_property_name, index_of, is_rooted_disk_path, map, maybe_extend_compiler_options,
    normalize_slashes, set_type_acquisition_value, set_watch_option_value, CommandLineOption,
    CommandLineOptionInterface, CompilerOptions, ConfigFileSpecs, Debug_, Diagnostic,
    DiagnosticMessage, DiagnosticRelatedInformationInterface, Diagnostics,
    ExtendedConfigCacheEntry, JsonConversionNotifier, Node, NodeInterface, ParseConfigHost, Path,
    TypeAcquisition, WatchOptions,
};

pub(super) fn create_compiler_diagnostic_only_if_json<TSourceFile: Borrow<Node>>(
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
            source_file
                .as_ref()
                .map(|source_file| source_file.borrow())
                .unwrap(),
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
        type_acquisition: Some(Rc::new(type_acquisition)),
        extended_config_path,
    }
}

pub(super) fn parse_own_config_of_json_source_file<THost: ParseConfigHost>(
    source_file: &Node, /*TsConfigSourceFile*/
    host: &THost,
    base_path: &str,
    config_file_name: Option<&str>,
    errors: &mut Vec<Rc<Diagnostic>>,
) -> ParsedTsconfig {
    let mut options = get_default_compiler_options(config_file_name);
    let type_acquisition: RefCell<Option<TypeAcquisition>> = RefCell::new(None);
    let typing_options_type_acquisition: RefCell<Option<TypeAcquisition>> = RefCell::new(None);
    let watch_options: RefCell<Option<WatchOptions>> = RefCell::new(None);
    let extended_config_path: RefCell<Option<String>> = RefCell::new(None);
    let root_compiler_options: RefCell<Option<Vec<Rc<Node /*PropertyName*/>>>> = RefCell::new(None);

    let base_path_string = base_path.to_owned();
    let errors = RefCell::new(errors);
    let options_iterator = ParseOwnConfigOfJsonSourceFileOptionsIterator::new(
        &mut options,
        &base_path_string,
        &watch_options,
        config_file_name,
        &type_acquisition,
        &typing_options_type_acquisition,
        &extended_config_path,
        host,
        &errors,
        source_file,
        &root_compiler_options,
    );
    let json = convert_config_file_to_object(source_file, &errors, true, Some(&options_iterator));

    let mut type_acquisition = type_acquisition.borrow_mut();
    let typing_options_type_acquisition = typing_options_type_acquisition.borrow();
    if type_acquisition.is_none() {
        if let Some(typing_options_type_acquisition) = typing_options_type_acquisition.as_ref() {
            *type_acquisition = Some(
                if let Some(typing_options_type_acquisition_enable_auto_discovery) =
                    typing_options_type_acquisition.enable_auto_discovery
                {
                    TypeAcquisition {
                        enable_auto_discovery: None,
                        enable: Some(typing_options_type_acquisition_enable_auto_discovery),
                        include: typing_options_type_acquisition.include.clone(),
                        exclude: typing_options_type_acquisition.exclude.clone(),
                        disable_filename_based_type_acquisition: None,
                    }
                } else {
                    typing_options_type_acquisition.clone()
                },
            );
        } else {
            *type_acquisition = Some(get_default_type_acquisition(config_file_name));
        }
    }

    if let Some(root_compiler_options) = root_compiler_options.borrow().as_ref() {
        if let Some(json) = json.as_ref() {
            if !matches!(json, serde_json::Value::Object(map) if map.contains_key("compilerOptions"))
            {
                errors.borrow_mut().push(
                    Rc::new(
                        create_diagnostic_for_node_in_source_file(
                            source_file,
                            &root_compiler_options[0],
                            &Diagnostics::_0_should_be_set_inside_the_compilerOptions_object_of_the_config_json_file,
                            Some(vec![(&*get_text_of_property_name(&root_compiler_options[0])).to_owned()])
                        )
                        .into()
                    )
                );
            }
        }
    }

    let watch_options = watch_options.borrow();
    let extended_config_path = extended_config_path.borrow();
    ParsedTsconfig {
        raw: json,
        options: Some(Rc::new(options)),
        watch_options: watch_options
            .as_ref()
            .map(|watch_options| Rc::new(watch_options.clone())),
        type_acquisition: type_acquisition
            .as_ref()
            .map(|type_acquisition| Rc::new(type_acquisition.clone())),
        extended_config_path: extended_config_path.clone(),
    }
}

struct ParseOwnConfigOfJsonSourceFileOptionsIterator<'a, THost: ParseConfigHost> {
    options: RefCell<&'a mut CompilerOptions>,
    base_path: &'a str,
    watch_options: &'a RefCell<Option<WatchOptions>>,
    config_file_name: Option<&'a str>,
    type_acquisition: &'a RefCell<Option<TypeAcquisition>>,
    typing_options_type_acquisition: &'a RefCell<Option<TypeAcquisition>>,
    extended_config_path: &'a RefCell<Option<String>>,
    host: &'a THost,
    errors: &'a RefCell<&'a mut Vec<Rc<Diagnostic>>>,
    source_file: &'a Node,
    root_compiler_options: &'a RefCell<Option<Vec<Rc<Node>>>>,
}

impl<'a, THost: ParseConfigHost> ParseOwnConfigOfJsonSourceFileOptionsIterator<'a, THost> {
    pub fn new(
        options: &'a mut CompilerOptions,
        base_path: &'a str,
        watch_options: &'a RefCell<Option<WatchOptions>>,
        config_file_name: Option<&'a str>,
        type_acquisition: &'a RefCell<Option<TypeAcquisition>>,
        typing_options_type_acquisition: &'a RefCell<Option<TypeAcquisition>>,
        extended_config_path: &'a RefCell<Option<String>>,
        host: &'a THost,
        errors: &'a RefCell<&'a mut Vec<Rc<Diagnostic>>>,
        source_file: &'a Node,
        root_compiler_options: &'a RefCell<Option<Vec<Rc<Node>>>>,
    ) -> Self {
        Self {
            options: RefCell::new(options),
            base_path,
            watch_options,
            config_file_name,
            type_acquisition,
            typing_options_type_acquisition,
            extended_config_path,
            host,
            errors,
            source_file,
            root_compiler_options,
        }
    }
}

impl<'a, THost: ParseConfigHost> JsonConversionNotifier
    for ParseOwnConfigOfJsonSourceFileOptionsIterator<'a, THost>
{
    fn on_set_valid_option_key_value_in_parent(
        &self,
        parent_option: &str,
        option: &CommandLineOption,
        value: Option<&serde_json::Value>,
    ) {
        match parent_option {
            "compilerOptions" => {
                self.options
                    .borrow_mut()
                    .set_value_from_command_line_option(
                        &option,
                        normalize_option_value(option, self.base_path, value),
                    );
            }
            "watchOptions" => {
                let mut watch_options = self.watch_options.borrow_mut();
                if watch_options.is_none() {
                    *watch_options = Some(Default::default());
                }
                set_watch_option_value(
                    watch_options.as_mut().unwrap(),
                    &option,
                    normalize_option_value(option, self.base_path, value),
                );
            }
            "typeAcquisition" => {
                let mut type_acquisition = self.type_acquisition.borrow_mut();
                if type_acquisition.is_none() {
                    *type_acquisition = Some(get_default_type_acquisition(self.config_file_name));
                }
                set_type_acquisition_value(
                    type_acquisition.as_mut().unwrap(),
                    &option,
                    normalize_option_value(option, self.base_path, value),
                );
            }
            "typingOptions" => {
                let mut typing_options_type_acquisition =
                    self.typing_options_type_acquisition.borrow_mut();
                if typing_options_type_acquisition.is_none() {
                    *typing_options_type_acquisition =
                        Some(get_default_type_acquisition(self.config_file_name));
                }
                set_type_acquisition_value(
                    typing_options_type_acquisition.as_mut().unwrap(),
                    &option,
                    normalize_option_value(option, self.base_path, value),
                );
            }
            _ => Debug_.fail(Some("Unknown option")),
        }
    }

    fn on_set_valid_option_key_value_in_root(
        &self,
        key: &str,
        key_node: &Node, /*PropertyName*/
        value: Option<&serde_json::Value>,
        value_node: &Node, /*Expression*/
    ) {
        match key {
            "extends" => {
                let new_base = if let Some(config_file_name) = self.config_file_name {
                    directory_of_combined_path(config_file_name, self.base_path)
                } else {
                    self.base_path.to_owned()
                };
                *self.extended_config_path.borrow_mut() = get_extends_config_path(
                    match value {
                        Some(serde_json::Value::String(value)) => value,
                        _ => panic!("Expected string"),
                    },
                    self.host,
                    &new_base,
                    &mut self.errors.borrow_mut(),
                    |message, args| {
                        Rc::new(
                            create_diagnostic_for_node_in_source_file(
                                self.source_file,
                                value_node,
                                message,
                                args,
                            )
                            .into(),
                        )
                    },
                );
            }
            _ => (),
        }
    }

    fn on_set_unknown_option_key_value_in_root(
        &self,
        key: &str,
        key_node: &Node, /*PropertyName*/
        value: Option<&serde_json::Value>,
        value_node: &Node, /*Expression*/
    ) {
        if key == "excludes" {
            self.errors.borrow_mut().push(Rc::new(
                create_diagnostic_for_node_in_source_file(
                    self.source_file,
                    key_node,
                    &Diagnostics::Unknown_option_excludes_Did_you_mean_exclude,
                    None,
                )
                .into(),
            ));
        }
        command_options_without_build.with(|command_options_without_build_| {
            if find(command_options_without_build_, |opt, _| opt.name() == key).is_some() {
                let mut root_compiler_options = self.root_compiler_options.borrow_mut();
                if root_compiler_options.is_none() {
                    *root_compiler_options = Some(vec![]);
                }
                append(
                    root_compiler_options.as_mut().unwrap(),
                    Some(key_node.node_wrapper()),
                );
            }
        });
    }
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
