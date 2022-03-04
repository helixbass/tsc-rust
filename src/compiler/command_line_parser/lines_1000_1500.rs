use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    command_options_without_build, common_options_with_build, convert_json_option_of_custom_type,
    get_build_options_name_map, get_option_declaration_from_name, parse_option_value,
    parse_response_file, validate_json_option_value, watch_options_did_you_mean_diagnostics,
};
use crate::{
    create_compiler_diagnostic, for_each, get_spelling_suggestion, starts_with, trim_string,
    AlternateModeDiagnostics, CharacterCodes, CommandLineOption, CommandLineOptionBase,
    CommandLineOptionInterface, CommandLineOptionOfBooleanType, CommandLineOptionOfListType,
    CommandLineOptionOfStringType, CommandLineOptionType, CompilerOptions, CompilerOptionsBuilder,
    CompilerOptionsValue, Diagnostic, DiagnosticMessage, Diagnostics, DidYouMeanOptionsDiagnostics,
    ModuleKind, ParsedCommandLine, ScriptTarget, StringOrDiagnosticMessage, WatchOptions,
};

thread_local! {
    pub(crate) static option_declarations: Vec<Rc<CommandLineOption>> =
        common_options_with_build.with(|common_options_with_build_| {
            command_options_without_build.with(|command_options_without_build_| {
                common_options_with_build_
                    .iter()
                    .chain(command_options_without_build_.iter())
                    .map(Clone::clone)
                    .collect()
            })
        });
}

thread_local! {
    pub(crate) static semantic_diagnostics_option_declarations: Vec<Rc<CommandLineOption>> =
        option_declarations.with(|option_declarations_| {
            option_declarations_
                .iter()
                .filter(|option| option.affects_semantic_diagnostics())
                .map(Clone::clone)
                .collect()
        });
}

thread_local! {
    pub(crate) static affects_emit_option_declarations: Vec<Rc<CommandLineOption>> =
        option_declarations.with(|option_declarations_| {
            option_declarations_
                .iter()
                .filter(|option| option.affects_emit())
                .map(Clone::clone)
                .collect()
        });
}

thread_local! {
    pub(crate) static module_resolution_option_declarations: Vec<Rc<CommandLineOption>> =
        option_declarations.with(|option_declarations_| {
            option_declarations_
                .iter()
                .filter(|option| option.affects_module_resolution())
                .map(Clone::clone)
                .collect()
        });
}

thread_local! {
    pub(crate) static source_file_affecting_compiler_options: Vec<Rc<CommandLineOption>> =
        option_declarations.with(|option_declarations_| {
            option_declarations_
                .iter()
                .filter(|option| {
                    option.affects_source_file()
                        || option.affects_module_resolution()
                        || option.affects_bind_diagnostics()
                })
                .map(Clone::clone)
                .collect()
        });
}

thread_local! {
    pub(crate) static options_affecting_program_structure: Vec<Rc<CommandLineOption>> =
        option_declarations.with(|option_declarations_| {
            option_declarations_
                .iter()
                .filter(|option| option.affects_program_structure())
                .map(Clone::clone)
                .collect()
        });
}

thread_local! {
    pub(crate) static transpile_option_value_compiler_options: Vec<Rc<CommandLineOption>> =
        option_declarations.with(|option_declarations_| {
            option_declarations_
                .iter()
                .filter(|option| option.transpile_option_value().is_some())
                .map(Clone::clone)
                .collect()
        });
}

thread_local! {
    pub(crate) static options_for_build: Vec<Rc<CommandLineOption>> = vec![
        CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
            _command_line_option_wrapper: RefCell::new(None),
            name: "verbose".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: Some("v".to_owned()),
            description: Some(Diagnostics::Enable_verbose_logging),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Command_line_Options),
            strict_flag: None,
            affects_source_file: None,
            affects_module_resolution: None,
            affects_bind_diagnostics: None,
            affects_semantic_diagnostics: None,
            affects_emit: None,
            affects_program_structure: None,
            transpile_option_value: None,
        })
        .into(),
        CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
            _command_line_option_wrapper: RefCell::new(None),
            name: "dry".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: Some("d".to_owned()),
            description: Some(Diagnostics::Show_what_would_be_built_or_deleted_if_specified_with_clean),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Command_line_Options),
            strict_flag: None,
            affects_source_file: None,
            affects_module_resolution: None,
            affects_bind_diagnostics: None,
            affects_semantic_diagnostics: None,
            affects_emit: None,
            affects_program_structure: None,
            transpile_option_value: None,
        })
        .into(),
        CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
            _command_line_option_wrapper: RefCell::new(None),
            name: "force".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: Some("f".to_owned()),
            description: Some(Diagnostics::Build_all_projects_including_those_that_appear_to_be_up_to_date),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Command_line_Options),
            strict_flag: None,
            affects_source_file: None,
            affects_module_resolution: None,
            affects_bind_diagnostics: None,
            affects_semantic_diagnostics: None,
            affects_emit: None,
            affects_program_structure: None,
            transpile_option_value: None,
        })
        .into(),
        CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
            _command_line_option_wrapper: RefCell::new(None),
            name: "clean".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: Some(Diagnostics::Delete_the_outputs_of_all_projects),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Command_line_Options),
            strict_flag: None,
            affects_source_file: None,
            affects_module_resolution: None,
            affects_bind_diagnostics: None,
            affects_semantic_diagnostics: None,
            affects_emit: None,
            affects_program_structure: None,
            transpile_option_value: None,
        })
        .into(),
    ];
}

thread_local! {
    pub(crate) static build_opts: Vec<Rc<CommandLineOption>> =
        common_options_with_build.with(|common_options_with_build_| {
            options_for_build.with(|options_for_build_| {
                common_options_with_build_
                    .iter()
                    .chain(options_for_build_.iter())
                    .map(Clone::clone)
                    .collect()
            })
        });
}

thread_local! {
    pub(crate) static type_acquisition_declarations: Vec<Rc<CommandLineOption>> = vec![
        CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
            _command_line_option_wrapper: RefCell::new(None),
            name: "enableAutoDiscovery".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: None,
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: None,
            strict_flag: None,
            affects_source_file: None,
            affects_module_resolution: None,
            affects_bind_diagnostics: None,
            affects_semantic_diagnostics: None,
            affects_emit: None,
            affects_program_structure: None,
            transpile_option_value: None,
        })
        .into(),
        CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
            _command_line_option_wrapper: RefCell::new(None),
            name: "enable".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: None,
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: None,
            strict_flag: None,
            affects_source_file: None,
            affects_module_resolution: None,
            affects_bind_diagnostics: None,
            affects_semantic_diagnostics: None,
            affects_emit: None,
            affects_program_structure: None,
            transpile_option_value: None,
        })
        .into(),
        CommandLineOptionOfListType::new(CommandLineOptionBase {
            _command_line_option_wrapper: RefCell::new(None),
            name: "include".to_string(),
            type_: CommandLineOptionType::List,
            is_file_path: None,
            short_name: None,
            description: None,
            default_value_description: None,
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: None,
            strict_flag: None,
            affects_source_file: None,
            affects_module_resolution: None,
            affects_bind_diagnostics: None,
            affects_semantic_diagnostics: None,
            affects_emit: None,
            affects_program_structure: None,
            transpile_option_value: None,
        },
            CommandLineOptionOfStringType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "include".to_string(),
                type_: CommandLineOptionType::String,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: None,
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
        )
        .into(),
        CommandLineOptionOfListType::new(CommandLineOptionBase {
            _command_line_option_wrapper: RefCell::new(None),
            name: "exclude".to_string(),
            type_: CommandLineOptionType::List,
            is_file_path: None,
            short_name: None,
            description: None,
            default_value_description: None,
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: None,
            strict_flag: None,
            affects_source_file: None,
            affects_module_resolution: None,
            affects_bind_diagnostics: None,
            affects_semantic_diagnostics: None,
            affects_emit: None,
            affects_program_structure: None,
            transpile_option_value: None,
        },
            CommandLineOptionOfStringType::new(CommandLineOptionBase {
                _command_line_option_wrapper: RefCell::new(None),
                name: "exclude".to_string(),
                type_: CommandLineOptionType::String,
                is_file_path: None,
                short_name: None,
                description: None,
                default_value_description: None,
                param_type: None,
                is_tsconfig_only: None,
                is_command_line_only: None,
                show_in_simplified_help_view: None,
                category: None,
                strict_flag: None,
                affects_source_file: None,
                affects_module_resolution: None,
                affects_bind_diagnostics: None,
                affects_semantic_diagnostics: None,
                affects_emit: None,
                affects_program_structure: None,
                transpile_option_value: None,
            })
            .into(),
        )
        .into(),
        CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
            _command_line_option_wrapper: RefCell::new(None),
            name: "disableFilenameBasedTypeAcquisition".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: None,
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: None,
            strict_flag: None,
            affects_source_file: None,
            affects_module_resolution: None,
            affects_bind_diagnostics: None,
            affects_semantic_diagnostics: None,
            affects_emit: None,
            affects_program_structure: None,
            transpile_option_value: None,
        })
        .into(),
    ];
}

pub struct OptionsNameMap {
    pub options_name_map: HashMap<String, Rc<CommandLineOption>>,
    pub short_option_names: HashMap<String, String>,
}

pub(crate) fn create_option_name_map(
    option_declarations_: &[Rc<CommandLineOption>],
) -> OptionsNameMap {
    let mut options_name_map = HashMap::new();
    let mut short_option_names = HashMap::new();
    for_each(option_declarations_, |option, _| {
        options_name_map.insert(option.name().to_lowercase(), option.clone());
        if let Some(option_short_name) = option.maybe_short_name() {
            short_option_names.insert(option_short_name.to_owned(), option.name().to_owned());
        }
        Option::<()>::None
    });

    OptionsNameMap {
        options_name_map,
        short_option_names,
    }
}

thread_local! {
    pub(super) static options_name_map_cache: RefCell<Option<Rc<OptionsNameMap>>> = RefCell::new(None);
}

pub(crate) fn get_options_name_map() -> Rc<OptionsNameMap> {
    options_name_map_cache.with(|options_name_map_cache_| {
        let mut options_name_map_cache_ = options_name_map_cache_.borrow_mut();
        if options_name_map_cache_.is_none() {
            option_declarations.with(|option_declarations_| {
                *options_name_map_cache_ =
                    Some(Rc::new(create_option_name_map(&option_declarations_)));
            });
        }
        options_name_map_cache_.as_ref().unwrap().clone()
    })
}

thread_local! {
    pub(super) static compiler_options_alternate_mode: Rc<AlternateModeDiagnostics> = Rc::new(AlternateModeDiagnostics {
        diagnostic: &Diagnostics::Compiler_option_0_may_only_be_used_with_build,
        get_options_name_map: get_build_options_name_map,
    });
}

thread_local! {
    pub(crate) static default_init_compiler_options: Rc<CompilerOptions> =
        Rc::new(CompilerOptionsBuilder::default()
            .module(Some(ModuleKind::CommonJS))
            .target(Some(ScriptTarget::ES2016))
            .strict(Some(true))
            .es_module_interop(Some(true))
            .force_consistent_casing_in_file_names(Some(true))
            .skip_lib_check(Some(true))
            .build().unwrap());
}

pub(crate) fn convert_enable_auto_discovery_to_enable(
    type_acquisition: Option<&serde_json::Value>,
) -> Option<serde_json::Value> {
    if let Some(type_acquisition) =
        type_acquisition.filter(|type_acquisition| match type_acquisition {
            serde_json::Value::Object(type_acquisition) => {
                type_acquisition.contains_key("enableAutoDiscovery")
                    && !type_acquisition.contains_key("enable")
            }
            _ => false,
        })
    {
        let type_acquisition = match type_acquisition {
            serde_json::Value::Object(type_acquisition) => type_acquisition,
            _ => panic!("Expected object"),
        };
        return Some(serde_json::Value::Object({
            let map = serde_json::Map::new();
            map.insert(
                "enable".to_owned(),
                type_acquisition.get("enableAutoDiscovery").unwrap().clone(),
            );
            map.insert(
                "include".to_owned(),
                type_acquisition
                    .get("include")
                    .map_or_else(|| serde_json::Value::Array(vec![]), Clone::clone),
            );
            map.insert(
                "exclude".to_owned(),
                type_acquisition
                    .get("exclude")
                    .map_or_else(|| serde_json::Value::Array(vec![]), Clone::clone),
            );
            map
        }));
    }
    type_acquisition.map(Clone::clone)
}

pub(crate) fn create_compiler_diagnostic_for_invalid_custom_type(
    opt: &CommandLineOption, /*CommandLineOptionOfCustomType*/
) -> Rc<Diagnostic> {
    create_diagnostic_for_invalid_custom_type(opt, |message, args| {
        Rc::new(create_compiler_diagnostic(message, args).into())
    })
}

pub(super) fn create_diagnostic_for_invalid_custom_type<
    TCreateDiagnostic: FnMut(&DiagnosticMessage, Option<Vec<String>>) -> Rc<Diagnostic>,
>(
    opt: &CommandLineOption, /*CommandLineOptionOfCustomType*/
    mut create_diagnostic: TCreateDiagnostic,
) -> Rc<Diagnostic> {
    let names_of_type = opt
        .type_()
        .as_map()
        .keys()
        .map(|key| format!("'{}'", key))
        .collect::<Vec<_>>()
        .join(", ");
    create_diagnostic(
        &Diagnostics::Argument_for_0_option_must_be_Colon_1,
        Some(vec![format!("--{}", opt.name()), names_of_type]),
    )
}

pub(crate) fn parse_custom_type_option(
    opt: &CommandLineOption, /*CommandLineOptionOfCustomType*/
    value: Option<&str>,
    errors: &mut Vec<Rc<Diagnostic>>,
) -> CompilerOptionsValue {
    convert_json_option_of_custom_type(opt, Some(trim_string(value.unwrap_or(""))), errors)
}

pub(crate) fn parse_list_type_option(
    opt: &CommandLineOption, /*CommandLineOptionOfListType*/
    value: Option<&str>,
    errors: &mut Vec<Rc<Diagnostic>>,
) -> Option<Vec<String>> {
    let value = value.unwrap_or("");
    if starts_with(value, "-") {
        return None;
    }
    if value == "" {
        return Some(vec![]);
    }
    let values = value.split(",");
    let opt_as_command_line_option_of_list_type = opt.as_command_line_option_of_list_type();
    match opt_as_command_line_option_of_list_type.element.type_() {
        // CommandLineOptionType::Number =>
        CommandLineOptionType::String => Some(
            values
                .filter_map(|v| {
                    match validate_json_option_value(
                        &opt_as_command_line_option_of_list_type.element,
                        Some(&serde_json::Value::String(v.to_owned())),
                        errors,
                    ) {
                        CompilerOptionsValue::String(v) => v,
                        _ => panic!("Expected string"),
                    }
                })
                .collect(),
        ),
        _ => panic!("Expected vec of strings"),
    }
}

// export interface OptionsBase {
//     [option: string]: CompilerOptionsValue | TsConfigSourceFile | undefined;
// }

pub(crate) trait ParseCommandLineWorkerDiagnostics: DidYouMeanOptionsDiagnostics {
    fn get_options_name_map(&self) -> Rc<OptionsNameMap>;
    fn option_type_mismatch_diagnostic(&self) -> &DiagnosticMessage;
    fn as_did_you_mean_options_diagnostics(&self) -> &dyn DidYouMeanOptionsDiagnostics;
}

pub(super) fn get_option_name(option: &CommandLineOption) -> &str {
    option.name()
}

pub(super) fn create_unknown_option_error<
    TCreateDiagnostics: FnMut(&DiagnosticMessage, Option<Vec<String>>) -> Rc<Diagnostic>,
>(
    unknown_option: &str,
    diagnostics: &dyn DidYouMeanOptionsDiagnostics,
    create_diagnostics: TCreateDiagnostics,
    unknown_option_error_text: Option<&str>,
) -> Rc<Diagnostic> {
    if let Some(diagnostics_alternate_mode) = diagnostics.maybe_alternate_mode() {
        if (diagnostics_alternate_mode.get_options_name_map)()
            .options_name_map
            .contains_key(&unknown_option.to_lowercase())
        {
            return create_diagnostics(
                diagnostics_alternate_mode.diagnostic,
                Some(vec![unknown_option.to_owned()]),
            );
        }
    }

    let possible_option = get_spelling_suggestion(
        unknown_option,
        &diagnostics.option_declarations(),
        |candidate| Some(get_option_name(candidate).to_owned()),
    );
    match possible_option {
        Some(possible_option) => create_diagnostics(
            diagnostics.unknown_did_you_mean_diagnostic(),
            Some(vec![
                unknown_option_error_text
                    .unwrap_or(unknown_option)
                    .to_owned(),
                possible_option.name().to_owned(),
            ]),
        ),
        None => create_diagnostics(
            diagnostics.unknown_option_diagnostic(),
            Some(vec![unknown_option_error_text
                .unwrap_or(unknown_option)
                .to_owned()]),
        ),
    }
}

pub(super) fn hash_map_to_compiler_options(
    options: &HashMap<String, CompilerOptionsValue>,
) -> CompilerOptions {
    unimplemented!()
}

pub(super) fn hash_map_to_watch_options(
    options: &HashMap<String, CompilerOptionsValue>,
) -> WatchOptions {
    unimplemented!()
}

pub(super) fn parse_command_line_worker<TReadFile: FnMut(&str) -> Option<String>>(
    diagnostics: &dyn ParseCommandLineWorkerDiagnostics,
    command_line: &[String],
    read_file: Option<TReadFile>,
) -> ParsedCommandLine {
    let mut options: HashMap<String, CompilerOptionsValue> = HashMap::new();
    let watch_options: RefCell<Option<HashMap<String, CompilerOptionsValue>>> = RefCell::new(None);
    let mut file_names: Vec<String> = vec![];
    let mut errors: Vec<Rc<Diagnostic>> = vec![];

    parse_strings(
        &mut file_names,
        diagnostics,
        &mut options,
        &mut errors,
        &watch_options,
        read_file,
        command_line,
    );

    ParsedCommandLine {
        options: Rc::new(hash_map_to_compiler_options(&options)),
        watch_options: watch_options
            .borrow()
            .as_ref()
            .map(|watch_options| Rc::new(hash_map_to_watch_options(watch_options))),
        file_names,
        errors,
        type_acquisition: None,
        project_references: None,
        raw: None,
        wildcard_directories: None,
        compile_on_save: None,
    }
}

pub(super) fn parse_strings<TReadFile: FnMut(&str) -> Option<String>>(
    file_names: &mut Vec<String>,
    diagnostics: &dyn ParseCommandLineWorkerDiagnostics,
    options: &mut HashMap<String, CompilerOptionsValue>,
    errors: &mut Vec<Rc<Diagnostic>>,
    watch_options: &RefCell<Option<HashMap<String, CompilerOptionsValue>>>,
    read_file: Option<TReadFile>,
    args: &[String],
) {
    let mut i = 0;
    while i < args.len() {
        let s = &args[i];
        i += 1;
        let s_as_chars: Vec<char> = s.chars().collect::<Vec<_>>();
        if matches!(s_as_chars.get(0).copied(), Some(CharacterCodes::at)) {
            parse_response_file(
                read_file,
                errors,
                file_names,
                diagnostics,
                options,
                watch_options,
                &s_as_chars[1..].iter().collect::<String>(),
            );
        } else if matches!(s_as_chars.get(0).copied(), Some(CharacterCodes::minus)) {
            let input_option_name =
                if matches!(s_as_chars.get(1).copied(), Some(CharacterCodes::minus)) {
                    &s_as_chars[2..]
                } else {
                    &s_as_chars[1..]
                }
                .iter()
                .collect::<String>();
            let opt = get_option_declaration_from_name(
                || diagnostics.get_options_name_map(),
                &input_option_name,
                Some(true),
            );
            if let Some(opt) = opt {
                i = parse_option_value(args, i, diagnostics, &opt, options, errors);
            } else {
                let watch_opt = get_option_declaration_from_name(
                    || watch_options_did_you_mean_diagnostics().get_options_name_map(),
                    &input_option_name,
                    Some(true),
                );
                if let Some(watch_opt) = watch_opt {
                    let mut watch_options = watch_options.borrow_mut();
                    if watch_options.is_none() {
                        *watch_options = Some(HashMap::new());
                    }
                    i = parse_option_value(
                        args,
                        i,
                        &*watch_options_did_you_mean_diagnostics(),
                        &watch_opt,
                        watch_options.as_mut().unwrap(),
                        errors,
                    );
                } else {
                    errors.push(create_unknown_option_error(
                        &input_option_name,
                        diagnostics.as_did_you_mean_options_diagnostics(),
                        |message, args| Rc::new(create_compiler_diagnostic(message, args).into()),
                        Some(s),
                    ));
                }
            }
        } else {
            file_names.push(s.clone());
        }
    }
}
