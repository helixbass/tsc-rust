use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{
    create_diagnostic_for_invalid_custom_type, create_unknown_option_error, get_options_name_map,
};
use crate::{
    create_diagnostic_for_node_in_source_file, create_get_canonical_file_name, get_directory_path,
    get_normalized_absolute_path, get_relative_path_from_file, get_text_of_property_name,
    is_computed_non_literal_name, is_string_double_quoted, is_string_literal,
    unescape_leading_underscores, CommandLineOption, CommandLineOptionInterface,
    CommandLineOptionMapTypeValue, CommandLineOptionType, CompilerOptions, CompilerOptionsValue,
    Diagnostic, Diagnostics, DidYouMeanOptionsDiagnostics, JsonConversionNotifier,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, Number, OptionsNameMap,
    ParsedCommandLine, ProjectReference, Push, SyntaxKind, ToHashMapOfCompilerOptionsValues,
};

pub(super) fn is_root_option_map(
    known_root_options: Option<&CommandLineOption>,
    known_options: Option<&HashMap<String, Rc<CommandLineOption>>>,
) -> bool {
    match known_root_options {
        None => false,
        Some(known_root_options) => match known_root_options {
            CommandLineOption::TsConfigOnlyOption(known_root_options) => {
                matches!(known_root_options.element_options.as_deref(), Some(element_options) if matches!(known_options, Some(known_options) if ptr::eq(element_options, known_options)))
            }
            _ => false,
        },
    }
}

pub(super) fn convert_object_literal_expression_to_json<
    TJsonConversionNotifier: JsonConversionNotifier,
>(
    return_value: bool,
    errors: &RefCell<&mut Push<Rc<Diagnostic>>>,
    source_file: &Node, /*JsonSourceFile*/
    json_conversion_notifier: Option<&TJsonConversionNotifier>,
    known_root_options: Option<&CommandLineOption>,
    node: &Node, /*ObjectLiteralExpression*/
    known_options: Option<&HashMap<String, Rc<CommandLineOption>>>,
    extra_key_diagnostics: Option<&dyn DidYouMeanOptionsDiagnostics>,
    parent_option: Option<&str>,
) -> Option<serde_json::Value> {
    let mut result = if return_value {
        Some(serde_json::Map::new())
    } else {
        None
    };
    for element in &node.as_object_literal_expression().properties {
        if element.kind() != SyntaxKind::PropertyAssignment {
            errors.borrow_mut().push(Rc::new(
                create_diagnostic_for_node_in_source_file(
                    source_file,
                    element,
                    &Diagnostics::Property_assignment_expected,
                    None,
                )
                .into(),
            ));
            continue;
        }

        let element_as_property_assignment = element.as_property_assignment();
        if let Some(element_as_property_assignment_question_token) =
            element_as_property_assignment.question_token.as_ref()
        {
            errors.borrow_mut().push(Rc::new(
                create_diagnostic_for_node_in_source_file(
                    source_file,
                    element_as_property_assignment_question_token,
                    &Diagnostics::The_0_modifier_can_only_be_used_in_TypeScript_files,
                    Some(vec!["?".to_owned()]),
                )
                .into(),
            ));
        }
        if !is_double_quoted_string(source_file, &element_as_property_assignment.name()) {
            errors.borrow_mut().push(Rc::new(
                create_diagnostic_for_node_in_source_file(
                    source_file,
                    &element_as_property_assignment.name(),
                    &Diagnostics::String_literal_with_double_quotes_expected,
                    None,
                )
                .into(),
            ));
        }

        let text_of_key = if is_computed_non_literal_name(&element_as_property_assignment.name()) {
            None
        } else {
            Some(get_text_of_property_name(
                &element_as_property_assignment.name(),
            ))
        };
        let key_text = text_of_key.map(|text_of_key| unescape_leading_underscores(&text_of_key));
        let option = match (key_text.as_ref(), known_options) {
            (Some(key_text), Some(known_options)) => {
                known_options.get(key_text).map(|option| &**option)
            }
            _ => None,
        };
        if let Some(key_text) = key_text.as_ref() {
            if let Some(extra_key_diagnostics) = extra_key_diagnostics {
                if option.is_none() {
                    if known_options.is_some() {
                        errors.borrow_mut().push(create_unknown_option_error(
                            key_text,
                            extra_key_diagnostics,
                            |message, args| {
                                Rc::new(
                                    create_diagnostic_for_node_in_source_file(
                                        source_file,
                                        &element_as_property_assignment.name(),
                                        message,
                                        args,
                                    )
                                    .into(),
                                )
                            },
                            None,
                        ))
                    } else {
                        errors.borrow_mut().push(Rc::new(
                            create_diagnostic_for_node_in_source_file(
                                source_file,
                                &element_as_property_assignment.name(),
                                extra_key_diagnostics.unknown_option_diagnostic(),
                                Some(vec![key_text.clone()]),
                            )
                            .into(),
                        ));
                    }
                }
            }
        }
        let value = convert_property_value_to_json(
            errors,
            source_file,
            json_conversion_notifier,
            return_value,
            known_root_options,
            &element_as_property_assignment.initializer,
            option,
        );
        if let Some(key_text) = key_text {
            if return_value {
                if let Some(value) = value.as_ref() {
                    result
                        .as_mut()
                        .unwrap()
                        .insert(key_text.clone(), value.clone());
                }
            }
            if let Some(json_conversion_notifier) = json_conversion_notifier {
                if parent_option.is_some() || is_root_option_map(known_root_options, known_options)
                {
                    let is_valid_option_value = is_compiler_options_value(option, value.as_ref());
                    if let Some(parent_option) = parent_option {
                        if is_valid_option_value {
                            json_conversion_notifier.on_set_valid_option_key_value_in_parent(
                                parent_option,
                                option.unwrap(),
                                value.as_ref(),
                            );
                        }
                    } else if is_root_option_map(known_root_options, known_options) {
                        if is_valid_option_value {
                            json_conversion_notifier.on_set_valid_option_key_value_in_root(
                                &key_text,
                                &element_as_property_assignment.name(),
                                value.as_ref(),
                                &element_as_property_assignment.initializer,
                            );
                        } else if option.is_none() {
                            json_conversion_notifier.on_set_unknown_option_key_value_in_root(
                                &key_text,
                                &element_as_property_assignment.name(),
                                value.as_ref(),
                                &element_as_property_assignment.initializer,
                            );
                        }
                    }
                }
            }
        }
    }
    result.map(|result| serde_json::Value::Object(result))
}

pub(super) fn convert_array_literal_expression_to_json<
    TJsonConversionNotifier: JsonConversionNotifier,
>(
    errors: &RefCell<&mut Push<Rc<Diagnostic>>>,
    source_file: &Node, /*JsonSourceFile*/
    json_conversion_notifier: Option<&TJsonConversionNotifier>,
    return_value: bool,
    known_root_options: Option<&CommandLineOption>,
    elements: &NodeArray, /*<Expression>*/
    element_option: Option<&CommandLineOption>,
) -> Option<serde_json::Value> {
    if !return_value {
        elements.iter().for_each(|element| {
            convert_property_value_to_json(
                errors,
                source_file,
                json_conversion_notifier,
                return_value,
                known_root_options,
                element,
                element_option,
            );
        });
        return None;
    }

    Some(serde_json::Value::Array(
        elements
            .iter()
            .filter_map(|element| {
                convert_property_value_to_json(
                    errors,
                    source_file,
                    json_conversion_notifier,
                    return_value,
                    known_root_options,
                    element,
                    element_option,
                )
            })
            .collect::<Vec<serde_json::Value>>(),
    ))
}

pub(super) fn convert_property_value_to_json<TJsonConversionNotifier: JsonConversionNotifier>(
    errors: &RefCell<&mut Push<Rc<Diagnostic>>>,
    source_file: &Node, /*JsonSourceFile*/
    json_conversion_notifier: Option<&TJsonConversionNotifier>,
    return_value: bool,
    known_root_options: Option<&CommandLineOption>,
    value_expression: &Node, /*Expression*/
    option: Option<&CommandLineOption>,
) -> Option<serde_json::Value> {
    let mut invalid_reported: Option<bool> = None;
    match value_expression.kind() {
        SyntaxKind::TrueKeyword => {
            report_invalid_option_value(
                errors,
                &mut invalid_reported,
                source_file,
                value_expression,
                option,
                Some(
                    matches!(option, Some(option) if !matches!(option, CommandLineOption::CommandLineOptionOfBooleanType(_))),
                ),
            );
            return validate_value(
                invalid_reported,
                option,
                errors,
                source_file,
                value_expression,
                Some(serde_json::Value::Bool(true)),
            );
        }

        SyntaxKind::FalseKeyword => {
            report_invalid_option_value(
                errors,
                &mut invalid_reported,
                source_file,
                value_expression,
                option,
                Some(
                    matches!(option, Some(option) if !matches!(option, CommandLineOption::CommandLineOptionOfBooleanType(_))),
                ),
            );
            return validate_value(
                invalid_reported,
                option,
                errors,
                source_file,
                value_expression,
                Some(serde_json::Value::Bool(false)),
            );
        }

        SyntaxKind::NullKeyword => {
            report_invalid_option_value(
                errors,
                &mut invalid_reported,
                source_file,
                value_expression,
                option,
                Some(matches!(option, Some(option) if option.name() == "extends")),
            );
            return validate_value(
                invalid_reported,
                option,
                errors,
                source_file,
                value_expression,
                Some(serde_json::Value::Null),
            );
        }

        SyntaxKind::StringLiteral => {
            if !is_double_quoted_string(source_file, value_expression) {
                errors.borrow_mut().push(Rc::new(
                    create_diagnostic_for_node_in_source_file(
                        source_file,
                        value_expression,
                        &Diagnostics::String_literal_with_double_quotes_expected,
                        None,
                    )
                    .into(),
                ));
            }
            report_invalid_option_value(
                errors,
                &mut invalid_reported,
                source_file,
                value_expression,
                option,
                Some(
                    matches!(option, Some(option) if !matches!(option, CommandLineOption::CommandLineOptionOfStringType(_))),
                ),
            );
            let text = value_expression.as_literal_like_node().text();
            if let Some(custom_option) = option.as_ref() {
                if let CommandLineOptionType::Map(custom_option_type) = custom_option.type_() {
                    if !custom_option_type.contains_key(&&*text.to_lowercase()) {
                        errors
                            .borrow_mut()
                            .push(create_diagnostic_for_invalid_custom_type(
                                custom_option,
                                |message, args| {
                                    Rc::new(
                                        create_diagnostic_for_node_in_source_file(
                                            source_file,
                                            value_expression,
                                            message,
                                            args,
                                        )
                                        .into(),
                                    )
                                },
                            ));
                        invalid_reported = Some(true);
                    }
                }
            }
            return validate_value(
                invalid_reported,
                option,
                errors,
                source_file,
                value_expression,
                Some(serde_json::Value::String(text.clone())),
            );
        }

        SyntaxKind::NumericLiteral => {
            report_invalid_option_value(
                errors,
                &mut invalid_reported,
                source_file,
                value_expression,
                option,
                Some(
                    matches!(option, Some(option) if !matches!(option, CommandLineOption::CommandLineOptionOfNumberType(_))),
                ),
            );
            return validate_value(
                invalid_reported,
                option,
                errors,
                source_file,
                value_expression,
                Some(serde_json::Value::Number(
                    (&**value_expression.as_literal_like_node().text())
                        .parse()
                        .unwrap(),
                )),
            );
        }

        SyntaxKind::PrefixUnaryExpression => {
            let value_expression_as_prefix_unary_expression =
                value_expression.as_prefix_unary_expression();
            if value_expression_as_prefix_unary_expression.operator != SyntaxKind::MinusToken
                || value_expression_as_prefix_unary_expression.operand.kind()
                    != SyntaxKind::NumericLiteral
            {
            } else {
                report_invalid_option_value(
                    errors,
                    &mut invalid_reported,
                    source_file,
                    value_expression,
                    option,
                    Some(
                        matches!(option, Some(option) if !matches!(option, CommandLineOption::CommandLineOptionOfNumberType(_))),
                    ),
                );
                return validate_value(
                    invalid_reported,
                    option,
                    errors,
                    source_file,
                    value_expression,
                    Some(serde_json::Value::Number(
                        serde_json::Number::from_f64(
                            -Into::<Number>::into(
                                &**value_expression_as_prefix_unary_expression
                                    .operand
                                    .as_literal_like_node()
                                    .text(),
                            )
                            .value(),
                        )
                        .unwrap(),
                    )),
                );
            }
        }

        SyntaxKind::ObjectLiteralExpression => {
            report_invalid_option_value(
                errors,
                &mut invalid_reported,
                source_file,
                value_expression,
                option,
                Some(
                    matches!(option, Some(option) if !matches!(option, CommandLineOption::TsConfigOnlyOption(_))),
                ),
            );
            let object_literal_expression = value_expression;

            if let Some(option) = option {
                let option_as_ts_config_only_option = option.as_ts_config_only_option();
                let element_options = option_as_ts_config_only_option.element_options.as_deref();
                let extra_key_diagnostics = option_as_ts_config_only_option
                    .extra_key_diagnostics
                    .as_ref()
                    .map(|extra_key_diagnostics| {
                        extra_key_diagnostics.as_did_you_mean_options_diagnostics()
                    });
                let option_name = option.name();
                let converted = convert_object_literal_expression_to_json(
                    return_value,
                    errors,
                    source_file,
                    json_conversion_notifier,
                    known_root_options,
                    object_literal_expression,
                    element_options,
                    extra_key_diagnostics,
                    Some(option_name),
                );
                return validate_value(
                    invalid_reported,
                    Some(option),
                    errors,
                    source_file,
                    value_expression,
                    converted,
                );
            } else {
                let converted = convert_object_literal_expression_to_json(
                    return_value,
                    errors,
                    source_file,
                    json_conversion_notifier,
                    known_root_options,
                    object_literal_expression,
                    None,
                    None,
                    None,
                );
                return validate_value(
                    invalid_reported,
                    option,
                    errors,
                    source_file,
                    value_expression,
                    converted,
                );
            }
        }

        SyntaxKind::ArrayLiteralExpression => {
            report_invalid_option_value(
                errors,
                &mut invalid_reported,
                source_file,
                value_expression,
                option,
                Some(
                    matches!(option, Some(option) if !matches!(option, CommandLineOption::CommandLineOptionOfListType(_))),
                ),
            );
            return validate_value(
                invalid_reported,
                option,
                errors,
                source_file,
                value_expression,
                convert_array_literal_expression_to_json(
                    errors,
                    source_file,
                    json_conversion_notifier,
                    return_value,
                    known_root_options,
                    &value_expression.as_array_literal_expression().elements,
                    option.and_then(|option| match option {
                        CommandLineOption::CommandLineOptionOfListType(option) => {
                            Some(&*option.element)
                        }
                        _ => None,
                    }),
                ),
            );
        }

        _ => (),
    }

    if option.is_some() {
        report_invalid_option_value(
            errors,
            &mut invalid_reported,
            source_file,
            value_expression,
            option,
            Some(true),
        );
    } else {
        errors.borrow_mut().push(Rc::new(
            create_diagnostic_for_node_in_source_file(
                source_file,
                value_expression,
                &Diagnostics::Property_value_can_only_be_string_literal_numeric_literal_true_false_null_object_literal_or_array_literal,
                None
            )
            .into()
        ));
    }

    None
}

pub(super) fn validate_value(
    invalid_reported: Option<bool>,
    option: Option<&CommandLineOption>,
    errors: &RefCell<&mut Push<Rc<Diagnostic>>>,
    source_file: &Node,      /*JsonSourceFile*/
    value_expression: &Node, /*Expression*/
    value: Option<serde_json::Value>,
) -> Option<serde_json::Value> {
    if !matches!(invalid_reported, Some(true)) {
        let diagnostic = option
            .and_then(|option| option.maybe_extra_validation())
            .and_then(|extra_validation| extra_validation(value.as_ref()));
        if let Some((diagnostic_message, args)) = diagnostic {
            errors.borrow_mut().push(Rc::new(
                create_diagnostic_for_node_in_source_file(
                    source_file,
                    value_expression,
                    diagnostic_message,
                    args,
                )
                .into(),
            ));
            return None;
        }
    }
    value
}

pub(super) fn report_invalid_option_value(
    errors: &RefCell<&mut Push<Rc<Diagnostic>>>,
    invalid_reported: &mut Option<bool>,
    source_file: &Node,      /*JsonSourceFile*/
    value_expression: &Node, /*Expression*/
    option: Option<&CommandLineOption>,
    is_error: Option<bool>,
) {
    if matches!(is_error, Some(true)) {
        errors.borrow_mut().push(Rc::new(
            create_diagnostic_for_node_in_source_file(
                source_file,
                value_expression,
                &Diagnostics::Compiler_option_0_requires_a_value_of_type_1,
                Some(vec![
                    option.unwrap().name().to_owned(),
                    get_compiler_option_value_type_string(option.unwrap()).to_owned(),
                ]),
            )
            .into(),
        ));
        *invalid_reported = Some(true);
    }
}

pub(super) fn is_double_quoted_string(
    source_file: &Node, /*JsonSourceFile*/
    node: &Node,
) -> bool {
    is_string_literal(node) && is_string_double_quoted(node, source_file)
}

pub(super) fn get_compiler_option_value_type_string(option: &CommandLineOption) -> &'static str {
    match option {
        CommandLineOption::CommandLineOptionOfListType(_) => "Array",
        CommandLineOption::CommandLineOptionOfStringType(_) => "string",
        CommandLineOption::CommandLineOptionOfNumberType(_) => "number",
        CommandLineOption::CommandLineOptionOfBooleanType(_) => "boolean",
        CommandLineOption::TsConfigOnlyOption(_) => "object",
        CommandLineOption::CommandLineOptionOfCustomType(_) => "string",
    }
}

pub(super) fn is_compiler_options_value(
    option: Option<&CommandLineOption>,
    value: Option<&serde_json::Value>,
) -> bool {
    if let Some(option) = option {
        if match value {
            None => true,
            Some(value) => match value {
                serde_json::Value::Null => true,
                _ => false,
            },
        } {
            return true;
        }
        let value = value.unwrap();
        if matches!(option.type_(), CommandLineOptionType::List) {
            return matches!(value, serde_json::Value::Array(_));
        }
        return match option.type_() {
            CommandLineOptionType::String => matches!(value, serde_json::Value::String(_)),
            CommandLineOptionType::Number => matches!(value, serde_json::Value::Number(_)),
            CommandLineOptionType::Boolean => matches!(value, serde_json::Value::Bool(_)),
            CommandLineOptionType::Object => matches!(value, serde_json::Value::Object(_)),
            CommandLineOptionType::Map(_) => matches!(value, serde_json::Value::String(_)),
            CommandLineOptionType::List => panic!("Already handled list"),
        };
    }
    false
}

#[derive(Serialize)]
pub(crate) struct TSConfig {
    pub compiler_options: Rc<CompilerOptions>,
    pub compile_on_save: Option<bool>,
    pub exclude: Option<Vec<String>>,
    pub files: Option<Vec<String>>,
    pub include: Option<Vec<String>>,
    pub references: Option<Vec<Rc<ProjectReference>>>,
}

pub trait ConvertToTSConfigHost {
    fn get_current_directory(&self) -> String;
    fn use_case_sensitive_file_names(&self) -> bool;
}

pub(crate) fn convert_to_tsconfig(
    config_parse_result: &ParsedCommandLine,
    config_file_name: &str,
    host: &dyn ConvertToTSConfigHost,
) -> TSConfig {
    let get_canonical_file_name =
        create_get_canonical_file_name(host.use_case_sensitive_file_names());
    let maybe_config_file_specs =
        config_parse_result
            .options
            .config_file
            .as_ref()
            .and_then(|config_file| {
                config_file
                    .as_source_file()
                    .maybe_config_file_specs()
                    .clone()
            });
    let matches_specs_callback: Option<MatchesSpecs> = maybe_config_file_specs
        .as_ref()
        .and_then(|config_file_specs| config_file_specs.validated_include_specs.as_ref())
        .map(|validated_include_specs| {
            matches_specs(
                config_file_name,
                Some(validated_include_specs),
                maybe_config_file_specs
                    .as_ref()
                    .and_then(|config_file_specs| {
                        config_file_specs.validated_exclude_specs.as_deref()
                    }),
                host,
            )
        });
    let files = config_parse_result
        .file_names
        .iter()
        .filter(|file_name| match matches_specs_callback.as_ref() {
            Some(matches_specs_callback) => matches_specs_callback.call(file_name),
            None => true,
        })
        .map(|f| {
            get_relative_path_from_file(
                &get_normalized_absolute_path(
                    config_file_name,
                    Some(&host.get_current_directory()),
                ),
                &get_normalized_absolute_path(f, Some(&host.get_current_directory())),
                get_canonical_file_name,
            )
        })
        .collect::<Vec<String>>();
    let option_map = serialize_compiler_options(
        &config_parse_result.options,
        Some(SerializeOptionBaseObjectPathOptions {
            config_file_path: get_normalized_absolute_path(
                config_file_name,
                Some(&host.get_current_directory()),
            ),
            use_case_sensitive_file_names: host.use_case_sensitive_file_names(),
        }),
    );
    unimplemented!()
}

pub(super) fn matches_specs(
    path: &str,
    include_specs: Option<&[String]>,
    exclude_specs: Option<&[String]>,
    host: &dyn ConvertToTSConfigHost,
) -> MatchesSpecs {
    unimplemented!()
}

pub(super) struct MatchesSpecs {}

impl MatchesSpecs {
    pub fn call(&self, file_name: &str) -> bool {
        unimplemented!()
    }
}

pub(super) fn get_custom_type_map_of_command_line_option(
    option_definition: &CommandLineOption,
) -> Option<&HashMap<&'static str, CommandLineOptionMapTypeValue>> {
    match option_definition.type_() {
        CommandLineOptionType::String
        | CommandLineOptionType::Number
        | CommandLineOptionType::Boolean
        | CommandLineOptionType::Object => None,
        CommandLineOptionType::List => get_custom_type_map_of_command_line_option(
            &option_definition
                .as_command_line_option_of_list_type()
                .element,
        ),
        CommandLineOptionType::Map(map) => Some(map),
    }
}

pub(super) fn get_name_of_compiler_option_value(
    value: &CompilerOptionsValue,
    custom_type_map: &HashMap<&'static str, CommandLineOptionMapTypeValue>,
) -> Option<&'static str> {
    for (key, map_value) in custom_type_map {
        if &map_value.as_compiler_options_value() == value {
            return Some(key);
        }
    }
    None
}

pub(super) struct SerializeOptionBaseObjectPathOptions {
    pub config_file_path: String,
    pub use_case_sensitive_file_names: bool,
}

pub(super) fn serialize_compiler_options(
    options: &CompilerOptions,
    path_options: Option<SerializeOptionBaseObjectPathOptions>,
) -> HashMap<&'static str, CompilerOptionsValue> {
    serialize_option_base_object(options, &get_options_name_map(), path_options)
}

pub(super) fn serialize_option_base_object<TOptions: ToHashMapOfCompilerOptionsValues>(
    options: &TOptions,
    options_name_map: &OptionsNameMap,
    path_options: Option<SerializeOptionBaseObjectPathOptions>,
) -> HashMap<&'static str, CompilerOptionsValue> {
    let options_name_map = &options_name_map.options_name_map;
    let full = options.to_hash_map_of_compiler_options_values();
    let get_canonical_file_name = path_options.as_ref().map(|path_options| {
        create_get_canonical_file_name(path_options.use_case_sensitive_file_names)
    });

    let mut result: HashMap<&'static str, CompilerOptionsValue> = HashMap::new();
    for (name, value) in full.into_iter() {
        if options_name_map.contains_key(name)
            && matches!(options_name_map.get(name).unwrap().maybe_category(), Some(category) if ptr::eq(category, &Diagnostics::Command_line_Options) || ptr::eq(category, &Diagnostics::Output_Formatting))
        {
            continue;
        }
        let option_definition = options_name_map.get(&name.to_lowercase());
        if let Some(option_definition) = option_definition {
            let custom_type_map = get_custom_type_map_of_command_line_option(option_definition);
            match custom_type_map {
                None => {
                    if path_options.is_some() && option_definition.is_file_path() {
                        let path_options = path_options.as_ref().unwrap();
                        result.insert(
                            name,
                            CompilerOptionsValue::String(Some(get_relative_path_from_file(
                                &path_options.config_file_path,
                                &get_normalized_absolute_path(
                                    &match value {
                                        CompilerOptionsValue::String(Some(value)) => value,
                                        _ => panic!("Expected string"),
                                    },
                                    Some(&get_directory_path(&path_options.config_file_path)),
                                ),
                                get_canonical_file_name.unwrap(),
                            ))),
                        );
                    } else {
                        result.insert(name, value);
                    }
                }
                Some(custom_type_map) => {
                    if matches!(option_definition.type_(), CommandLineOptionType::List) {
                        result.insert(
                            name,
                            CompilerOptionsValue::VecString(Some(
                                match value {
                                    CompilerOptionsValue::VecString(Some(value)) => value,
                                    _ => panic!("Expected vec of strings"),
                                }
                                .iter()
                                .map(|element| {
                                    get_name_of_compiler_option_value(
                                        &CompilerOptionsValue::String(Some(element.clone())),
                                        custom_type_map,
                                    )
                                    .unwrap()
                                    .to_owned()
                                })
                                .collect::<Vec<String>>(),
                            )),
                        );
                    } else {
                        if let Some(custom_name) =
                            get_name_of_compiler_option_value(&value, &custom_type_map)
                        {
                            result.insert(
                                name,
                                CompilerOptionsValue::String(Some(custom_name.to_owned())),
                            );
                        }
                    }
                }
            }
        }
    }

    result
}
