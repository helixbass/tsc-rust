use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{create_diagnostic_for_invalid_custom_type, create_unknown_option_error};
use crate::{
    create_diagnostic_for_node_in_source_file, create_get_canonical_file_name,
    get_normalized_absolute_path, get_relative_path_from_file, get_text_of_property_name,
    is_computed_non_literal_name, is_string_double_quoted, is_string_literal,
    unescape_leading_underscores, CommandLineOption, CommandLineOptionInterface,
    CommandLineOptionType, CompilerOptions, CompilerOptionsValue, Diagnostic, Diagnostics,
    DidYouMeanOptionsDiagnostics, JsonConversionNotifier, NamedDeclarationInterface, Node,
    NodeArray, NodeInterface, Number, ParsedCommandLine, ProjectReference, Push, SyntaxKind,
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

pub(super) struct SerializeOptionBaseObjectPathOptions {
    pub config_file_path: String,
    pub use_case_sensitive_file_names: bool,
}

pub(super) fn serialize_compiler_options(
    options: &CompilerOptions,
    path_options: Option<SerializeOptionBaseObjectPathOptions>,
) -> HashMap<&'static str, CompilerOptionsValue> {
    let mut result = HashMap::new();
    let get_canonical_file_name = path_options.as_ref().map(|path_options| {
        create_get_canonical_file_name(path_options.use_case_sensitive_file_names)
    });

    if options.all.is_some() {
        result.insert("all", options.all.into());
    }
    if options.allow_js.is_some() {
        result.insert("allowJs", options.allow_js.into());
    }
    if options.allow_non_ts_extensions.is_some() {
        result.insert(
            "allowNonTsExtensions",
            options.allow_non_ts_extensions.into(),
        );
    }
    if options.allow_synthetic_default_imports.is_some() {
        result.insert(
            "allowSyntheticDefaultImports",
            options.allow_synthetic_default_imports.into(),
        );
    }
    if options.allow_umd_global_access.is_some() {
        result.insert(
            "allowUmdGlobalAccess",
            options.allow_umd_global_access.into(),
        );
    }
    if options.allow_unreachable_code.is_some() {
        result.insert(
            "allowUnreachableCode",
            options.allow_unreachable_code.into(),
        );
    }
    if options.allow_unused_labels.is_some() {
        result.insert("allowUnusedLabels", options.allow_unused_labels.into());
    }
    if options.always_strict.is_some() {
        result.insert("alwaysStrict", options.always_strict.into());
    }
    if options.base_url.is_some() {
        result.insert("baseUrl", options.base_url.clone().into());
    }
    if options.build.is_some() {
        result.insert("build", options.build.into());
    }
    if options.charset.is_some() {
        result.insert("charset", options.charset.clone().into());
    }
    if options.check_js.is_some() {
        result.insert("checkJs", options.check_js.into());
    }
    if options.config_file_path.is_some() {
        result.insert("configFilePath", options.config_file_path.clone().into());
    }
    if options.config_file.is_some() {
        result.insert("configFile", options.config_file.clone().into());
    }
    if options.declaration.is_some() {
        result.insert("declaration", options.declaration.into());
    }
    if options.declaration_map.is_some() {
        result.insert("declarationMap", options.declaration_map.into());
    }
    if options.emit_declaration_only.is_some() {
        result.insert("emitDeclarationOnly", options.emit_declaration_only.into());
    }
    if options.declaration_dir.is_some() {
        result.insert("declarationDir", options.declaration_dir.clone().into());
    }
    if options.diagnostics.is_some() {
        result.insert("diagnostics", options.diagnostics.into());
    }
    if options.extended_diagnostics.is_some() {
        result.insert("extendedDiagnostics", options.extended_diagnostics.into());
    }
    if options.disable_size_limit.is_some() {
        result.insert("disableSizeLimit", options.disable_size_limit.into());
    }
    if options
        .disable_source_of_project_reference_redirect
        .is_some()
    {
        result.insert(
            "disableSourceOfProjectReferenceRedirect",
            options.disable_source_of_project_reference_redirect.into(),
        );
    }
    if options.disable_solution_searching.is_some() {
        result.insert(
            "disableSolutionSearching",
            options.disable_solution_searching.into(),
        );
    }
    if options.disable_referenced_project_load.is_some() {
        result.insert(
            "disableReferencedProjectLoad",
            options.disable_referenced_project_load.into(),
        );
    }
    if options.downlevel_iteration.is_some() {
        result.insert("downlevelIteration", options.downlevel_iteration.into());
    }
    if options.emit_bom.is_some() {
        result.insert("emitBom", options.emit_bom.into());
    }
    if options.emit_decorator_metadata.is_some() {
        result.insert(
            "emitDecoratorMetadata",
            options.emit_decorator_metadata.into(),
        );
    }
    if options.exact_optional_property_types.is_some() {
        result.insert(
            "exactOptionalPropertyTypes",
            options.exact_optional_property_types.into(),
        );
    }
    if options.experimental_decorators.is_some() {
        result.insert(
            "experimentalDecorators",
            options.experimental_decorators.into(),
        );
    }
    if options.force_consistent_casing_in_file_names.is_some() {
        result.insert(
            "forceConsistentCasingInFileNames",
            options.force_consistent_casing_in_file_names.into(),
        );
    }
    if options.generate_cpu_profile.is_some() {
        result.insert(
            "generateCpuProfile",
            options.generate_cpu_profile.clone().into(),
        );
    }
    if options.generate_trace.is_some() {
        result.insert("generateTrace", options.generate_trace.clone().into());
    }
    if options.help.is_some() {
        result.insert("help", options.help.into());
    }
    if options.import_helpers.is_some() {
        result.insert("importHelpers", options.import_helpers.into());
    }
    if options.imports_not_used_as_values.is_some() {
        result.insert(
            "importsNotUsedAsValues",
            options.imports_not_used_as_values.into(),
        );
    }
    if options.init.is_some() {
        result.insert("init", options.init.into());
    }
    if options.inline_source_map.is_some() {
        result.insert("inlineSourceMap", options.inline_source_map.into());
    }
    if options.inline_sources.is_some() {
        result.insert("inlineSources", options.inline_sources.into());
    }
    if options.isolated_modules.is_some() {
        result.insert("isolatedModules", options.isolated_modules.into());
    }
    if options.jsx.is_some() {
        result.insert("jsx", options.jsx.into());
    }
    if options.keyof_strings_only.is_some() {
        result.insert("keyofStringsOnly", options.keyof_strings_only.into());
    }
    if options.lib.is_some() {
        result.insert("lib", options.lib.clone().into());
    }
    if options.list_emitted_files.is_some() {
        result.insert("listEmittedFiles", options.list_emitted_files.into());
    }
    if options.list_files.is_some() {
        result.insert("listFiles", options.list_files.into());
    }
    if options.explain_files.is_some() {
        result.insert("explainFiles", options.explain_files.into());
    }
    if options.list_files_only.is_some() {
        result.insert("listFilesOnly", options.list_files_only.into());
    }
    if options.locale.is_some() {
        result.insert("locale", options.locale.clone().into());
    }
    if options.map_root.is_some() {
        result.insert("mapRoot", options.map_root.clone().into());
    }
    if options.max_node_module_js_depth.is_some() {
        result.insert(
            "maxNodeModuleJsDepth",
            options.max_node_module_js_depth.into(),
        );
    }
    if options.module.is_some() {
        result.insert("module", options.module.into());
    }
    if options.module_resolution.is_some() {
        result.insert("moduleResolution", options.module_resolution.into());
    }
    if options.new_line.is_some() {
        result.insert("newLine", options.new_line.into());
    }
    if options.no_emit.is_some() {
        result.insert("noEmit", options.no_emit.into());
    }
    if options.no_emit_for_js_files.is_some() {
        result.insert("noEmitForJsFiles", options.no_emit_for_js_files.into());
    }
    if options.no_emit_helpers.is_some() {
        result.insert("noEmitHelpers", options.no_emit_helpers.into());
    }
    if options.no_emit_on_error.is_some() {
        result.insert("noEmitOnError", options.no_emit_on_error.into());
    }
    if options.no_error_truncation.is_some() {
        result.insert("noErrorTruncation", options.no_error_truncation.into());
    }
    if options.no_fallthrough_cases_in_switch.is_some() {
        result.insert(
            "noFallthroughCasesInSwitch",
            options.no_fallthrough_cases_in_switch.into(),
        );
    }
    if options.no_implicit_any.is_some() {
        result.insert("noImplicitAny", options.no_implicit_any.into());
    }
    if options.no_implicit_returns.is_some() {
        result.insert("noImplicitReturns", options.no_implicit_returns.into());
    }
    if options.no_implicit_this.is_some() {
        result.insert("noImplicitThis", options.no_implicit_this.into());
    }
    if options.no_strict_generic_checks.is_some() {
        result.insert(
            "noStrictGenericChecks",
            options.no_strict_generic_checks.into(),
        );
    }
    if options.no_unused_locals.is_some() {
        result.insert("noUnusedLocals", options.no_unused_locals.into());
    }
    if options.no_unused_parameters.is_some() {
        result.insert("noUnusedParameters", options.no_unused_parameters.into());
    }
    if options.no_implicit_use_strict.is_some() {
        result.insert("noImplicitUseStrict", options.no_implicit_use_strict.into());
    }
    if options.no_property_access_from_index_signature.is_some() {
        result.insert(
            "noPropertyAccessFromIndexSignature",
            options.no_property_access_from_index_signature.into(),
        );
    }
    if options
        .assume_changes_only_affect_direct_dependencies
        .is_some()
    {
        result.insert(
            "assumeChangesOnlyAffectDirectDependencies",
            options
                .assume_changes_only_affect_direct_dependencies
                .into(),
        );
    }
    if options.no_lib.is_some() {
        result.insert("noLib", options.no_lib.into());
    }
    if options.no_resolve.is_some() {
        result.insert("noResolve", options.no_resolve.into());
    }
    if options.no_unchecked_indexed_access.is_some() {
        result.insert(
            "noUncheckedIndexedAccess",
            options.no_unchecked_indexed_access.into(),
        );
    }
    if options.out.is_some() {
        result.insert("out", options.out.clone().into());
    }
    if options.out_dir.is_some() {
        result.insert("outDir", options.out_dir.clone().into());
    }
    if options.out_file.is_some() {
        result.insert("outFile", options.out_file.clone().into());
    }
    if options.paths.is_some() {
        result.insert("paths", options.paths.clone().into());
    }
    if options.paths_base_path.is_some() {
        result.insert("pathsBasePath", options.paths_base_path.clone().into());
    }
    if options.plugins.is_some() {
        result.insert("plugins", options.plugins.clone().into());
    }
    if options.preserve_const_enums.is_some() {
        result.insert("preserveConstEnums", options.preserve_const_enums.into());
    }
    if options.no_implicit_override.is_some() {
        result.insert("noImplicitOverride", options.no_implicit_override.into());
    }
    if options.preserve_symlinks.is_some() {
        result.insert("preserveSymlinks", options.preserve_symlinks.into());
    }
    if options.preserve_value_imports.is_some() {
        result.insert(
            "preserveValueImports",
            options.preserve_value_imports.into(),
        );
    }
    if options.preserve_watch_output.is_some() {
        result.insert("preserveWatchOutput", options.preserve_watch_output.into());
    }
    if options.project.is_some() {
        result.insert("project", options.project.clone().into());
    }
    if options.pretty.is_some() {
        result.insert("pretty", options.pretty.into());
    }
    if options.react_namespace.is_some() {
        result.insert("reactNamespace", options.react_namespace.clone().into());
    }
    if options.jsx_factory.is_some() {
        result.insert("jsxFactory", options.jsx_factory.clone().into());
    }
    if options.jsx_fragment_factory.is_some() {
        result.insert(
            "jsxFragmentFactory",
            options.jsx_fragment_factory.clone().into(),
        );
    }
    if options.jsx_import_source.is_some() {
        result.insert("jsxImportSource", options.jsx_import_source.clone().into());
    }
    if options.composite.is_some() {
        result.insert("composite", options.composite.into());
    }
    if options.incremental.is_some() {
        result.insert("incremental", options.incremental.into());
    }
    if options.ts_build_info_file.is_some() {
        result.insert("tsBuildInfoFile", options.ts_build_info_file.clone().into());
    }
    if options.remove_comments.is_some() {
        result.insert("removeComments", options.remove_comments.into());
    }
    if options.root_dir.is_some() {
        result.insert("rootDir", options.root_dir.clone().into());
    }
    if options.root_dirs.is_some() {
        result.insert("rootDirs", options.root_dirs.clone().into());
    }
    if options.skip_lib_check.is_some() {
        result.insert("skipLibCheck", options.skip_lib_check.into());
    }
    if options.skip_default_lib_check.is_some() {
        result.insert("skipDefaultLibCheck", options.skip_default_lib_check.into());
    }
    if options.source_map.is_some() {
        result.insert("sourceMap", options.source_map.into());
    }
    if options.source_root.is_some() {
        result.insert("sourceRoot", options.source_root.clone().into());
    }
    if options.strict.is_some() {
        result.insert("strict", options.strict.into());
    }
    if options.strict_function_types.is_some() {
        result.insert("strictFunctionTypes", options.strict_function_types.into());
    }
    if options.strict_bind_call_apply.is_some() {
        result.insert("strictBindCallApply", options.strict_bind_call_apply.into());
    }
    if options.strict_null_checks.is_some() {
        result.insert("strictNullChecks", options.strict_null_checks.into());
    }
    if options.strict_property_initialization.is_some() {
        result.insert(
            "strictPropertyInitialization",
            options.strict_property_initialization.into(),
        );
    }
    if options.strip_internal.is_some() {
        result.insert("stripInternal", options.strip_internal.into());
    }
    if options.suppress_excess_property_errors.is_some() {
        result.insert(
            "suppressExcessPropertyErrors",
            options.suppress_excess_property_errors.into(),
        );
    }
    if options.suppress_implicit_any_index_errors.is_some() {
        result.insert(
            "suppressImplicitAnyIndexErrors",
            options.suppress_implicit_any_index_errors.into(),
        );
    }
    if options.suppress_output_path_check.is_some() {
        result.insert(
            "suppressOutputPathCheck",
            options.suppress_output_path_check.into(),
        );
    }
    if options.target.is_some() {
        result.insert("target", options.target.into());
    }
    if options.trace_resolution.is_some() {
        result.insert("traceResolution", options.trace_resolution.into());
    }
    if options.use_unknown_in_catch_variables.is_some() {
        result.insert(
            "useUnknownInCatchVariables",
            options.use_unknown_in_catch_variables.into(),
        );
    }
    if options.resolve_json_module.is_some() {
        result.insert("resolveJsonModule", options.resolve_json_module.into());
    }
    if options.types.is_some() {
        result.insert("types", options.types.clone().into());
    }
    if options.type_roots.is_some() {
        result.insert("typeRoots", options.type_roots.clone().into());
    }
    if options.version.is_some() {
        result.insert("version", options.version.into());
    }
    if options.watch.is_some() {
        result.insert("watch", options.watch.into());
    }
    if options.es_module_interop.is_some() {
        result.insert("esModuleInterop", options.es_module_interop.into());
    }
    if options.show_config.is_some() {
        result.insert("showConfig", options.show_config.into());
    }
    if options.use_define_for_class_fields.is_some() {
        result.insert(
            "useDefineForClassFields",
            options.use_define_for_class_fields.into(),
        );
    }
    result
}
