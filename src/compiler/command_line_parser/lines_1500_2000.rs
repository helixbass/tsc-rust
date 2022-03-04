use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    compiler_options_alternate_mode, convert_property_value_to_json, create_option_name_map,
    option_declarations, parse_command_line_worker, parse_custom_type_option,
    parse_list_type_option, parse_strings, validate_json_option_value_compiler_options_value,
    ParseCommandLineWorkerDiagnostics,
};
use crate::{
    build_opts, create_compiler_diagnostic, create_diagnostic_for_node_in_source_file, find,
    get_base_file_name, is_array_literal_expression, is_object_literal_expression,
    maybe_text_char_at_index, starts_with, text_char_at_index, text_substring,
    AlternateModeDiagnostics, BuildOptions, CharacterCodes, CommandLineOption,
    CommandLineOptionInterface, CommandLineOptionType, CompilerOptionsValue, Diagnostic,
    DiagnosticMessage, DiagnosticRelatedInformationInterface, Diagnostics,
    DidYouMeanOptionsDiagnostics, Node, NodeInterface, OptionsNameMap, ParsedCommandLine, Push,
    SyntaxKind, WatchOptions,
};
use local_macros::enum_unwrapped;

pub(super) fn parse_response_file<TReadFile: FnMut(&str) -> Option<String>>(
    file_name: &str,
    read_file: Option<TReadFile>,
    errors: &mut Vec<Rc<Diagnostic>>,
    file_names: &mut Vec<String>,
    diagnostics: &dyn ParseCommandLineWorkerDiagnostics,
    options: &mut HashMap<String, CompilerOptionsValue>,
    watch_options: &RefCell<Option<HashMap<String, CompilerOptionsValue>>>,
) {
    let text: StringOrRcDiagnostic = try_read_file(
        file_name,
        read_file.unwrap_or_else(|| |file_name| get_sys().read_file(file_name)),
    );
    match text {
        StringOrRcDiagnostic::RcDiagnostic(text) => {
            errors.push(text);
        }
        StringOrRcDiagnostic::String(text) => {
            let mut args: Vec<String> = vec![];
            let mut pos = 0;
            let text_as_chars = text.chars().collect::<Vec<_>>();
            loop {
                while pos < text_as_chars.len()
                    && text_char_at_index(text, pos) <= CharacterCodes::space
                {
                    pos += 1;
                }
                if pos >= text_as_chars.len() {
                    break;
                }
                let start = pos;
                if text_char_at_index(text, start) == CharacterCodes::double_quote {
                    pos += 1;
                    while pos < text_as_chars.len()
                        && text_char_at_index(text, pos) != CharacterCodes::double_quote
                    {
                        pos += 1;
                    }
                    if pos < text_as_chars.len() {
                        args.push(text_substring(text, start + 1, pos));
                        pos += 1;
                    } else {
                        errors.push(Rc::new(
                            create_compiler_diagnostic(
                                &Diagnostics::Unterminated_quoted_string_in_response_file_0,
                                Some(vec![file_name.to_owned()]),
                            )
                            .into(),
                        ));
                    }
                } else {
                    while matches!(maybe_text_char_at_index(text, pos), Some(ch) if ch > CharacterCodes::space)
                    {
                        pos += 1;
                    }
                    args.push(text_substring(text, start, pos));
                }
            }
            parse_strings(
                file_names,
                diagnostics,
                options,
                errors,
                watch_options,
                &args,
            );
        }
    }
}

pub(super) fn parse_option_value(
    args: &[String],
    mut i: usize,
    diagnostics: &dyn ParseCommandLineWorkerDiagnostics,
    opt: &CommandLineOption,
    options: &mut HashMap<String, CompilerOptionsValue>,
    errors: &mut Vec<Rc<Diagnostic>>,
) -> usize {
    if opt.is_tsconfig_only() {
        let opt_value = args.get(i).map(|opt_value| &**opt_value);
        if matches!(opt_value, Some("null")) {
            // options[opt.name] = undefined;
            i += 1;
        } else if matches!(opt.type_(), CommandLineOptionType::Boolean) {
            if matches!(opt_value, Some("false")) {
                options.insert(
                    opt.name().to_owned(),
                    validate_json_option_value_compiler_options_value(
                        opt,
                        CompilerOptionsValue::Bool(Some(false)),
                        errors,
                    ),
                );
                i += 1;
            } else {
                if matches!(opt_value, Some("true")) {
                    i += 1;
                }
                errors.push(Rc::new(create_compiler_diagnostic(&Diagnostics::Option_0_can_only_be_specified_in_tsconfig_json_file_or_set_to_null_on_command_line, Some(vec![opt.name().to_owned()])).into()));
            }
        } else {
            errors.push(Rc::new(create_compiler_diagnostic(&Diagnostics::Option_0_can_only_be_specified_in_tsconfig_json_file_or_set_to_null_on_command_line, Some(vec![opt.name().to_owned()])).into()));
            if match opt_value {
                None => false,
                Some(opt_value) => !opt_value.is_empty() && !starts_with(opt_value, "-"),
            } {
                i += 1;
            }
        }
    } else {
        if match args.get(i) {
            None => true,
            Some(arg) => arg.is_empty(),
        } && !matches!(opt.type_(), CommandLineOptionType::Boolean)
        {
            errors.push(Rc::new(
                create_compiler_diagnostic(
                    diagnostics.option_type_mismatch_diagnostic(),
                    Some(vec![
                        opt.name().to_owned(),
                        get_compiler_option_value_type_string(opt),
                    ]),
                )
                .into(),
            ));
        }
        if !matches!(args.get(i).map(|arg| &**arg), Some("null")) {
            match opt.type_() {
                CommandLineOptionType::Number => {
                    options.insert(
                        opt.name().to_owned(),
                        validate_json_option_value_compiler_options_value(
                            opt,
                            CompilerOptionsValue::Usize(Some(args[i].parse::<usize>().unwrap())),
                            errors,
                        ),
                    );
                    i += 1;
                }
                CommandLineOptionType::Boolean => {
                    let opt_value = args.get(i).map(|arg| &**arg);
                    options.insert(
                        opt.name().to_owned(),
                        validate_json_option_value_compiler_options_value(
                            opt,
                            CompilerOptionsValue::Bool(Some(!matches!(opt_value, Some("false")))),
                            errors,
                        ),
                    );
                    if matches!(opt_value, Some("false") | Some("true")) {
                        i += 1;
                    }
                }
                CommandLineOptionType::String => {
                    options.insert(
                        opt.name().to_owned(),
                        validate_json_option_value_compiler_options_value(
                            opt,
                            CompilerOptionsValue::String(Some(
                                args.get(i)
                                    .map_or_else(|| "".to_owned(), |arg| arg.to_owned()),
                            )),
                            errors,
                        ),
                    );
                    i += 1;
                }
                CommandLineOptionType::List => {
                    let result: CompilerOptionsValue =
                        parse_list_type_option(opt, args.get(i), errors).unwrap_or_else(|| vec![]);
                    let result_is_some = result.is_some();
                    options.insert(opt.name().to_owned(), result.or_empty_vec());
                    if result_is_some {
                        i += 1;
                    }
                }
                _ => {
                    options.insert(
                        opt.name().to_owned(),
                        parse_custom_type_option(opt, args.get(i).map(|arg| &**arg), errors),
                    );
                    i += 1;
                }
            }
        } else {
            // options[opt.name] = undefined;
            i += 1;
        }
    }
    i
}

thread_local! {
    pub(crate) static compiler_options_did_you_mean_diagnostics: Rc<dyn ParseCommandLineWorkerDiagnostics> = Rc::new(CompilerOptionsDidYouMeanDiagnostics::new());
}

pub struct CompilerOptionsDidYouMeanDiagnostics {}

impl CompilerOptionsDidYouMeanDiagnostics {
    pub fn new() -> Self {
        Self {}
    }
}

impl DidYouMeanOptionsDiagnostics for CompilerOptionsDidYouMeanDiagnostics {
    fn maybe_alternate_mode(&self) -> Option<Rc<AlternateModeDiagnostics>> {
        Some(
            compiler_options_alternate_mode
                .with(|compiler_options_alternate_mode_| compiler_options_alternate_mode_.clone()),
        )
    }

    fn option_declarations(&self) -> Vec<Rc<CommandLineOption>> {
        option_declarations.with(|option_declarations_| option_declarations_.clone())
    }

    fn unknown_option_diagnostic(&self) -> &DiagnosticMessage {
        &Diagnostics::Unknown_compiler_option_0
    }

    fn unknown_did_you_mean_diagnostic(&self) -> &DiagnosticMessage {
        &Diagnostics::Unknown_compiler_option_0_Did_you_mean_1
    }
}

impl ParseCommandLineWorkerDiagnostics for CompilerOptionsDidYouMeanDiagnostics {
    fn get_options_name_map(&self) -> Rc<OptionsNameMap> {
        get_options_name_map()
    }

    fn option_type_mismatch_diagnostic(&self) -> &DiagnosticMessage {
        &Diagnostics::Compiler_option_0_expects_an_argument
    }
}

pub fn parse_command_line<TReadFile: FnMut(&str) -> Option<String>>(
    command_line: &[String],
    read_file: Option<TReadFile>,
) -> ParsedCommandLine {
    compiler_options_did_you_mean_diagnostics.with(|compiler_options_did_you_mean_diagnostics_| {
        parse_command_line_worker(
            &compiler_options_did_you_mean_diagnostics_,
            command_line,
            read_file,
        )
    })
}

pub(super) fn get_option_declaration_from_name<TGetOptionNameMap: FnMut() -> Rc<OptionsNameMap>>(
    get_option_name_map: TGetOptionNameMap,
    option_name: &str,
    allow_short: Option<bool>,
) -> Option<Rc<CommandLineOption>> {
    let allow_short = allow_short.unwrap_or(false);
    let mut option_name = option_name.to_lowercase();
    let option_name_map = get_option_name_map();
    let options_name_map = &option_name_map.options_name_map;
    let short_option_names = &option_name_map.short_option_names;
    if allow_short {
        let short = short_option_names.get(&option_name);
        if let Some(short) = short {
            option_name = short.clone();
        }
    }
    options_name_map.get(option_name).map(Clone::clone)
}

pub(crate) struct ParsedBuildCommand {
    pub build_options: BuildOptions,
    pub watch_options: Option<WatchOptions>,
    pub projects: Vec<String>,
    pub errors: Vec<Rc<Diagnostic>>,
}

thread_local! {
    pub(super) static build_options_name_map_cache: RefCell<Option<Rc<OptionsNameMap>>> = RefCell::new(None);
}

pub(crate) fn get_build_options_name_map() -> Rc<OptionsNameMap> {
    build_options_name_map_cache.with(|build_options_name_map_cache_| {
        let mut build_options_name_map_cache_ = build_options_name_map_cache_.borrow_mut();
        if build_options_name_map_cache_.is_none() {
            build_opts.with(|build_opts_| {
                *build_options_name_map_cache_ =
                    Some(Rc::new(create_option_name_map(&build_opts_)));
            });
        }
        build_options_name_map_cache_.as_ref().unwrap().clone()
    })
}

pub(crate) fn parse_build_command(args: &[String]) -> ParsedBuildCommand {
    ParsedBuildCommand {
        build_options: BuildOptions {
            clean: None,
            watch: None,
            help: None,
            pretty: None,
            locale: None,
            generate_cpu_profile: None,
        },
        watch_options: None,
        projects: vec![],
        errors: vec![],
    }
}

pub(crate) fn get_diagnostic_text(
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> String {
    let diagnostic = create_compiler_diagnostic(message, args);
    enum_unwrapped!(diagnostic.message_text(), [DiagnosticMessageText, String]).clone()
}

pub trait DiagnosticReporter {
    fn call(&self, diagnostic: Rc<Diagnostic>);
}

pub trait ConfigFileDiagnosticsReporter {}

pub(crate) enum StringOrRcDiagnostic {
    String(String),
    RcDiagnostic(Rc<Diagnostic>),
}

impl From<String> for StringOrRcDiagnostic {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<Rc<Diagnostic>> for StringOrRcDiagnostic {
    fn from(value: Rc<Diagnostic>) -> Self {
        Self::RcDiagnostic(value)
    }
}

pub(crate) fn try_read_file<TReadFile: FnMut(&str) -> Option<String>>(
    file_name: &str,
    read_file: TReadFile,
) -> StringOrRcDiagnostic {
    let text = read_file(file_name); // TODO: should read_file (eg System::read_file()) return a Result? And then could mimic the error including message here?
    match text {
        None => Rc::new(
            create_compiler_diagnostic(
                &Diagnostics::Cannot_read_file_0,
                Some(vec![file_name.to_owned()]),
            )
            .into(),
        )
        .into(),
        Some(text) => text.into(),
    }
}

pub(super) fn get_tsconfig_root_options_map() -> Rc<CommandLineOption> {
    unimplemented!()
}

pub(crate) trait JsonConversionNotifier {
    fn on_set_valid_option_key_value_in_parent(
        &self,
        parent_option: &str,
        option: &CommandLineOption,
        value: Option<&serde_json::Value>,
    );
    fn on_set_valid_option_key_value_in_root(
        &self,
        key: &str,
        key_node: &Node, /*PropertyName*/
        value: Option<&serde_json::Value>,
        value_node: &Node, /*Expression*/
    );
    fn on_set_unknown_option_key_value_in_root(
        &self,
        key: &str,
        key_node: &Node, /*PropertyName*/
        value: Option<&serde_json::Value>,
        value_node: &Node, /*Expression*/
    );
}

pub(crate) struct JsonConversionNotifierDummy {}

impl JsonConversionNotifier for JsonConversionNotifierDummy {
    fn on_set_valid_option_key_value_in_parent(
        &self,
        parent_option: &str,
        option: &CommandLineOption,
        value: Option<&serde_json::Value>,
    ) {
        unimplemented!()
    }

    fn on_set_valid_option_key_value_in_root(
        &self,
        key: &str,
        key_node: &Node, /*PropertyName*/
        value: Option<&serde_json::Value>,
        value_node: &Node, /*Expression*/
    ) {
        unimplemented!()
    }

    fn on_set_unknown_option_key_value_in_root(
        &self,
        key: &str,
        key_node: &Node, /*PropertyName*/
        value: Option<&serde_json::Value>,
        value_node: &Node, /*Expression*/
    ) {
        unimplemented!()
    }
}

pub(super) fn convert_config_file_to_object<TOptionsIterator: JsonConversionNotifier>(
    source_file: &Node, /*JsonSourceFile*/
    errors: &RefCell<&mut Vec<Rc<Diagnostic>>>,
    report_options_errors: bool,
    options_iterator: Option<&TOptionsIterator>,
) -> Option<serde_json::Value> {
    let source_file_as_source_file = source_file.as_source_file();
    let root_expression = source_file_as_source_file
        .statements
        .get(0)
        .map(|statement| statement.as_expression_statement().expression.clone());
    let known_root_options: Option<Rc<CommandLineOption>> = if report_options_errors {
        Some(get_tsconfig_root_options_map())
    } else {
        None
    };
    if let Some(root_expression) = root_expression
        .as_ref()
        .filter(|root_expression| root_expression.kind() != SyntaxKind::ObjectLiteralExpression)
    {
        errors.borrow_mut().push(Rc::new(
            create_diagnostic_for_node_in_source_file(
                source_file,
                root_expression,
                &Diagnostics::The_root_value_of_a_0_file_must_be_an_object,
                Some(vec![
                    if get_base_file_name(&source_file_as_source_file.file_name(), None, None)
                        == "jsconfig.json"
                    {
                        "jsconfig.json".to_owned()
                    } else {
                        "tsconfig.json".to_owned()
                    },
                ]),
            )
            .into(),
        ));
        if is_array_literal_expression(root_expression) {
            let first_object = find(
                &root_expression.as_array_literal_expression().elements,
                |element, _| is_object_literal_expression(element),
            );
            if let Some(first_object) = first_object {
                return convert_to_object_worker(
                    source_file,
                    Some(&**first_object),
                    errors,
                    true,
                    known_root_options.as_deref(),
                    options_iterator,
                );
            }
        }
        return Some(serde_json::Value::Object(serde_json::Map::new()));
    }
    convert_to_object_worker(
        source_file,
        root_expression,
        errors,
        true,
        known_root_options.as_deref(),
        options_iterator,
    )
}

pub fn convert_to_object(
    source_file: &Node, /*JsonSourceFile*/
    errors: &mut Push<Rc<Diagnostic>>,
) -> Option<serde_json::Value> {
    convert_to_object_worker(
        source_file,
        source_file
            .as_source_file()
            .statements
            .get(0)
            .map(|statement| statement.as_expression_statement().expression.clone()),
        &RefCell::new(errors),
        true,
        None,
        Option::<&JsonConversionNotifierDummy>::None,
    )
}

pub(crate) fn convert_to_object_worker<
    TRootExpression: Borrow<Node>,
    TJsonConversionNotifier: JsonConversionNotifier,
>(
    source_file: &Node, /*JsonSourceFile*/
    root_expression: Option<TRootExpression>,
    errors: &RefCell<&mut Push<Rc<Diagnostic>>>,
    return_value: bool,
    known_root_options: Option<&CommandLineOption>,
    json_conversion_notifier: Option<&TJsonConversionNotifier>,
) -> Option<serde_json::Value> {
    if root_expression.is_none() {
        return if return_value {
            Some(serde_json::Value::Object(serde_json::Map::new()))
        } else {
            None
        };
    }
    let root_expression = root_expression.unwrap();
    let root_expression = root_expression.borrow();

    convert_property_value_to_json(
        errors,
        source_file,
        json_conversion_notifier,
        return_value,
        known_root_options,
        root_expression,
        known_root_options,
    )
}
