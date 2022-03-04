use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{
    command_options_without_build, common_options_with_build, convert_property_value_to_json,
    create_option_name_map, is_compiler_options_value, parse_command_line_worker,
};
use crate::{
    build_opts, create_compiler_diagnostic, create_diagnostic_for_node_in_source_file, find,
    for_each, get_base_file_name, get_text_of_property_name, is_array_literal_expression,
    is_computed_non_literal_name, is_object_literal_expression, is_string_double_quoted,
    is_string_literal, unescape_leading_underscores, AlternateModeDiagnostics, BuildOptions,
    CommandLineOption, CommandLineOptionBase, CommandLineOptionInterface,
    CommandLineOptionOfBooleanType, CommandLineOptionOfListType, CommandLineOptionOfStringType,
    CommandLineOptionType, CompilerOptions, CompilerOptionsBuilder, Diagnostic, DiagnosticMessage,
    DiagnosticRelatedInformationInterface, Diagnostics, DidYouMeanOptionsDiagnostics, ModuleKind,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, Number, OptionsNameMap,
    ParsedCommandLine, Push, ScriptTarget, StringOrDiagnosticMessage, SyntaxKind, WatchOptions,
};
use local_macros::enum_unwrapped;

pub fn parse_command_line<TReadFile: FnMut(&str) -> Option<String>>(
    command_line: &[String],
    read_file: Option<TReadFile>,
) -> ParsedCommandLine {
    parse_command_line_worker(command_line)
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
