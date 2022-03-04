use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{command_options_without_build, common_options_with_build, is_compiler_options_value};
use crate::{
    create_compiler_diagnostic, create_diagnostic_for_node_in_source_file, find, for_each,
    get_base_file_name, get_text_of_property_name, is_array_literal_expression,
    is_computed_non_literal_name, is_object_literal_expression, is_string_double_quoted,
    is_string_literal, unescape_leading_underscores, AlternateModeDiagnostics, BuildOptions,
    CommandLineOption, CommandLineOptionBase, CommandLineOptionInterface,
    CommandLineOptionOfBooleanType, CommandLineOptionOfListType, CommandLineOptionOfStringType,
    CommandLineOptionType, CompilerOptions, CompilerOptionsBuilder, Diagnostic, DiagnosticMessage,
    DiagnosticRelatedInformationInterface, Diagnostics, DidYouMeanOptionsDiagnostics, ModuleKind,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, Number, ParsedCommandLine, Push,
    ScriptTarget, StringOrDiagnosticMessage, SyntaxKind, WatchOptions,
};
use local_macros::enum_unwrapped;

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

// pub(crate) fn convert_enable_auto_discovery_to_enable(type_acquisition: )

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

pub(super) fn create_unknown_option_error<
    TCreateDiagnostics: FnMut(&DiagnosticMessage, Option<Vec<String>>) -> Rc<Diagnostic>,
>(
    unknown_option: &str,
    diagnostics: &Box<dyn DidYouMeanOptionsDiagnostics>,
    create_diagnostics: TCreateDiagnostics,
    unknown_option_error_text: Option<&str>,
) -> Rc<Diagnostic> {
    unimplemented!()
}

pub(super) fn parse_command_line_worker(command_line: &[String]) -> ParsedCommandLine {
    ParsedCommandLine {
        options: Rc::new(CompilerOptions {
            all: None,
            allow_js: None,
            allow_non_ts_extensions: None,
            allow_synthetic_default_imports: None,
            allow_umd_global_access: None,
            allow_unreachable_code: None,
            allow_unused_labels: None,
            always_strict: None,
            base_url: None,
            build: None,
            charset: None,
            check_js: None,
            config_file_path: None,
            config_file: None,
            declaration: None,
            declaration_map: None,
            emit_declaration_only: None,
            declaration_dir: None,
            diagnostics: None,
            extended_diagnostics: None,
            disable_size_limit: None,
            disable_source_of_project_reference_redirect: None,
            disable_solution_searching: None,
            disable_referenced_project_load: None,
            downlevel_iteration: None,
            emit_bom: None,
            emit_decorator_metadata: None,
            exact_optional_property_types: None,
            experimental_decorators: None,
            force_consistent_casing_in_file_names: None,
            generate_cpu_profile: None,
            generate_trace: None,
            help: None,
            import_helpers: None,
            imports_not_used_as_values: None,
            init: None,
            inline_source_map: None,
            inline_sources: None,
            isolated_modules: None,
            jsx: None,
            keyof_strings_only: None,
            lib: None,
            list_emitted_files: None,
            list_files: None,
            explain_files: None,
            list_files_only: None,
            locale: None,
            map_root: None,
            max_node_module_js_depth: None,
            module: None,
            module_resolution: None,
            new_line: None,
            no_emit: None,
            no_emit_for_js_files: None,
            no_emit_helpers: None,
            no_emit_on_error: None,
            no_error_truncation: None,
            no_fallthrough_cases_in_switch: None,
            no_implicit_any: None,
            no_implicit_returns: None,
            no_implicit_this: None,
            no_strict_generic_checks: None,
            no_unused_locals: None,
            no_unused_parameters: None,
            no_implicit_use_strict: None,
            no_property_access_from_index_signature: None,
            assume_changes_only_affect_direct_dependencies: None,
            no_lib: None,
            no_resolve: None,
            no_unchecked_indexed_access: None,
            out: None,
            out_dir: None,
            out_file: None,
            paths: None,
            paths_base_path: None,
            plugins: None,
            preserve_const_enums: None,
            no_implicit_override: None,
            preserve_symlinks: None,
            preserve_value_imports: None,
            preserve_watch_output: None,
            project: None,
            pretty: None,
            react_namespace: None,
            jsx_factory: None,
            jsx_fragment_factory: None,
            jsx_import_source: None,
            composite: None,
            incremental: None,
            ts_build_info_file: None,
            remove_comments: None,
            root_dir: None,
            root_dirs: None,
            skip_lib_check: None,
            skip_default_lib_check: None,
            source_map: None,
            source_root: None,
            strict: None,
            strict_function_types: None,
            strict_bind_call_apply: None,
            strict_null_checks: None,
            strict_property_initialization: None,
            strip_internal: None,
            suppress_excess_property_errors: None,
            suppress_implicit_any_index_errors: None,
            suppress_output_path_check: None,
            target: None,
            trace_resolution: None,
            use_unknown_in_catch_variables: None,
            resolve_json_module: None,
            types: None,
            type_roots: None,
            version: None,
            watch: None,
            es_module_interop: None,
            show_config: None,
            use_define_for_class_fields: None,
        }),
        project_references: None,
        file_names: command_line.to_vec(),
        watch_options: None,
        errors: vec![],
        compile_on_save: None,
        raw: None,
        type_acquisition: None,
        wildcard_directories: None,
    }
}

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

pub(super) fn is_root_option_map(
    known_root_options: Option<&CommandLineOption>,
    known_options: Option<&HashMap<String, CommandLineOption>>,
) -> bool {
    match known_root_options {
        None => false,
        Some(known_root_options) => match known_root_options {
            CommandLineOption::TsConfigOnlyOption(known_root_options) => {
                matches!(known_root_options.element_options.as_ref(), Some(element_options) if matches!(known_options, Some(known_options) if ptr::eq(element_options, known_options)))
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
    known_options: Option<&HashMap<String, CommandLineOption>>,
    extra_key_diagnostics: Option<&Box<dyn DidYouMeanOptionsDiagnostics>>,
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
            (Some(key_text), Some(known_options)) => known_options.get(key_text),
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
            if let Some(mut json_conversion_notifier) = json_conversion_notifier {
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

pub(super) fn convert_array_literal_expression_to_json(
    element: &NodeArray, /*<Expression>*/
    element_option: Option<&CommandLineOption>,
) -> Option<serde_json::Value> {
    unimplemented!()
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
                let element_options = option_as_ts_config_only_option.element_options.as_ref();
                let extra_key_diagnostics = option_as_ts_config_only_option
                    .extra_key_diagnostics
                    .as_ref();
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
