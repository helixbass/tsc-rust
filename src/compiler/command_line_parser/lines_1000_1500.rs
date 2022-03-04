use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{command_options_without_build, common_options_with_build, get_build_options_name_map};
use crate::{
    for_each, AlternateModeDiagnostics, CommandLineOption, CommandLineOptionBase,
    CommandLineOptionInterface, CommandLineOptionOfBooleanType, CommandLineOptionOfListType,
    CommandLineOptionOfStringType, CommandLineOptionType, CompilerOptions, CompilerOptionsBuilder,
    Diagnostic, DiagnosticMessage, Diagnostics, DidYouMeanOptionsDiagnostics, ModuleKind,
    ParsedCommandLine, ScriptTarget, StringOrDiagnosticMessage,
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
