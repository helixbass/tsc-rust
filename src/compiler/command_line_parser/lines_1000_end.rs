use serde::Serialize;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use super::{command_options_without_build, common_options_with_build};
use crate::{
    create_compiler_diagnostic, BuildOptions, CommandLineOption, CommandLineOptionInterface,
    CompilerOptions, Diagnostic, DiagnosticMessage, DiagnosticRelatedInformationInterface, Node,
    ParsedCommandLine, Push, System, WatchOptions,
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
    pub(crate) static options_for_build: Vec<Rc<CommandLineOption>> = vec![];
}

thread_local! {
    pub(crate) static build_opts: Vec<Rc<CommandLineOption>> = vec![];
}

pub struct OptionsNameMap {
    pub options_name_map: HashMap<String, Rc<CommandLineOption>>,
    pub short_option_names: HashMap<String, String>,
}

pub fn parse_command_line<TReadFile: FnMut(&str) -> Option<String>>(
    command_line: &[String],
    read_file: Option<TReadFile>,
) -> ParsedCommandLine {
    parse_command_line_worker(command_line)
}

fn parse_command_line_worker(command_line: &[String]) -> ParsedCommandLine {
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
    }
}

pub(crate) struct ParsedBuildCommand {
    pub build_options: BuildOptions,
    pub watch_options: Option<WatchOptions>,
    pub projects: Vec<String>,
    pub errors: Vec<Rc<Diagnostic>>,
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

pub(crate) type JsonConversionNotifier = ();

pub(crate) fn convert_to_object_worker<TRootExpression: Borrow<Node>>(
    source_file: &Node, /*JsonSourceFile*/
    root_expression: Option<TRootExpression>,
    errors: &mut Push<Rc<Diagnostic>>,
    return_value: bool,
    known_root_options: Option<CommandLineOption>,
    json_conversion_notifier: Option<JsonConversionNotifier>,
) {
    unimplemented!()
}

#[derive(Serialize)]
pub(crate) struct TSConfig {}

pub(crate) fn convert_to_tsconfig(
    config_parse_result: &ParsedCommandLine,
    config_file_name: &str,
    host: &dyn System, /*ConvertToTSConfigHost*/
) -> TSConfig {
    unimplemented!()
}

pub(crate) fn convert_to_options_with_absolute_paths<TToAbsolutePath: FnMut(&str) -> String>(
    options: Rc<CompilerOptions>,
    to_absolute_path: TToAbsolutePath,
) -> Rc<CompilerOptions> {
    options
}

pub struct ParsedTsconfig {}

pub struct ExtendedConfigCacheEntry {
    pub extended_result: Rc<Node /*TsConfigSourceFile*/>,
    pub extended_config: Option<ParsedTsconfig>,
}
