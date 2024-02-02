use std::{cell::RefCell, collections::HashMap, io, rc::Rc};

use id_arena::Id;
use indexmap::IndexMap;
use itertools::Itertools;
use local_macros::enum_unwrapped;

use super::{
    command_options_without_build, common_options_with_build, convert_json_option_of_custom_type,
    get_build_options_name_map, get_option_declaration_from_name, parse_option_value,
    parse_response_file, validate_json_option_value, watch_options_did_you_mean_diagnostics,
};
use crate::{
    create_compiler_diagnostic, for_each, get_spelling_suggestion, starts_with, trim_string,
    AlternateModeDiagnostics, BuildOptions, CharacterCodes, CommandLineOption,
    CommandLineOptionBaseBuilder, CommandLineOptionInterface, CommandLineOptionOfListType,
    CommandLineOptionType, CompilerOptions, CompilerOptionsBuilder, CompilerOptionsValue,
    Diagnostic, DiagnosticMessage, Diagnostics, DidYouMeanOptionsDiagnostics, HasArena,
    ModuleKind, ParsedCommandLineWithBaseOptions, ScriptTarget, StringOrDiagnosticMessage,
    WatchOptions,
    InArena, per_arena, AllArenas,
};

pub fn option_declarations(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(
            common_options_with_build(arena)
                .ref_(arena)
                .iter()
                .chain(command_options_without_build(arena).ref_(arena).iter())
                .copied()
                .collect()
        )
    )
}

pub(crate) fn semantic_diagnostics_option_declarations(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(
            option_declarations(arena)
                .ref_(arena)
                .iter()
                .filter(|option| option.ref_(arena).affects_semantic_diagnostics())
                .copied()
                .collect()
        )
    )
}

pub(crate) fn affects_emit_option_declarations(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(
            option_declarations(arena)
                .ref_(arena)
                .iter()
                .filter(|option| option.ref_(arena).affects_emit())
                .copied()
                .collect()
        )
    )
}

pub(crate) fn module_resolution_option_declarations(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(
            option_declarations(arena)
                .ref_(arena)
                .iter()
                .filter(|option| option.ref_(arena).affects_module_resolution())
                .copied()
                .collect()
        )
    )
}

pub(crate) fn source_file_affecting_compiler_options(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(
            option_declarations(arena)
                .ref_(arena)
                .iter()
                .filter(|option| {
                    option.ref_(arena).affects_source_file()
                        || option.ref_(arena).affects_module_resolution()
                        || option.ref_(arena).affects_bind_diagnostics()
                })
                .copied()
                .collect()
        )
    )
}

pub(crate) fn options_affecting_program_structure(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(
            option_declarations(arena)
                .ref_(arena)
                .iter()
                .filter(|option| option.ref_(arena).affects_program_structure())
                .copied()
                .collect()
        )
    )
}

pub(crate) fn transpile_option_value_compiler_options(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(
            option_declarations(arena)
                .ref_(arena)
                .iter()
                .filter(|option| option.ref_(arena).transpile_option_value().is_some())
                .copied()
                .collect()
        )
    )
}

pub(crate) fn options_for_build(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(
            vec![
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("verbose")
                    .type_(CommandLineOptionType::Boolean)
                    .short_name("v")
                    .description(&Diagnostics::Enable_verbose_logging)
                    .default_value_description("false".to_owned())
                    .category(&Diagnostics::Command_line_Options)
                    .build().unwrap().try_into().unwrap()),
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("dry")
                    .type_(CommandLineOptionType::Boolean)
                    .short_name("d")
                    .description(&Diagnostics::Show_what_would_be_built_or_deleted_if_specified_with_clean)
                    .default_value_description("false".to_owned())
                    .category(&Diagnostics::Command_line_Options)
                    .build().unwrap().try_into().unwrap()),
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("force")
                    .type_(CommandLineOptionType::Boolean)
                    .short_name("f")
                    .description(&Diagnostics::Build_all_projects_including_those_that_appear_to_be_up_to_date)
                    .default_value_description("false".to_owned())
                    .category(&Diagnostics::Command_line_Options)
                    .build().unwrap().try_into().unwrap()),
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("clean".to_string())
                    .type_(CommandLineOptionType::Boolean)
                    .description(&Diagnostics::Delete_the_outputs_of_all_projects)
                    .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                    .category(&Diagnostics::Command_line_Options)
                    .build().unwrap().try_into().unwrap()),
            ]
        )
    )
}

pub(crate) fn build_opts(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(
            common_options_with_build(arena)
                .ref_(arena)
                .iter()
                .chain(options_for_build(arena).ref_(arena).iter())
                .copied()
                .collect()
        )
    )
}

pub(crate) fn type_acquisition_declarations(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(
            vec![
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                        .name("enableAutoDiscovery".to_string())
                        .type_(CommandLineOptionType::Boolean)
                        .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                        .build().unwrap().try_into().unwrap()),
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("enable".to_string())
                    .type_(CommandLineOptionType::Boolean)
                    .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                    .build().unwrap().try_into().unwrap()),
                arena.alloc_command_line_option(CommandLineOptionOfListType::new(
                    CommandLineOptionBaseBuilder::default()
                        .name("include".to_string())
                        .type_(CommandLineOptionType::List)
                        .build().unwrap(),
                    arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                        .name("include".to_string())
                        .type_(CommandLineOptionType::String)
                        .build().unwrap().try_into().unwrap()),
                ).into()),
                arena.alloc_command_line_option(CommandLineOptionOfListType::new(
                    CommandLineOptionBaseBuilder::default()
                        .name("exclude".to_string())
                        .type_(CommandLineOptionType::List)
                        .build().unwrap(),
                    arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                        .name("exclude".to_string())
                        .type_(CommandLineOptionType::String)
                        .build().unwrap().try_into().unwrap()),
                ).into()),
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("disableFilenameBasedTypeAcquisition".to_string())
                    .type_(CommandLineOptionType::Boolean)
                    .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                    .build().unwrap().try_into().unwrap()),
            ]
        )
    )
}

pub struct OptionsNameMap {
    pub options_name_map: HashMap<String, Id<CommandLineOption>>,
    pub short_option_names: HashMap<String, String>,
}

pub fn create_option_name_map(option_declarations_: &[Id<CommandLineOption>], arena: &impl HasArena) -> OptionsNameMap {
    let mut options_name_map = HashMap::new();
    let mut short_option_names = HashMap::new();
    for_each(option_declarations_, |option, _| {
        options_name_map.insert(option.ref_(arena).name().to_lowercase(), option.clone());
        if let Some(option_short_name) = option.ref_(arena).maybe_short_name() {
            short_option_names.insert(option_short_name.to_owned(), option.ref_(arena).name().to_owned());
        }
        Option::<()>::None
    });

    OptionsNameMap {
        options_name_map,
        short_option_names,
    }
}

pub(crate) fn get_options_name_map(arena: &impl HasArena) -> Id<OptionsNameMap> {
    per_arena!(
        OptionsNameMap,
        arena,
        arena.alloc_options_name_map(create_option_name_map(&option_declarations(arena).ref_(arena), arena))
    )
}

thread_local! {
    static compiler_options_alternate_mode_: Rc<AlternateModeDiagnostics> = Rc::new(AlternateModeDiagnostics {
        diagnostic: &Diagnostics::Compiler_option_0_may_only_be_used_with_build,
        get_options_name_map: Box::new(|arena: &AllArenas| get_build_options_name_map(arena)),
    });
}

pub(super) fn compiler_options_alternate_mode() -> Rc<AlternateModeDiagnostics> {
    compiler_options_alternate_mode_
        .with(|compiler_options_alternate_mode| compiler_options_alternate_mode.clone())
}

// TODO: make static (per-arena)?
pub(crate) fn get_default_init_compiler_options(arena: &impl HasArena) -> Id<CompilerOptions> {
    arena.alloc_compiler_options(CompilerOptionsBuilder::default()
        .module(ModuleKind::CommonJS)
        .target(ScriptTarget::ES2016)
        .strict(true)
        .es_module_interop(true)
        .force_consistent_casing_in_file_names(true)
        .skip_lib_check(true)
        .build().unwrap())
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
            let mut map = serde_json::Map::new();
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
    arena: &impl HasArena,
) -> Id<Diagnostic> {
    create_diagnostic_for_invalid_custom_type(opt, |message, args| {
        arena.alloc_diagnostic(create_compiler_diagnostic(message, args).into())
    })
}

pub(super) fn create_diagnostic_for_invalid_custom_type(
    opt: &CommandLineOption, /*CommandLineOptionOfCustomType*/
    mut create_diagnostic: impl FnMut(&DiagnosticMessage, Option<Vec<String>>) -> Id<Diagnostic>,
) -> Id<Diagnostic> {
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

pub fn parse_custom_type_option(
    opt: &CommandLineOption, /*CommandLineOptionOfCustomType*/
    value: Option<&str>,
    errors: &mut Vec<Id<Diagnostic>>,
    arena: &impl HasArena,
) -> CompilerOptionsValue {
    convert_json_option_of_custom_type(opt, Some(trim_string(value.unwrap_or(""))), errors, arena)
}

pub fn parse_list_type_option(
    opt: &CommandLineOption, /*CommandLineOptionOfListType*/
    value: Option<&str>,
    errors: &mut Vec<Id<Diagnostic>>,
    arena: &impl HasArena,
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
    match opt_as_command_line_option_of_list_type.element.ref_(arena).type_() {
        // CommandLineOptionType::Number =>
        CommandLineOptionType::String => Some(
            values
                .filter_map(|v| {
                    match validate_json_option_value(
                        &opt_as_command_line_option_of_list_type.element.ref_(arena),
                        Some(&serde_json::Value::String(v.to_owned())),
                        errors,
                        arena,
                    ) {
                        CompilerOptionsValue::String(v) => v,
                        _ => panic!("Expected string"),
                    }
                })
                .collect(),
        ),
        CommandLineOptionType::Map(_) => Some(
            values
                .filter_map(|v| {
                    match parse_custom_type_option(
                        &opt_as_command_line_option_of_list_type.element.ref_(arena),
                        Some(v),
                        errors,
                        arena,
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

pub trait ParseCommandLineWorkerDiagnostics: DidYouMeanOptionsDiagnostics {
    fn get_options_name_map(&self) -> Id<OptionsNameMap>;
    fn option_type_mismatch_diagnostic(&self) -> &DiagnosticMessage;
    fn as_did_you_mean_options_diagnostics(&self) -> &dyn DidYouMeanOptionsDiagnostics;
}

pub(super) fn get_option_name(option: &CommandLineOption) -> &str {
    option.name()
}

pub(super) fn create_unknown_option_error(
    unknown_option: &str,
    diagnostics: &dyn DidYouMeanOptionsDiagnostics,
    mut create_diagnostics: impl FnMut(&DiagnosticMessage, Option<Vec<String>>) -> Id<Diagnostic>,
    unknown_option_error_text: Option<&str>,
    arena: &impl HasArena,
) -> Id<Diagnostic> {
    if let Some(diagnostics_alternate_mode) = diagnostics.maybe_alternate_mode() {
        if (diagnostics_alternate_mode.get_options_name_map)(arena.arena())
            .ref_(arena).options_name_map
            .contains_key(&unknown_option.to_lowercase())
        {
            return create_diagnostics(
                diagnostics_alternate_mode.diagnostic,
                Some(vec![unknown_option.to_owned()]),
            );
        }
    }

    let diagnostics_option_declarations = diagnostics.option_declarations();
    let possible_option = get_spelling_suggestion(
        unknown_option,
        &*diagnostics_option_declarations.ref_(arena),
        |candidate: &Id<CommandLineOption>| Some(get_option_name(&candidate.ref_(arena)).to_owned()),
    );
    match possible_option {
        Some(possible_option) => create_diagnostics(
            diagnostics.unknown_did_you_mean_diagnostic(),
            Some(vec![
                unknown_option_error_text
                    .unwrap_or(unknown_option)
                    .to_owned(),
                possible_option.ref_(arena).name().to_owned(),
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

pub fn hash_map_to_build_options(options: &IndexMap<String, CompilerOptionsValue>) -> BuildOptions {
    let mut build_options: BuildOptions = Default::default();
    for (option_name, value) in options {
        match &**option_name {
            "dry" => {
                build_options.dry = enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "force" => {
                build_options.force = enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "verbose" => {
                build_options.verbose =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "clean" => {
                build_options.clean = enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "watch" => {
                build_options.watch = enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "help" => {
                build_options.help = enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "preserveWatchOutput" => {
                build_options.preserve_watch_output =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "listEmittedFiles" => {
                build_options.list_emitted_files =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "listFiles" => {
                build_options.list_files =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "explainFiles" => {
                build_options.explain_files =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "pretty" => {
                build_options.pretty = enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "incremental" => {
                build_options.incremental =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "assumeChangesOnlyAffectDirectDependencies" => {
                build_options.assume_changes_only_affect_direct_dependencies =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "traceResolution" => {
                build_options.trace_resolution =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "diagnostics" => {
                build_options.diagnostics =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "extendedDiagnostics" => {
                build_options.extended_diagnostics =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "locale" => {
                build_options.locale =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "generateCpuProfile" => {
                build_options.generate_cpu_profile =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "generateTrace" => {
                build_options.generate_trace =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            _ => panic!("Unknown build option"),
        }
    }
    build_options
}

pub fn hash_map_to_compiler_options(
    options: &IndexMap<impl AsRef<str>, CompilerOptionsValue>,
) -> CompilerOptions {
    let mut compiler_options: CompilerOptions = Default::default();
    for (option_name, value) in options {
        let option_name = option_name.as_ref();
        match option_name {
            "all" => {
                compiler_options.all = enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "allowJs" => {
                compiler_options.allow_js =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "allowNonTsExtensions" => {
                compiler_options.allow_non_ts_extensions =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "allowSyntheticDefaultImports" => {
                compiler_options.allow_synthetic_default_imports =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "allowUmdGlobalAccess" => {
                compiler_options.allow_umd_global_access =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "allowUnreachableCode" => {
                compiler_options.allow_unreachable_code =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "allowUnusedLabels" => {
                compiler_options.allow_unused_labels =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "alwaysStrict" => {
                compiler_options.always_strict =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "baseUrl" => {
                compiler_options.base_url =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "build" => {
                compiler_options.build =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "charset" => {
                compiler_options.charset =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "checkJs" => {
                compiler_options.check_js =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "configFilePath" => {
                compiler_options.config_file_path =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "configFile" => {
                compiler_options.config_file =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, SourceFile]);
            }
            "declaration" => {
                compiler_options.declaration =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "declarationMap" => {
                compiler_options.declaration_map =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "emitDeclarationOnly" => {
                compiler_options.emit_declaration_only =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "declarationDir" => {
                compiler_options.declaration_dir =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "diagnostics" => {
                compiler_options.diagnostics =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "extendedDiagnostics" => {
                compiler_options.extended_diagnostics =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "disableSizeLimit" => {
                compiler_options.disable_size_limit =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "disableSourceOfProjectReferenceRedirect" => {
                compiler_options.disable_source_of_project_reference_redirect =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "disableSolutionSearching" => {
                compiler_options.disable_solution_searching =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "disableReferencedProjectLoad" => {
                compiler_options.disable_referenced_project_load =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "downlevelIteration" => {
                compiler_options.downlevel_iteration =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "emitBom" => {
                compiler_options.emit_bom =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "emitDecoratorMetadata" => {
                compiler_options.emit_decorator_metadata =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "exactOptionalPropertyTypes" => {
                compiler_options.exact_optional_property_types =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "experimentalDecorators" => {
                compiler_options.experimental_decorators =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "forceConsistentCasingInFileNames" => {
                compiler_options.force_consistent_casing_in_file_names =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "generateCpuProfile" => {
                compiler_options.generate_cpu_profile =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "generateTrace" => {
                compiler_options.generate_trace =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "help" => {
                compiler_options.help =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "importHelpers" => {
                compiler_options.import_helpers =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "importsNotUsedAsValues" => {
                compiler_options.imports_not_used_as_values = enum_unwrapped!(
                    value.clone(),
                    [CompilerOptionsValue, ImportsNotUsedAsValues]
                );
            }
            "init" => {
                compiler_options.init =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "inlineSourceMap" => {
                compiler_options.inline_source_map =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "inlineSources" => {
                compiler_options.inline_sources =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "isolatedModules" => {
                compiler_options.isolated_modules =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "jsx" => {
                compiler_options.jsx =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, JsxEmit]);
            }
            "keyofStringsOnly" => {
                compiler_options.keyof_strings_only =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "lib" => {
                compiler_options.lib =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, VecString]);
            }
            "listEmittedFiles" => {
                compiler_options.list_emitted_files =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "listFiles" => {
                compiler_options.list_files =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "explainFiles" => {
                compiler_options.explain_files =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "listFilesOnly" => {
                compiler_options.list_files_only =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "locale" => {
                compiler_options.locale =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "mapRoot" => {
                compiler_options.map_root =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "maxNodeModuleJsDepth" => {
                compiler_options.max_node_module_js_depth =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Usize]);
            }
            "module" => {
                compiler_options.module =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, ModuleKind]);
            }
            "moduleResolution" => {
                compiler_options.module_resolution =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, ModuleResolutionKind]);
            }
            "newLine" => {
                compiler_options.new_line =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, NewLineKind]);
            }
            "noEmit" => {
                compiler_options.no_emit =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noEmitForJsFiles" => {
                compiler_options.no_emit_for_js_files =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noEmitHelpers" => {
                compiler_options.no_emit_helpers =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noEmitOnError" => {
                compiler_options.no_emit_on_error =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noErrorTruncation" => {
                compiler_options.no_error_truncation =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noFallthroughCasesInSwitch" => {
                compiler_options.no_fallthrough_cases_in_switch =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noImplicitAny" => {
                compiler_options.no_implicit_any =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noImplicitReturns" => {
                compiler_options.no_implicit_returns =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noImplicitThis" => {
                compiler_options.no_implicit_this =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noStrictGenericChecks" => {
                compiler_options.no_strict_generic_checks =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noUnusedLocals" => {
                compiler_options.no_unused_locals =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noUnusedParameters" => {
                compiler_options.no_unused_parameters =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noImplicitUseStrict" => {
                compiler_options.no_implicit_use_strict =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noPropertyAccessFromIndexSignature" => {
                compiler_options.no_property_access_from_index_signature =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "assumeChangesOnlyAffectDirectDependencies" => {
                compiler_options.assume_changes_only_affect_direct_dependencies =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noLib" => {
                compiler_options.no_lib =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noResolve" => {
                compiler_options.no_resolve =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noUncheckedIndexedAccess" => {
                compiler_options.no_unchecked_indexed_access =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "out" => {
                compiler_options.out =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "outDir" => {
                compiler_options.out_dir =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "outFile" => {
                compiler_options.out_file =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "paths" => {
                compiler_options.paths =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, MapLikeVecString]);
            }
            "pathsBasePath" => {
                compiler_options.paths_base_path =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "plugins" => {
                compiler_options.plugins =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, VecPluginImport]);
            }
            "preserveConstEnums" => {
                compiler_options.preserve_const_enums =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "noImplicitOverride" => {
                compiler_options.no_implicit_override =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "preserveSymlinks" => {
                compiler_options.preserve_symlinks =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "preserveValueImports" => {
                compiler_options.preserve_value_imports =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "preserveWatchOutput" => {
                compiler_options.preserve_watch_output =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "project" => {
                compiler_options.project =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "pretty" => {
                compiler_options.pretty =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "reactNamespace" => {
                compiler_options.react_namespace =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "jsxFactory" => {
                compiler_options.jsx_factory =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "jsxFragmentFactory" => {
                compiler_options.jsx_fragment_factory =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "jsxImportSource" => {
                compiler_options.jsx_import_source =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "composite" => {
                compiler_options.composite =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "incremental" => {
                compiler_options.incremental =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "tsBuildInfoFile" => {
                compiler_options.ts_build_info_file =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "removeComments" => {
                compiler_options.remove_comments =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "rootDir" => {
                compiler_options.root_dir =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "rootDirs" => {
                compiler_options.root_dirs =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, VecString]);
            }
            "skipLibCheck" => {
                compiler_options.skip_lib_check =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "skipDefaultLibCheck" => {
                compiler_options.skip_default_lib_check =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "sourceMap" => {
                compiler_options.source_map =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "sourceRoot" => {
                compiler_options.source_root =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, String]);
            }
            "strict" => {
                compiler_options.strict =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "strictFunctionTypes" => {
                compiler_options.strict_function_types =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "strictBindCallApply" => {
                compiler_options.strict_bind_call_apply =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "strictNullChecks" => {
                compiler_options.strict_null_checks =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "strictPropertyInitialization" => {
                compiler_options.strict_property_initialization =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "stripInternal" => {
                compiler_options.strip_internal =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "suppressExcessPropertyErrors" => {
                compiler_options.suppress_excess_property_errors =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "suppressImplicitAnyIndexErrors" => {
                compiler_options.suppress_implicit_any_index_errors =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "suppressOutputPathCheck" => {
                compiler_options.suppress_output_path_check =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "target" => {
                compiler_options.target =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, ScriptTarget]);
            }
            "traceResolution" => {
                compiler_options.trace_resolution =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "useUnknownInCatchVariables" => {
                compiler_options.use_unknown_in_catch_variables =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "resolveJsonModule" => {
                compiler_options.resolve_json_module =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "types" => {
                compiler_options.types =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, VecString]);
            }
            "typeRoots" => {
                compiler_options.type_roots =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, VecString]);
            }
            "version" => {
                compiler_options.version =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "watch" => {
                compiler_options.watch =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "esModuleInterop" => {
                compiler_options.es_module_interop =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "showConfig" => {
                compiler_options.show_config =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "useDefineForClassFields" => {
                compiler_options.use_define_for_class_fields =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            _ => panic!("Unknown compiler option"),
        }
    }
    compiler_options
}

pub(super) fn hash_map_to_watch_options(
    options: &IndexMap<String, CompilerOptionsValue>,
) -> WatchOptions {
    let mut watch_options: WatchOptions = Default::default();
    for (option_name, value) in options {
        match &**option_name {
            "watchFile" => {
                watch_options.watch_file =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, WatchFileKind]);
            }
            "watchDirectory" => {
                watch_options.watch_directory =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, WatchDirectoryKind]);
            }
            "fallbackPolling" => {
                watch_options.fallback_polling =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, PollingWatchKind]);
            }
            "synchronousWatchDirectory" => {
                watch_options.synchronous_watch_directory =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, Bool]);
            }
            "excludeDirectories" => {
                watch_options.exclude_directories =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, VecString]);
            }
            "excludeFiles" => {
                watch_options.exclude_files =
                    enum_unwrapped!(value.clone(), [CompilerOptionsValue, VecString]);
            }
            _ => panic!("Unknown watch option"),
        }
    }
    watch_options
}

pub fn parse_command_line_worker(
    diagnostics: &dyn ParseCommandLineWorkerDiagnostics,
    command_line: &[String],
    read_file: Option<impl Fn(&str) -> io::Result<Option<String>>>,
    arena: &impl HasArena,
) -> ParsedCommandLineWithBaseOptions {
    let mut options: IndexMap<String, CompilerOptionsValue> = Default::default();
    let watch_options: RefCell<Option<IndexMap<String, CompilerOptionsValue>>> = Default::default();
    let mut file_names: Vec<String> = Default::default();
    let mut errors: Vec<Id<Diagnostic>> = Default::default();

    parse_strings(
        &mut file_names,
        diagnostics,
        &mut options,
        &mut errors,
        &watch_options,
        read_file.as_ref(),
        command_line,
        arena,
    );

    let watch_options = watch_options.borrow();
    ParsedCommandLineWithBaseOptions {
        options,
        watch_options: watch_options
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

pub(super) fn parse_strings(
    file_names: &mut Vec<String>,
    diagnostics: &dyn ParseCommandLineWorkerDiagnostics,
    options: &mut IndexMap<String, CompilerOptionsValue>,
    errors: &mut Vec<Id<Diagnostic>>,
    watch_options: &RefCell<Option<IndexMap<String, CompilerOptionsValue>>>,
    read_file: Option<&impl Fn(&str) -> io::Result<Option<String>>>,
    args: &[String],
    arena: &impl HasArena,
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
                arena,
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
                arena,
            );
            if let Some(opt) = opt {
                i = parse_option_value(args, i, diagnostics, &opt.ref_(arena), options, errors, arena);
            } else {
                let watch_opt = get_option_declaration_from_name(
                    || watch_options_did_you_mean_diagnostics().get_options_name_map(),
                    &input_option_name,
                    Some(true),
                    arena,
                );
                if let Some(watch_opt) = watch_opt {
                    let mut watch_options = watch_options.borrow_mut();
                    if watch_options.is_none() {
                        *watch_options = Some(Default::default());
                    }
                    i = parse_option_value(
                        args,
                        i,
                        &*watch_options_did_you_mean_diagnostics(),
                        &watch_opt.ref_(arena),
                        watch_options.as_mut().unwrap(),
                        errors,
                        arena,
                    );
                } else {
                    errors.push(create_unknown_option_error(
                        &input_option_name,
                        diagnostics.as_did_you_mean_options_diagnostics(),
                        |message, args| arena.alloc_diagnostic(create_compiler_diagnostic(message, args).into()),
                        Some(s),
                        arena,
                    ));
                }
            }
        } else {
            file_names.push(s.clone());
        }
    }
}
