use std::{collections::HashMap, iter::FromIterator, rc::Rc};

use id_arena::Id;
use indexmap::IndexMap;

use super::spec_to_diagnostic;
use crate::{
    per_arena, CommandLineOption, CommandLineOptionBaseBuilder, CommandLineOptionMapTypeValue,
    CommandLineOptionOfListType, CommandLineOptionType, CompilerOptionsValue, Diagnostics,
    HasArena, ImportsNotUsedAsValues, JsxEmit, ModuleKind, ModuleResolutionKind, NewLineKind,
    PollingWatchKind, ScriptTarget, StringOrDiagnosticMessage, TsConfigOnlyOption,
    WatchDirectoryKind, WatchFileKind,
};

macro_rules! command_line_option_per_arena {
    ($arena:expr, $builder:expr $(,)?) => {
        $crate::per_arena!(
            $crate::CommandLineOption,
            $arena,
            $arena.alloc_command_line_option(
                $builder
                    .build()
                    .unwrap()
                    .try_into_command_line_option($arena)
                    .unwrap()
            )
        )
    };
}

pub(crate) fn compile_on_save_command_line_option(arena: &impl HasArena) -> Id<CommandLineOption> {
    command_line_option_per_arena!(
        arena,
        CommandLineOptionBaseBuilder::default()
            .name("compileOnSave".to_string())
            .type_(CommandLineOptionType::Boolean)
            .default_value_description(StringOrDiagnosticMessage::String("false".to_string())),
    )
}

thread_local! {
    pub(crate) static jsx_option_map: IndexMap<&'static str, JsxEmit> =
        IndexMap::from_iter(IntoIterator::into_iter([
            ("preserve", JsxEmit::Preserve),
            ("react-native", JsxEmit::ReactNative),
            ("react", JsxEmit::React),
            ("react-jsx", JsxEmit::ReactJSX),
            ("react-jsxdev", JsxEmit::ReactJSXDev),
        ]));
}

thread_local! {
    pub(crate) static inverse_jsx_option_map: HashMap<JsxEmit, &'static str> =
        jsx_option_map.with(|jsx_option_map_| {
            HashMap::from_iter(jsx_option_map_.iter().map(|(key, value)| (*value, *key)))
        });
}

thread_local! {
    pub(crate) static lib_entries: Vec<(&'static str, &'static str)> = vec![
        ("es5", "lib.es5.d.ts"),
        ("es6", "lib.es2015.d.ts"),
        ("es2015", "lib.es2015.d.ts"),
        ("es7", "lib.es2016.d.ts"),
        ("es2016", "lib.es2016.d.ts"),
        ("es2017", "lib.es2017.d.ts"),
        ("es2018", "lib.es2018.d.ts"),
        ("es2019", "lib.es2019.d.ts"),
        ("es2020", "lib.es2020.d.ts"),
        ("es2021", "lib.es2021.d.ts"),
        ("esnext", "lib.esnext.d.ts"),
        ("dom", "lib.dom.d.ts"),
        ("dom.iterable", "lib.dom.iterable.d.ts"),
        ("webworker", "lib.webworker.d.ts"),
        (
            "webworker.importscripts",
            "lib.webworker.importscripts.d.ts",
        ),
        ("webworker.iterable", "lib.webworker.iterable.d.ts"),
        ("scripthost", "lib.scripthost.d.ts"),
        ("es2015.core", "lib.es2015.core.d.ts"),
        ("es2015.collection", "lib.es2015.collection.d.ts"),
        ("es2015.generator", "lib.es2015.generator.d.ts"),
        ("es2015.iterable", "lib.es2015.iterable.d.ts"),
        ("es2015.promise", "lib.es2015.promise.d.ts"),
        ("es2015.proxy", "lib.es2015.proxy.d.ts"),
        ("es2015.reflect", "lib.es2015.reflect.d.ts"),
        ("es2015.symbol", "lib.es2015.symbol.d.ts"),
        (
            "es2015.symbol.wellknown",
            "lib.es2015.symbol.wellknown.d.ts",
        ),
        ("es2016.array.include", "lib.es2016.array.include.d.ts"),
        ("es2017.object", "lib.es2017.object.d.ts"),
        ("es2017.sharedmemory", "lib.es2017.sharedmemory.d.ts"),
        ("es2017.string", "lib.es2017.string.d.ts"),
        ("es2017.intl", "lib.es2017.intl.d.ts"),
        ("es2017.typedarrays", "lib.es2017.typedarrays.d.ts"),
        ("es2018.asyncgenerator", "lib.es2018.asyncgenerator.d.ts"),
        ("es2018.asynciterable", "lib.es2018.asynciterable.d.ts"),
        ("es2018.intl", "lib.es2018.intl.d.ts"),
        ("es2018.promise", "lib.es2018.promise.d.ts"),
        ("es2018.regexp", "lib.es2018.regexp.d.ts"),
        ("es2019.array", "lib.es2019.array.d.ts"),
        ("es2019.object", "lib.es2019.object.d.ts"),
        ("es2019.string", "lib.es2019.string.d.ts"),
        ("es2019.symbol", "lib.es2019.symbol.d.ts"),
        ("es2020.bigint", "lib.es2020.bigint.d.ts"),
        ("es2020.promise", "lib.es2020.promise.d.ts"),
        ("es2020.sharedmemory", "lib.es2020.sharedmemory.d.ts"),
        ("es2020.string", "lib.es2020.string.d.ts"),
        (
            "es2020.symbol.wellknown",
            "lib.es2020.symbol.wellknown.d.ts",
        ),
        ("es2020.intl", "lib.es2020.intl.d.ts"),
        ("es2021.promise", "lib.es2021.promise.d.ts"),
        ("es2021.string", "lib.es2021.string.d.ts"),
        ("es2021.weakref", "lib.es2021.weakref.d.ts"),
        ("es2021.intl", "lib.es2021.intl.d.ts"),
        ("esnext.array", "lib.es2019.array.d.ts"),
        ("esnext.symbol", "lib.es2019.symbol.d.ts"),
        ("esnext.asynciterable", "lib.es2018.asynciterable.d.ts"),
        ("esnext.intl", "lib.esnext.intl.d.ts"),
        ("esnext.bigint", "lib.es2020.bigint.d.ts"),
        ("esnext.string", "lib.es2021.string.d.ts"),
        ("esnext.promise", "lib.es2021.promise.d.ts"),
        ("esnext.weakref", "lib.es2021.weakref.d.ts"),
    ];
}

thread_local! {
    pub(crate) static libs: Vec<&'static str> =
        lib_entries.with(|lib_entries_| lib_entries_.iter().map(|entry| entry.0).collect());
}

thread_local! {
    pub(crate) static lib_map: IndexMap<&'static str, &'static str> = lib_entries
        .with(|lib_entries_| IndexMap::from_iter(lib_entries_.iter().map(|tuple| (tuple.0, tuple.1))));
}

pub(crate) fn options_for_watch(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(vec![
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("watchFile".to_string())
                .type_(CommandLineOptionType::Map(
                    IndexMap::from_iter([
                        ("fixedpollinginterval", CommandLineOptionMapTypeValue::WatchFileKind(WatchFileKind::FixedPollingInterval)),
                        ("prioritypollinginterval", CommandLineOptionMapTypeValue::WatchFileKind(WatchFileKind::PriorityPollingInterval)),
                        ("dynamicprioritypolling", CommandLineOptionMapTypeValue::WatchFileKind(WatchFileKind::DynamicPriorityPolling)),
                        ("fixedchunksizepolling", CommandLineOptionMapTypeValue::WatchFileKind(WatchFileKind::FixedChunkSizePolling)),
                        ("usefsevents", CommandLineOptionMapTypeValue::WatchFileKind(WatchFileKind::UseFsEvents)),
                        ("usefseventsonparentdirectory", CommandLineOptionMapTypeValue::WatchFileKind(WatchFileKind::UseFsEventsOnParentDirectory)),
                    ])
                ))
                .description(&Diagnostics::Specify_how_the_TypeScript_watch_mode_works)
                .category(&Diagnostics::Watch_and_Build_Modes)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("watchDirectory".to_string())
                .type_(CommandLineOptionType::Map(
                    IndexMap::from_iter(IntoIterator::into_iter([
                        ("usefsevents", CommandLineOptionMapTypeValue::WatchDirectoryKind(WatchDirectoryKind::UseFsEvents)),
                        ("fixedpollinginterval", CommandLineOptionMapTypeValue::WatchDirectoryKind(WatchDirectoryKind::FixedPollingInterval)),
                        ("dynamicprioritypolling", CommandLineOptionMapTypeValue::WatchDirectoryKind(WatchDirectoryKind::DynamicPriorityPolling)),
                        ("fixedchunksizepolling", CommandLineOptionMapTypeValue::WatchDirectoryKind(WatchDirectoryKind::FixedChunkSizePolling)),
                    ]))
                ))
                .description(&Diagnostics::Specify_how_directories_are_watched_on_systems_that_lack_recursive_file_watching_functionality)
                .category(&Diagnostics::Watch_and_Build_Modes)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("fallbackPolling".to_string())
                .type_(CommandLineOptionType::Map(
                    IndexMap::from_iter(IntoIterator::into_iter([
                        ("fixedinterval", CommandLineOptionMapTypeValue::PollingWatchKind(PollingWatchKind::FixedInterval)),
                        ("priorityinterval", CommandLineOptionMapTypeValue::PollingWatchKind(PollingWatchKind::PriorityInterval)),
                        ("dynamicpriority", CommandLineOptionMapTypeValue::PollingWatchKind(PollingWatchKind::DynamicPriority)),
                        ("fixedchunksize", CommandLineOptionMapTypeValue::PollingWatchKind(PollingWatchKind::FixedChunkSize)),
                    ]))
                ))
                .description(&Diagnostics::Specify_what_approach_the_watcher_should_use_if_the_system_runs_out_of_native_file_watchers)
                .category(&Diagnostics::Watch_and_Build_Modes)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("synchronousWatchDirectory".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Synchronously_call_callbacks_and_update_the_state_of_directory_watchers_on_platforms_that_don_t_support_recursive_watching_natively)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Watch_and_Build_Modes)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionOfListType::new(CommandLineOptionBaseBuilder::default()
                .name("excludeDirectories".to_string())
                .type_(CommandLineOptionType::List)
                .description(&Diagnostics::Remove_a_list_of_directories_from_the_watch_process)
                .category(&Diagnostics::Watch_and_Build_Modes)
                .build().unwrap(),
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("excludeDirectories".to_string())
                    .type_(CommandLineOptionType::String)
                    .is_file_path(true)
                    .extra_validation(Rc::new(|value: Option<&serde_json::Value>| spec_to_diagnostic(value.unwrap().as_str().unwrap(), None).map(|ret| (ret.0, Some(vec![ret.1])))))
                    .extra_validation_compiler_options_value(Rc::new(|value: &CompilerOptionsValue| spec_to_diagnostic(value.as_option_string().unwrap(), None).map(|ret| (ret.0, Some(vec![ret.1])))))
                    .build().unwrap().try_into_command_line_option(arena).unwrap()),
                arena,
            )
            .into()),
            arena.alloc_command_line_option(CommandLineOptionOfListType::new(CommandLineOptionBaseBuilder::default()
                .name("excludeFiles".to_string())
                .type_(CommandLineOptionType::List)
                .description(&Diagnostics::Remove_a_list_of_files_from_the_watch_mode_s_processing)
                .category(&Diagnostics::Watch_and_Build_Modes)
                .build().unwrap(),
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("excludeFiles".to_string())
                    .type_(CommandLineOptionType::String)
                    .is_file_path(true)
                    .extra_validation(Rc::new(|value: Option<&serde_json::Value>| spec_to_diagnostic(value.unwrap().as_str().unwrap(), None).map(|ret| (ret.0, Some(vec![ret.1])))))
                    .extra_validation_compiler_options_value(Rc::new(|value: &CompilerOptionsValue| spec_to_diagnostic(value.as_option_string().unwrap(), None).map(|ret| (ret.0, Some(vec![ret.1])))))
                    .build().unwrap().try_into_command_line_option(arena).unwrap()),
                arena,
            )
            .into()),
        ])
    )
}

pub(crate) fn common_options_with_build(arena: &impl HasArena) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(vec![
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("help".to_string())
                .type_(CommandLineOptionType::Boolean)
                .short_name("h".to_string())
                .description(&Diagnostics::Print_this_message)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Command_line_Options)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("help".to_string())
                .type_(CommandLineOptionType::Boolean)
                .short_name("?".to_string())
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("watch".to_string())
                .type_(CommandLineOptionType::Boolean)
                .short_name("w".to_string())
                .description(&Diagnostics::Watch_input_files)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Command_line_Options)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("preserveWatchOutput".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_wiping_the_console_in_watch_mode)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(false)
                .category(&Diagnostics::Output_Formatting)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("listFiles".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Print_all_of_the_files_read_during_the_compilation)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Compiler_Diagnostics)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("explainFiles".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Print_files_read_during_the_compilation_including_why_it_was_included)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Compiler_Diagnostics)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("listEmittedFiles".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Print_the_names_of_emitted_files_after_a_compilation)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Compiler_Diagnostics)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("pretty".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enable_color_and_formatting_in_TypeScript_s_output_to_make_compiler_errors_easier_to_read)
                .default_value_description(StringOrDiagnosticMessage::String("true".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Output_Formatting)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("traceResolution".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Log_paths_used_during_the_moduleResolution_process)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Compiler_Diagnostics)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("diagnostics".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Output_compiler_performance_information_after_building)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Compiler_Diagnostics)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("extendedDiagnostics".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Output_more_detailed_compiler_performance_information_after_building)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Compiler_Diagnostics)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("generateCpuProfile".to_string())
                .type_(CommandLineOptionType::String)
                .is_file_path(true)
                .description(&Diagnostics::Emit_a_v8_CPU_profile_of_the_compiler_run_for_debugging)
                .default_value_description(StringOrDiagnosticMessage::String("profile.cpuprofile".to_string()))
                .param_type(&Diagnostics::FILE_OR_DIRECTORY)
                .category(&Diagnostics::Compiler_Diagnostics)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("generateTrace".to_string())
                .type_(CommandLineOptionType::String)
                .is_file_path(true)
                .description(&Diagnostics::Generates_an_event_trace_and_a_list_of_types)
                .param_type(&Diagnostics::DIRECTORY)
                .is_command_line_only(true)
                .category(&Diagnostics::Compiler_Diagnostics)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("incremental".to_string())
                .type_(CommandLineOptionType::Boolean)
                .short_name("i".to_string())
                .description(&Diagnostics::Enable_incremental_compilation)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::false_unless_composite_is_set))
                .category(&Diagnostics::Projects)
                .transpile_option_value(None)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("assumeChangesOnlyAffectDirectDependencies".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Have_recompiles_in_projects_that_use_incremental_and_watch_mode_assume_that_changes_within_a_file_will_only_affect_files_directly_depending_on_it)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Watch_and_Build_Modes)
                .affects_semantic_diagnostics(true)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("locale".to_string())
                .type_(CommandLineOptionType::String)
                .description(&Diagnostics::Set_the_language_of_the_messaging_from_TypeScript_This_does_not_affect_emit)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::Platform_specific))
                .is_command_line_only(true)
                .category(&Diagnostics::Command_line_Options)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
        ])
    )
}

pub(crate) fn target_option_declaration(
    arena: &impl HasArena,
) -> Id<CommandLineOption /*CommandLineOptionOfCustomType*/> {
    command_line_option_per_arena!(
        arena,
        CommandLineOptionBaseBuilder::default()
            .name("target".to_string())
            .type_(CommandLineOptionType::Map(
                IndexMap::from_iter(IntoIterator::into_iter([
                    ("es3", CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES3)),
                    ("es5", CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES5)),
                    ("es6", CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2015)),
                    ("es2015", CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2015)),
                    ("es2016", CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2016)),
                    ("es2017", CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2017)),
                    ("es2018", CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2018)),
                    ("es2019", CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2019)),
                    ("es2020", CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2020)),
                    ("es2021", CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2021)),
                    ("esnext", CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ESNext)),
                ]))
            ))
            .short_name("t".to_string())
            .description(&Diagnostics::Set_the_JavaScript_language_version_for_emitted_JavaScript_and_include_compatible_library_declarations)
            .default_value_description(StringOrDiagnosticMessage::String("ES3".to_string()))
            .param_type(&Diagnostics::VERSION)
            .show_in_simplified_help_view(true)
            .category(&Diagnostics::Language_and_Environment)
            .affects_source_file(true)
            .affects_module_resolution(true)
            .affects_emit(true),
    )
}

pub(crate) fn command_options_without_build(
    arena: &impl HasArena,
) -> Id<Vec<Id<CommandLineOption>>> {
    per_arena!(
        Vec<Id<CommandLineOption>>,
        arena,
        arena.alloc_vec_command_line_option(vec![
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("all".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Show_all_compiler_options)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Command_line_Options)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("version".to_string())
                .type_(CommandLineOptionType::Boolean)
                .short_name("v".to_string())
                .description(&Diagnostics::Print_the_compiler_s_version)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Command_line_Options)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("init".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Initializes_a_TypeScript_project_and_creates_a_tsconfig_json_file)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Command_line_Options)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("project".to_string())
                .type_(CommandLineOptionType::String)
                .is_file_path(true)
                .short_name("p".to_string())
                .description(&Diagnostics::Compile_the_project_given_the_path_to_its_configuration_file_or_to_a_folder_with_a_tsconfig_json)
                .param_type(&Diagnostics::FILE_OR_DIRECTORY)
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Command_line_Options)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("build".to_string())
                .type_(CommandLineOptionType::Boolean)
                .short_name("b".to_string())
                .description(&Diagnostics::Build_one_or_more_projects_and_their_dependencies_if_out_of_date)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Command_line_Options)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("showConfig".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Print_the_final_configuration_instead_of_building)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .is_command_line_only(true)
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Command_line_Options)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("listFilesOnly".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Print_names_of_files_that_are_part_of_the_compilation_and_then_stop_processing)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .is_command_line_only(true)
                .category(&Diagnostics::Command_line_Options)
                .affects_semantic_diagnostics(true)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            target_option_declaration(arena),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("module".to_string())
                    .type_(CommandLineOptionType::Map(
                        IndexMap::from_iter(IntoIterator::into_iter([
                            ("none", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::None)),
                            ("commonjs", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::CommonJS)),
                            ("amd", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::AMD)),
                            ("system", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::System)),
                            ("umd", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::UMD)),
                            ("es6", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::ES2015)),
                            ("es2015", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::ES2015)),
                            ("es2020", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::ES2020)),
                            ("es2022", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::ES2022)),
                            ("esnext", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::ESNext)),
                            ("node12", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::Node12)),
                            ("nodenext", CommandLineOptionMapTypeValue::ModuleKind(ModuleKind::NodeNext)),
                        ]))
                    ))
                    .short_name("m".to_string())
                    .description(&Diagnostics::Specify_what_module_code_is_generated)
                    .param_type(&Diagnostics::KIND)
                    .show_in_simplified_help_view(true)
                    .category(&Diagnostics::Modules)
                    .affects_module_resolution(true)
                    .affects_emit(true)
                    .build().unwrap().try_into_command_line_option(arena).unwrap()),
                arena.alloc_command_line_option(CommandLineOptionOfListType::new(CommandLineOptionBaseBuilder::default()
                    .name("lib".to_string())
                    .type_(CommandLineOptionType::List)
                    .description(&Diagnostics::Specify_a_set_of_bundled_library_declaration_files_that_describe_the_target_runtime_environment)
                    .show_in_simplified_help_view(true)
                    .category(&Diagnostics::Language_and_Environment)
                    .affects_program_structure(true)
                    .transpile_option_value(None)
                    .build().unwrap(),
                    arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                        .name("lib".to_string())
                        .type_(CommandLineOptionType::Map(
                            lib_map.with(|lib_map_| IndexMap::from_iter(lib_map_.iter().map(|(key, value)|
                               (*key, CommandLineOptionMapTypeValue::StaticStr(*value))
                            )))
                        ))
                        .build().unwrap().try_into_command_line_option(arena).unwrap()),
                    arena,
                )
                .into()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("allowJs".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Allow_JavaScript_files_to_be_a_part_of_your_program_Use_the_checkJS_option_to_get_errors_from_these_files)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::JavaScript_Support)
                .affects_module_resolution(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("checkJs".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enable_error_reporting_in_type_checked_JavaScript_files)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::JavaScript_Support)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("jsx".to_string())
                .type_(jsx_option_map.with(|jsx_option_map_| CommandLineOptionType::Map(
                    IndexMap::from_iter(
                        jsx_option_map_.iter().map(|(key, value)| (*key, CommandLineOptionMapTypeValue::JsxEmit(*value)))
                   )
                )))
                .description(&Diagnostics::Specify_what_JSX_code_is_generated)
                .default_value_description(StringOrDiagnosticMessage::String("undefined".to_string()))
                .param_type(&Diagnostics::KIND)
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Language_and_Environment)
                .affects_source_file(true)
                .affects_module_resolution(true)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("declaration".to_string())
                .type_(CommandLineOptionType::Boolean)
                .short_name("d".to_string())
                .description(&Diagnostics::Generate_d_ts_files_from_TypeScript_and_JavaScript_files_in_your_project)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::false_unless_composite_is_set))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .transpile_option_value(None)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("declarationMap".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Create_sourcemaps_for_d_ts_files)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .transpile_option_value(None)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("emitDeclarationOnly".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Only_output_d_ts_files_and_not_JavaScript_files)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .transpile_option_value(None)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("sourceMap".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Create_source_map_files_for_emitted_JavaScript_files)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("outFile".to_string())
                .type_(CommandLineOptionType::String)
                .is_file_path(true)
                .description(&Diagnostics::Specify_a_file_that_bundles_all_outputs_into_one_JavaScript_file_If_declaration_is_true_also_designates_a_file_that_bundles_all_d_ts_output)
                .param_type(&Diagnostics::FILE)
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .transpile_option_value(None)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("outDir".to_string())
                .type_(CommandLineOptionType::String)
                .is_file_path(true)
                .description(&Diagnostics::Specify_an_output_folder_for_all_emitted_files)
                .param_type(&Diagnostics::DIRECTORY)
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("rootDir".to_string())
                .type_(CommandLineOptionType::String)
                .is_file_path(true)
                .description(&Diagnostics::Specify_the_root_folder_within_your_source_files)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::Computed_from_the_list_of_input_files))
                .param_type(&Diagnostics::LOCATION)
                .category(&Diagnostics::Modules)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("composite".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enable_constraints_that_allow_a_TypeScript_project_to_be_used_with_project_references)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .is_tsconfig_only(true)
                .category(&Diagnostics::Projects)
                .affects_emit(true)
                .transpile_option_value(None)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("tsBuildInfoFile".to_string())
                .type_(CommandLineOptionType::String)
                .is_file_path(true)
                .description(&Diagnostics::Specify_the_folder_for_tsbuildinfo_incremental_compilation_files)
                .default_value_description(StringOrDiagnosticMessage::String(".tsbuildinfo".to_string()))
                .param_type(&Diagnostics::FILE)
                .category(&Diagnostics::Projects)
                .affects_emit(true)
                .transpile_option_value(None)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("removeComments".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_emitting_comments)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noEmit".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_emitting_files_from_a_compilation)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Emit)
                .transpile_option_value(None)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("importHelpers".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Allow_importing_helper_functions_from_tslib_once_per_project_instead_of_including_them_per_file)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("importsNotUsedAsValues".to_string())
                .type_(CommandLineOptionType::Map(IndexMap::from_iter(IntoIterator::into_iter([
                   ("remove", CommandLineOptionMapTypeValue::ImportsNotUsedAsValues(ImportsNotUsedAsValues::Remove)),
                   ("preserve", CommandLineOptionMapTypeValue::ImportsNotUsedAsValues(ImportsNotUsedAsValues::Preserve)),
                   ("error", CommandLineOptionMapTypeValue::ImportsNotUsedAsValues(ImportsNotUsedAsValues::Error)),
                ]))))
                .description(&Diagnostics::Specify_emit_Slashchecking_behavior_for_imports_that_are_only_used_for_types)
                .category(&Diagnostics::Emit)
                .affects_semantic_diagnostics(true)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("downlevelIteration".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Emit_more_compliant_but_verbose_and_less_performant_JavaScript_for_iteration)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("isolatedModules".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Ensure_that_each_file_can_be_safely_transpiled_without_relying_on_other_imports)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Interop_Constraints)
                .transpile_option_value(Some(true))
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("strict".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enable_all_strict_type_checking_options)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Type_Checking)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noImplicitAny".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enable_error_reporting_for_expressions_and_declarations_with_an_implied_any_type)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::false_unless_strict_is_set))
                .category(&Diagnostics::Type_Checking)
                .strict_flag(true)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("strictNullChecks".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::When_type_checking_take_into_account_null_and_undefined)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::false_unless_strict_is_set))
                .category(&Diagnostics::Type_Checking)
                .strict_flag(true)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("strictFunctionTypes".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::When_assigning_functions_check_to_ensure_parameters_and_the_return_values_are_subtype_compatible)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::false_unless_strict_is_set))
                .category(&Diagnostics::Type_Checking)
                .strict_flag(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("strictBindCallApply".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Check_that_the_arguments_for_bind_call_and_apply_methods_match_the_original_function)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::false_unless_strict_is_set))
                .category(&Diagnostics::Type_Checking)
                .strict_flag(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("strictPropertyInitialization".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Check_for_class_properties_that_are_declared_but_not_set_in_the_constructor)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::false_unless_strict_is_set))
                .category(&Diagnostics::Type_Checking)
                .strict_flag(true)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noImplicitThis".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enable_error_reporting_when_this_is_given_the_type_any)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::false_unless_strict_is_set))
                .category(&Diagnostics::Type_Checking)
                .strict_flag(true)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("useUnknownInCatchVariables".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Type_catch_clause_variables_as_unknown_instead_of_any)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Type_Checking)
                .strict_flag(true)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("alwaysStrict".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Ensure_use_strict_is_always_emitted)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::false_unless_strict_is_set))
                .category(&Diagnostics::Type_Checking)
                .strict_flag(true)
                .affects_source_file(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noUnusedLocals".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enable_error_reporting_when_a_local_variables_aren_t_read)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Type_Checking)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noUnusedParameters".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Raise_an_error_when_a_function_parameter_isn_t_read)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Type_Checking)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("exactOptionalPropertyTypes".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Interpret_optional_property_types_as_written_rather_than_adding_undefined)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Type_Checking)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noImplicitReturns".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enable_error_reporting_for_codepaths_that_do_not_explicitly_return_in_a_function)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Type_Checking)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noFallthroughCasesInSwitch".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enable_error_reporting_for_fallthrough_cases_in_switch_statements)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Type_Checking)
                .affects_bind_diagnostics(true)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noUncheckedIndexedAccess".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Include_undefined_in_index_signature_results)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Type_Checking)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noImplicitOverride".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Ensure_overriding_members_in_derived_classes_are_marked_with_an_override_modifier)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Type_Checking)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noPropertyAccessFromIndexSignature".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enforces_using_indexed_accessors_for_keys_declared_using_an_indexed_type)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(false)
                .category(&Diagnostics::Type_Checking)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("moduleResolution".to_string())
                .type_(CommandLineOptionType::Map(IndexMap::from_iter(IntoIterator::into_iter([
                    ("node", CommandLineOptionMapTypeValue::ModuleResolutionKind(ModuleResolutionKind::NodeJs)),
                    ("classic", CommandLineOptionMapTypeValue::ModuleResolutionKind(ModuleResolutionKind::Classic)),
                    ("node12", CommandLineOptionMapTypeValue::ModuleResolutionKind(ModuleResolutionKind::Node12)),
                    ("nodenext", CommandLineOptionMapTypeValue::ModuleResolutionKind(ModuleResolutionKind::NodeNext)),
                ]))))
                .description(&Diagnostics::Specify_how_TypeScript_looks_up_a_file_from_a_given_module_specifier)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::module_AMD_or_UMD_or_System_or_ES6_then_Classic_Otherwise_Node))
                .param_type(&Diagnostics::STRATEGY)
                .category(&Diagnostics::Modules)
                .affects_module_resolution(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("baseUrl".to_string())
                .type_(CommandLineOptionType::String)
                .is_file_path(true)
                .description(&Diagnostics::Specify_the_base_directory_to_resolve_non_relative_module_names)
                .category(&Diagnostics::Modules)
                .affects_module_resolution(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(TsConfigOnlyOption::new(CommandLineOptionBaseBuilder::default()
                .name("paths".to_string())
                .type_(CommandLineOptionType::Object)
                .description(&Diagnostics::Specify_a_set_of_entries_that_re_map_imports_to_additional_lookup_locations)
                .is_tsconfig_only(true)
                .category(&Diagnostics::Modules)
                .affects_module_resolution(true)
                .transpile_option_value(None)
                .build().unwrap(),
                 None, None, arena)
            .into()),
            arena.alloc_command_line_option(CommandLineOptionOfListType::new(CommandLineOptionBaseBuilder::default()
                .name("rootDirs".to_string())
                .type_(CommandLineOptionType::List)
                .description(&Diagnostics::Allow_multiple_folders_to_be_treated_as_one_when_resolving_modules)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::Computed_from_the_list_of_input_files))
                .is_tsconfig_only(true)
                .category(&Diagnostics::Modules)
                .affects_module_resolution(true)
                .transpile_option_value(None)
                .build().unwrap(),
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("rootDirs".to_string())
                    .type_(CommandLineOptionType::String)
                    .is_file_path(true)
                    .build().unwrap().try_into_command_line_option(arena).unwrap()),
                arena,
            )
            .into()),
            arena.alloc_command_line_option(CommandLineOptionOfListType::new(CommandLineOptionBaseBuilder::default()
                .name("typeRoots".to_string())
                .type_(CommandLineOptionType::List)
                .description(&Diagnostics::Specify_multiple_folders_that_act_like_Slashnode_modules_Slash_types)
                .category(&Diagnostics::Modules)
                .affects_module_resolution(true)
                .build().unwrap(),
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("typeRoots".to_string())
                    .type_(CommandLineOptionType::String)
                    .is_file_path(true)
                    .build().unwrap().try_into_command_line_option(arena).unwrap()),
                arena,
            )
            .into()),
            arena.alloc_command_line_option(CommandLineOptionOfListType::new(CommandLineOptionBaseBuilder::default()
                .name("types".to_string())
                .type_(CommandLineOptionType::List)
                .description(&Diagnostics::Specify_type_package_names_to_be_included_without_being_referenced_in_a_source_file)
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Modules)
                .affects_program_structure(true)
                .transpile_option_value(None)
                .build().unwrap(),
                arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                    .name("types".to_string())
                    .type_(CommandLineOptionType::String)
                    .build().unwrap().try_into_command_line_option(arena).unwrap()),
                arena,
            )
            .into()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("allowSyntheticDefaultImports".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Allow_import_x_from_y_when_a_module_doesn_t_have_a_default_export)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::module_system_or_esModuleInterop))
                .category(&Diagnostics::Interop_Constraints)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("esModuleInterop".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Emit_additional_JavaScript_to_ease_support_for_importing_CommonJS_modules_This_enables_allowSyntheticDefaultImports_for_type_compatibility)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .show_in_simplified_help_view(true)
                .category(&Diagnostics::Interop_Constraints)
                .affects_semantic_diagnostics(true)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("preserveSymlinks".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_resolving_symlinks_to_their_realpath_This_correlates_to_the_same_flag_in_node)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Interop_Constraints)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("allowUmdGlobalAccess".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Allow_accessing_UMD_globals_from_modules)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Modules)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("sourceRoot".to_string())
                .type_(CommandLineOptionType::String)
                .description(&Diagnostics::Specify_the_root_path_for_debuggers_to_find_the_reference_source_code)
                .param_type(&Diagnostics::LOCATION)
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("mapRoot".to_string())
                .type_(CommandLineOptionType::String)
                .description(&Diagnostics::Specify_the_location_where_debugger_should_locate_map_files_instead_of_generated_locations)
                .param_type(&Diagnostics::LOCATION)
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("inlineSourceMap".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Include_sourcemap_files_inside_the_emitted_JavaScript)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("inlineSources".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Include_source_code_in_the_sourcemaps_inside_the_emitted_JavaScript)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("experimentalDecorators".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enable_experimental_support_for_TC39_stage_2_draft_decorators)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Language_and_Environment)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("emitDecoratorMetadata".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Emit_design_type_metadata_for_decorated_declarations_in_source_files)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Language_and_Environment)
                .affects_semantic_diagnostics(true)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("jsxFactory".to_string())
                .type_(CommandLineOptionType::String)
                .description(&Diagnostics::Specify_the_JSX_factory_function_used_when_targeting_React_JSX_emit_e_g_React_createElement_or_h)
                .default_value_description(StringOrDiagnosticMessage::String("`React.createElement`".to_string()))
                .category(&Diagnostics::Language_and_Environment)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("jsxFragmentFactory".to_string())
                .type_(CommandLineOptionType::String)
                .description(&Diagnostics::Specify_the_JSX_Fragment_reference_used_for_fragments_when_targeting_React_JSX_emit_e_g_React_Fragment_or_Fragment)
                .category(&Diagnostics::Language_and_Environment)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("jsxImportSource".to_string())
                .type_(CommandLineOptionType::String)
                .description(&Diagnostics::Specify_module_specifier_used_to_import_the_JSX_factory_functions_when_using_jsx_Colon_react_jsx_Asterisk)
                .default_value_description(StringOrDiagnosticMessage::String("react".to_string()))
                .category(&Diagnostics::Language_and_Environment)
                .affects_module_resolution(true)
                .affects_semantic_diagnostics(true)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("resolveJsonModule".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Enable_importing_json_files)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Modules)
                .affects_module_resolution(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("out".to_string())
                .type_(CommandLineOptionType::String)
                .is_file_path(false)
                .description(&Diagnostics::Deprecated_setting_Use_outFile_instead)
                .param_type(&Diagnostics::FILE)
                .category(&Diagnostics::Backwards_Compatibility)
                .affects_emit(true)
                .transpile_option_value(None)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("reactNamespace".to_string())
                .type_(CommandLineOptionType::String)
                .description(&Diagnostics::Specify_the_object_invoked_for_createElement_This_only_applies_when_targeting_react_JSX_emit)
                .default_value_description(StringOrDiagnosticMessage::String("`React`".to_string()))
                .category(&Diagnostics::Language_and_Environment)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("skipDefaultLibCheck".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Skip_type_checking_d_ts_files_that_are_included_with_TypeScript)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Completeness)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("charset".to_string())
                .type_(CommandLineOptionType::String)
                .description(&Diagnostics::No_longer_supported_In_early_versions_manually_set_the_text_encoding_for_reading_files)
                .default_value_description(StringOrDiagnosticMessage::String("utf8".to_string()))
                .category(&Diagnostics::Backwards_Compatibility)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("emitBOM".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Emit_a_UTF_8_Byte_Order_Mark_BOM_in_the_beginning_of_output_files)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("newLine".to_string())
                .type_(CommandLineOptionType::Map(IndexMap::from_iter(IntoIterator::into_iter([
                    ("crlf", CommandLineOptionMapTypeValue::NewLineKind(NewLineKind::CarriageReturnLineFeed)),
                    ("lf", CommandLineOptionMapTypeValue::NewLineKind(NewLineKind::LineFeed)),
                ]))))
                .description(&Diagnostics::Set_the_newline_character_for_emitting_files)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::Platform_specific))
                .param_type(&Diagnostics::NEWLINE)
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noErrorTruncation".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_truncating_types_in_error_messages)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Output_Formatting)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noLib".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_including_any_library_files_including_the_default_lib_d_ts)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Language_and_Environment)
                .affects_program_structure(true)
                .transpile_option_value(Some(true))
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noResolve".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disallow_import_s_require_s_or_reference_s_from_expanding_the_number_of_files_TypeScript_should_add_to_a_project)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Modules)
                .affects_module_resolution(true)
                .transpile_option_value(Some(true))
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("stripInternal".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_emitting_declarations_that_have_internal_in_their_JSDoc_comments)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("disableSizeLimit".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Remove_the_20mb_cap_on_total_source_code_size_for_JavaScript_files_in_the_TypeScript_language_server)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Editor_Support)
                .affects_program_structure(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("disableSourceOfProjectReferenceRedirect".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_preferring_source_files_instead_of_declaration_files_when_referencing_composite_projects)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .is_tsconfig_only(true)
                .category(&Diagnostics::Projects)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("disableSolutionSearching".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Opt_a_project_out_of_multi_project_reference_checking_when_editing)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .is_tsconfig_only(true)
                .category(&Diagnostics::Projects)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("disableReferencedProjectLoad".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Reduce_the_number_of_projects_loaded_automatically_by_TypeScript)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .is_tsconfig_only(true)
                .category(&Diagnostics::Projects)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noImplicitUseStrict".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_adding_use_strict_directives_in_emitted_JavaScript_files)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Backwards_Compatibility)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noEmitHelpers".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_generating_custom_helper_functions_like_extends_in_compiled_output)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noEmitOnError".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_emitting_files_if_any_type_checking_errors_are_reported)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .transpile_option_value(None)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("preserveConstEnums".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_erasing_const_enum_declarations_in_generated_code)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("declarationDir".to_string())
                .type_(CommandLineOptionType::String)
                .is_file_path(true)
                .description(&Diagnostics::Specify_the_output_directory_for_generated_declaration_files)
                .param_type(&Diagnostics::DIRECTORY)
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .transpile_option_value(None)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("skipLibCheck".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Skip_type_checking_all_d_ts_files)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Completeness)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("allowUnusedLabels".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_error_reporting_for_unused_labels)
                .default_value_description(StringOrDiagnosticMessage::String("undefined".to_string()))
                .category(&Diagnostics::Type_Checking)
                .affects_bind_diagnostics(true)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("allowUnreachableCode".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_error_reporting_for_unreachable_code)
                .default_value_description(StringOrDiagnosticMessage::String("undefined".to_string()))
                .category(&Diagnostics::Type_Checking)
                .affects_bind_diagnostics(true)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("suppressExcessPropertyErrors".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_reporting_of_excess_property_errors_during_the_creation_of_object_literals)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Backwards_Compatibility)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("suppressImplicitAnyIndexErrors".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Suppress_noImplicitAny_errors_when_indexing_objects_that_lack_index_signatures)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Backwards_Compatibility)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("forceConsistentCasingInFileNames".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Ensure_that_casing_is_correct_in_imports)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Interop_Constraints)
                .affects_module_resolution(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("maxNodeModuleJsDepth".to_string())
                .type_(CommandLineOptionType::Number)
                .description(&Diagnostics::Specify_the_maximum_folder_depth_used_for_checking_JavaScript_files_from_node_modules_Only_applicable_with_allowJs)
                .default_value_description(StringOrDiagnosticMessage::String("0".to_string()))
                .category(&Diagnostics::JavaScript_Support)
                .affects_module_resolution(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("noStrictGenericChecks".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Disable_strict_checking_of_generic_signatures_in_function_types)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Backwards_Compatibility)
                .affects_semantic_diagnostics(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("useDefineForClassFields".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Emit_ECMAScript_standard_compliant_class_fields)
                .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::true_for_ES2022_and_above_including_ESNext))
                .category(&Diagnostics::Language_and_Environment)
                .affects_semantic_diagnostics(true)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("preserveValueImports".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Preserve_unused_imported_values_in_the_JavaScript_output_that_would_otherwise_be_removed)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Emit)
                .affects_emit(true)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                .name("keyofStringsOnly".to_string())
                .type_(CommandLineOptionType::Boolean)
                .description(&Diagnostics::Make_keyof_only_return_strings_instead_of_string_numbers_or_symbols_Legacy_option)
                .default_value_description(StringOrDiagnosticMessage::String("false".to_string()))
                .category(&Diagnostics::Backwards_Compatibility)
                .build().unwrap().try_into_command_line_option(arena).unwrap()),
            arena.alloc_command_line_option(CommandLineOptionOfListType::new(CommandLineOptionBaseBuilder::default()
                .name("plugins".to_string())
                .type_(CommandLineOptionType::List)
                .description(&Diagnostics::List_of_language_service_plugins)
                .category(&Diagnostics::Editor_Support)
                .build().unwrap(),
                arena.alloc_command_line_option(TsConfigOnlyOption::new(CommandLineOptionBaseBuilder::default()
                    .name("plugins".to_string())
                    .type_(CommandLineOptionType::Object)
                    .build().unwrap(),
                     None, None, arena)
                .into()),
                arena,
            )
            .into()),
        ])
    )
}
