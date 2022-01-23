use std::array::IntoIter;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::rc::Rc;

use crate::{
    CommandLineOption, CommandLineOptionBase, CommandLineOptionMapTypeValue,
    CommandLineOptionOfBooleanType, CommandLineOptionOfCustomType, CommandLineOptionOfStringType,
    CommandLineOptionType, CompilerOptions, Diagnostics, ParsedCommandLine, ScriptTarget,
    StringOrDiagnosticMessage,
};

thread_local! {
    pub(crate) static common_options_with_build: Vec<CommandLineOption> = vec![
        CommandLineOptionOfBooleanType::new(CommandLineOptionBase {
            name: "help".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: Some("h".to_string()),
            description: Some(Diagnostics::Print_this_message),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: Some(true),
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
            name: "help".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: Some("?".to_string()),
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
            name: "watch".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: Some("w".to_string()),
            description: Some(Diagnostics::Watch_input_files),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: Some(true),
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
            name: "preserveWatchOutput".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: Some(Diagnostics::Disable_wiping_the_console_in_watch_mode),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: Some(false),
            category: Some(Diagnostics::Output_Formatting),
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
            name: "listFiles".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: Some(Diagnostics::Print_all_of_the_files_read_during_the_compilation),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Compiler_Diagnostics),
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
            name: "explainFiles".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: Some(Diagnostics::Print_files_read_during_the_compilation_including_why_it_was_included),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Compiler_Diagnostics),
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
            name: "listEmittedFiles".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: Some(Diagnostics::Print_the_names_of_emitted_files_after_a_compilation),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Compiler_Diagnostics),
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
            name: "pretty".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: Some(Diagnostics::Enable_color_and_formatting_in_TypeScript_s_output_to_make_compiler_errors_easier_to_read),
            default_value_description: Some(StringOrDiagnosticMessage::String("true".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: Some(true),
            category: Some(Diagnostics::Output_Formatting),
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
            name: "traceResolution".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: Some(Diagnostics::Log_paths_used_during_the_moduleResolution_process),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Compiler_Diagnostics),
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
            name: "diagnostics".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: Some(Diagnostics::Output_compiler_performance_information_after_building),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Compiler_Diagnostics),
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
            name: "extendedDiagnostics".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: Some(Diagnostics::Output_more_detailed_compiler_performance_information_after_building),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Compiler_Diagnostics),
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
        CommandLineOptionOfStringType::new(CommandLineOptionBase {
            name: "generateCpuProfile".to_string(),
            type_: CommandLineOptionType::String,
            is_file_path: Some(true),
            short_name: None,
            description: Some(Diagnostics::Emit_a_v8_CPU_profile_of_the_compiler_run_for_debugging),
            default_value_description: Some(StringOrDiagnosticMessage::String("profile.cpuprofile".to_string())),
            param_type: Some(Diagnostics::FILE_OR_DIRECTORY),
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Compiler_Diagnostics),
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
        CommandLineOptionOfStringType::new(CommandLineOptionBase {
            name: "generateTrace".to_string(),
            type_: CommandLineOptionType::String,
            is_file_path: Some(true),
            short_name: None,
            description: Some(Diagnostics::Generates_an_event_trace_and_a_list_of_types),
            default_value_description: None,
            param_type: Some(Diagnostics::DIRECTORY),
            is_tsconfig_only: None,
            is_command_line_only: Some(true),
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Compiler_Diagnostics),
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
            name: "incremental".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: Some("i".to_string()),
            description: Some(Diagnostics::Enable_incremental_compilation),
            default_value_description: Some(StringOrDiagnosticMessage::DiagnosticMessage(Diagnostics::false_unless_composite_is_set)),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Projects),
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
            name: "assumeChangesOnlyAffectDirectDependencies".to_string(),
            type_: CommandLineOptionType::Boolean,
            is_file_path: None,
            short_name: None,
            description: Some(Diagnostics::Have_recompiles_in_projects_that_use_incremental_and_watch_mode_assume_that_changes_within_a_file_will_only_affect_files_directly_depending_on_it),
            default_value_description: Some(StringOrDiagnosticMessage::String("false".to_string())),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: None,
            category: Some(Diagnostics::Watch_and_Build_Modes),
            strict_flag: None,
            affects_source_file: None,
            affects_module_resolution: None,
            affects_bind_diagnostics: None,
            affects_semantic_diagnostics: Some(true),
            affects_emit: Some(true),
            affects_program_structure: None,
            transpile_option_value: None,
        })
        .into(),
        CommandLineOptionOfStringType::new(CommandLineOptionBase {
            name: "locale".to_string(),
            type_: CommandLineOptionType::String,
            is_file_path: None,
            short_name: None,
            description: Some(Diagnostics::Set_the_language_of_the_messaging_from_TypeScript_This_does_not_affect_emit),
            default_value_description: Some(StringOrDiagnosticMessage::DiagnosticMessage(Diagnostics::Platform_specific)),
            param_type: None,
            is_tsconfig_only: None,
            is_command_line_only: Some(true),
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

pub(crate) static target_option_declaration: CommandLineOption /*CommandLineOptionOfCustomType*/ =
        CommandLineOptionOfCustomType::new(CommandLineOptionBase {
            name: "target".to_string(),
            type_: CommandLineOptionType::Map(
                HashMap::from_iter(IntoIter::new([
                    ("es3".to_string(), CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES3)),
                    ("es5".to_string(), CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES5)),
                    ("es6".to_string(), CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2015)),
                    ("es2015".to_string(), CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2015)),
                    ("es2016".to_string(), CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2016)),
                    ("es2017".to_string(), CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2017)),
                    ("es2018".to_string(), CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2018)),
                    ("es2019".to_string(), CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2019)),
                    ("es2020".to_string(), CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2020)),
                    ("es2021".to_string(), CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ES2021)),
                    ("esnext".to_string(), CommandLineOptionMapTypeValue::ScriptTarget(ScriptTarget::ESNext)),
                ]))
            ),
            is_file_path: None,
            short_name: Some("t".to_string()),
            description: Some(Diagnostics::Set_the_JavaScript_language_version_for_emitted_JavaScript_and_include_compatible_library_declarations),
            default_value_description: Some(StringOrDiagnosticMessage::String("ES3".to_string())),
            param_type: Some(Diagnostics::VERSION),
            is_tsconfig_only: None,
            is_command_line_only: None,
            show_in_simplified_help_view: Some(true),
            category: Some(Diagnostics::Language_and_Environment),
            strict_flag: None,
            affects_source_file: Some(true),
            affects_module_resolution: Some(true),
            affects_bind_diagnostics: None,
            affects_semantic_diagnostics: None,
            affects_emit: Some(true),
            affects_program_structure: None,
            transpile_option_value: None,
        }).into();

thread_local! {
    pub(crate) static semantic_diagnostics_option_declarations: Vec<CommandLineOption> =
        option_declarations.with(|option_declarations_| {
            option_declarations_
                .iter()
                .filter(|option| option.affects_semantic_diagnostics())
                .collect()
        });
}

thread_local! {
    pub(crate) static affects_emit_option_declarations: Vec<CommandLineOption> =
        option_declarations.with(|option_declarations_| {
            option_declarations_
                .iter()
                .filter(|option| option.affects_emit())
                .collect()
        });
}

thread_local! {
    pub(crate) static module_resolution_option_declarations: Vec<CommandLineOption> =
        option_declarations.with(|option_declarations_| {
            option_declarations_
                .iter()
                .filter(|option| option.affects_module_resolution())
                .collect()
        });
}

pub(crate) struct OptionsNameMap {
    pub options_name_map: HashMap<String, CommandLineOption>,
    pub short_option_names: HashMap<String, String>,
}

pub fn parse_command_line(command_line: &[String]) -> ParsedCommandLine {
    parse_command_line_worker(command_line)
}

fn parse_command_line_worker(command_line: &[String]) -> ParsedCommandLine {
    ParsedCommandLine {
        options: Rc::new(CompilerOptions {
            target: None,
            module: None,
        }),
        file_names: command_line.to_vec(),
    }
}
