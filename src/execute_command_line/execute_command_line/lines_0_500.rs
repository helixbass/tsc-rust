use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{
    create_watch_of_config_file, create_watch_of_files_and_compiler_options, perform_compilation,
    perform_incremental_compilation, report_watch_mode_without_sys_support, write_config_file,
    ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine,
};
use crate::{
    combine_paths, compare_strings_case_insensitive, contains, contains_rc,
    convert_to_options_with_absolute_paths, convert_to_tsconfig, create_compiler_diagnostic,
    create_diagnostic_reporter, file_extension_is, file_extension_is_one_of, filter,
    find_config_file, for_each, format_message, get_diagnostic_text, get_line_starts,
    get_normalized_absolute_path, is_incremental_compilation, is_watch_set, normalize_path,
    option_declarations, options_for_build, options_for_watch, pad_left, pad_right,
    parse_config_file_with_system, sort, string_contains, supported_js_extensions_flat,
    supported_ts_extensions_flat, validate_locale_and_set_language, version, BuildOptions,
    CommandLineOption, CommandLineOptionInterface, CommandLineOptionType, CompilerOptions,
    DiagnosticMessage, DiagnosticReporter, Diagnostics, ExitStatus, ExtendedConfigCacheEntry,
    Extension, Node, ParsedCommandLine, Program, StringOrDiagnosticMessage, System,
    TypeCheckerHost,
};

pub(super) struct Statistic {
    pub name: String,
    pub value: String,
}

pub(super) fn count_lines(program: &Program) -> HashMap<&'static str, usize> {
    let mut counts = get_counts_map();
    for_each(&*program.get_source_files(), |file, _| {
        let key = get_count_key(program, file);
        let line_count = get_line_starts(file.as_source_file()).len();
        counts.insert(key, *counts.get(key).unwrap() + line_count);
        Option::<()>::None
    });
    counts
}

pub(super) fn count_nodes(program: &Program) -> HashMap<&'static str, usize> {
    let mut counts = get_counts_map();
    for_each(&*program.get_source_files(), |file, _| {
        let key = get_count_key(program, file);
        let file_as_source_file = file.as_source_file();
        let line_count = get_line_starts(file_as_source_file).len();
        counts.insert(
            key,
            *counts.get(key).unwrap() + file_as_source_file.node_count(),
        );
        Option::<()>::None
    });
    counts
}

pub(super) fn get_counts_map() -> HashMap<&'static str, usize> {
    let mut counts = HashMap::new();
    counts.insert("Library", 0);
    counts.insert("Definitions", 0);
    counts.insert("TypeScript", 0);
    counts.insert("JavaScript", 0);
    counts.insert("JSON", 0);
    counts.insert("Other", 0);
    counts
}

pub(super) fn get_count_key(program: &Program, file: &Node /*SourceFile*/) -> &'static str {
    let file_as_source_file = file.as_source_file();
    if program.is_source_file_default_library(file) {
        return "Library";
    } else if file_as_source_file.is_declaration_file() {
        return "Definitions";
    }

    let path = file_as_source_file.path();
    if file_extension_is_one_of(
        &path,
        &supported_ts_extensions_flat
            .iter()
            .map(|extension| extension.to_str())
            .collect::<Vec<_>>(),
    ) {
        "TypeScript"
    } else if file_extension_is_one_of(
        &path,
        &supported_js_extensions_flat
            .iter()
            .map(|extension| extension.to_str())
            .collect::<Vec<_>>(),
    ) {
        "JavaScript"
    } else if file_extension_is(&path, Extension::Json.to_str()) {
        "JSON"
    } else {
        "Other"
    }
}

pub(super) fn update_report_diagnostic(
    sys: Rc<dyn System>,
    existing: Rc<dyn DiagnosticReporter>,
    options: CompilerOptionsOrBuildOptions,
) -> Rc<dyn DiagnosticReporter> {
    if should_be_pretty(&*sys, options) {
        create_diagnostic_reporter(sys, Some(true))
    } else {
        existing
    }
}

pub(super) enum CompilerOptionsOrBuildOptions {
    CompilerOptions(Rc<CompilerOptions>),
    BuildOptions(Rc<BuildOptions>),
}

impl CompilerOptionsOrBuildOptions {
    pub fn pretty(&self) -> Option<bool> {
        match self {
            Self::CompilerOptions(options) => options.pretty,
            Self::BuildOptions(options) => options.pretty,
        }
    }
}

impl From<Rc<CompilerOptions>> for CompilerOptionsOrBuildOptions {
    fn from(value: Rc<CompilerOptions>) -> Self {
        Self::CompilerOptions(value)
    }
}

impl From<Rc<BuildOptions>> for CompilerOptionsOrBuildOptions {
    fn from(value: Rc<BuildOptions>) -> Self {
        Self::BuildOptions(value)
    }
}

pub(super) fn default_is_pretty(sys: &dyn System) -> bool {
    matches!(sys.write_output_is_tty(), Some(true))
        && sys.get_environment_variable("NO_COLOR").is_empty()
}

pub(super) fn should_be_pretty(sys: &dyn System, options: CompilerOptionsOrBuildOptions) -> bool {
    if
    /* !options || */
    options.pretty().is_none() {
        return default_is_pretty(sys);
    }
    options.pretty().unwrap()
}

pub(super) fn get_options_for_help(command_line: &ParsedCommandLine) -> Vec<Rc<CommandLineOption>> {
    option_declarations.with(|option_declarations_| {
        if matches!(command_line.options.all, Some(true)) {
            sort(option_declarations_, |a, b| {
                compare_strings_case_insensitive(a.name(), b.name())
            })
            .to_vec()
        } else {
            filter(option_declarations_, |v: &Rc<CommandLineOption>| {
                v.show_in_simplified_help_view()
            })
        }
    })
}

pub(super) fn print_version(sys: &dyn System) {
    sys.write(&format!(
        "{}{}",
        get_diagnostic_text(&Diagnostics::Version_0, Some(vec![version.to_owned()])),
        sys.new_line()
    ));
}

pub(super) fn create_colors(sys: &dyn System) -> Rc<dyn Colors> {
    let show_colors = default_is_pretty(sys);
    if !show_colors {
        return Rc::new(ColorsPassthrough::new());
    }

    let is_windows = !sys.get_environment_variable("OS").is_empty()
        && string_contains(
            &sys.get_environment_variable("OS").to_lowercase(),
            "windows",
        );
    let is_windows_terminal = !sys.get_environment_variable("WT_SESSION").is_empty();
    let is_vs_code = !sys.get_environment_variable("TERM_PROGRAM").is_empty()
        && sys.get_environment_variable("TERM_PROGRAM") == "vscode";

    let supports_richer_colors = sys.get_environment_variable("COLORTERM") == "truecolor"
        || sys.get_environment_variable("TERM") == "xterm-256color";

    Rc::new(ColorsConcrete::new(
        is_windows,
        is_windows_terminal,
        is_vs_code,
        supports_richer_colors,
    ))
}

pub(super) trait Colors {
    fn bold(&self, str_: &str) -> String;
    fn blue(&self, str_: &str) -> String;
    fn blue_background(&self, str_: &str) -> String;
    fn bright_white(&self, str_: &str) -> String;
}

pub(super) struct ColorsPassthrough {}

impl ColorsPassthrough {
    pub fn new() -> Self {
        Self {}
    }
}

impl Colors for ColorsPassthrough {
    fn bold(&self, str_: &str) -> String {
        str_.to_owned()
    }

    fn blue(&self, str_: &str) -> String {
        str_.to_owned()
    }

    fn blue_background(&self, str_: &str) -> String {
        str_.to_owned()
    }

    fn bright_white(&self, str_: &str) -> String {
        str_.to_owned()
    }
}

pub(super) struct ColorsConcrete {
    is_windows: bool,
    is_windows_terminal: bool,
    is_vs_code: bool,
    supports_richer_colors: bool,
}

impl ColorsConcrete {
    pub fn new(
        is_windows: bool,
        is_windows_terminal: bool,
        is_vs_code: bool,
        supports_richer_colors: bool,
    ) -> Self {
        Self {
            is_windows,
            is_windows_terminal,
            is_vs_code,
            supports_richer_colors,
        }
    }
}

impl Colors for ColorsConcrete {
    fn bold(&self, str_: &str) -> String {
        format!("\x1b[1m{}\x1b[22m", str_)
    }

    fn blue(&self, str_: &str) -> String {
        if self.is_windows && !self.is_windows_terminal && !self.is_vs_code {
            return self.bright_white(str_);
        }

        format!("\x1b[94m{}\x1b[39m", str_)
    }

    fn blue_background(&self, str_: &str) -> String {
        if self.supports_richer_colors {
            format!("\x1B[48;5;68m{}\x1B[39;49m", str_)
        } else {
            format!("\x1b[44m{}\x1B[39;49m", str_)
        }
    }

    fn bright_white(&self, str_: &str) -> String {
        format!("\x1b[97m{}\x1b[39m", str_)
    }
}

pub(super) fn get_display_name_text_of_option(option: &CommandLineOption) -> String {
    format!(
        "--{}{}",
        option.name(),
        match option.maybe_short_name() {
            Some(short_name) => format!(", -{}", short_name),
            None => "".to_owned(),
        }
    )
}

pub(super) fn generate_option_output(
    sys: &dyn System,
    option: &CommandLineOption,
    right_align_of_left: usize,
    left_align_of_right: usize,
) -> Vec<String> {
    let mut text = vec![];
    let colors = create_colors(sys);

    let name = get_display_name_text_of_option(option);

    let value_candidates = get_value_candidate(option);
    let default_value_description = match option.maybe_default_value_description() {
        Some(StringOrDiagnosticMessage::DiagnosticMessage(default_value_description)) => {
            Some(get_diagnostic_text(default_value_description, None))
        }
        Some(StringOrDiagnosticMessage::String(default_value_description)) => {
            Some(default_value_description.clone())
        }
        None => None,
    };
    let terminal_width = sys.get_width_of_terminal().unwrap_or(0);

    if terminal_width >= 80 {
        let mut description = "".to_owned();
        if let Some(option_description) = option.maybe_description() {
            description = get_diagnostic_text(option_description, None);
        }
        text.append(&mut get_pretty_output(
            &*colors,
            &name,
            &description,
            right_align_of_left,
            left_align_of_right,
            terminal_width,
            true,
        ));
        text.push(sys.new_line().to_owned());
        if show_additional_info_output(value_candidates.as_ref(), option) {
            if let Some(value_candidates) = value_candidates {
                text.append(&mut get_pretty_output(
                    &*colors,
                    &value_candidates.value_type,
                    &value_candidates.possible_values,
                    right_align_of_left,
                    left_align_of_right,
                    terminal_width,
                    false,
                ));
                text.push(sys.new_line().to_owned());
            }
            if let Some(default_value_description) = default_value_description {
                text.append(&mut get_pretty_output(
                    &*colors,
                    &get_diagnostic_text(&Diagnostics::default_Colon, None),
                    &default_value_description,
                    right_align_of_left,
                    left_align_of_right,
                    terminal_width,
                    false,
                ));
                text.push(sys.new_line().to_owned());
            }
        }
        text.push(sys.new_line().to_owned());
    } else {
        text.push(colors.blue(&name));
        text.push(sys.new_line().to_owned());
        if let Some(option_description) = option.maybe_description() {
            let description = get_diagnostic_text(option_description, None);
            text.push(description);
        }
        text.push(sys.new_line().to_owned());
        if show_additional_info_output(value_candidates.as_ref(), option) {
            if let Some(value_candidates) = value_candidates.as_ref() {
                text.push(format!(
                    "{} {}",
                    value_candidates.value_type, value_candidates.possible_values
                ));
            }
            if let Some(default_value_description) = default_value_description {
                if value_candidates.is_some() {
                    text.push(sys.new_line().to_owned());
                }
                let diag_type = get_diagnostic_text(&Diagnostics::default_Colon, None);
                text.push(format!("{} {}", diag_type, default_value_description));
            }

            text.push(sys.new_line().to_owned());
        }
        text.push(sys.new_line().to_owned());
    }
    text
}

pub(super) struct ValueCandidate {
    pub value_type: String,
    pub possible_values: String,
}

impl ValueCandidate {
    pub fn new(value_type: String, possible_values: String) -> Self {
        Self {
            value_type,
            possible_values,
        }
    }
}

pub(super) fn show_additional_info_output(
    value_candidates: Option<&ValueCandidate>,
    option: &CommandLineOption,
) -> bool {
    let ignore_values = vec!["string"];
    let ignored_descriptions = vec![None, Some("false"), Some("n/a")];
    let default_value_description = &option.maybe_default_value_description();
    if matches!(
        option.maybe_category(),
        Some(category) if ptr::eq(category, &*Diagnostics::Command_line_Options)
    ) {
        return false;
    }

    if matches!(value_candidates, Some(value_candidates) if contains(Some(&ignore_values), &&*value_candidates.possible_values))
        && contains(
            Some(&ignored_descriptions),
            &default_value_description
                .and_then(
                    |default_value_description| match default_value_description {
                        StringOrDiagnosticMessage::DiagnosticMessage(_) => None,
                        StringOrDiagnosticMessage::String(default_value_description) => {
                            Some(default_value_description)
                        }
                    },
                )
                .map(|default_value_description| &**default_value_description),
        )
    {
        return false;
    }
    true
}

pub(super) fn get_pretty_output(
    colors: &dyn Colors,
    left: &str,
    right: &str,
    right_align_of_left: usize,
    left_align_of_right: usize,
    terminal_width: usize,
    color_left: bool,
) -> Vec<String> {
    let mut res = vec![];
    let mut is_first_line = true;
    let mut remain_right = right;
    let right_character_number = terminal_width - left_align_of_right;
    while !remain_right.is_empty() {
        let mut cur_left = "".to_owned();
        if is_first_line {
            cur_left = pad_left(left, right_align_of_left, None).into_owned();
            cur_left = pad_right(&cur_left, left_align_of_right).into_owned();
            cur_left = if color_left {
                colors.blue(&cur_left)
            } else {
                cur_left
            };
        } else {
            cur_left = pad_left("", left_align_of_right, None).into_owned();
        }

        // TODO: is it a problem that this is using string bytes vs chars?
        // let cur_right = text_substring(remain_right, 0, right_character_number);
        let cur_right = &remain_right[0..right_character_number];
        remain_right = &remain_right[right_character_number..];
        res.push(format!("{}{}", cur_left, cur_right));
        is_first_line = false;
    }
    res
}

pub(super) fn get_value_candidate(option: &CommandLineOption) -> Option<ValueCandidate> {
    if matches!(option.type_(), CommandLineOptionType::Object) {
        return None;
    }

    Some(ValueCandidate::new(
        get_value_type(option),
        get_possible_values(option),
    ))
}

pub(super) fn get_value_type(option: &CommandLineOption) -> String {
    match option.type_() {
        CommandLineOptionType::String
        | CommandLineOptionType::Number
        | CommandLineOptionType::Boolean => get_diagnostic_text(&Diagnostics::type_Colon, None),
        CommandLineOptionType::List => get_diagnostic_text(&Diagnostics::one_or_more_Colon, None),
        _ => get_diagnostic_text(&Diagnostics::one_of_Colon, None),
    }
}

pub(super) fn get_possible_values(option: &CommandLineOption) -> String {
    match option.type_() {
        CommandLineOptionType::String
        | CommandLineOptionType::Number
        | CommandLineOptionType::Boolean => option.type_().as_str().to_owned(),
        CommandLineOptionType::List => {
            get_possible_values(&option.as_command_line_option_of_list_type().element)
        }
        CommandLineOptionType::Object => "".to_owned(),
        CommandLineOptionType::Map(type_) => {
            let keys = /*arrayFrom(*/type_.keys().map(|key| *key).collect::<Vec<_>>()/*)*/;
            keys.join(", ")
        }
    }
}

pub(super) fn generate_group_option_output(
    sys: &dyn System,
    options_list: &[Rc<CommandLineOption>],
) -> Vec<String> {
    let mut max_length = 0;
    for option in options_list {
        let cur_length = get_display_name_text_of_option(option).len();
        max_length = if max_length > cur_length {
            max_length
        } else {
            cur_length
        };
    }

    let right_align_of_left_part = max_length + 2;
    let left_align_of_right_part = right_align_of_left_part + 2;
    let mut lines = vec![];
    for option in options_list {
        let mut tmp = generate_option_output(
            sys,
            option,
            right_align_of_left_part,
            left_align_of_right_part,
        );
        lines.append(&mut tmp);
    }
    if lines[lines.len() - 2] != sys.new_line() {
        lines.push(sys.new_line().to_owned());
    }
    lines
}

pub(super) fn generate_section_options_output(
    sys: &dyn System,
    section_name: &str,
    options: &[Rc<CommandLineOption>],
    sub_category: bool,
    before_options_description: Option<&str>,
    after_options_description: Option<&str>,
) -> Vec<String> {
    let mut res = vec![];
    res.push(format!(
        "{}{}{}",
        create_colors(sys).bold(section_name),
        sys.new_line(),
        sys.new_line()
    ));
    if let Some(before_options_description) = before_options_description {
        res.push(format!(
            "{}{}{}",
            before_options_description,
            sys.new_line(),
            sys.new_line()
        ));
    }
    if !sub_category {
        res.append(&mut generate_group_option_output(sys, options));
        if let Some(after_options_description) = after_options_description {
            res.push(format!(
                "{}{}{}",
                after_options_description,
                sys.new_line(),
                sys.new_line()
            ));
        }
        return res;
    }
    let mut category_map = HashMap::new();
    for option in options {
        if option.maybe_category().is_none() {
            continue;
        }
        let cur_category = get_diagnostic_text(option.maybe_category().unwrap(), None);
        let options_of_cur_category = category_map.entry(cur_category).or_insert_with(|| vec![]);
        options_of_cur_category.push(option.clone());
    }
    for (key, value) in category_map {
        res.push(format!("### {}{}{}", key, sys.new_line(), sys.new_line()));
        res.append(&mut generate_group_option_output(sys, &value));
    }
    if let Some(after_options_description) = after_options_description {
        res.push(format!(
            "{}{}{}",
            after_options_description,
            sys.new_line(),
            sys.new_line()
        ));
    }
    res
}

pub(super) fn print_easy_help(sys: &dyn System, simple_options: &[Rc<CommandLineOption>]) {
    let colors = create_colors(sys);
    let mut output = get_header(
        sys,
        &format!(
            "{} - {}",
            get_diagnostic_text(&Diagnostics::tsc_Colon_The_TypeScript_Compiler, None),
            get_diagnostic_text(&Diagnostics::Version_0, Some(vec![version.to_owned()]))
        ),
    );
    output.push(format!(
        "{}{}{}",
        colors.bold(&get_diagnostic_text(&Diagnostics::COMMON_COMMANDS, None)),
        sys.new_line(),
        sys.new_line()
    ));

    example(
        &mut output,
        &*colors,
        sys,
        &vec!["tsc"],
        &Diagnostics::Compiles_the_current_project_tsconfig_json_in_the_working_directory,
    );
    example(&mut output, &*colors, sys, &vec!["tsc app.ts util.ts"], &Diagnostics::Ignoring_tsconfig_json_compiles_the_specified_files_with_default_compiler_options);
    example(
        &mut output,
        &*colors,
        sys,
        &vec!["tsc -b"],
        &Diagnostics::Build_a_composite_project_in_the_working_directory,
    );
    example(&mut output, &*colors, sys, &vec!["tsc --init"], &Diagnostics::Creates_a_tsconfig_json_with_the_recommended_settings_in_the_working_directory);
    example(
        &mut output,
        &*colors,
        sys,
        &vec!["tsc -p ./path/to/tsconfig.json"],
        &Diagnostics::Compiles_the_TypeScript_project_located_at_the_specified_path,
    );
    example(
        &mut output,
        &*colors,
        sys,
        &vec!["tsc --help --all"],
        &Diagnostics::An_expanded_version_of_this_information_showing_all_possible_compiler_options,
    );
    example(
        &mut output,
        &*colors,
        sys,
        &vec!["tsc --noEmit", "tsc --target esnext"],
        &Diagnostics::Compiles_the_current_project_with_additional_settings,
    );

    let cli_commands = simple_options.iter().filter(|opt| opt.is_command_line_only() || matches!(opt.maybe_category(), Some(category) if category == &*Diagnostics::Command_line_Options)).map(Clone::clone).collect::<Vec<_>>();
    let config_opts = simple_options
        .iter()
        .filter(|opt| !contains_rc(Some(&cli_commands), opt))
        .map(Clone::clone)
        .collect::<Vec<_>>();

    output.append(&mut generate_section_options_output(
        sys,
        &get_diagnostic_text(&Diagnostics::COMMAND_LINE_FLAGS, None),
        &cli_commands,
        false,
        None,
        None,
    ));
    output.append(&mut generate_section_options_output(
        sys,
        &get_diagnostic_text(&Diagnostics::COMMON_COMPILER_OPTIONS, None),
        &config_opts,
        false,
        None,
        Some(&format_message(
            None,
            &Diagnostics::You_can_learn_about_all_of_the_compiler_options_at_0,
            Some(vec!["https://aka.ms/tsconfig-reference".to_owned()]),
        )),
    ));

    for line in output {
        sys.write(&line);
    }
}

pub(super) fn example(
    output: &mut Vec<String>,
    colors: &dyn Colors,
    sys: &dyn System,
    ex: &[&str],
    desc: &DiagnosticMessage,
) {
    let examples = ex;
    for example in examples {
        output.push(format!("  {}{}", colors.blue(example), sys.new_line()));
    }
    output.push(format!(
        "  {}{}{}",
        get_diagnostic_text(desc, None),
        sys.new_line(),
        sys.new_line()
    ));
}

pub(super) fn print_all_help(
    sys: &dyn System,
    compiler_options: &[Rc<CommandLineOption>],
    build_options: &[Rc<CommandLineOption>],
    watch_options: &[Rc<CommandLineOption>],
) {
    let mut output = get_header(
        sys,
        &format!(
            "{} - {}",
            get_diagnostic_text(&Diagnostics::tsc_Colon_The_TypeScript_Compiler, None),
            get_diagnostic_text(&Diagnostics::Version_0, Some(vec![version.to_owned()]))
        ),
    );
    output.append(&mut generate_section_options_output(
        sys,
        &get_diagnostic_text(&Diagnostics::ALL_COMPILER_OPTIONS, None),
        compiler_options,
        true,
        None,
        Some(&format_message(
            None,
            &Diagnostics::You_can_learn_about_all_of_the_compiler_options_at_0,
            Some(vec!["https://aka.ms/tsconfig-reference".to_owned()]),
        )),
    ));
    output.append(&mut generate_section_options_output(
        sys,
        &get_diagnostic_text(&Diagnostics::WATCH_OPTIONS, None),
        watch_options,
        false,
        Some(&get_diagnostic_text(&Diagnostics::Including_watch_w_will_start_watching_the_current_project_for_the_file_changes_Once_set_you_can_config_watch_mode_with_Colon, None)),
        None
    ));
    output.append(&mut generate_section_options_output(
        sys,
        &get_diagnostic_text(&Diagnostics::BUILD_OPTIONS, None),
        build_options,
        false,
        Some(&format_message(
            None,
            &Diagnostics::Using_build_b_will_make_tsc_behave_more_like_a_build_orchestrator_than_a_compiler_This_is_used_to_trigger_building_composite_projects_which_you_can_learn_more_about_at_0,
            Some(vec!["https://aka.ms/tsc-composite-builds".to_owned()]),
        )),
        None
    ));
    for line in output {
        sys.write(&line);
    }
}

pub(super) fn print_build_help(sys: &dyn System, build_options: &[Rc<CommandLineOption>]) {
    let mut output = get_header(
        sys,
        &format!(
            "{} - {}",
            get_diagnostic_text(&Diagnostics::tsc_Colon_The_TypeScript_Compiler, None),
            get_diagnostic_text(&Diagnostics::Version_0, Some(vec![version.to_owned()]))
        ),
    );
    output.append(&mut generate_section_options_output(
        sys,
        &get_diagnostic_text(&Diagnostics::BUILD_OPTIONS, None),
        build_options,
        false,
        Some(&format_message(
            None,
            &Diagnostics::Using_build_b_will_make_tsc_behave_more_like_a_build_orchestrator_than_a_compiler_This_is_used_to_trigger_building_composite_projects_which_you_can_learn_more_about_at_0,
            Some(vec!["https://aka.ms/tsc-composite-builds".to_owned()]),
        )),
        None
    ));
    for line in output {
        sys.write(&line);
    }
}

pub(super) fn get_header(sys: &dyn System, message: &str) -> Vec<String> {
    let colors = create_colors(sys);
    let mut header = vec![];
    let terminal_width = sys.get_width_of_terminal().unwrap_or(0);
    let ts_icon_length = 5;

    let ts_icon_first_line = colors.blue_background(&pad_left("", ts_icon_length, None));
    let ts_icon_second_line =
        colors.blue_background(&colors.bright_white(&pad_left("TS ", ts_icon_length, None)));
    if terminal_width >= message.len() + ts_icon_length {
        let right_align = if terminal_width > 120 {
            120
        } else {
            terminal_width
        };
        let left_align = right_align - ts_icon_length;
        header.push(format!(
            "{}{}{}",
            pad_right(message, left_align),
            ts_icon_first_line,
            sys.new_line()
        ));
        header.push(format!(
            "{}{}{}",
            pad_left("", left_align, None),
            ts_icon_second_line,
            sys.new_line()
        ));
    } else {
        header.push(format!("{}{}", message, sys.new_line()));
        header.push(sys.new_line().to_owned());
    }
    header
}

pub(super) fn print_help(sys: &dyn System, command_line: &ParsedCommandLine) {
    if !matches!(command_line.options.all, Some(true)) {
        print_easy_help(sys, &get_options_for_help(command_line));
    } else {
        options_for_build.with(|options_for_build_| {
            options_for_watch.with(|options_for_watch_| {
                print_all_help(
                    sys,
                    &get_options_for_help(command_line),
                    &options_for_build_,
                    &options_for_watch_,
                )
            })
        });
    }
}

pub(super) fn execute_command_line_worker<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    sys: Rc<dyn System>,
    cb: &mut TCallback,
    command_line: &mut ParsedCommandLine,
) {
    let mut report_diagnostic = create_diagnostic_reporter(sys.clone(), None);
    if matches!(command_line.options.build, Some(true)) {
        report_diagnostic.call(Rc::new(
            create_compiler_diagnostic(
                &Diagnostics::Option_build_must_be_the_first_command_line_argument,
                None,
            )
            .into(),
        ));
        sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
    }

    let mut config_file_name: Option<String> = None;
    if let Some(command_line_options_locale) = command_line.options.locale.as_ref() {
        if !command_line_options_locale.is_empty() {
            validate_locale_and_set_language(
                command_line_options_locale,
                &*sys,
                Some(&mut command_line.errors),
            );
        }
    }

    if !command_line.errors.is_empty() {
        command_line
            .errors
            .iter()
            .for_each(|error| report_diagnostic.call(error.clone()));
        sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
    }

    if matches!(command_line.options.init, Some(true)) {
        write_config_file(
            &*sys,
            &*report_diagnostic,
            &command_line.options,
            &command_line.file_names,
        );
        sys.exit(Some(ExitStatus::Success));
    }

    if matches!(command_line.options.version, Some(true)) {
        print_version(&*sys);
        sys.exit(Some(ExitStatus::Success));
    }

    if matches!(command_line.options.help, Some(true))
        || matches!(command_line.options.all, Some(true))
    {
        print_help(&*sys, &command_line);
        sys.exit(Some(ExitStatus::Success));
    }

    if matches!(command_line.options.watch, Some(true))
        && matches!(command_line.options.list_files_only, Some(true))
    {
        report_diagnostic.call(Rc::new(
            create_compiler_diagnostic(
                &Diagnostics::Options_0_and_1_cannot_be_combined,
                Some(vec!["watch".to_owned(), "listFilesOnly".to_owned()]),
            )
            .into(),
        ));
        sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
    }

    if let Some(command_line_options_project) = command_line.options.project.as_ref() {
        if !command_line.file_names.is_empty() {
            report_diagnostic.call(Rc::new(
                create_compiler_diagnostic(
                    &Diagnostics::Option_project_cannot_be_mixed_with_source_files_on_a_command_line,
                    None
                )
                .into(),
            ));
            sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
        }

        let file_or_directory = normalize_path(command_line_options_project);
        if file_or_directory.is_empty() || sys.directory_exists(&file_or_directory) {
            config_file_name = Some(combine_paths(
                &file_or_directory,
                &vec![Some("tsconfig.json")],
            ));
            if !sys.file_exists(config_file_name.as_ref().unwrap()) {
                report_diagnostic.call(Rc::new(
                    create_compiler_diagnostic(
                        &Diagnostics::Cannot_find_a_tsconfig_json_file_at_the_specified_directory_Colon_0,
                        Some(vec![command_line_options_project.to_owned()])
                    )
                    .into(),
                ));
                sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
            }
        } else {
            config_file_name = Some(file_or_directory);
            if !sys.file_exists(config_file_name.as_ref().unwrap()) {
                report_diagnostic.call(Rc::new(
                    create_compiler_diagnostic(
                        &Diagnostics::The_specified_path_does_not_exist_Colon_0,
                        Some(vec![command_line_options_project.to_owned()]),
                    )
                    .into(),
                ));
                sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
            }
        }
    } else if command_line.file_names.is_empty() {
        let search_path = normalize_path(&sys.get_current_directory());
        config_file_name =
            find_config_file(&search_path, |file_name| sys.file_exists(file_name), None);
    }

    if command_line.file_names.is_empty() && config_file_name.is_none() {
        if matches!(command_line.options.show_config, Some(true)) {
            report_diagnostic.call(Rc::new(
                create_compiler_diagnostic(
                    &Diagnostics::Cannot_find_a_tsconfig_json_file_at_the_current_directory_Colon_0,
                    Some(vec![normalize_path(&sys.get_current_directory())]),
                )
                .into(),
            ));
        } else {
            print_version(&*sys);
            print_help(&*sys, &command_line);
        }
        sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
    }

    let current_directory = sys.get_current_directory();
    let command_line_options =
        convert_to_options_with_absolute_paths(command_line.options.clone(), |file_name| {
            get_normalized_absolute_path(file_name, Some(&current_directory))
        });

    if let Some(config_file_name) = config_file_name {
        let mut extended_config_cache: HashMap<String, ExtendedConfigCacheEntry> = HashMap::new();
        let config_parse_result = Rc::new(
            parse_config_file_with_system(
                &config_file_name,
                command_line_options.clone(),
                Some(&mut extended_config_cache),
                command_line.watch_options.clone(),
                sys.clone(),
                report_diagnostic.clone(),
            )
            .unwrap(),
        );
        if matches!(command_line.options.show_config, Some(true)) {
            if !config_parse_result.errors.is_empty() {
                report_diagnostic = update_report_diagnostic(
                    sys.clone(),
                    report_diagnostic,
                    config_parse_result.options.clone().into(),
                );
                config_parse_result
                    .errors
                    .iter()
                    .for_each(|error| report_diagnostic.call(error.clone()));
                sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
            }
            sys.write(&format!(
                "{}{}",
                serde_json::to_string_pretty(&convert_to_tsconfig(
                    &config_parse_result,
                    &config_file_name,
                    sys.as_convert_to_tsconfig_host()
                ))
                .unwrap(),
                sys.new_line()
            ));
            sys.exit(Some(ExitStatus::Success));
        }
        report_diagnostic = update_report_diagnostic(
            sys.clone(),
            report_diagnostic,
            config_parse_result.options.clone().into(),
        );
        if is_watch_set(&config_parse_result.options) {
            if report_watch_mode_without_sys_support(&*sys, &*report_diagnostic) {
                return;
            }
            create_watch_of_config_file(
                sys.clone(),
                cb,
                report_diagnostic,
                config_parse_result,
                command_line_options,
                command_line.watch_options.clone(),
                extended_config_cache,
            );
        } else if is_incremental_compilation(&config_parse_result.options) {
            perform_incremental_compilation(
                sys.clone(),
                cb,
                report_diagnostic,
                &config_parse_result,
            );
        } else {
            perform_compilation(sys, cb, report_diagnostic, &config_parse_result);
        }
    } else {
        if matches!(command_line.options.show_config, Some(true)) {
            sys.write(&format!(
                "{}{}",
                serde_json::to_string_pretty(&convert_to_tsconfig(
                    &command_line,
                    &combine_paths(&current_directory, &vec![Some("tsconfig.json")]),
                    sys.as_convert_to_tsconfig_host()
                ))
                .unwrap(),
                sys.new_line()
            ));
            sys.exit(Some(ExitStatus::Success));
        }
        report_diagnostic = update_report_diagnostic(
            sys.clone(),
            report_diagnostic,
            command_line_options.clone().into(),
        );
        if is_watch_set(&command_line_options) {
            if report_watch_mode_without_sys_support(&*sys, &*report_diagnostic) {
                return;
            }
            create_watch_of_files_and_compiler_options(
                &*sys,
                cb,
                report_diagnostic,
                &command_line.file_names,
                command_line_options,
                command_line.watch_options.clone(),
            );
        } else if is_incremental_compilation(&command_line_options) {
            command_line.options = command_line_options;
            perform_incremental_compilation(sys, cb, report_diagnostic, &command_line);
        } else {
            command_line.options = command_line_options;
            perform_compilation(sys, cb, report_diagnostic, &command_line);
        }
    }
}
