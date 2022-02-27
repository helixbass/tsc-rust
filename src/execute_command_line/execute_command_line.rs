use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    compare_strings_case_insensitive, contains, create_diagnostic_reporter, create_program,
    emit_files_and_report_errors_and_get_exit_status, file_extension_is, file_extension_is_one_of,
    filter, for_each, get_diagnostic_text, get_line_starts, option_declarations, pad_left,
    pad_right, parse_command_line, sort, string_contains, supported_js_extensions_flat,
    supported_ts_extensions_flat, text_substring, version, BuildOptions, CommandLineOption,
    CommandLineOptionInterface, CommandLineOptionType, CompilerOptions, CreateProgramOptions,
    DiagnosticReporter, Diagnostics, EmitAndSemanticDiagnosticsBuilderProgram, Extension, Node,
    ParsedCommandLine, Program, StringOrDiagnosticMessage, System, TypeCheckerHost,
};

struct Statistic {
    pub name: String,
    pub value: String,
}

fn count_lines(program: &Program) -> HashMap<&'static str, usize> {
    let mut counts = get_counts_map();
    for_each(program.get_source_files(), |file, _| {
        let key = get_count_key(program, file);
        let line_count = get_line_starts(file.as_source_file()).len();
        counts.insert(key, *counts.get(key).unwrap() + line_count);
        Option::<()>::None
    });
    counts
}

fn count_nodes(program: &Program) -> HashMap<&'static str, usize> {
    let mut counts = get_counts_map();
    for_each(program.get_source_files(), |file, _| {
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

fn get_counts_map() -> HashMap<&'static str, usize> {
    let mut counts = HashMap::new();
    counts.insert("Library", 0);
    counts.insert("Definitions", 0);
    counts.insert("TypeScript", 0);
    counts.insert("JavaScript", 0);
    counts.insert("JSON", 0);
    counts.insert("Other", 0);
    counts
}

fn get_count_key(program: &Program, file: &Node /*SourceFile*/) -> &'static str {
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

fn update_report_diagnostic(
    sys: &dyn System,
    existing: Rc<dyn DiagnosticReporter>,
    options: CompilerOptionsOrBuildOptions,
) -> Rc<dyn DiagnosticReporter> {
    if should_be_pretty(sys, options) {
        create_diagnostic_reporter(sys, Some(true))
    } else {
        existing
    }
}

enum CompilerOptionsOrBuildOptions {
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

fn default_is_pretty(sys: &dyn System) -> bool {
    matches!(sys.write_output_is_tty(), Some(true))
        && sys.get_environment_variable("NO_COLOR").is_empty()
}

fn should_be_pretty(sys: &dyn System, options: CompilerOptionsOrBuildOptions) -> bool {
    if
    /* !options || */
    options.pretty().is_none() {
        return default_is_pretty(sys);
    }
    options.pretty().unwrap()
}

fn get_options_for_help(command_line: &ParsedCommandLine) -> Vec<Rc<CommandLineOption>> {
    option_declarations.with(|option_declarations_| {
        if matches!(command_line.options.all, Some(true)) {
            sort(option_declarations_, |a, b| {
                compare_strings_case_insensitive(a.name(), b.name())
            })
            .to_vec()
        } else {
            filter(Some(&*option_declarations_), |v| {
                v.show_in_simplified_help_view()
            })
            .unwrap()
        }
    })
}

fn print_version(sys: &dyn System) {
    sys.write(&format!(
        "{}{}",
        get_diagnostic_text(&Diagnostics::Version_0, Some(vec![version.to_owned()])),
        sys.new_line()
    ));
}

fn create_colors(sys: &dyn System) -> Rc<dyn Colors> {
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

trait Colors {
    fn bold(&self, str_: &str) -> String;
    fn blue(&self, str_: &str) -> String;
    fn blue_background(&self, str_: &str) -> String;
    fn bright_white(&self, str_: &str) -> String;
}

struct ColorsPassthrough {}

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

struct ColorsConcrete {
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

fn get_display_name_text_of_option(option: &CommandLineOption) -> String {
    format!(
        "--{}{}",
        option.name(),
        match option.maybe_short_name() {
            Some(short_name) => format!(", -{}", short_name),
            None => "".to_owned(),
        }
    )
}

fn generate_option_output(
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

struct ValueCandidate {
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

fn show_additional_info_output(
    value_candidates: Option<&ValueCandidate>,
    option: &CommandLineOption,
) -> bool {
    let ignore_values = vec!["string"];
    let ignored_descriptions = vec![None, Some("false"), Some("n/a")];
    let default_value_description = &option.maybe_default_value_description();
    if matches!(
        option.maybe_category(),
        Some(&Diagnostics::Command_line_Options)
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

fn get_pretty_output(
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

fn get_value_candidate(option: &CommandLineOption) -> Option<ValueCandidate> {
    if matches!(option.type_(), CommandLineOptionType::Object) {
        return None;
    }

    Some(ValueCandidate::new(
        get_value_type(option),
        get_possible_values(option),
    ))
}

fn get_value_type(option: &CommandLineOption) -> String {
    match option.type_() {
        CommandLineOptionType::String
        | CommandLineOptionType::Number
        | CommandLineOptionType::Boolean => get_diagnostic_text(&Diagnostics::type_Colon, None),
        CommandLineOptionType::List => get_diagnostic_text(&Diagnostics::one_or_more_Colon, None),
        _ => get_diagnostic_text(&Diagnostics::one_of_Colon, None),
    }
}

fn get_possible_values(option: &CommandLineOption) -> String {
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

fn generate_group_option_output(
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

fn generate_section_options_output(
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
        let options_of_cur_category = category_map.entry(cur_category).or_insert(vec![]);
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

pub fn execute_command_line<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    system: &dyn System,
    mut cb: TCallback,
    command_line_args: &[String],
) {
    let command_line = parse_command_line(command_line_args);
    execute_command_line_worker(system, command_line)
}

pub enum ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine {
    Program(Rc<Program>),
    EmitAndSemanticDiagnosticsBuilderProgram(Rc<dyn EmitAndSemanticDiagnosticsBuilderProgram>),
    ParsedCommandLine(Rc<ParsedCommandLine>),
}

fn execute_command_line_worker(sys: &dyn System, command_line: ParsedCommandLine) {
    perform_compilation(sys, command_line)
}

fn perform_compilation(_sys: &dyn System, config: ParsedCommandLine) {
    let program_options = CreateProgramOptions {
        root_names: &config.file_names,
        options: config.options,
    };
    let program = create_program(program_options);
    let _exit_status = emit_files_and_report_errors_and_get_exit_status(program);
}
