use gc::{Finalize, Gc, GcCell, Trace};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::rc::Rc;

use super::{
    compile_on_save_command_line_option, compiler_options_alternate_mode,
    convert_property_value_to_json, create_option_name_map, get_compiler_option_value_type_string,
    get_option_name, get_options_name_map, hash_map_to_build_options, option_declarations,
    options_for_watch, parse_command_line_worker, parse_custom_type_option,
    parse_json_source_file_config_file_content, parse_list_type_option, parse_strings,
    type_acquisition_declarations, validate_json_option_value, ParseCommandLineWorkerDiagnostics,
};
use crate::{
    array_to_map, build_opts, create_compiler_diagnostic,
    create_diagnostic_for_node_in_source_file, create_get_canonical_file_name, find,
    get_base_file_name, get_directory_path, get_normalized_absolute_path, get_sys,
    is_array_literal_expression, is_object_literal_expression, maybe_text_char_at_index,
    parse_json_text, starts_with, text_char_at_index, text_substring, to_path,
    AlternateModeDiagnostics, BaseNode, BuildOptions, CharacterCodes, CommandLineOption,
    CommandLineOptionBase, CommandLineOptionInterface, CommandLineOptionOfListType,
    CommandLineOptionOfStringType, CommandLineOptionType, CompilerOptions, CompilerOptionsValue,
    Diagnostic, DiagnosticMessage, DiagnosticRelatedInformationInterface, Diagnostics,
    DidYouMeanOptionsDiagnostics, ExtendedConfigCacheEntry, FileExtensionInfo,
    HasStatementsInterface, LanguageVariant, Node, NodeArray, NodeFlags, NodeInterface,
    OptionsNameMap, ParseConfigHost, ParsedCommandLine, ParsedCommandLineWithBaseOptions, Push,
    ScriptKind, ScriptTarget, SourceFile, StringOrDiagnosticMessage, SyntaxKind, TransformFlags,
    TsConfigOnlyOption, WatchOptions,
};
use local_macros::enum_unwrapped;

pub(super) fn parse_response_file<TReadFile: Fn(&str) -> io::Result<Option<String>>>(
    read_file: Option<&TReadFile>,
    errors: &mut Vec<Gc<Diagnostic>>,
    file_names: &mut Vec<String>,
    diagnostics: &dyn ParseCommandLineWorkerDiagnostics,
    options: &mut HashMap<String, CompilerOptionsValue>,
    watch_options: &RefCell<Option<HashMap<String, CompilerOptionsValue>>>,
    file_name: &str,
) {
    let sys = get_sys();
    let text: StringOrRcDiagnostic = try_read_file(file_name, |file_name| match read_file {
        Some(read_file) => read_file(file_name),
        None => sys.read_file(file_name),
    });
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
                    && text_char_at_index(&text_as_chars, pos) <= CharacterCodes::space
                {
                    pos += 1;
                }
                if pos >= text_as_chars.len() {
                    break;
                }
                let start = pos;
                if text_char_at_index(&text_as_chars, start) == CharacterCodes::double_quote {
                    pos += 1;
                    while pos < text_as_chars.len()
                        && text_char_at_index(&text_as_chars, pos) != CharacterCodes::double_quote
                    {
                        pos += 1;
                    }
                    if pos < text_as_chars.len() {
                        args.push(text_substring(&text_as_chars, start + 1, pos));
                        pos += 1;
                    } else {
                        errors.push(Gc::new(
                            create_compiler_diagnostic(
                                &Diagnostics::Unterminated_quoted_string_in_response_file_0,
                                Some(vec![file_name.to_owned()]),
                            )
                            .into(),
                        ));
                    }
                } else {
                    while matches!(maybe_text_char_at_index(&text_as_chars, pos), Some(ch) if ch > CharacterCodes::space)
                    {
                        pos += 1;
                    }
                    args.push(text_substring(&text_as_chars, start, pos));
                }
            }
            parse_strings(
                file_names,
                diagnostics,
                options,
                errors,
                watch_options,
                read_file,
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
    errors: &mut Vec<Gc<Diagnostic>>,
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
                    validate_json_option_value(opt, Some(&serde_json::Value::Bool(false)), errors),
                );
                i += 1;
            } else {
                if matches!(opt_value, Some("true")) {
                    i += 1;
                }
                errors.push(Gc::new(create_compiler_diagnostic(&Diagnostics::Option_0_can_only_be_specified_in_tsconfig_json_file_or_set_to_null_on_command_line, Some(vec![opt.name().to_owned()])).into()));
            }
        } else {
            errors.push(Gc::new(create_compiler_diagnostic(&Diagnostics::Option_0_can_only_be_specified_in_tsconfig_json_file_or_set_to_null_on_command_line, Some(vec![opt.name().to_owned()])).into()));
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
            errors.push(Gc::new(
                create_compiler_diagnostic(
                    diagnostics.option_type_mismatch_diagnostic(),
                    Some(vec![
                        opt.name().to_owned(),
                        get_compiler_option_value_type_string(opt).to_owned(),
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
                        validate_json_option_value(
                            opt,
                            Some(&serde_json::Value::Number(
                                args[i].parse::<usize>().unwrap().into(),
                            )),
                            errors,
                        ),
                    );
                    i += 1;
                }
                CommandLineOptionType::Boolean => {
                    let opt_value = args.get(i).map(|arg| &**arg);
                    options.insert(
                        opt.name().to_owned(),
                        validate_json_option_value(
                            opt,
                            Some(&serde_json::Value::Bool(!matches!(
                                opt_value,
                                Some("false")
                            ))),
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
                        validate_json_option_value(
                            opt,
                            Some(&serde_json::Value::String(
                                args.get(i)
                                    .map_or_else(|| "".to_owned(), |arg| arg.to_owned()),
                            )),
                            errors,
                        ),
                    );
                    i += 1;
                }
                CommandLineOptionType::List => {
                    let result = CompilerOptionsValue::VecString(Some(
                        parse_list_type_option(opt, args.get(i).map(|arg| &**arg), errors)
                            .unwrap_or_else(|| vec![]),
                    ));
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
    static compiler_options_did_you_mean_diagnostics_: Rc<dyn ParseCommandLineWorkerDiagnostics> = Rc::new(CompilerOptionsDidYouMeanDiagnostics::new());
}

pub(crate) fn compiler_options_did_you_mean_diagnostics(
) -> Rc<dyn ParseCommandLineWorkerDiagnostics> {
    compiler_options_did_you_mean_diagnostics_.with(|compiler_options_did_you_mean_diagnostics| {
        compiler_options_did_you_mean_diagnostics.clone()
    })
}

pub struct CompilerOptionsDidYouMeanDiagnostics {}

impl CompilerOptionsDidYouMeanDiagnostics {
    pub fn new() -> Self {
        Self {}
    }
}

impl DidYouMeanOptionsDiagnostics for CompilerOptionsDidYouMeanDiagnostics {
    fn maybe_alternate_mode(&self) -> Option<Rc<AlternateModeDiagnostics>> {
        Some(compiler_options_alternate_mode())
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

    fn as_did_you_mean_options_diagnostics(&self) -> &dyn DidYouMeanOptionsDiagnostics {
        self
    }
}

pub fn parse_command_line<TReadFile: Fn(&str) -> io::Result<Option<String>>>(
    command_line: &[String],
    read_file: Option<TReadFile>,
) -> ParsedCommandLine {
    parse_command_line_worker(
        &*compiler_options_did_you_mean_diagnostics(),
        command_line,
        read_file,
    )
    .into_parsed_command_line()
}

pub(crate) fn get_option_from_name(
    option_name: &str,
    allow_short: Option<bool>,
) -> Option<Rc<CommandLineOption>> {
    get_option_declaration_from_name(get_options_name_map, option_name, allow_short)
}

pub(super) fn get_option_declaration_from_name<TGetOptionNameMap: FnMut() -> Rc<OptionsNameMap>>(
    mut get_option_name_map: TGetOptionNameMap,
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
    options_name_map.get(&option_name).map(Clone::clone)
}

pub(crate) struct ParsedBuildCommand {
    pub build_options: BuildOptions,
    pub watch_options: Option<Rc<WatchOptions>>,
    pub projects: Vec<String>,
    pub errors: Vec<Gc<Diagnostic>>,
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

thread_local! {
    static build_options_alternate_mode_: Rc<AlternateModeDiagnostics> = Rc::new(AlternateModeDiagnostics {
        diagnostic: &Diagnostics::Compiler_option_0_may_not_be_used_with_build,
        get_options_name_map,
    });
}

pub(super) fn build_options_alternate_mode() -> Rc<AlternateModeDiagnostics> {
    build_options_alternate_mode_
        .with(|build_options_alternate_mode| build_options_alternate_mode.clone())
}

thread_local! {
    static build_options_did_you_mean_diagnostics_: Rc<dyn ParseCommandLineWorkerDiagnostics> = Rc::new(BuildOptionsDidYouMeanDiagnostics::new());
}

pub struct BuildOptionsDidYouMeanDiagnostics {}

impl BuildOptionsDidYouMeanDiagnostics {
    pub fn new() -> Self {
        Self {}
    }
}

impl DidYouMeanOptionsDiagnostics for BuildOptionsDidYouMeanDiagnostics {
    fn maybe_alternate_mode(&self) -> Option<Rc<AlternateModeDiagnostics>> {
        Some(build_options_alternate_mode())
    }

    fn option_declarations(&self) -> Vec<Rc<CommandLineOption>> {
        build_opts.with(|build_opts_| build_opts_.clone())
    }

    fn unknown_option_diagnostic(&self) -> &DiagnosticMessage {
        &Diagnostics::Unknown_build_option_0
    }

    fn unknown_did_you_mean_diagnostic(&self) -> &DiagnosticMessage {
        &Diagnostics::Unknown_build_option_0_Did_you_mean_1
    }
}

impl ParseCommandLineWorkerDiagnostics for BuildOptionsDidYouMeanDiagnostics {
    fn get_options_name_map(&self) -> Rc<OptionsNameMap> {
        get_build_options_name_map()
    }

    fn option_type_mismatch_diagnostic(&self) -> &DiagnosticMessage {
        &Diagnostics::Build_option_0_requires_a_value_of_type_1
    }

    fn as_did_you_mean_options_diagnostics(&self) -> &dyn DidYouMeanOptionsDiagnostics {
        self
    }
}

pub(super) fn build_options_did_you_mean_diagnostics() -> Rc<dyn ParseCommandLineWorkerDiagnostics>
{
    build_options_did_you_mean_diagnostics_.with(|build_options_did_you_mean_diagnostics| {
        build_options_did_you_mean_diagnostics.clone()
    })
}

pub(crate) fn parse_build_command(args: &[String]) -> ParsedBuildCommand {
    let ParsedCommandLineWithBaseOptions {
        options,
        watch_options,
        file_names: mut projects,
        mut errors,
        ..
    } = parse_command_line_worker(
        &*build_options_did_you_mean_diagnostics(),
        args,
        Option::<fn(&str) -> io::Result<Option<String>>>::None,
    );
    let build_options: BuildOptions = hash_map_to_build_options(&options);

    if projects.is_empty() {
        projects.push(".".to_owned());
    }

    if matches!(build_options.clean, Some(true)) && matches!(build_options.force, Some(true)) {
        errors.push(Gc::new(
            create_compiler_diagnostic(
                &Diagnostics::Options_0_and_1_cannot_be_combined,
                Some(vec!["clean".to_owned(), "force".to_owned()]),
            )
            .into(),
        ));
    }
    if matches!(build_options.clean, Some(true)) && matches!(build_options.verbose, Some(true)) {
        errors.push(Gc::new(
            create_compiler_diagnostic(
                &Diagnostics::Options_0_and_1_cannot_be_combined,
                Some(vec!["clean".to_owned(), "verbose".to_owned()]),
            )
            .into(),
        ));
    }
    if matches!(build_options.clean, Some(true)) && matches!(build_options.watch, Some(true)) {
        errors.push(Gc::new(
            create_compiler_diagnostic(
                &Diagnostics::Options_0_and_1_cannot_be_combined,
                Some(vec!["clean".to_owned(), "watch".to_owned()]),
            )
            .into(),
        ));
    }
    if matches!(build_options.watch, Some(true)) && matches!(build_options.dry, Some(true)) {
        errors.push(Gc::new(
            create_compiler_diagnostic(
                &Diagnostics::Options_0_and_1_cannot_be_combined,
                Some(vec!["watch".to_owned(), "dry".to_owned()]),
            )
            .into(),
        ));
    }

    ParsedBuildCommand {
        build_options,
        watch_options,
        projects,
        errors,
    }
}

pub(crate) fn get_diagnostic_text(
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> String {
    let diagnostic = create_compiler_diagnostic(message, args);
    enum_unwrapped!(diagnostic.message_text(), [DiagnosticMessageText, String]).clone()
}

pub trait DiagnosticReporter: Trace + Finalize {
    fn call(&self, diagnostic: Gc<Diagnostic>);
}

pub trait ConfigFileDiagnosticsReporter {
    fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Gc<Diagnostic>);
}

pub trait ParseConfigFileHost:
    ParseConfigHost + ConfigFileDiagnosticsReporter + Trace + Finalize
{
    fn get_current_directory(&self) -> String;
}

pub fn get_parsed_command_line_of_config_file<THost: ParseConfigFileHost>(
    config_file_name: &str,
    options_to_extend: Option<Gc<CompilerOptions>>,
    host: &THost,
    extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
    watch_options_to_extend: Option<Rc<WatchOptions>>,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
) -> Option<ParsedCommandLine> {
    let config_file_text = try_read_file(config_file_name, |file_name| host.read_file(file_name));
    if let StringOrRcDiagnostic::RcDiagnostic(ref config_file_text) = config_file_text {
        host.on_un_recoverable_config_file_diagnostic(config_file_text.clone());
        return None;
    }
    let config_file_text = enum_unwrapped!(config_file_text, [StringOrRcDiagnostic, String]);

    let result = parse_json_text(config_file_name, config_file_text);
    let cwd = host.get_current_directory();
    let result_as_source_file = result.as_source_file();
    result_as_source_file.set_path(to_path(
        config_file_name,
        Some(&cwd),
        create_get_canonical_file_name(host.use_case_sensitive_file_names()),
    ));
    result_as_source_file.set_resolved_path(Some(result_as_source_file.path().clone()));
    result_as_source_file.set_original_file_name(Some(result_as_source_file.file_name().clone()));
    Some(parse_json_source_file_config_file_content(
        &result,
        host,
        &get_normalized_absolute_path(&get_directory_path(config_file_name), Some(&cwd)),
        options_to_extend,
        Some(&get_normalized_absolute_path(config_file_name, Some(&cwd))),
        None,
        extra_file_extensions,
        extended_config_cache,
        watch_options_to_extend,
    ))
}

pub fn read_config_file<TReadFile: FnMut(&str) -> io::Result<Option<String>>>(
    file_name: &str,
    read_file: TReadFile,
) -> ReadConfigFileReturn {
    let text_or_diagnostic = try_read_file(file_name, read_file);
    match text_or_diagnostic {
        StringOrRcDiagnostic::String(text_or_diagnostic) => {
            parse_config_file_text_to_json(file_name, text_or_diagnostic)
        }
        StringOrRcDiagnostic::RcDiagnostic(text_or_diagnostic) => ReadConfigFileReturn {
            config: Some(serde_json::Value::Object(serde_json::Map::new())),
            error: Some(text_or_diagnostic),
        },
    }
}

pub struct ReadConfigFileReturn {
    pub config: Option<serde_json::Value>,
    pub error: Option<Gc<Diagnostic>>,
}

pub fn parse_config_file_text_to_json(file_name: &str, json_text: String) -> ReadConfigFileReturn {
    let json_source_file = parse_json_text(file_name, json_text);
    let mut json_source_file_parse_diagnostics =
        json_source_file.as_source_file().parse_diagnostics();
    let config = convert_config_file_to_object(
        &json_source_file,
        json_source_file_parse_diagnostics.clone(),
        false,
        Option::<&JsonConversionNotifierDummy>::None,
    );
    let json_source_file_parse_diagnostics = (*json_source_file_parse_diagnostics).borrow();
    ReadConfigFileReturn {
        config,
        error: if !json_source_file_parse_diagnostics.is_empty() {
            Some(json_source_file_parse_diagnostics[0].clone())
        } else {
            None
        },
    }
}

pub fn read_json_config_file<TReadFile: FnMut(&str) -> io::Result<Option<String>>>(
    file_name: &str,
    read_file: TReadFile,
) -> Gc<Node /*TsConfigSourceFile*/> {
    let text_or_diagnostic = try_read_file(file_name, read_file);
    match text_or_diagnostic {
        StringOrRcDiagnostic::String(text_or_diagnostic) => {
            parse_json_text(file_name, text_or_diagnostic)
        }
        StringOrRcDiagnostic::RcDiagnostic(text_or_diagnostic) => {
            let base_node = BaseNode::new(
                SyntaxKind::Unknown,
                NodeFlags::None,
                TransformFlags::None,
                -1,
                -1,
            );
            let end_of_file_token: Gc<Node> = BaseNode::new(
                SyntaxKind::EndOfFileToken,
                NodeFlags::None,
                TransformFlags::None,
                -1,
                -1,
            )
            .into();
            let source_file: Gc<Node> = SourceFile::new(
                base_node,
                NodeArray::new(vec![], -1, -1, false, None),
                end_of_file_token,
                file_name.to_owned(),
                "".to_owned(),
                ScriptTarget::Latest,
                LanguageVariant::Standard,
                ScriptKind::Unknown,
                false,
                false,
            )
            .into();
            source_file
                .as_source_file()
                .set_parse_diagnostics(Gc::new(GcCell::new(vec![text_or_diagnostic])));
            source_file
        }
    }
}

pub(crate) enum StringOrRcDiagnostic {
    String(String),
    RcDiagnostic(Gc<Diagnostic>),
}

impl From<String> for StringOrRcDiagnostic {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<Gc<Diagnostic>> for StringOrRcDiagnostic {
    fn from(value: Gc<Diagnostic>) -> Self {
        Self::RcDiagnostic(value)
    }
}

pub(crate) fn try_read_file<TReadFile: FnMut(&str) -> io::Result<Option<String>>>(
    file_name: &str,
    mut read_file: TReadFile,
) -> StringOrRcDiagnostic {
    match read_file(file_name) {
        Err(e) => Into::<StringOrRcDiagnostic>::into(Gc::new(
            create_compiler_diagnostic(
                &Diagnostics::Cannot_read_file_0_Colon_1,
                Some(vec![file_name.to_owned(), e.to_string()]),
            )
            .into(),
        )),
        Ok(None) => Into::<StringOrRcDiagnostic>::into(Gc::new(
            create_compiler_diagnostic(
                &Diagnostics::Cannot_read_file_0,
                Some(vec![file_name.to_owned()]),
            )
            .into(),
        )),
        Ok(Some(text)) => text.into(),
    }
}

pub(super) fn command_line_options_to_map(
    options: &[Rc<CommandLineOption>],
) -> HashMap<String, Rc<CommandLineOption>> {
    array_to_map(
        options,
        |option| Some(get_option_name(option).to_owned()),
        |item| item.clone(),
    )
}

thread_local! {
    static type_acquisition_did_you_mean_diagnostics_: Rc<dyn DidYouMeanOptionsDiagnostics> = Rc::new(TypeAcquisitionDidYouMeanDiagnostics::new());
}

struct TypeAcquisitionDidYouMeanDiagnostics {}

impl TypeAcquisitionDidYouMeanDiagnostics {
    pub fn new() -> Self {
        Self {}
    }
}

impl DidYouMeanOptionsDiagnostics for TypeAcquisitionDidYouMeanDiagnostics {
    fn maybe_alternate_mode(&self) -> Option<Rc<AlternateModeDiagnostics>> {
        None
    }

    fn option_declarations(&self) -> Vec<Rc<CommandLineOption>> {
        type_acquisition_declarations
            .with(|type_acquisition_declarations_| type_acquisition_declarations_.clone())
    }

    fn unknown_option_diagnostic(&self) -> &DiagnosticMessage {
        &Diagnostics::Unknown_type_acquisition_option_0
    }

    fn unknown_did_you_mean_diagnostic(&self) -> &DiagnosticMessage {
        &Diagnostics::Unknown_type_acquisition_option_0_Did_you_mean_1
    }
}

pub(super) fn type_acquisition_did_you_mean_diagnostics() -> Rc<dyn DidYouMeanOptionsDiagnostics> {
    type_acquisition_did_you_mean_diagnostics_.with(|type_acquisition_did_you_mean_diagnostics| {
        type_acquisition_did_you_mean_diagnostics.clone()
    })
}

thread_local! {
    pub(super) static watch_options_name_map_cache: RefCell<Option<Rc<OptionsNameMap>>> = RefCell::new(None);
}

pub(super) fn get_watch_options_name_map() -> Rc<OptionsNameMap> {
    watch_options_name_map_cache.with(|watch_options_name_map_cache_| {
        let mut watch_options_name_map_cache_ = watch_options_name_map_cache_.borrow_mut();
        if watch_options_name_map_cache_.is_none() {
            options_for_watch.with(|options_for_watch_| {
                *watch_options_name_map_cache_ =
                    Some(Rc::new(create_option_name_map(&options_for_watch_)));
            });
        }
        watch_options_name_map_cache_.as_ref().unwrap().clone()
    })
}

thread_local! {
    static watch_options_did_you_mean_diagnostics_: Rc<dyn ParseCommandLineWorkerDiagnostics> = Rc::new(WatchOptionsDidYouMeanDiagnostics::new());
}

pub struct WatchOptionsDidYouMeanDiagnostics {}

impl WatchOptionsDidYouMeanDiagnostics {
    pub fn new() -> Self {
        Self {}
    }
}

impl DidYouMeanOptionsDiagnostics for WatchOptionsDidYouMeanDiagnostics {
    fn maybe_alternate_mode(&self) -> Option<Rc<AlternateModeDiagnostics>> {
        None
    }

    fn option_declarations(&self) -> Vec<Rc<CommandLineOption>> {
        options_for_watch.with(|options_for_watch_| options_for_watch_.clone())
    }

    fn unknown_option_diagnostic(&self) -> &DiagnosticMessage {
        &Diagnostics::Unknown_watch_option_0
    }

    fn unknown_did_you_mean_diagnostic(&self) -> &DiagnosticMessage {
        &Diagnostics::Unknown_watch_option_0_Did_you_mean_1
    }
}

impl ParseCommandLineWorkerDiagnostics for WatchOptionsDidYouMeanDiagnostics {
    fn get_options_name_map(&self) -> Rc<OptionsNameMap> {
        get_watch_options_name_map()
    }

    fn option_type_mismatch_diagnostic(&self) -> &DiagnosticMessage {
        &Diagnostics::Watch_option_0_requires_a_value_of_type_1
    }

    fn as_did_you_mean_options_diagnostics(&self) -> &dyn DidYouMeanOptionsDiagnostics {
        self
    }
}

pub(super) fn watch_options_did_you_mean_diagnostics() -> Rc<dyn ParseCommandLineWorkerDiagnostics>
{
    watch_options_did_you_mean_diagnostics_.with(|watch_options_did_you_mean_diagnostics| {
        watch_options_did_you_mean_diagnostics.clone()
    })
}

thread_local! {
    static command_line_compiler_options_map_cache: RefCell<Option<Rc<HashMap<String, Rc<CommandLineOption>>>>> = RefCell::new(None);
}

pub(super) fn get_command_line_compiler_options_map() -> Rc<HashMap<String, Rc<CommandLineOption>>>
{
    command_line_compiler_options_map_cache.with(|command_line_compiler_options_map_cache_| {
        let mut command_line_compiler_options_map_cache_ =
            command_line_compiler_options_map_cache_.borrow_mut();
        if command_line_compiler_options_map_cache_.is_none() {
            *command_line_compiler_options_map_cache_ =
                Some(Rc::new(option_declarations.with(|option_declarations_| {
                    command_line_options_to_map(option_declarations_)
                })));
        }
        command_line_compiler_options_map_cache_
            .as_ref()
            .unwrap()
            .clone()
    })
}

thread_local! {
    static command_line_watch_options_map_cache: RefCell<Option<Rc<HashMap<String, Rc<CommandLineOption>>>>> = RefCell::new(None);
}

pub(super) fn get_command_line_watch_options_map() -> Rc<HashMap<String, Rc<CommandLineOption>>> {
    command_line_watch_options_map_cache.with(|command_line_watch_options_map_cache_| {
        let mut command_line_watch_options_map_cache_ =
            command_line_watch_options_map_cache_.borrow_mut();
        if command_line_watch_options_map_cache_.is_none() {
            *command_line_watch_options_map_cache_ =
                Some(Rc::new(options_for_watch.with(|options_for_watch_| {
                    command_line_options_to_map(options_for_watch_)
                })));
        }
        command_line_watch_options_map_cache_
            .as_ref()
            .unwrap()
            .clone()
    })
}

thread_local! {
    static command_line_type_acquisition_map_cache: RefCell<Option<Rc<HashMap<String, Rc<CommandLineOption>>>>> = RefCell::new(None);
}

pub(super) fn get_command_line_type_acquisition_map() -> Rc<HashMap<String, Rc<CommandLineOption>>>
{
    command_line_type_acquisition_map_cache.with(|command_line_type_acquisition_map_cache_| {
        let mut command_line_type_acquisition_map_cache_ =
            command_line_type_acquisition_map_cache_.borrow_mut();
        if command_line_type_acquisition_map_cache_.is_none() {
            *command_line_type_acquisition_map_cache_ = Some(Rc::new(
                type_acquisition_declarations.with(|type_acquisition_declarations_| {
                    command_line_options_to_map(type_acquisition_declarations_)
                }),
            ));
        }
        command_line_type_acquisition_map_cache_
            .as_ref()
            .unwrap()
            .clone()
    })
}

pub(super) const tsconfig_root_options_dummy_name: &str = "TSCONFIG ROOT OPTIONS";
thread_local! {
    static _tsconfig_root_options: RefCell<Option<Rc<CommandLineOption>>> = RefCell::new(None);
}
pub(super) fn get_tsconfig_root_options_map() -> Rc<CommandLineOption> {
    _tsconfig_root_options.with(|tsconfig_root_options| {
        let mut tsconfig_root_options = tsconfig_root_options.borrow_mut();
        if tsconfig_root_options.is_none() {
            *tsconfig_root_options = Some(Rc::new(
                TsConfigOnlyOption::new(
                    CommandLineOptionBase {
                        _command_line_option_wrapper: RefCell::new(None),
                        name: tsconfig_root_options_dummy_name.to_owned(),
                        type_: CommandLineOptionType::Object,
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
                    Some(Rc::new(command_line_options_to_map(&vec![
                        TsConfigOnlyOption::new(
                            CommandLineOptionBase {
                                _command_line_option_wrapper: RefCell::new(None),
                                name: "compilerOptions".to_string(),
                                type_: CommandLineOptionType::Object,
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
                            Some(get_command_line_compiler_options_map()),
                            Some(compiler_options_did_you_mean_diagnostics().into()),
                        )
                        .into(),
                        TsConfigOnlyOption::new(
                            CommandLineOptionBase {
                                _command_line_option_wrapper: RefCell::new(None),
                                name: "watchOptions".to_string(),
                                type_: CommandLineOptionType::Object,
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
                            Some(get_command_line_watch_options_map()),
                            Some(watch_options_did_you_mean_diagnostics().into()),
                        )
                        .into(),
                        TsConfigOnlyOption::new(
                            CommandLineOptionBase {
                                _command_line_option_wrapper: RefCell::new(None),
                                name: "typingOptions".to_string(),
                                type_: CommandLineOptionType::Object,
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
                            Some(get_command_line_type_acquisition_map()),
                            Some(type_acquisition_did_you_mean_diagnostics().into()),
                        )
                        .into(),
                        TsConfigOnlyOption::new(
                            CommandLineOptionBase {
                                _command_line_option_wrapper: RefCell::new(None),
                                name: "typeAcquisition".to_string(),
                                type_: CommandLineOptionType::Object,
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
                            Some(get_command_line_type_acquisition_map()),
                            Some(type_acquisition_did_you_mean_diagnostics().into()),
                        )
                        .into(),
                        CommandLineOptionOfStringType::new(CommandLineOptionBase {
                            _command_line_option_wrapper: RefCell::new(None),
                            name: "extends".to_string(),
                            type_: CommandLineOptionType::String,
                            is_file_path: None,
                            short_name: None,
                            description: None,
                            default_value_description: None,
                            param_type: None,
                            is_tsconfig_only: None,
                            is_command_line_only: None,
                            show_in_simplified_help_view: None,
                            category: Some(&Diagnostics::File_Management),
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
                        CommandLineOptionOfListType::new(
                            CommandLineOptionBase {
                                _command_line_option_wrapper: RefCell::new(None),
                                name: "references".to_string(),
                                type_: CommandLineOptionType::List,
                                is_file_path: None,
                                short_name: None,
                                description: None,
                                default_value_description: None,
                                param_type: None,
                                is_tsconfig_only: None,
                                is_command_line_only: None,
                                show_in_simplified_help_view: None,
                                category: Some(&Diagnostics::Projects),
                                strict_flag: None,
                                affects_source_file: None,
                                affects_module_resolution: None,
                                affects_bind_diagnostics: None,
                                affects_semantic_diagnostics: None,
                                affects_emit: None,
                                affects_program_structure: None,
                                transpile_option_value: None,
                            },
                            TsConfigOnlyOption::new(
                                CommandLineOptionBase {
                                    _command_line_option_wrapper: RefCell::new(None),
                                    name: "references".to_string(),
                                    type_: CommandLineOptionType::Object,
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
                                None,
                                None,
                            )
                            .into(),
                        )
                        .into(),
                        CommandLineOptionOfListType::new(
                            CommandLineOptionBase {
                                _command_line_option_wrapper: RefCell::new(None),
                                name: "files".to_string(),
                                type_: CommandLineOptionType::List,
                                is_file_path: None,
                                short_name: None,
                                description: None,
                                default_value_description: None,
                                param_type: None,
                                is_tsconfig_only: None,
                                is_command_line_only: None,
                                show_in_simplified_help_view: None,
                                category: Some(&Diagnostics::File_Management),
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
                                name: "files".to_string(),
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
                        CommandLineOptionOfListType::new(
                            CommandLineOptionBase {
                                _command_line_option_wrapper: RefCell::new(None),
                                name: "include".to_string(),
                                type_: CommandLineOptionType::List,
                                is_file_path: None,
                                short_name: None,
                                description: None,
                                default_value_description: Some(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::if_files_is_specified_otherwise_Asterisk_Asterisk_Slash_Asterisk)),
                                param_type: None,
                                is_tsconfig_only: None,
                                is_command_line_only: None,
                                show_in_simplified_help_view: None,
                                category: Some(&Diagnostics::File_Management),
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
                        CommandLineOptionOfListType::new(
                            CommandLineOptionBase {
                                _command_line_option_wrapper: RefCell::new(None),
                                name: "exclude".to_string(),
                                type_: CommandLineOptionType::List,
                                is_file_path: None,
                                short_name: None,
                                description: None,
                                default_value_description: Some(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::node_modules_bower_components_jspm_packages_plus_the_value_of_outDir_if_one_is_specified)),
                                param_type: None,
                                is_tsconfig_only: None,
                                is_command_line_only: None,
                                show_in_simplified_help_view: None,
                                category: Some(&Diagnostics::File_Management),
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
                        compile_on_save_command_line_option(),
                    ]))),
                    None,
                )
                .into(),
            ));
        }
        tsconfig_root_options.as_ref().unwrap().clone()
    })
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
    errors: Gc<GcCell<Vec<Gc<Diagnostic>>>>,
    report_options_errors: bool,
    options_iterator: Option<&TOptionsIterator>,
) -> Option<serde_json::Value> {
    let source_file_as_source_file = source_file.as_source_file();
    let root_expression = source_file_as_source_file
        .statements()
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
        errors.borrow_mut().push(Gc::new(
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
    errors: Gc<GcCell<Push<Gc<Diagnostic>>>>,
) -> Option<serde_json::Value> {
    convert_to_object_worker(
        source_file,
        source_file
            .as_source_file()
            .statements()
            .get(0)
            .map(|statement| statement.as_expression_statement().expression.clone()),
        errors,
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
    errors: Gc<GcCell<Push<Gc<Diagnostic>>>>,
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
