use std::{cell::RefCell, collections::HashMap, io, rc::Rc};

use id_arena::Id;
use indexmap::IndexMap;
use local_macros::enum_unwrapped;

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
    get_base_file_name, get_directory_path, get_normalized_absolute_path, get_sys, impl_has_arena,
    is_array_literal_expression, is_object_literal_expression, maybe_text_char_at_index,
    parse_json_text, per_arena, starts_with, text_char_at_index, text_substring, to_path,
    AllArenas, AlternateModeDiagnostics, BaseNode, BuildOptions, CharacterCodes, CommandLineOption,
    CommandLineOptionBaseBuilder, CommandLineOptionInterface, CommandLineOptionOfListType,
    CommandLineOptionType, CompilerOptions, CompilerOptionsValue, Diagnostic, DiagnosticMessage,
    DiagnosticMessageText, DiagnosticRelatedInformationInterface, Diagnostics,
    DidYouMeanOptionsDiagnostics, ExtendedConfigCacheEntry, FileExtensionInfo, HasArena,
    HasStatementsInterface, InArena, LanguageVariant, Node, NodeArray, NodeFlags, NodeInterface,
    OptionsNameMap, ParseConfigHost, ParsedCommandLine, ParsedCommandLineWithBaseOptions, Push,
    RcDynDidYouMeanOptionsDiagnosticsOrRcDynParseCommandLineWorkerDiagnostics, ScriptKind,
    ScriptTarget, SourceFile, StringOrDiagnosticMessage, SyntaxKind, TransformFlags,
    TsConfigOnlyOption, WatchOptions,
};

pub(super) fn parse_response_file(
    read_file: Option<&impl Fn(&str) -> io::Result<Option<String>>>,
    errors: &mut Vec<Id<Diagnostic>>,
    file_names: &mut Vec<String>,
    diagnostics: Id<Box<dyn ParseCommandLineWorkerDiagnostics>>,
    options: &mut IndexMap<String, CompilerOptionsValue>,
    watch_options: &RefCell<Option<IndexMap<String, CompilerOptionsValue>>>,
    file_name: &str,
    arena: &impl HasArena,
) {
    let sys = get_sys(arena);
    let text: StringOrRcDiagnostic = try_read_file(
        file_name,
        |file_name| match read_file {
            Some(read_file) => read_file(file_name),
            None => sys.ref_(arena).read_file(file_name),
        },
        arena,
    );
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
                        errors.push(
                            arena.alloc_diagnostic(
                                create_compiler_diagnostic(
                                    &Diagnostics::Unterminated_quoted_string_in_response_file_0,
                                    Some(vec![file_name.to_owned()]),
                                )
                                .into(),
                            ),
                        );
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
                arena,
            );
        }
    }
}

pub(super) fn parse_option_value(
    args: &[String],
    mut i: usize,
    diagnostics: Id<Box<dyn ParseCommandLineWorkerDiagnostics>>,
    opt: &CommandLineOption,
    options: &mut IndexMap<String, CompilerOptionsValue>,
    errors: &mut Vec<Id<Diagnostic>>,
    arena: &impl HasArena,
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
                    validate_json_option_value(
                        opt,
                        Some(&serde_json::Value::Bool(false)),
                        errors,
                        arena,
                    ),
                );
                i += 1;
            } else {
                if matches!(opt_value, Some("true")) {
                    i += 1;
                }
                errors.push(
                    arena.alloc_diagnostic(
                        create_compiler_diagnostic(
                            &Diagnostics::Option_0_can_only_be_specified_in_tsconfig_json_file_or_set_to_false_or_null_on_command_line,
                            Some(vec![
                                opt.name().to_owned(),
                            ]),
                        ).into()
                    )
                );
            }
        } else {
            errors.push(
                arena.alloc_diagnostic(
                    create_compiler_diagnostic(
                        &Diagnostics::Option_0_can_only_be_specified_in_tsconfig_json_file_or_set_to_null_on_command_line,
                        Some(vec![
                            opt.name().to_owned(),
                        ]),
                    ).into()
                )
            );
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
            errors.push(
                arena.alloc_diagnostic(
                    create_compiler_diagnostic(
                        diagnostics.ref_(arena).option_type_mismatch_diagnostic(),
                        Some(vec![
                            opt.name().to_owned(),
                            get_compiler_option_value_type_string(opt).to_owned(),
                        ]),
                    )
                    .into(),
                ),
            );
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
                            arena,
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
                            arena,
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
                            arena,
                        ),
                    );
                    i += 1;
                }
                CommandLineOptionType::List => {
                    let result = CompilerOptionsValue::VecString(parse_list_type_option(
                        opt,
                        args.get(i).map(|arg| &**arg),
                        errors,
                        arena,
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
                        parse_custom_type_option(opt, args.get(i).map(|arg| &**arg), errors, arena),
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

pub fn compiler_options_did_you_mean_diagnostics(
    arena: &impl HasArena,
) -> Id<Box<dyn ParseCommandLineWorkerDiagnostics>> {
    per_arena!(
        Box<dyn ParseCommandLineWorkerDiagnostics>,
        arena,
        CompilerOptionsDidYouMeanDiagnostics::new(arena)
    )
}

pub struct CompilerOptionsDidYouMeanDiagnostics {
    arena: *const AllArenas,
}

impl CompilerOptionsDidYouMeanDiagnostics {
    pub fn new(arena: &impl HasArena) -> Id<Box<dyn ParseCommandLineWorkerDiagnostics>> {
        arena.alloc_parse_command_line_worker_diagnostics(Box::new(Self {
            arena: arena.arena(),
        }))
    }
}

impl DidYouMeanOptionsDiagnostics for CompilerOptionsDidYouMeanDiagnostics {
    fn maybe_alternate_mode(&self) -> Option<Rc<AlternateModeDiagnostics>> {
        Some(compiler_options_alternate_mode())
    }

    fn option_declarations(&self) -> Id<Vec<Id<CommandLineOption>>> {
        option_declarations(self)
    }

    fn unknown_option_diagnostic(&self) -> &'static DiagnosticMessage {
        &Diagnostics::Unknown_compiler_option_0
    }

    fn unknown_did_you_mean_diagnostic(&self) -> &'static DiagnosticMessage {
        &Diagnostics::Unknown_compiler_option_0_Did_you_mean_1
    }
}

impl ParseCommandLineWorkerDiagnostics for CompilerOptionsDidYouMeanDiagnostics {
    fn get_options_name_map(&self) -> Id<OptionsNameMap> {
        get_options_name_map(self)
    }

    fn option_type_mismatch_diagnostic(&self) -> &'static DiagnosticMessage {
        &Diagnostics::Compiler_option_0_expects_an_argument
    }

    fn as_did_you_mean_options_diagnostics(&self) -> &(dyn DidYouMeanOptionsDiagnostics + 'static) {
        self
    }
}

impl_has_arena!(CompilerOptionsDidYouMeanDiagnostics);

pub fn parse_command_line(
    command_line: &[String],
    read_file: Option<impl Fn(&str) -> io::Result<Option<String>>>,
    arena: &impl HasArena,
) -> ParsedCommandLine {
    parse_command_line_worker(
        compiler_options_did_you_mean_diagnostics(arena),
        command_line,
        read_file,
        arena,
    )
    .into_parsed_command_line(arena)
}

#[allow(dead_code)]
pub(crate) fn get_option_from_name(
    option_name: &str,
    allow_short: Option<bool>,
    arena: &impl HasArena,
) -> Option<Id<CommandLineOption>> {
    get_option_declaration_from_name(
        || get_options_name_map(arena),
        option_name,
        allow_short,
        arena,
    )
}

pub(super) fn get_option_declaration_from_name(
    mut get_option_name_map: impl FnMut() -> Id<OptionsNameMap>,
    option_name: &str,
    allow_short: Option<bool>,
    arena: &impl HasArena,
) -> Option<Id<CommandLineOption>> {
    let allow_short = allow_short.unwrap_or(false);
    let mut option_name = option_name.to_lowercase();
    let option_name_map = get_option_name_map();
    let option_name_map_ref = option_name_map.ref_(arena);
    let options_name_map = &option_name_map_ref.options_name_map;
    let short_option_names = &option_name_map_ref.short_option_names;
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
    pub errors: Vec<Id<Diagnostic>>,
}

thread_local! {
    pub(super) static build_options_name_map_cache: RefCell<Option<Id<OptionsNameMap>>> = RefCell::new(None);
}

pub(crate) fn get_build_options_name_map(arena: &impl HasArena) -> Id<OptionsNameMap> {
    per_arena!(
        OptionsNameMap,
        arena,
        arena.alloc_options_name_map(create_option_name_map(
            &build_opts(arena).ref_(arena),
            arena
        ))
    )
}

thread_local! {
    static build_options_alternate_mode_: Rc<AlternateModeDiagnostics> = Rc::new(AlternateModeDiagnostics {
        diagnostic: &Diagnostics::Compiler_option_0_may_not_be_used_with_build,
        get_options_name_map: Box::new(|arena: &AllArenas| get_options_name_map(arena)),
    });
}

pub(super) fn build_options_alternate_mode() -> Rc<AlternateModeDiagnostics> {
    build_options_alternate_mode_
        .with(|build_options_alternate_mode| build_options_alternate_mode.clone())
}

pub struct BuildOptionsDidYouMeanDiagnostics {
    arena: *const AllArenas,
}

impl BuildOptionsDidYouMeanDiagnostics {
    pub fn new(arena: &impl HasArena) -> Id<Box<dyn ParseCommandLineWorkerDiagnostics>> {
        arena.alloc_parse_command_line_worker_diagnostics(Box::new(Self {
            arena: arena.arena(),
        }))
    }
}

impl DidYouMeanOptionsDiagnostics for BuildOptionsDidYouMeanDiagnostics {
    fn maybe_alternate_mode(&self) -> Option<Rc<AlternateModeDiagnostics>> {
        Some(build_options_alternate_mode())
    }

    fn option_declarations(&self) -> Id<Vec<Id<CommandLineOption>>> {
        build_opts(self)
    }

    fn unknown_option_diagnostic(&self) -> &'static DiagnosticMessage {
        &Diagnostics::Unknown_build_option_0
    }

    fn unknown_did_you_mean_diagnostic(&self) -> &'static DiagnosticMessage {
        &Diagnostics::Unknown_build_option_0_Did_you_mean_1
    }
}

impl ParseCommandLineWorkerDiagnostics for BuildOptionsDidYouMeanDiagnostics {
    fn get_options_name_map(&self) -> Id<OptionsNameMap> {
        get_build_options_name_map(self)
    }

    fn option_type_mismatch_diagnostic(&self) -> &'static DiagnosticMessage {
        &Diagnostics::Build_option_0_requires_a_value_of_type_1
    }

    fn as_did_you_mean_options_diagnostics(&self) -> &(dyn DidYouMeanOptionsDiagnostics + 'static) {
        self
    }
}

impl_has_arena!(BuildOptionsDidYouMeanDiagnostics);

pub(super) fn build_options_did_you_mean_diagnostics(
    arena: &impl HasArena,
) -> Id<Box<dyn ParseCommandLineWorkerDiagnostics>> {
    per_arena!(
        Box<dyn ParseCommandLineWorkerDiagnostics>,
        arena,
        BuildOptionsDidYouMeanDiagnostics::new(arena)
    )
}

pub(crate) fn parse_build_command(args: &[String], arena: &impl HasArena) -> ParsedBuildCommand {
    let ParsedCommandLineWithBaseOptions {
        options,
        watch_options,
        file_names: mut projects,
        mut errors,
        ..
    } = parse_command_line_worker(
        build_options_did_you_mean_diagnostics(arena),
        args,
        Option::<fn(&str) -> io::Result<Option<String>>>::None,
        arena,
    );
    let build_options: BuildOptions = hash_map_to_build_options(&options);

    if projects.is_empty() {
        projects.push(".".to_owned());
    }

    if matches!(build_options.clean, Some(true)) && matches!(build_options.force, Some(true)) {
        errors.push(
            arena.alloc_diagnostic(
                create_compiler_diagnostic(
                    &Diagnostics::Options_0_and_1_cannot_be_combined,
                    Some(vec!["clean".to_owned(), "force".to_owned()]),
                )
                .into(),
            ),
        );
    }
    if matches!(build_options.clean, Some(true)) && matches!(build_options.verbose, Some(true)) {
        errors.push(
            arena.alloc_diagnostic(
                create_compiler_diagnostic(
                    &Diagnostics::Options_0_and_1_cannot_be_combined,
                    Some(vec!["clean".to_owned(), "verbose".to_owned()]),
                )
                .into(),
            ),
        );
    }
    if matches!(build_options.clean, Some(true)) && matches!(build_options.watch, Some(true)) {
        errors.push(
            arena.alloc_diagnostic(
                create_compiler_diagnostic(
                    &Diagnostics::Options_0_and_1_cannot_be_combined,
                    Some(vec!["clean".to_owned(), "watch".to_owned()]),
                )
                .into(),
            ),
        );
    }
    if matches!(build_options.watch, Some(true)) && matches!(build_options.dry, Some(true)) {
        errors.push(
            arena.alloc_diagnostic(
                create_compiler_diagnostic(
                    &Diagnostics::Options_0_and_1_cannot_be_combined,
                    Some(vec!["watch".to_owned(), "dry".to_owned()]),
                )
                .into(),
            ),
        );
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

pub trait DiagnosticReporter {
    fn call(&self, diagnostic: Id<Diagnostic>) -> io::Result<()>;
}

pub trait ConfigFileDiagnosticsReporter {
    fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Id<Diagnostic>);
}

pub trait ParseConfigFileHost: ParseConfigHost + ConfigFileDiagnosticsReporter {
    fn get_current_directory(&self) -> io::Result<String>;
}

pub fn get_parsed_command_line_of_config_file(
    config_file_name: &str,
    options_to_extend: Option<Id<CompilerOptions>>,
    host: &impl ParseConfigFileHost,
    extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
    watch_options_to_extend: Option<Rc<WatchOptions>>,
    extra_file_extensions: Option<&[FileExtensionInfo]>,
    arena: &impl HasArena,
) -> io::Result<Option<ParsedCommandLine>> {
    let config_file_text = try_read_file(
        config_file_name,
        |file_name| host.read_file(file_name),
        arena,
    );
    if let StringOrRcDiagnostic::RcDiagnostic(ref config_file_text) = config_file_text {
        host.on_un_recoverable_config_file_diagnostic(config_file_text.clone());
        return Ok(None);
    }
    let config_file_text = enum_unwrapped!(config_file_text, [StringOrRcDiagnostic, String]);

    let result = parse_json_text(config_file_name, config_file_text, arena);
    let cwd = host.get_current_directory()?;
    let result_ref = result.ref_(arena);
    let result_as_source_file = result_ref.as_source_file();
    result_as_source_file.set_path(to_path(
        config_file_name,
        Some(&cwd),
        create_get_canonical_file_name(host.use_case_sensitive_file_names()),
    ));
    result_as_source_file.set_resolved_path(Some(result_as_source_file.path().clone()));
    result_as_source_file.set_original_file_name(Some(result_as_source_file.file_name().clone()));
    Ok(Some(parse_json_source_file_config_file_content(
        result,
        host,
        &get_normalized_absolute_path(&get_directory_path(config_file_name), Some(&cwd)),
        options_to_extend,
        Some(&get_normalized_absolute_path(config_file_name, Some(&cwd))),
        None,
        extra_file_extensions,
        extended_config_cache,
        watch_options_to_extend,
        arena,
    )?))
}

pub fn read_config_file(
    file_name: &str,
    read_file: impl FnMut(&str) -> io::Result<Option<String>>,
    arena: &impl HasArena,
) -> io::Result<ReadConfigFileReturn> {
    let text_or_diagnostic = try_read_file(file_name, read_file, arena);
    Ok(match text_or_diagnostic {
        StringOrRcDiagnostic::String(text_or_diagnostic) => {
            parse_config_file_text_to_json(file_name, text_or_diagnostic, arena)?
        }
        StringOrRcDiagnostic::RcDiagnostic(text_or_diagnostic) => ReadConfigFileReturn {
            config: Some(serde_json::Value::Object(serde_json::Map::new())),
            error: Some(text_or_diagnostic),
        },
    })
}

pub struct ReadConfigFileReturn {
    pub config: Option<serde_json::Value>,
    pub error: Option<Id<Diagnostic>>,
}

pub fn parse_config_file_text_to_json(
    file_name: &str,
    json_text: String,
    arena: &impl HasArena,
) -> io::Result<ReadConfigFileReturn> {
    let json_source_file = parse_json_text(file_name, json_text, arena);
    let json_source_file_parse_diagnostics = json_source_file
        .ref_(arena)
        .as_source_file()
        .parse_diagnostics();
    let config = convert_config_file_to_object(
        json_source_file,
        json_source_file_parse_diagnostics.clone(),
        false,
        Option::<&JsonConversionNotifierDummy>::None,
        arena,
    )?;
    let json_source_file_parse_diagnostics = json_source_file_parse_diagnostics.ref_(arena);
    Ok(ReadConfigFileReturn {
        config,
        error: if !json_source_file_parse_diagnostics.is_empty() {
            Some(json_source_file_parse_diagnostics[0].clone())
        } else {
            None
        },
    })
}

pub fn read_json_config_file(
    file_name: &str,
    read_file: impl FnMut(&str) -> io::Result<Option<String>>,
    arena: &impl HasArena,
) -> Id<Node /*TsConfigSourceFile*/> {
    let text_or_diagnostic = try_read_file(file_name, read_file, arena);
    match text_or_diagnostic {
        StringOrRcDiagnostic::String(text_or_diagnostic) => {
            parse_json_text(file_name, text_or_diagnostic, arena)
        }
        StringOrRcDiagnostic::RcDiagnostic(text_or_diagnostic) => {
            let base_node = BaseNode::new(
                SyntaxKind::Unknown,
                NodeFlags::None,
                TransformFlags::None,
                -1,
                -1,
                arena,
            );
            let end_of_file_token = BaseNode::new(
                SyntaxKind::EndOfFileToken,
                NodeFlags::None,
                TransformFlags::None,
                -1,
                -1,
                arena,
            )
            .alloc(arena.arena());
            let source_file = SourceFile::new(
                base_node,
                NodeArray::new(vec![], -1, -1, false, None, arena),
                end_of_file_token,
                file_name.to_owned(),
                "".to_owned(),
                ScriptTarget::Latest,
                LanguageVariant::Standard,
                ScriptKind::Unknown,
                false,
                false,
            )
            .alloc(arena.arena());
            source_file
                .ref_(arena)
                .as_source_file()
                .set_parse_diagnostics(arena.alloc_vec_diagnostic(vec![text_or_diagnostic]));
            source_file
        }
    }
}

pub(crate) enum StringOrRcDiagnostic {
    String(String),
    RcDiagnostic(Id<Diagnostic>),
}

impl From<String> for StringOrRcDiagnostic {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<Id<Diagnostic>> for StringOrRcDiagnostic {
    fn from(value: Id<Diagnostic>) -> Self {
        Self::RcDiagnostic(value)
    }
}

pub(crate) fn try_read_file(
    file_name: &str,
    mut read_file: impl FnMut(&str) -> io::Result<Option<String>>,
    arena: &impl HasArena,
) -> StringOrRcDiagnostic {
    match read_file(file_name) {
        Err(e) => Into::<StringOrRcDiagnostic>::into(
            arena.alloc_diagnostic(
                create_compiler_diagnostic(
                    &Diagnostics::Cannot_read_file_0_Colon_1,
                    Some(vec![file_name.to_owned(), e.to_string()]),
                )
                .into(),
            ),
        ),
        Ok(None) => Into::<StringOrRcDiagnostic>::into(
            arena.alloc_diagnostic(
                create_compiler_diagnostic(
                    &Diagnostics::Cannot_read_file_0,
                    Some(vec![file_name.to_owned()]),
                )
                .into(),
            ),
        ),
        Ok(Some(text)) => text.into(),
    }
}

pub(super) fn command_line_options_to_map(
    options: &[Id<CommandLineOption>],
    arena: &impl HasArena,
) -> HashMap<String, Id<CommandLineOption>> {
    array_to_map(
        options,
        |option| Some(get_option_name(&option.ref_(arena)).to_owned()),
        |item| item.clone(),
    )
}

struct TypeAcquisitionDidYouMeanDiagnostics {
    arena: *const AllArenas,
}

impl TypeAcquisitionDidYouMeanDiagnostics {
    pub fn new(arena: &impl HasArena) -> Id<Box<dyn DidYouMeanOptionsDiagnostics>> {
        arena.alloc_did_you_mean_options_diagnostics(Box::new(Self {
            arena: arena.arena(),
        }))
    }
}

impl DidYouMeanOptionsDiagnostics for TypeAcquisitionDidYouMeanDiagnostics {
    fn maybe_alternate_mode(&self) -> Option<Rc<AlternateModeDiagnostics>> {
        None
    }

    fn option_declarations(&self) -> Id<Vec<Id<CommandLineOption>>> {
        type_acquisition_declarations(self)
    }

    fn unknown_option_diagnostic(&self) -> &'static DiagnosticMessage {
        &Diagnostics::Unknown_type_acquisition_option_0
    }

    fn unknown_did_you_mean_diagnostic(&self) -> &'static DiagnosticMessage {
        &Diagnostics::Unknown_type_acquisition_option_0_Did_you_mean_1
    }
}

impl_has_arena!(TypeAcquisitionDidYouMeanDiagnostics);

pub(super) fn type_acquisition_did_you_mean_diagnostics(
    arena: &impl HasArena,
) -> Id<Box<dyn DidYouMeanOptionsDiagnostics>> {
    per_arena!(
        Box<dyn DidYouMeanOptionsDiagnostics>,
        arena,
        TypeAcquisitionDidYouMeanDiagnostics::new(arena),
    )
}

pub(super) fn get_watch_options_name_map(arena: &impl HasArena) -> Id<OptionsNameMap> {
    per_arena!(
        OptionsNameMap,
        arena,
        arena.alloc_options_name_map(create_option_name_map(
            &options_for_watch(arena).ref_(arena),
            arena
        ))
    )
}

pub struct WatchOptionsDidYouMeanDiagnostics {
    arena: *const AllArenas,
}

impl WatchOptionsDidYouMeanDiagnostics {
    pub fn new(arena: &impl HasArena) -> Id<Box<dyn ParseCommandLineWorkerDiagnostics>> {
        arena.alloc_parse_command_line_worker_diagnostics(Box::new(Self {
            arena: arena.arena(),
        }))
    }
}

impl DidYouMeanOptionsDiagnostics for WatchOptionsDidYouMeanDiagnostics {
    fn maybe_alternate_mode(&self) -> Option<Rc<AlternateModeDiagnostics>> {
        None
    }

    fn option_declarations(&self) -> Id<Vec<Id<CommandLineOption>>> {
        options_for_watch(self)
    }

    fn unknown_option_diagnostic(&self) -> &'static DiagnosticMessage {
        &Diagnostics::Unknown_watch_option_0
    }

    fn unknown_did_you_mean_diagnostic(&self) -> &'static DiagnosticMessage {
        &Diagnostics::Unknown_watch_option_0_Did_you_mean_1
    }
}

impl ParseCommandLineWorkerDiagnostics for WatchOptionsDidYouMeanDiagnostics {
    fn get_options_name_map(&self) -> Id<OptionsNameMap> {
        get_watch_options_name_map(self)
    }

    fn option_type_mismatch_diagnostic(&self) -> &'static DiagnosticMessage {
        &Diagnostics::Watch_option_0_requires_a_value_of_type_1
    }

    fn as_did_you_mean_options_diagnostics(&self) -> &(dyn DidYouMeanOptionsDiagnostics + 'static) {
        self
    }
}

impl_has_arena!(WatchOptionsDidYouMeanDiagnostics);

pub(super) fn watch_options_did_you_mean_diagnostics(
    arena: &impl HasArena,
) -> Id<Box<dyn ParseCommandLineWorkerDiagnostics>> {
    per_arena!(
        Box<dyn ParseCommandLineWorkerDiagnostics>,
        arena,
        WatchOptionsDidYouMeanDiagnostics::new(arena),
    )
}

pub(super) fn get_command_line_compiler_options_map(
    arena: &impl HasArena,
) -> Id<HashMap<String, Id<CommandLineOption>>> {
    per_arena!(
        HashMap<String, Id<CommandLineOption>>,
        arena,
        arena.alloc_command_line_options_map(
            command_line_options_to_map(&option_declarations(arena).ref_(arena), arena)
        )
    )
}

pub(super) fn get_command_line_watch_options_map(
    arena: &impl HasArena,
) -> Id<HashMap<String, Id<CommandLineOption>>> {
    per_arena!(
        HashMap<String, Id<CommandLineOption>>,
        arena,
        arena.alloc_command_line_options_map(
            command_line_options_to_map(&options_for_watch(arena).ref_(arena), arena)
        )
    )
}

pub(super) fn get_command_line_type_acquisition_map(
    arena: &impl HasArena,
) -> Id<HashMap<String, Id<CommandLineOption>>> {
    per_arena!(
        HashMap<String, Id<CommandLineOption>>,
        arena,
        arena.alloc_command_line_options_map(
            command_line_options_to_map(&type_acquisition_declarations(arena).ref_(arena), arena)
        )
    )
}

pub(super) const tsconfig_root_options_dummy_name: &str = "TSCONFIG ROOT OPTIONS";
thread_local! {
    static _tsconfig_root_options: RefCell<Option<Id<CommandLineOption>>> = RefCell::new(None);
}
pub(super) fn get_tsconfig_root_options_map(arena: &impl HasArena) -> Id<CommandLineOption> {
    per_arena!(
        CommandLineOption,
        arena,
        arena.alloc_command_line_option(
            TsConfigOnlyOption::new(
                CommandLineOptionBaseBuilder::default()
                    .name(tsconfig_root_options_dummy_name.to_owned())
                    .type_(CommandLineOptionType::Object)
                    .build().unwrap(),
                Some(arena.alloc_command_line_options_map(command_line_options_to_map(&vec![
                    arena.alloc_command_line_option(TsConfigOnlyOption::new(
                        CommandLineOptionBaseBuilder::default()
                            .name("compilerOptions".to_string())
                            .type_(CommandLineOptionType::Object)
                            .build().unwrap(),
                        Some(get_command_line_compiler_options_map(arena)),
                        Some(RcDynDidYouMeanOptionsDiagnosticsOrRcDynParseCommandLineWorkerDiagnostics::from_parse_command_line_worker_diagnostics(compiler_options_did_you_mean_diagnostics(arena), arena)),
                        arena,
                    )
                    .into()),
                    arena.alloc_command_line_option(TsConfigOnlyOption::new(
                        CommandLineOptionBaseBuilder::default()
                            .name("watchOptions".to_string())
                            .type_(CommandLineOptionType::Object)
                            .build().unwrap(),
                        Some(get_command_line_watch_options_map(arena)),
                        Some(RcDynDidYouMeanOptionsDiagnosticsOrRcDynParseCommandLineWorkerDiagnostics::from_parse_command_line_worker_diagnostics(watch_options_did_you_mean_diagnostics(arena), arena)),
                        arena,
                    )
                    .into()),
                    arena.alloc_command_line_option(TsConfigOnlyOption::new(
                        CommandLineOptionBaseBuilder::default()
                            .name("typingOptions".to_string())
                            .type_(CommandLineOptionType::Object)
                            .build().unwrap(),
                        Some(get_command_line_type_acquisition_map(arena)),
                        Some(RcDynDidYouMeanOptionsDiagnosticsOrRcDynParseCommandLineWorkerDiagnostics::from_did_you_mean_options_diagnostics(type_acquisition_did_you_mean_diagnostics(arena), arena)),
                        arena,
                    )
                    .into()),
                    arena.alloc_command_line_option(TsConfigOnlyOption::new(
                        CommandLineOptionBaseBuilder::default()
                            .name("typeAcquisition".to_string())
                            .type_(CommandLineOptionType::Object)
                            .build().unwrap(),
                        Some(get_command_line_type_acquisition_map(arena)),
                        Some(RcDynDidYouMeanOptionsDiagnosticsOrRcDynParseCommandLineWorkerDiagnostics::from_did_you_mean_options_diagnostics(type_acquisition_did_you_mean_diagnostics(arena), arena)),
                        arena,
                    )
                    .into()),
                    arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                        .name("extends".to_string())
                        .type_(CommandLineOptionType::String)
                        .category(&Diagnostics::File_Management)
                        .build().unwrap().try_into_command_line_option(arena).unwrap()),
                    arena.alloc_command_line_option(CommandLineOptionOfListType::new(
                        CommandLineOptionBaseBuilder::default()
                            .name("references".to_string())
                            .type_(CommandLineOptionType::List)
                            .category(&Diagnostics::Projects)
                            .build().unwrap(),
                        arena.alloc_command_line_option(TsConfigOnlyOption::new(
                            CommandLineOptionBaseBuilder::default()
                                .name("references".to_string())
                                .type_(CommandLineOptionType::Object)
                                .build().unwrap(),
                            None,
                            None,
                            arena,
                        )
                        .into()),
                        arena,
                    )
                    .into()),
                    arena.alloc_command_line_option(CommandLineOptionOfListType::new(
                        CommandLineOptionBaseBuilder::default()
                            .name("files".to_string())
                            .type_(CommandLineOptionType::List)
                            .category(&Diagnostics::File_Management)
                            .build().unwrap(),
                        arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                            .name("files".to_string())
                            .type_(CommandLineOptionType::String)
                            .build().unwrap().try_into_command_line_option(arena).unwrap()),
                        arena,
                    )
                    .into()),
                    arena.alloc_command_line_option(CommandLineOptionOfListType::new(
                        CommandLineOptionBaseBuilder::default()
                            .name("include".to_string())
                            .type_(CommandLineOptionType::List)
                            .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::if_files_is_specified_otherwise_Asterisk_Asterisk_Slash_Asterisk))
                            .category(&Diagnostics::File_Management)
                            .build().unwrap(),
                        arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                            .name("include".to_string())
                            .type_(CommandLineOptionType::String)
                            .build().unwrap().try_into_command_line_option(arena).unwrap()),
                        arena,
                    )
                    .into()),
                    arena.alloc_command_line_option(CommandLineOptionOfListType::new(
                        CommandLineOptionBaseBuilder::default()
                            .name("exclude".to_string())
                            .type_(CommandLineOptionType::List)
                            .default_value_description(StringOrDiagnosticMessage::DiagnosticMessage(&Diagnostics::node_modules_bower_components_jspm_packages_plus_the_value_of_outDir_if_one_is_specified))
                            .category(&Diagnostics::File_Management)
                            .build().unwrap(),
                        arena.alloc_command_line_option(CommandLineOptionBaseBuilder::default()
                            .name("exclude".to_string())
                            .type_(CommandLineOptionType::String)
                            .build().unwrap().try_into_command_line_option(arena).unwrap()),
                        arena,
                    )
                    .into()),
                    compile_on_save_command_line_option(arena),
                ], arena))),
                None,
                arena,
            )
            .into(),
        )
    )
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
        key_node: Id<Node>, /*PropertyName*/
        value: Option<&serde_json::Value>,
        value_node: Id<Node>, /*Expression*/
    ) -> io::Result<()>;
    fn on_set_unknown_option_key_value_in_root(
        &self,
        key: &str,
        key_node: Id<Node>, /*PropertyName*/
        value: Option<&serde_json::Value>,
        value_node: Id<Node>, /*Expression*/
    );
}

pub(crate) struct JsonConversionNotifierDummy {}

impl JsonConversionNotifier for JsonConversionNotifierDummy {
    fn on_set_valid_option_key_value_in_parent(
        &self,
        _parent_option: &str,
        _option: &CommandLineOption,
        _value: Option<&serde_json::Value>,
    ) {
        unimplemented!()
    }

    fn on_set_valid_option_key_value_in_root(
        &self,
        _key: &str,
        _key_node: Id<Node>, /*PropertyName*/
        _value: Option<&serde_json::Value>,
        _value_node: Id<Node>, /*Expression*/
    ) -> io::Result<()> {
        unimplemented!()
    }

    fn on_set_unknown_option_key_value_in_root(
        &self,
        _key: &str,
        _key_node: Id<Node>, /*PropertyName*/
        _value: Option<&serde_json::Value>,
        _value_node: Id<Node>, /*Expression*/
    ) {
        unimplemented!()
    }
}

pub(super) fn convert_config_file_to_object(
    source_file: Id<Node>, /*JsonSourceFile*/
    errors: Id<Vec<Id<Diagnostic>>>,
    report_options_errors: bool,
    options_iterator: Option<&impl JsonConversionNotifier>,
    arena: &impl HasArena,
) -> io::Result<Option<serde_json::Value>> {
    let source_file_ref = source_file.ref_(arena);
    let source_file_as_source_file = source_file_ref.as_source_file();
    let root_expression = source_file_as_source_file
        .statements()
        .ref_(arena)
        .get(0)
        .map(|statement| statement.ref_(arena).as_expression_statement().expression);
    let known_root_options: Option<Id<CommandLineOption>> = if report_options_errors {
        Some(get_tsconfig_root_options_map(arena))
    } else {
        None
    };
    if let Some(root_expression) = root_expression.filter(|root_expression| {
        root_expression.ref_(arena).kind() != SyntaxKind::ObjectLiteralExpression
    }) {
        errors.ref_mut(arena).push(
            arena.alloc_diagnostic(
                create_diagnostic_for_node_in_source_file(
                    source_file,
                    root_expression,
                    &Diagnostics::The_root_value_of_a_0_file_must_be_an_object,
                    Some(vec![if get_base_file_name(
                        &source_file_as_source_file.file_name(),
                        None,
                        None,
                    ) == "jsconfig.json"
                    {
                        "jsconfig.json".to_owned()
                    } else {
                        "tsconfig.json".to_owned()
                    }]),
                    arena,
                )
                .into(),
            ),
        );
        if is_array_literal_expression(&root_expression.ref_(arena)) {
            let first_object = find(
                &root_expression
                    .ref_(arena)
                    .as_array_literal_expression()
                    .elements
                    .ref_(arena),
                |element, _| is_object_literal_expression(&element.ref_(arena)),
            )
            .copied();
            if let Some(first_object) = first_object {
                return convert_to_object_worker(
                    source_file,
                    Some(first_object),
                    errors,
                    true,
                    known_root_options,
                    options_iterator,
                    arena,
                );
            }
        }
        return Ok(Some(serde_json::Value::Object(serde_json::Map::new())));
    }
    convert_to_object_worker(
        source_file,
        root_expression,
        errors,
        true,
        known_root_options,
        options_iterator,
        arena,
    )
}

pub fn convert_to_object(
    source_file: Id<Node>, /*JsonSourceFile*/
    errors: Id<Push<Id<Diagnostic>>>,
    arena: &impl HasArena,
) -> io::Result<Option<serde_json::Value>> {
    convert_to_object_worker(
        source_file,
        source_file
            .ref_(arena)
            .as_source_file()
            .statements()
            .ref_(arena)
            .get(0)
            .map(|statement| statement.ref_(arena).as_expression_statement().expression),
        errors,
        true,
        None,
        Option::<&JsonConversionNotifierDummy>::None,
        arena,
    )
}

pub(crate) fn convert_to_object_worker(
    source_file: Id<Node>, /*JsonSourceFile*/
    root_expression: Option<Id<Node>>,
    errors: Id<Push<Id<Diagnostic>>>,
    return_value: bool,
    known_root_options: Option<Id<CommandLineOption>>,
    json_conversion_notifier: Option<&impl JsonConversionNotifier>,
    arena: &impl HasArena,
) -> io::Result<Option<serde_json::Value>> {
    let Some(root_expression) = root_expression else {
        return Ok(if return_value {
            Some(serde_json::Value::Object(serde_json::Map::new()))
        } else {
            None
        });
    };

    convert_property_value_to_json(
        errors,
        source_file,
        json_conversion_notifier,
        return_value,
        known_root_options,
        root_expression,
        known_root_options,
        arena,
    )
}
