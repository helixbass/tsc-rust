use std::{borrow::Cow, collections::HashMap, convert::TryInto, io, marker::PhantomData, rc::Rc};

use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;
use local_macros::enum_unwrapped;

use crate::{
    add_range, chain_diagnostic_messages, contains, convert_to_relative_path, count_where,
    create_compiler_diagnostic, create_get_canonical_file_name, ends_with,
    external_helpers_module_name_text, file_extension_is, find, flatten_diagnostic_message_text,
    for_each, for_each_entry, format_color_and_reset, format_diagnostic,
    format_diagnostics_with_color_and_context, get_directory_path, get_emit_script_target,
    get_normalized_absolute_path, get_parsed_command_line_of_config_file, get_pattern_from_spec,
    get_referenced_file_location, get_regex_from_pattern, get_sys, is_reference_file_location,
    is_referenced_file, maybe_for_each, out_file, package_id_to_string,
    sort_and_deduplicate_diagnostics, target_option_declaration, text_substring, BuilderProgram,
    CancellationToken, CommandLineOptionInterface, CommandLineOptionMapTypeValue,
    CompilerHost, CompilerOptions, ConfigFileDiagnosticsReporter, CreateProgram,
    CustomTransformers, Debug_, Diagnostic, DiagnosticCategory, DiagnosticMessage,
    DiagnosticMessageChain, DiagnosticRelatedInformationInterface, DiagnosticReporter, Diagnostics,
    EmitAndSemanticDiagnosticsBuilderProgram, EmitResult, ExitStatus, ExtendedConfigCacheEntry,
    Extension, FileExtensionInfo, FileIncludeKind, FileIncludeReason,
    ForegroundColorEscapeSequences, FormatDiagnosticsHost, HasArena, Matches, ModuleResolutionHost,
    Node, ParseConfigFileHost, ParseConfigHost, ParsedCommandLine, Program, ProgramHost,
    ProjectReference, ReferenceFileLocationOrSyntheticReferenceFileLocation,
    ReportEmitErrorSummary, ResolvedProjectReference, ScriptReferenceHost, SortedArray,
    SourceFileLike, StringOrRcNode, System, WatchCompilerHost, WatchCompilerHostOfConfigFile,
    WatchHost, WatchOptions, WatchStatusReporter, WriteFileCallback, AllArenas, InArena,
};

fn get_sys_format_diagnostics_host(arena: &impl HasArena) -> /*Option<*/Gc<SysFormatDiagnosticsHost>/*>*/ {
    /*sys ?*/ Gc::new(SysFormatDiagnosticsHost::new(get_sys(arena), arena))
}

#[derive(Trace, Finalize)]
struct SysFormatDiagnosticsHost {
    system: Id<Box<dyn System>>,
    #[unsafe_ignore_trace]
    get_canonical_file_name: fn(&str) -> String,
}

impl SysFormatDiagnosticsHost {
    pub fn new(system: Id<Box<dyn System>>, arena: &impl HasArena) -> Self {
        let system_use_case_sensitive_file_names = system.ref_(arena).use_case_sensitive_file_names();
        Self {
            system,
            get_canonical_file_name: create_get_canonical_file_name(
                system_use_case_sensitive_file_names,
            ),
        }
    }
}

impl FormatDiagnosticsHost for SysFormatDiagnosticsHost {
    fn get_current_directory(&self) -> io::Result<String> {
        self.system.ref_(self).get_current_directory()
    }

    fn get_new_line(&self) -> Cow<'_, str> {
        self.system.ref_(self).new_line().to_owned().into()
    }

    fn get_canonical_file_name(&self, file_name: &str) -> String {
        (self.get_canonical_file_name)(file_name)
    }
}

impl HasArena for SysFormatDiagnosticsHost {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub fn create_diagnostic_reporter(
    system: Id<Box<dyn System>>,
    pretty: Option<bool>,
    arena: &impl HasArena,
) -> Id<Box<dyn DiagnosticReporter>> {
    let host: Gc<SysFormatDiagnosticsHost> =
        if system == get_sys(arena)
        /*&& sysFormatDiagnosticsHost*/
        {
            get_sys_format_diagnostics_host(arena)
        } else {
            Gc::new(SysFormatDiagnosticsHost::new(system.clone(), arena))
        };
    arena.alloc_diagnostic_reporter(Box::new(DiagnosticReporterConcrete::new(
        host, pretty, system,
    )))
}

#[derive(Trace, Finalize)]
struct DiagnosticReporterConcrete {
    host: Gc<SysFormatDiagnosticsHost>,
    pretty: bool,
    diagnostics: GcCell<Vec<Id<Diagnostic>>>,
    system: Id<Box<dyn System>>,
}

impl DiagnosticReporterConcrete {
    pub fn new(
        host: Gc<SysFormatDiagnosticsHost>,
        pretty: Option<bool>,
        system: Id<Box<dyn System>>,
    ) -> Self {
        Self {
            host,
            pretty: pretty.unwrap_or(false),
            diagnostics: Default::default(),
            system,
        }
    }
}

impl DiagnosticReporter for DiagnosticReporterConcrete {
    fn call(&self, diagnostic: Id<Diagnostic>) -> io::Result<()> {
        if !self.pretty {
            self.system
                .ref_(self).write(&format_diagnostic(&diagnostic.ref_(self), &*self.host, self)?);
            return Ok(());
        }

        let mut diagnostics = self.diagnostics.borrow_mut();
        diagnostics.push(diagnostic);
        self.system.ref_(self).write(&format!(
            "{}{}",
            format_diagnostics_with_color_and_context(&diagnostics, &*self.host, self)?,
            self.host.get_new_line()
        ));
        diagnostics.pop();

        Ok(())
    }
}

impl HasArena for DiagnosticReporterConcrete {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

fn clear_screen_if_not_watching_for_file_changes(
    system: &dyn System,
    diagnostic: &Diagnostic,
    options: &CompilerOptions,
) -> bool {
    if system.is_clear_screen_supported()
        && !matches!(options.preserve_watch_output, Some(true))
        && !matches!(options.extended_diagnostics, Some(true))
        && !matches!(options.diagnostics, Some(true))
        && contains(Some(&**screen_starting_message_codes), &diagnostic.code())
    {
        system.clear_screen();
        return true;
    }

    false
}

lazy_static! {
    pub static ref screen_starting_message_codes: Vec<u32> = vec![
        Diagnostics::Starting_compilation_in_watch_mode.code,
        Diagnostics::File_change_detected_Starting_incremental_compilation.code
    ];
}

fn get_plain_diagnostic_following_new_lines(diagnostic: &Diagnostic, new_line: &str) -> String {
    if contains(Some(&**screen_starting_message_codes), &diagnostic.code()) {
        format!("{}{}", new_line, new_line)
    } else {
        new_line.to_owned()
    }
}

pub fn get_locale_time_string(_system: &dyn System) -> String {
    // TODO: how to mimic .toLocaleTimeString()?
    unimplemented!()
}

pub fn create_watch_status_reporter(
    system: Id<Box<dyn System>>,
    pretty: Option<bool>,
) -> WatchStatusReporterConcrete {
    WatchStatusReporterConcrete::new(system, pretty)
}

pub struct WatchStatusReporterConcrete {
    system: Id<Box<dyn System>>,
    pretty: bool,
}

impl WatchStatusReporterConcrete {
    pub fn new(system: Id<Box<dyn System>>, pretty: Option<bool>) -> Self {
        Self {
            system,
            pretty: pretty.unwrap_or(false),
        }
    }
}

impl WatchStatusReporter for WatchStatusReporterConcrete {
    fn call(
        &self,
        diagnostic: Id<Diagnostic>,
        new_line: &str,
        options: Id<CompilerOptions>,
        _error_count: Option<usize>,
    ) {
        if self.pretty {
            clear_screen_if_not_watching_for_file_changes(&**self.system.ref_(self), &diagnostic.ref_(self), &options.ref_(self));
            let mut output = format!(
                "[{}] ",
                format_color_and_reset(
                    &get_locale_time_string(&**self.system.ref_(self)),
                    ForegroundColorEscapeSequences::Grey
                )
            );
            output.push_str(&format!(
                "{}{}{}",
                flatten_diagnostic_message_text(
                    Some(diagnostic.ref_(self).message_text()),
                    self.system.ref_(self).new_line(),
                    None
                ),
                new_line,
                new_line
            ));
            self.system.ref_(self).write(&output);
        } else {
            let mut output = "".to_owned();

            if !clear_screen_if_not_watching_for_file_changes(&**self.system.ref_(self), &diagnostic.ref_(self), &options.ref_(self))
            {
                output.push_str(new_line);
            }

            output.push_str(&format!("{} - ", get_locale_time_string(&**self.system.ref_(self))));
            output.push_str(&format!(
                "{}{}",
                flatten_diagnostic_message_text(
                    Some(diagnostic.ref_(self).message_text()),
                    self.system.ref_(self).new_line(),
                    None
                ),
                get_plain_diagnostic_following_new_lines(&diagnostic.ref_(self), new_line)
            ));

            self.system.ref_(self).write(&output);
        }
    }
}

impl HasArena for WatchStatusReporterConcrete {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub fn parse_config_file_with_system(
    config_file_name: &str,
    options_to_extend: Id<CompilerOptions>,
    extended_config_cache: Option<&mut HashMap<String, ExtendedConfigCacheEntry>>,
    watch_options_to_extend: Option<Rc<WatchOptions>>,
    system: Id<Box<dyn System>>,
    report_diagnostic: Id<Box<dyn DiagnosticReporter>>,
    arena: &impl HasArena,
) -> io::Result<Option<ParsedCommandLine>> {
    let host = ParseConfigFileWithSystemHost::new(system, report_diagnostic);
    get_parsed_command_line_of_config_file(
        config_file_name,
        Some(options_to_extend),
        &host,
        extended_config_cache,
        watch_options_to_extend,
        None,
        arena,
    )
}

#[derive(Trace, Finalize)]
struct ParseConfigFileWithSystemHost {
    system: Id<Box<dyn System>>,
    report_diagnostic: Id<Box<dyn DiagnosticReporter>>,
}

impl ParseConfigFileWithSystemHost {
    pub fn new(
        system: Id<Box<dyn System>>,
        report_diagnostic: Id<Box<dyn DiagnosticReporter>>,
    ) -> Self {
        Self {
            system,
            report_diagnostic,
        }
    }
}

impl ParseConfigFileHost for ParseConfigFileWithSystemHost {
    fn get_current_directory(&self) -> io::Result<String> {
        self.system.ref_(self).get_current_directory()
    }
}

impl ParseConfigHost for ParseConfigFileWithSystemHost {
    fn use_case_sensitive_file_names(&self) -> bool {
        self.system.ref_(self).use_case_sensitive_file_names()
    }

    fn read_directory(
        &self,
        root_dir: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> io::Result<Vec<String>> {
        self.system
            .ref_(self).read_directory(root_dir, Some(extensions), excludes, Some(includes), depth)
    }

    fn file_exists(&self, path: &str) -> bool {
        self.system.ref_(self).file_exists(path)
    }

    fn read_file(&self, path: &str) -> io::Result<Option<String>> {
        self.system.ref_(self).read_file(path)
    }

    fn is_trace_supported(&self) -> bool {
        false
    }

    fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
        self
    }
}

impl ConfigFileDiagnosticsReporter for ParseConfigFileWithSystemHost {
    fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Id<Diagnostic>) {
        report_unrecoverable_diagnostic(&**self.system.ref_(self), &**self.report_diagnostic.ref_(self), diagnostic)
    }
}

impl HasArena for ParseConfigFileWithSystemHost {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub fn get_error_count_for_summary(diagnostics: &[Id<Diagnostic>], arena: &impl HasArena) -> usize {
    count_where(Some(diagnostics), |diagnostic, _| {
        diagnostic.ref_(arena).category() == DiagnosticCategory::Error
    })
}

pub fn get_watch_error_summary_diagnostic_message(
    error_count: usize,
) -> &'static DiagnosticMessage {
    if error_count == 1 {
        &Diagnostics::Found_1_error_Watching_for_file_changes
    } else {
        &Diagnostics::Found_0_errors_Watching_for_file_changes
    }
}

pub fn get_error_summary_text(error_count: usize, new_line: &str) -> String {
    if error_count == 0 {
        return "".to_owned();
    }
    let d = create_compiler_diagnostic(
        if error_count == 1 {
            &Diagnostics::Found_1_error
        } else {
            &Diagnostics::Found_0_errors
        },
        Some(vec![error_count.to_string()]),
    );
    format!(
        "{}{}{}{}",
        new_line,
        flatten_diagnostic_message_text(Some(d.message_text()), new_line, None),
        new_line,
        new_line
    )
}

#[derive(Trace, Finalize)]
pub enum ProgramOrBuilderProgram {
    Program(Id<Program>),
    BuilderProgram(Gc<Box<dyn BuilderProgram>>),
}

impl ProgramOrBuilderProgram {
    fn get_compiler_options(&self, arena: &impl HasArena) -> Id<CompilerOptions> {
        match self {
            Self::Program(program) => program.ref_(arena).get_compiler_options(),
            Self::BuilderProgram(program) => program.get_compiler_options(),
        }
    }

    fn get_source_files(&self, arena: &impl HasArena) -> Vec<Id<Node>> {
        match self {
            Self::Program(program) => program.ref_(arena).get_source_files().clone(),
            Self::BuilderProgram(program) => program.get_source_files().to_owned(),
        }
    }
}

impl From<Id<Program>> for ProgramOrBuilderProgram {
    fn from(value: Id<Program>) -> Self {
        Self::Program(value)
    }
}

impl From<Gc<Box<dyn BuilderProgram>>> for ProgramOrBuilderProgram {
    fn from(value: Gc<Box<dyn BuilderProgram>>) -> Self {
        Self::BuilderProgram(value)
    }
}

pub fn is_builder_program(program: &ProgramOrBuilderProgram) -> bool {
    matches!(program, ProgramOrBuilderProgram::BuilderProgram(_))
}

pub fn list_files(program: ProgramOrBuilderProgram, mut write: impl FnMut(&str), arena: &impl HasArena) {
    let options = program.get_compiler_options(arena);
    if matches!(options.ref_(arena).explain_files, Some(true)) {
        explain_files(
            &if is_builder_program(&program) {
                enum_unwrapped!(&program, [ProgramOrBuilderProgram, BuilderProgram]).get_program()
            } else {
                enum_unwrapped!(&program, [ProgramOrBuilderProgram, Program]).clone()
            }.ref_(arena),
            write,
        );
    } else if matches!(options.ref_(arena).list_files, Some(true))
        || matches!(options.ref_(arena).list_files_only, Some(true))
    {
        for_each(program.get_source_files(arena), |file, _| {
            write(&file.ref_(arena).as_source_file().file_name());
            Option::<()>::None
        });
    }
}

pub fn explain_files(program: &Program, mut write: impl FnMut(&str)) {
    let reasons = program.get_file_include_reasons();
    let get_canonical_file_name =
        create_get_canonical_file_name(program.use_case_sensitive_file_names());
    let relative_file_name = |file_name: &str| {
        convert_to_relative_path(file_name, &program.get_current_directory(), |file_name| {
            get_canonical_file_name(file_name)
        })
    };
    for &file in &*program.get_source_files() {
        write(&to_file_name(file.clone(), Some(&relative_file_name), program));
        (*reasons)
            .borrow()
            .get(&file.ref_(program).as_source_file().path())
            .map(|reasons| {
                reasons.iter().for_each(|&reason| {
                    write(&format!(
                        "  {}",
                        file_include_reason_to_diagnostics(
                            program,
                            &reason.ref_(program),
                            Some(relative_file_name),
                            program,
                        )
                        .message_text
                    ))
                })
            });
        explain_if_file_is_redirect(file, Some(relative_file_name), program).map(|diagnostics| {
            diagnostics
                .iter()
                .for_each(|d| write(&format!("  {}", d.message_text)))
        });
    }
}

pub fn explain_if_file_is_redirect(
    file: Id<Node>, /*SourceFile*/
    file_name_convertor: Option<impl Fn(&str) -> String>,
    arena: &impl HasArena,
) -> Option<Vec<DiagnosticMessageChain>> {
    let mut result: Option<Vec<DiagnosticMessageChain>> = None;
    let file_ref = file.ref_(arena);
    let file_as_source_file = file_ref.as_source_file();
    if file_as_source_file.maybe_path().as_ref()
        != file_as_source_file.maybe_resolved_path().as_ref()
    {
        if result.is_none() {
            result = Some(vec![]);
        }
        result.as_mut().unwrap().push(chain_diagnostic_messages(
            None,
            &Diagnostics::File_is_output_of_project_reference_source_0,
            Some(vec![to_file_name(
                file_as_source_file.original_file_name().clone(),
                file_name_convertor.as_ref(),
                arena,
            )]),
        ));
    }
    if let Some(file_redirect_info) = file_as_source_file.maybe_redirect_info().as_ref() {
        if result.is_none() {
            result = Some(vec![]);
        }
        result.as_mut().unwrap().push(chain_diagnostic_messages(
            None,
            &Diagnostics::File_redirects_to_file_0,
            Some(vec![to_file_name(
                file_redirect_info.redirect_target.clone(),
                file_name_convertor.as_ref(),
                arena,
            )]),
        ));
    }
    result
}

pub fn get_matched_file_spec(program: &Program, file_name: &str, arena: &impl HasArena) -> Option<String> {
    let program_compiler_options = program.get_compiler_options();
    let config_file = program_compiler_options.ref_(arena).config_file;
    let config_file_config_file_specs_validated_files_spec = config_file
        .and_then(|config_file| {
            config_file
                .ref_(program).as_source_file()
                .maybe_config_file_specs()
                .clone()
        })
        .and_then(|config_file_specs| config_file_specs.validated_files_spec.clone())?;

    let get_canonical_file_name =
        create_get_canonical_file_name(program.use_case_sensitive_file_names());
    let file_path = get_canonical_file_name(file_name);
    let config_file = config_file.unwrap();
    let config_file_ref = config_file.ref_(program);
    let config_file_as_source_file = config_file_ref.as_source_file();
    let base_path = get_directory_path(&get_normalized_absolute_path(
        &config_file_as_source_file.file_name(),
        Some(&program.get_current_directory()),
    ));
    find(
        &config_file_config_file_specs_validated_files_spec,
        |file_spec, _| {
            get_canonical_file_name(&get_normalized_absolute_path(file_spec, Some(&base_path)))
                == file_path
        },
    )
    .map(Clone::clone)
}

pub fn get_matched_include_spec(program: &Program, file_name: &str, arena: &impl HasArena) -> Option<String> {
    let program_compiler_options = program.get_compiler_options();
    let config_file = program_compiler_options.ref_(arena).config_file;
    let config_file_config_file_specs_validated_include_specs = config_file
        .and_then(|config_file| {
            config_file
                .ref_(program).as_source_file()
                .maybe_config_file_specs()
                .clone()
        })
        .and_then(|config_file_specs| config_file_specs.validated_include_specs.clone())?;

    let is_json_file = file_extension_is(file_name, Extension::Json.to_str());
    let config_file = config_file.unwrap();
    let config_file_ref = config_file.ref_(program);
    let config_file_as_source_file = config_file_ref.as_source_file();
    let base_path = get_directory_path(&get_normalized_absolute_path(
        &config_file_as_source_file.file_name(),
        Some(&program.get_current_directory()),
    ));
    let use_case_sensitive_file_names = program.use_case_sensitive_file_names();
    find(
        &config_file_config_file_specs_validated_include_specs,
        |include_spec, _| {
            if is_json_file && !ends_with(include_spec, Extension::Json.to_str()) {
                return false;
            }
            let pattern = get_pattern_from_spec(include_spec, &base_path, "files");
            match pattern {
                None => false,
                Some(pattern) => get_regex_from_pattern(
                    &format!("({})$", pattern),
                    use_case_sensitive_file_names,
                )
                .is_match(file_name)
                .unwrap(),
            }
        },
    )
    .map(Clone::clone)
}

pub fn file_include_reason_to_diagnostics(
    program: &Program,
    reason: &FileIncludeReason,
    file_name_convertor: Option<impl Fn(&str) -> String>,
    arena: &impl HasArena,
) -> DiagnosticMessageChain {
    let options = program.get_compiler_options();
    if is_referenced_file(Some(reason)) {
        let reason = enum_unwrapped!(reason, [FileIncludeReason, ReferencedFile]);
        let reference_location =
            get_referenced_file_location(|path| program.get_source_file_by_path(path), reason, arena);
        let reference_text = match &reference_location {
            ReferenceFileLocationOrSyntheticReferenceFileLocation::ReferenceFileLocation(reference_location) =>
                text_substring(
                    &reference_location.file.ref_(program).as_source_file().text_as_chars(),
                    reference_location.pos.try_into().unwrap(),
                    reference_location.end.try_into().unwrap(),
                ),
            ReferenceFileLocationOrSyntheticReferenceFileLocation::SyntheticReferenceFileLocation(reference_location) =>
                format!("\"{}\"", reference_location.text),
        };
        let message: &DiagnosticMessage;
        Debug_.assert(
            is_reference_file_location(&reference_location)
                || reason.kind == FileIncludeKind::Import,
            Some("Only synthetic references are imports"),
        );
        match reason.kind {
            FileIncludeKind::Import => {
                match &reference_location {
                    ReferenceFileLocationOrSyntheticReferenceFileLocation::ReferenceFileLocation(reference_location) => {
                        message = if reference_location.package_id.is_some() {
                            &Diagnostics::Imported_via_0_from_file_1_with_packageId_2
                        } else {
                            &Diagnostics::Imported_via_0_from_file_1
                        };
                    }
                    ReferenceFileLocationOrSyntheticReferenceFileLocation::SyntheticReferenceFileLocation(reference_location) => {
                        if reference_location.text == external_helpers_module_name_text {
                            message = if reference_location.package_id.is_some() {
                                &Diagnostics::Imported_via_0_from_file_1_with_packageId_2_to_import_importHelpers_as_specified_in_compilerOptions
                            } else {
                                &Diagnostics::Imported_via_0_from_file_1_to_import_importHelpers_as_specified_in_compilerOptions
                            };
                        } else {
                            message = if reference_location.package_id.is_some() {
                                &Diagnostics::Imported_via_0_from_file_1_with_packageId_2_to_import_jsx_and_jsxs_factory_functions
                            } else {
                                &Diagnostics::Imported_via_0_from_file_1_to_import_jsx_and_jsxs_factory_functions
                            };
                        }
                    }
                }
            }
            FileIncludeKind::ReferenceFile => {
                Debug_.assert(reference_location.maybe_package_id().is_none(), None);
                message = &Diagnostics::Referenced_via_0_from_file_1;
            }
            FileIncludeKind::TypeReferenceDirective => {
                message = if reference_location.maybe_package_id().is_some() {
                    &Diagnostics::Type_library_referenced_via_0_from_file_1_with_packageId_2
                } else {
                    &Diagnostics::Type_library_referenced_via_0_from_file_1
                };
            }
            FileIncludeKind::LibReferenceDirective => {
                Debug_.assert(reference_location.maybe_package_id().is_none(), None);
                 message = &Diagnostics::Type_library_referenced_via_0_from_file_1;
            }
            _ => {
                Debug_.assert_never(reason, None);
            }
        }
        return chain_diagnostic_messages(
            None,
            &message,
            Some(match reference_location.maybe_package_id() {
                Some(reference_location_package_id) => {
                    vec![
                        reference_text,
                        to_file_name(reference_location.file(), file_name_convertor.as_ref(), program),
                        package_id_to_string(reference_location_package_id),
                    ]
                }
                None => {
                    vec![
                        reference_text,
                        to_file_name(reference_location.file(), file_name_convertor.as_ref(), program),
                    ]
                }
            }),
        );
    }
    match reason {
        FileIncludeReason::RootFile(reason) => {
            if !options.ref_(arena).config_file.matches(|config_file| {
                config_file
                    .ref_(program).as_source_file()
                    .maybe_config_file_specs()
                    .is_some()
            }) {
                return chain_diagnostic_messages(
                    None,
                    &Diagnostics::Root_file_specified_for_compilation,
                    None,
                );
            }
            let file_name = get_normalized_absolute_path(
                &program.get_root_file_names()[reason.index],
                Some(&program.get_current_directory()),
            );
            let matched_by_files = get_matched_file_spec(program, &file_name, arena);
            if matched_by_files.is_some() {
                return chain_diagnostic_messages(
                    None,
                    &Diagnostics::Part_of_files_list_in_tsconfig_json,
                    None,
                );
            }
            let matched_by_include = get_matched_include_spec(program, &file_name, arena);
            if let Some(matched_by_include) = matched_by_include {
                chain_diagnostic_messages(
                    None,
                    &Diagnostics::Matched_by_include_pattern_0_in_1,
                    Some(vec![
                        matched_by_include,
                        to_file_name(
                            options.ref_(arena).config_file.clone().unwrap(),
                            file_name_convertor.as_ref(),
                            program,
                        ),
                    ]),
                )
            } else {
                chain_diagnostic_messages(
                    None,
                    &Diagnostics::Root_file_specified_for_compilation,
                    None,
                )
            }
        }
        FileIncludeReason::ProjectReferenceFile(reason) => {
            let is_output = reason.kind == FileIncludeKind::OutputFromProjectReference;
            let referenced_resolved_ref: Gc<ResolvedProjectReference> = Debug_.check_defined(
                program.get_resolved_project_references().as_ref().and_then(
                    |resolved_project_references| {
                        resolved_project_references
                            .get(reason.index)
                            .map(|option| option.clone().unwrap())
                    },
                ),
                None,
            );
            chain_diagnostic_messages(
                None,
                if out_file(&options.ref_(arena)).is_some() {
                    if is_output {
                        &Diagnostics::Output_from_referenced_project_0_included_because_1_specified
                    } else {
                        &Diagnostics::Source_from_referenced_project_0_included_because_1_specified
                    }
                } else {
                    if is_output {
                        &Diagnostics::Output_from_referenced_project_0_included_because_module_is_specified_as_none
                    } else {
                        &Diagnostics::Source_from_referenced_project_0_included_because_module_is_specified_as_none
                    }
                },
                Some(vec![
                    to_file_name(
                        {
                            let tmp: String = referenced_resolved_ref
                                .source_file
                                .ref_(program).as_source_file()
                                .file_name()
                                .clone();
                            tmp
                        },
                        file_name_convertor.as_ref(),
                        program,
                    ),
                    if options.ref_(arena).out_file.is_some() {
                        "--outFile"
                    } else {
                        "--out"
                    }
                    .to_owned(),
                ]),
            )
        }
        FileIncludeReason::AutomaticTypeDirectiveFile(reason) => chain_diagnostic_messages(
            None,
            if options.ref_(arena).types.is_some() {
                if reason.package_id.is_some() {
                    &Diagnostics::Entry_point_of_type_library_0_specified_in_compilerOptions_with_packageId_1
                } else {
                    &Diagnostics::Entry_point_of_type_library_0_specified_in_compilerOptions
                }
            } else {
                if reason.package_id.is_some() {
                    &Diagnostics::Entry_point_for_implicit_type_library_0_with_packageId_1
                } else {
                    &Diagnostics::Entry_point_for_implicit_type_library_0
                }
            },
            Some(match reason.package_id.as_ref() {
                Some(reason_package_id) => vec![
                    reason.type_reference.clone(),
                    package_id_to_string(reason_package_id),
                ],
                None => vec![reason.type_reference.clone()],
            }),
        ),
        FileIncludeReason::LibFile(reason) => {
            if let Some(reason_index) = reason.index {
                return chain_diagnostic_messages(
                    None,
                    &Diagnostics::Library_0_specified_in_compilerOptions,
                    Some(vec![options.ref_(arena).lib.as_ref().unwrap()[reason_index].clone()]),
                );
            }
            let target = target_option_declaration.with(|target_option_declaration_| {
                for_each_entry(target_option_declaration_.type_().as_map(), |value, key| {
                    if enum_unwrapped!(value, [CommandLineOptionMapTypeValue, ScriptTarget])
                        == &get_emit_script_target(&options.ref_(arena))
                    {
                        Some(key.to_string())
                    } else {
                        None
                    }
                })
            });
            chain_diagnostic_messages(
                None,
                if target.is_some() {
                    &Diagnostics::Default_library_for_target_0
                } else {
                    &Diagnostics::Default_library
                },
                target.map(|target| vec![target]),
            )
        }
        _ => {
            Debug_.assert_never(reason, None);
        }
    }
}

fn to_file_name(
    file: impl Into<StringOrRcNode>,
    file_name_convertor: Option<&impl Fn(&str) -> String>,
    arena: &impl HasArena,
) -> String {
    let file = file.into();
    let file_name = match file {
        StringOrRcNode::String(file) => file,
        StringOrRcNode::RcNode(file) => file.ref_(arena).as_source_file().file_name().clone(),
    };
    if let Some(file_name_convertor) = file_name_convertor {
        file_name_convertor(&file_name)
    } else {
        file_name
    }
}

struct EmitFilesAndReportErrorsReturn {
    emit_result: EmitResult,
    diagnostics: SortedArray<Id<Diagnostic>>,
}

fn emit_files_and_report_errors(
    program: Id<Program>,
    report_diagnostic: Id<Box<dyn DiagnosticReporter>>,
    write: Option<impl FnMut(&str)>,
    report_summary: Option<Rc<dyn ReportEmitErrorSummary>>,
    write_file: Option<Gc<Box<dyn WriteFileCallback>>>,
    cancellation_token: Option<Id<Box<dyn CancellationToken>>>,
    emit_only_dts_files: Option<bool>,
    custom_transformers: Option<&CustomTransformers>,
    arena: &impl HasArena,
) -> io::Result<EmitFilesAndReportErrorsReturn> {
    let is_list_files_only = program.ref_(arena).get_compiler_options().ref_(arena).list_files_only == Some(true);

    let mut all_diagnostics: Vec<Id<Diagnostic>> =
        program.ref_(arena).get_config_file_parsing_diagnostics().clone();
    let config_file_parsing_diagnostics_length = all_diagnostics.len();
    add_range(
        &mut all_diagnostics,
        Some(&program.ref_(arena).get_syntactic_diagnostics(None, cancellation_token.clone())),
        None,
        None,
    );

    if all_diagnostics.len() == config_file_parsing_diagnostics_length {
        add_range(
            &mut all_diagnostics,
            Some(&program.ref_(arena).get_options_diagnostics(cancellation_token.clone())),
            None,
            None,
        );

        if !is_list_files_only {
            add_range(
                &mut all_diagnostics,
                Some(&program.ref_(arena).get_global_diagnostics(cancellation_token.clone())?),
                None,
                None,
            );

            if all_diagnostics.len() == config_file_parsing_diagnostics_length {
                add_range(
                    &mut all_diagnostics,
                    Some(&program.ref_(arena).get_semantic_diagnostics(None, cancellation_token.clone())?),
                    None,
                    None,
                );
            }
        }
    }

    let emit_result = if is_list_files_only {
        EmitResult {
            emit_skipped: true,
            diagnostics: vec![],
            emitted_files: None,
            source_maps: None,
            exported_modules_from_declaration_emit: None,
        }
    } else {
        program.ref_(arena).emit(
            None,
            write_file,
            cancellation_token,
            emit_only_dts_files,
            custom_transformers,
            None,
        )?
    };
    let emitted_files = emit_result.emitted_files.as_ref();
    let emit_diagnostics = &emit_result.diagnostics;
    add_range(&mut all_diagnostics, Some(emit_diagnostics), None, None);

    let diagnostics = sort_and_deduplicate_diagnostics(&all_diagnostics, arena);
    for diagnostic in diagnostics.iter() {
        report_diagnostic.ref_(arena).call(diagnostic.clone())?;
    }
    if let Some(mut write) = write {
        let current_dir = program.ref_(arena).get_current_directory();
        maybe_for_each(emitted_files, |file, _| {
            let filepath = get_normalized_absolute_path(file, Some(&current_dir));
            write(&format!("TSFILE: {}", filepath));
            Option::<()>::None
        });
        list_files(program.clone().into(), write, arena);
    }

    if let Some(report_summary) = report_summary {
        report_summary.call(get_error_count_for_summary(&diagnostics, arena));
    }

    Ok(EmitFilesAndReportErrorsReturn {
        emit_result,
        diagnostics,
    })
}

pub fn emit_files_and_report_errors_and_get_exit_status(
    program: Id<Program>,
    report_diagnostic: Id<Box<dyn DiagnosticReporter>>,
    write: Option<impl FnMut(&str)>,
    report_summary: Option<Rc<dyn ReportEmitErrorSummary>>,
    write_file: Option<Gc<Box<dyn WriteFileCallback>>>,
    cancellation_token: Option<Id<Box<dyn CancellationToken>>>,
    emit_only_dts_files: Option<bool>,
    custom_transformers: Option<&CustomTransformers>,
    arena: &impl HasArena,
) -> io::Result<ExitStatus> {
    let EmitFilesAndReportErrorsReturn {
        emit_result,
        diagnostics,
    } = emit_files_and_report_errors(
        program,
        report_diagnostic,
        write,
        report_summary,
        write_file,
        cancellation_token,
        emit_only_dts_files,
        custom_transformers,
        arena,
    )?;
    // println!("diagnostics: {:#?}", diagnostics);

    if emit_result.emit_skipped && !diagnostics.is_empty() {
        return Ok(ExitStatus::DiagnosticsPresent_OutputsSkipped);
    } else if !diagnostics.is_empty() {
        return Ok(ExitStatus::DiagnosticsPresent_OutputsGenerated);
    }
    Ok(ExitStatus::Success)
}

fn report_unrecoverable_diagnostic(
    _system: &dyn System,
    _report_diagnostic: &dyn DiagnosticReporter,
    _diagnostic: Id<Diagnostic>,
) {
    unimplemented!()
}

pub struct CreateWatchCompilerHostOfConfigFileInput<
    'a,
    TBuilderProgram: BuilderProgram,
    // TCreateProgram: CreateProgram<TBuilderProgram>,
> {
    pub system: &'a dyn System,
    pub create_program: Option<&'a dyn CreateProgram<TBuilderProgram>>,
    pub report_diagnostic: Option<&'a dyn DiagnosticReporter>,
    pub report_watch_status: Option<Rc<dyn WatchStatusReporter>>,
    pub config_file_name: &'a str,
    pub options_to_extend: Option<&'a CompilerOptions>,
    pub watch_options_to_extend: Option<Rc<WatchOptions>>,
    pub extra_file_extensions: Option<&'a [FileExtensionInfo]>,
}

pub fn create_watch_compiler_host_of_config_file<'a, TBuilderProgram: BuilderProgram>(
    _: CreateWatchCompilerHostOfConfigFileInput<'a, TBuilderProgram>,
) -> impl WatchCompilerHostOfConfigFile<TBuilderProgram> {
    WatchCompilerHostOfConfigFileConcrete {
        phantom: PhantomData,
    }
}

pub struct WatchCompilerHostOfConfigFileConcrete<TBuilderProgram: BuilderProgram> {
    phantom: PhantomData<TBuilderProgram>,
}

impl<TBuilderProgram: BuilderProgram> WatchCompilerHostOfConfigFile<TBuilderProgram>
    for WatchCompilerHostOfConfigFileConcrete<TBuilderProgram>
{
}

impl<TBuilderProgram: BuilderProgram> WatchCompilerHost<TBuilderProgram>
    for WatchCompilerHostOfConfigFileConcrete<TBuilderProgram>
{
    fn as_program_host(&self) -> &dyn ProgramHost<TBuilderProgram> {
        self
    }
}

impl<TBuilderProgram: BuilderProgram> ProgramHost<TBuilderProgram>
    for WatchCompilerHostOfConfigFileConcrete<TBuilderProgram>
{
}

impl<TBuilderProgram: BuilderProgram> WatchHost
    for WatchCompilerHostOfConfigFileConcrete<TBuilderProgram>
{
}

impl<TBuilderProgram: BuilderProgram> ConfigFileDiagnosticsReporter
    for WatchCompilerHostOfConfigFileConcrete<TBuilderProgram>
{
    fn on_un_recoverable_config_file_diagnostic(&self, _diagnostic: Id<Diagnostic>) {
        unimplemented!()
    }
}

pub struct IncrementalCompilationOptions<'a> {
    pub root_names: &'a [String],
    pub options: &'a CompilerOptions,
    pub config_file_parsing_diagnostics: Option<&'a [Id<Diagnostic>]>,
    pub project_references: Option<&'a [Rc<ProjectReference>]>,
    pub host: Option<Id<Box<dyn CompilerHost>>>,
    pub report_diagnostic: Option<Id<Box<dyn DiagnosticReporter>>>,
    pub report_error_summary: Option<Rc<dyn ReportEmitErrorSummary>>,
    pub after_program_emit_and_diagnostics:
        Option<&'a dyn FnMut(Rc<dyn EmitAndSemanticDiagnosticsBuilderProgram>)>,
    pub system: Option<&'a dyn System>,
}

pub fn perform_incremental_compilation(_input: IncrementalCompilationOptions) -> ExitStatus {
    unimplemented!()
}
