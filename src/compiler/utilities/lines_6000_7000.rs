#![allow(non_upper_case_globals)]

use std::borrow::Cow;
use std::cmp;
use std::rc::Rc;

use crate::{
    compare_strings_case_sensitive, compare_strings_case_sensitive_maybe, compare_values, for_each,
    format_string_from_args, get_locale_specific_message, CommandLineOption,
    CommandLineOptionInterface, Comparison, CompilerOptions, CompilerOptionsValue, Diagnostic,
    DiagnosticInterface, DiagnosticMessage, DiagnosticMessageChain, DiagnosticMessageText,
    DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface, Extension, JsxEmit,
    LanguageVariant, ModuleKind, Pattern, ScriptKind, ScriptTarget,
};
use local_macros::enum_unwrapped;

pub fn chain_diagnostic_messages(
    details: Option<DiagnosticMessageChain>,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticMessageChain {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        text = format_string_from_args(&text, args);
    }
    DiagnosticMessageChain::new(
        text,
        message.category,
        message.code,
        details.map(|details| vec![details]),
    )
}

fn get_diagnostic_file_path<
    TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface,
>(
    diagnostic: &TDiagnosticRelatedInformation,
) -> Option<String> {
    diagnostic.maybe_file().and_then(|file| {
        file.as_source_file()
            .maybe_path()
            .as_ref()
            .map(|path| path.to_string())
    })
}

pub fn compare_diagnostics<TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface>(
    d1: &TDiagnosticRelatedInformation,
    d2: &TDiagnosticRelatedInformation,
) -> Comparison {
    let mut compared = compare_diagnostics_skip_related_information(d1, d2);
    if compared != Comparison::EqualTo {
        return compared;
    }
    if let Some(d1) = d1.maybe_as_diagnostic() {
        if let Some(d2) = d2.maybe_as_diagnostic() {
            compared = compare_related_information(d1, d2);
            if compared != Comparison::EqualTo {
                return compared;
            }
        }
    }
    Comparison::EqualTo
}

pub fn compare_diagnostics_skip_related_information<
    TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface,
>(
    d1: &TDiagnosticRelatedInformation,
    d2: &TDiagnosticRelatedInformation,
) -> Comparison {
    let mut compared = compare_strings_case_sensitive_maybe(
        get_diagnostic_file_path(d1).as_deref(),
        get_diagnostic_file_path(d2).as_deref(),
    );
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.start()), Some(d2.start()));
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.length()), Some(d2.length()));
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.code()), Some(d2.code()));
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_message_text(d1.message_text(), d2.message_text());
    if compared != Comparison::EqualTo {
        return compared;
    }
    Comparison::EqualTo
}

fn compare_related_information(d1: &Diagnostic, d2: &Diagnostic) -> Comparison {
    if d1.related_information().is_none() && d2.related_information().is_none() {
        return Comparison::EqualTo;
    }
    if let Some(d1_related_information) = d1.related_information().as_ref() {
        if let Some(d2_related_information) = d2.related_information().as_ref() {
            let compared = compare_values(
                Some(d1_related_information.len()),
                Some(d2_related_information.len()),
            );
            if compared != Comparison::EqualTo {
                return compared;
            }
            let compared_maybe = for_each(d1_related_information, |d1i, index| {
                let d2i = &d2_related_information[index];
                let compared = compare_diagnostics(&**d1i, &**d2i);
                match compared {
                    Comparison::EqualTo => None,
                    compared => Some(compared),
                }
            });
            if let Some(compared) = compared_maybe {
                if compared != Comparison::EqualTo {
                    return compared;
                }
            }
            return Comparison::EqualTo;
        }
    }
    if d1.related_information().is_some() {
        Comparison::LessThan
    } else {
        Comparison::GreaterThan
    }
}

fn compare_message_text(t1: &DiagnosticMessageText, t2: &DiagnosticMessageText) -> Comparison {
    if let DiagnosticMessageText::String(t1) = t1 {
        if let DiagnosticMessageText::String(t2) = t2 {
            return compare_strings_case_sensitive(t1, t2);
        }
    }
    if matches!(t1, DiagnosticMessageText::String(_)) {
        return Comparison::LessThan;
    }
    if matches!(t2, DiagnosticMessageText::String(_)) {
        return Comparison::GreaterThan;
    }
    let t1 = enum_unwrapped!(t1, [DiagnosticMessageText, DiagnosticMessageChain]);
    let t2 = enum_unwrapped!(t2, [DiagnosticMessageText, DiagnosticMessageChain]);
    let mut res = compare_strings_case_sensitive(&t1.message_text, &t2.message_text);
    if res != Comparison::EqualTo {
        return res;
    }
    if t1.next.is_none() && t2.next.is_none() {
        return Comparison::EqualTo;
    }
    if t1.next.is_none() {
        return Comparison::LessThan;
    }
    if t2.next.is_none() {
        return Comparison::GreaterThan;
    }
    let t1_next = t1.next.as_ref().unwrap();
    let t2_next = t2.next.as_ref().unwrap();
    let len = cmp::min(t1_next.len(), t2_next.len());
    for i in 0..len {
        res = compare_message_text(&t1_next[i].clone().into(), &t2_next[i].clone().into());
        if res != Comparison::EqualTo {
            return res;
        }
    }
    if t1_next.len() < t2_next.len() {
        return Comparison::LessThan;
    }
    if t1_next.len() > t2_next.len() {
        return Comparison::GreaterThan;
    }
    Comparison::EqualTo
}

pub fn get_language_variant(script_kind: ScriptKind) -> LanguageVariant {
    match script_kind {
        ScriptKind::TSX | ScriptKind::JSX | ScriptKind::JS | ScriptKind::JSON => {
            LanguageVariant::JSX
        }
        _ => LanguageVariant::Standard,
    }
}

pub fn get_emit_script_target(compiler_options: &CompilerOptions) -> ScriptTarget {
    compiler_options.target.unwrap_or_else(|| {
        if matches!(compiler_options.module, Some(ModuleKind::Node12)) {
            ScriptTarget::ES2020
        } else if matches!(compiler_options.module, Some(ModuleKind::NodeNext)) {
            ScriptTarget::ESNext
        } else {
            ScriptTarget::ES3
        }
    })
}

pub fn get_emit_module_kind(compiler_options: &CompilerOptions) -> ModuleKind {
    unimplemented!()
}

pub fn unused_label_is_error(options: &CompilerOptions) -> bool {
    matches!(options.allow_unused_labels, Some(false))
}

fn lookup_compiler_option_value(options: &CompilerOptions, name: &str) -> CompilerOptionsValue {
    match name {
        "all" => CompilerOptionsValue::Bool(options.all.clone()),
        "allow_js" => CompilerOptionsValue::Bool(options.allow_js.clone()),
        "allow_non_ts_extensions" => {
            CompilerOptionsValue::Bool(options.allow_non_ts_extensions.clone())
        }
        "allow_synthetic_default_imports" => {
            CompilerOptionsValue::Bool(options.allow_synthetic_default_imports.clone())
        }
        "allow_umd_global_access" => {
            CompilerOptionsValue::Bool(options.allow_umd_global_access.clone())
        }
        "allow_unreachable_code" => {
            CompilerOptionsValue::Bool(options.allow_unreachable_code.clone())
        }
        "allow_unused_labels" => CompilerOptionsValue::Bool(options.allow_unused_labels.clone()),
        "always_strict" => CompilerOptionsValue::Bool(options.always_strict.clone()),
        "base_url" => CompilerOptionsValue::String(options.base_url.clone()),
        "build" => CompilerOptionsValue::Bool(options.build.clone()),
        "charset" => CompilerOptionsValue::String(options.charset.clone()),
        "check_js" => CompilerOptionsValue::Bool(options.check_js.clone()),
        "config_file_path" => CompilerOptionsValue::String(options.config_file_path.clone()),
        "config_file" => CompilerOptionsValue::SourceFile(options.config_file.clone()),
        "declaration" => CompilerOptionsValue::Bool(options.declaration.clone()),
        "declaration_map" => CompilerOptionsValue::Bool(options.declaration_map.clone()),
        "emit_declaration_only" => {
            CompilerOptionsValue::Bool(options.emit_declaration_only.clone())
        }
        "declaration_dir" => CompilerOptionsValue::String(options.declaration_dir.clone()),
        "diagnostics" => CompilerOptionsValue::Bool(options.diagnostics.clone()),
        "extended_diagnostics" => CompilerOptionsValue::Bool(options.extended_diagnostics.clone()),
        "disable_size_limit" => CompilerOptionsValue::Bool(options.disable_size_limit.clone()),
        "disable_source_of_project_reference_redirect" => {
            CompilerOptionsValue::Bool(options.disable_source_of_project_reference_redirect.clone())
        }
        "disable_solution_searching" => {
            CompilerOptionsValue::Bool(options.disable_solution_searching.clone())
        }
        "disable_referenced_project_load" => {
            CompilerOptionsValue::Bool(options.disable_referenced_project_load.clone())
        }
        "downlevel_iteration" => CompilerOptionsValue::Bool(options.downlevel_iteration.clone()),
        "emit_bom" => CompilerOptionsValue::Bool(options.emit_bom.clone()),
        "emit_decorator_metadata" => {
            CompilerOptionsValue::Bool(options.emit_decorator_metadata.clone())
        }
        "exact_optional_property_types" => {
            CompilerOptionsValue::Bool(options.exact_optional_property_types.clone())
        }
        "experimental_decorators" => {
            CompilerOptionsValue::Bool(options.experimental_decorators.clone())
        }
        "force_consistent_casing_in_file_names" => {
            CompilerOptionsValue::Bool(options.force_consistent_casing_in_file_names.clone())
        }
        "generate_cpu_profile" => {
            CompilerOptionsValue::String(options.generate_cpu_profile.clone())
        }
        "generate_trace" => CompilerOptionsValue::String(options.generate_trace.clone()),
        "help" => CompilerOptionsValue::Bool(options.help.clone()),
        "import_helpers" => CompilerOptionsValue::Bool(options.import_helpers.clone()),
        "imports_not_used_as_values" => {
            CompilerOptionsValue::ImportsNotUsedAsValues(options.imports_not_used_as_values.clone())
        }
        "init" => CompilerOptionsValue::Bool(options.init.clone()),
        "inline_source_map" => CompilerOptionsValue::Bool(options.inline_source_map.clone()),
        "inline_sources" => CompilerOptionsValue::Bool(options.inline_sources.clone()),
        "isolated_modules" => CompilerOptionsValue::Bool(options.isolated_modules.clone()),
        "jsx" => CompilerOptionsValue::JsxEmit(options.jsx.clone()),
        "keyof_strings_only" => CompilerOptionsValue::Bool(options.keyof_strings_only.clone()),
        "lib" => CompilerOptionsValue::VecString(options.lib.clone()),
        "list_emitted_files" => CompilerOptionsValue::Bool(options.list_emitted_files.clone()),
        "list_files" => CompilerOptionsValue::Bool(options.list_files.clone()),
        "explain_files" => CompilerOptionsValue::Bool(options.explain_files.clone()),
        "list_files_only" => CompilerOptionsValue::Bool(options.list_files_only.clone()),
        "locale" => CompilerOptionsValue::String(options.locale.clone()),
        "map_root" => CompilerOptionsValue::String(options.map_root.clone()),
        "max_node_module_js_depth" => {
            CompilerOptionsValue::Usize(options.max_node_module_js_depth.clone())
        }
        "module" => CompilerOptionsValue::ModuleKind(options.module.clone()),
        "module_resolution" => {
            CompilerOptionsValue::ModuleResolutionKind(options.module_resolution.clone())
        }
        "new_line" => CompilerOptionsValue::NewLineKind(options.new_line.clone()),
        "no_emit" => CompilerOptionsValue::Bool(options.no_emit.clone()),
        "no_emit_for_js_files" => CompilerOptionsValue::Bool(options.no_emit_for_js_files.clone()),
        "no_emit_helpers" => CompilerOptionsValue::Bool(options.no_emit_helpers.clone()),
        "no_emit_on_error" => CompilerOptionsValue::Bool(options.no_emit_on_error.clone()),
        "no_error_truncation" => CompilerOptionsValue::Bool(options.no_error_truncation.clone()),
        "no_fallthrough_cases_in_switch" => {
            CompilerOptionsValue::Bool(options.no_fallthrough_cases_in_switch.clone())
        }
        "no_implicit_any" => CompilerOptionsValue::Bool(options.no_implicit_any.clone()),
        "no_implicit_returns" => CompilerOptionsValue::Bool(options.no_implicit_returns.clone()),
        "no_implicit_this" => CompilerOptionsValue::Bool(options.no_implicit_this.clone()),
        "no_strict_generic_checks" => {
            CompilerOptionsValue::Bool(options.no_strict_generic_checks.clone())
        }
        "no_unused_locals" => CompilerOptionsValue::Bool(options.no_unused_locals.clone()),
        "no_unused_parameters" => CompilerOptionsValue::Bool(options.no_unused_parameters.clone()),
        "no_implicit_use_strict" => {
            CompilerOptionsValue::Bool(options.no_implicit_use_strict.clone())
        }
        "no_property_access_from_index_signature" => {
            CompilerOptionsValue::Bool(options.no_property_access_from_index_signature.clone())
        }
        "assume_changes_only_affect_direct_dependencies" => CompilerOptionsValue::Bool(
            options
                .assume_changes_only_affect_direct_dependencies
                .clone(),
        ),
        "no_lib" => CompilerOptionsValue::Bool(options.no_lib.clone()),
        "no_resolve" => CompilerOptionsValue::Bool(options.no_resolve.clone()),
        "no_unchecked_indexed_access" => {
            CompilerOptionsValue::Bool(options.no_unchecked_indexed_access.clone())
        }
        "out" => CompilerOptionsValue::String(options.out.clone()),
        "out_dir" => CompilerOptionsValue::String(options.out_dir.clone()),
        "out_file" => CompilerOptionsValue::String(options.out_file.clone()),
        "paths" => CompilerOptionsValue::MapLikeVecString(options.paths.clone()),
        "paths_base_path" => CompilerOptionsValue::String(options.paths_base_path.clone()),
        "plugins" => CompilerOptionsValue::VecPluginImport(options.plugins.clone()),
        "preserve_const_enums" => CompilerOptionsValue::Bool(options.preserve_const_enums.clone()),
        "no_implicit_override" => CompilerOptionsValue::Bool(options.no_implicit_override.clone()),
        "preserve_symlinks" => CompilerOptionsValue::Bool(options.preserve_symlinks.clone()),
        "preserve_value_imports" => {
            CompilerOptionsValue::Bool(options.preserve_value_imports.clone())
        }
        "preserve_watch_output" => {
            CompilerOptionsValue::Bool(options.preserve_watch_output.clone())
        }
        "project" => CompilerOptionsValue::String(options.project.clone()),
        "pretty" => CompilerOptionsValue::Bool(options.pretty.clone()),
        "react_namespace" => CompilerOptionsValue::String(options.react_namespace.clone()),
        "jsx_factory" => CompilerOptionsValue::String(options.jsx_factory.clone()),
        "jsx_fragment_factory" => {
            CompilerOptionsValue::String(options.jsx_fragment_factory.clone())
        }
        "jsx_import_source" => CompilerOptionsValue::String(options.jsx_import_source.clone()),
        "composite" => CompilerOptionsValue::Bool(options.composite.clone()),
        "incremental" => CompilerOptionsValue::Bool(options.incremental.clone()),
        "ts_build_info_file" => CompilerOptionsValue::String(options.ts_build_info_file.clone()),
        "remove_comments" => CompilerOptionsValue::Bool(options.remove_comments.clone()),
        "root_dir" => CompilerOptionsValue::String(options.root_dir.clone()),
        "root_dirs" => CompilerOptionsValue::VecString(options.root_dirs.clone()),
        "skip_lib_check" => CompilerOptionsValue::Bool(options.skip_lib_check.clone()),
        "skip_default_lib_check" => {
            CompilerOptionsValue::Bool(options.skip_default_lib_check.clone())
        }
        "source_map" => CompilerOptionsValue::Bool(options.source_map.clone()),
        "source_root" => CompilerOptionsValue::String(options.source_root.clone()),
        "strict" => CompilerOptionsValue::Bool(options.strict.clone()),
        "strict_function_types" => {
            CompilerOptionsValue::Bool(options.strict_function_types.clone())
        }
        "strict_bind_call_apply" => {
            CompilerOptionsValue::Bool(options.strict_bind_call_apply.clone())
        }
        "strict_null_checks" => CompilerOptionsValue::Bool(options.strict_null_checks.clone()),
        "strict_property_initialization" => {
            CompilerOptionsValue::Bool(options.strict_property_initialization.clone())
        }
        "strip_internal" => CompilerOptionsValue::Bool(options.strip_internal.clone()),
        "suppress_excess_property_errors" => {
            CompilerOptionsValue::Bool(options.suppress_excess_property_errors.clone())
        }
        "suppress_implicit_any_index_errors" => {
            CompilerOptionsValue::Bool(options.suppress_implicit_any_index_errors.clone())
        }
        "suppress_output_path_check" => {
            CompilerOptionsValue::Bool(options.suppress_output_path_check.clone())
        }
        "target" => CompilerOptionsValue::ScriptTarget(options.target.clone()),
        "trace_resolution" => CompilerOptionsValue::Bool(options.trace_resolution.clone()),
        "use_unknown_in_catch_variables" => {
            CompilerOptionsValue::Bool(options.use_unknown_in_catch_variables.clone())
        }
        "resolve_json_module" => CompilerOptionsValue::Bool(options.resolve_json_module.clone()),
        "types" => CompilerOptionsValue::VecString(options.types.clone()),
        "type_roots" => CompilerOptionsValue::VecString(options.type_roots.clone()),
        "version" => CompilerOptionsValue::Bool(options.version.clone()),
        "watch" => CompilerOptionsValue::Bool(options.watch.clone()),
        "es_module_interop" => CompilerOptionsValue::Bool(options.es_module_interop.clone()),
        "show_config" => CompilerOptionsValue::Bool(options.show_config.clone()),
        "use_define_for_class_fields" => {
            CompilerOptionsValue::Bool(options.use_define_for_class_fields.clone())
        }
        _ => panic!("Unknown compiler option: {:?}", name),
    }
}

pub fn get_strict_option_value(
    compiler_options: &CompilerOptions,
    flag: &str, /*StrictOptionName*/
) -> bool {
    match lookup_compiler_option_value(compiler_options, flag).as_option_bool() {
        None => compiler_options.strict.unwrap_or(false),
        Some(bool_) => bool_,
    }
}

pub fn get_compiler_option_value(
    options: &CompilerOptions,
    option: &CommandLineOption,
) -> CompilerOptionsValue {
    if option.strict_flag() {
        CompilerOptionsValue::Bool(Some(get_strict_option_value(options, option.name())))
    } else {
        lookup_compiler_option_value(options, option.name())
    }
}

pub fn get_jsx_transform_enabled(options: &CompilerOptions) -> bool {
    let jsx = options.jsx;
    matches!(
        jsx,
        Some(JsxEmit::React | JsxEmit::ReactJSX | JsxEmit::ReactJSXDev)
    )
}

pub fn ensure_script_kind(file_name: &str, script_kind: Option<ScriptKind>) -> ScriptKind {
    script_kind.unwrap_or_else(|| {
        let script_kind = get_script_kind_from_file_name(file_name);
        if script_kind == ScriptKind::Unknown {
            ScriptKind::TS
        } else {
            script_kind
        }
    })
}

pub fn get_script_kind_from_file_name(file_name: &str) -> ScriptKind {
    let ext = file_name
        .rfind('.')
        .map(|extension_index| file_name[extension_index..].to_owned())
        .and_then(|ext| Extension::maybe_from_str(&ext));
    match ext {
        Some(Extension::Js) | Some(Extension::Cjs) | Some(Extension::Mjs) => ScriptKind::JS,
        Some(Extension::Jsx) => ScriptKind::JSX,
        Some(Extension::Ts) | Some(Extension::Cts) | Some(Extension::Mts) => ScriptKind::TS,
        Some(Extension::Tsx) => ScriptKind::TSX,
        Some(Extension::Json) => ScriptKind::JSON,
        _ => ScriptKind::Unknown,
    }
}

pub fn remove_file_extension<'path>(path: &'path str) -> Cow<'path, str> {
    unimplemented!()
}

pub enum StringOrPattern {
    String(String),
    Pattern(Pattern),
}

pub fn try_parse_pattern(pattern: &str) -> Option<StringOrPattern> {
    unimplemented!()
}

pub fn position_is_synthesized(pos: isize) -> bool {
    !(pos >= 0)
}

pub fn add_related_info(
    diagnostic: &Diagnostic,
    related_information: Vec<Rc<DiagnosticRelatedInformation>>,
) {
    if related_information.is_empty() {
        return /*diagnostic*/;
    }
    let mut diagnostic_related_information = diagnostic.related_information();
    if diagnostic_related_information.is_none() {
        *diagnostic_related_information = Some(vec![]);
    }
    // Debug.assert(diagnostic.relatedInformation !== emptyArray, "Diagnostic had empty array singleton for related info, but is still being constructed!");
    diagnostic_related_information
        .as_mut()
        .unwrap()
        .extend(related_information);
    // return diagnostic;
}
