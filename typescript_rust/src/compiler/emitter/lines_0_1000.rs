use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    io,
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;

use super::{create_brackets_map, TempFlags};
use crate::{
    base64_encode, change_extension, combine_paths, compare_paths,
    compute_common_source_directory_of_filenames, create_diagnostic_collection,
    create_get_canonical_file_name, create_source_map_generator, create_text_writer,
    directory_separator_str, encode_uri, ensure_path_is_non_module_name,
    ensure_trailing_directory_separator, factory, file_extension_is, file_extension_is_one_of,
    filter, get_are_declaration_maps_enabled, get_base_file_name,
    get_declaration_emit_extension_for_path, get_declaration_emit_output_file_path,
    get_directory_path, get_emit_declarations, get_emit_module_kind_from_module_and_target,
    get_factory, get_new_line_character, get_normalized_absolute_path,
    get_own_emit_output_file_path, get_relative_path_from_directory,
    get_relative_path_to_directory_or_url, get_root_length, get_source_file_path_in_new_dir,
    get_source_files_to_emit, get_synthetic_factory, get_sys, is_bundle, is_export_assignment,
    is_export_specifier, is_expression, is_identifier, is_incremental_compilation,
    is_json_source_file, is_option_str_empty, is_source_file, is_source_file_not_json,
    last_or_undefined, length, no_emit_notification, no_emit_substitution, normalize_path,
    normalize_slashes, out_file, remove_file_extension, resolve_path, supported_js_extensions_flat,
    transform_nodes, try_for_each_child, version, write_file, AllAccessorDeclarations, AllArenas,
    BaseNodeFactorySynthetic, BuildInfo, BundleBuildInfo, BundleFileInfo, BundleFileSection,
    BundleFileSectionInterface, BundleFileSectionKind, Comparison, CompilerOptions,
    CurrentParenthesizerRule, Debug_, DetachedCommentInfo, DiagnosticCollection,
    EmitBinaryExpression, EmitFileNames, EmitHint, EmitHost, EmitHostWriteFileCallback,
    EmitResolver, EmitResult, EmitTextWriter, EmitTransformers, ExportedModulesFromDeclarationEmit,
    Extension, JsxEmit, ListFormat, ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, Node,
    NodeArray, NodeId, NodeInterface, NonEmpty, ParenthesizerRules, ParsedCommandLine,
    PrintHandlers, Printer, PrinterOptions, RelativeToBuildInfo, ScriptReferenceHost,
    SourceMapEmitResult, SourceMapGenerator, SourceMapSource, StringOrNumber, Symbol,
    SymbolAccessibilityResult, SymbolVisibilityResult, SyntaxKind, TextRange,
    TransformNodesTransformationResult, TransformationResult, TransformerFactory,
    TypeReferenceSerializationKind,
};

lazy_static! {
    pub(super) static ref brackets: HashMap<ListFormat, (&'static str, &'static str)> =
        create_brackets_map();
}

pub(crate) fn is_build_info_file(file: &str) -> bool {
    file_extension_is(file, Extension::TsBuildInfo.to_str())
}

pub enum NodeOrVecNode {
    Node(Gc<Node>),
    VecNode(Vec<Gc<Node>>),
}

impl From<Vec<Gc<Node>>> for NodeOrVecNode {
    fn from(value: Vec<Gc<Node>>) -> Self {
        Self::VecNode(value)
    }
}

impl From<Gc<Node>> for NodeOrVecNode {
    fn from(value: Gc<Node>) -> Self {
        Self::Node(value)
    }
}

pub fn for_each_emitted_file_returns<TReturn>(
    host: &dyn EmitHost,
    mut action: impl FnMut(&EmitFileNames, Option<&Node /*SourceFile | Bundle*/>) -> Option<TReturn>,
    source_files_or_target_source_file: Option<impl Into<NodeOrVecNode>>,
    force_dts_emit: Option<bool>,
    only_build_info: Option<bool>,
    include_build_info: Option<bool>,
) -> Option<TReturn> {
    try_for_each_emitted_file_returns(
        host,
        |a, b| Ok(action(a, b)),
        source_files_or_target_source_file,
        force_dts_emit,
        only_build_info,
        include_build_info,
    )
    .unwrap()
}

pub fn try_for_each_emitted_file_returns<TReturn>(
    host: &dyn EmitHost,
    mut action: impl FnMut(
        &EmitFileNames,
        Option<&Node /*SourceFile | Bundle*/>,
    ) -> io::Result<Option<TReturn>>,
    source_files_or_target_source_file: Option<impl Into<NodeOrVecNode>>,
    force_dts_emit: Option<bool>,
    only_build_info: Option<bool>,
    include_build_info: Option<bool>,
) -> io::Result<Option<TReturn>> {
    let force_dts_emit = force_dts_emit.unwrap_or(false);
    let source_files = match source_files_or_target_source_file.map(Into::into) {
        Some(NodeOrVecNode::VecNode(source_files_or_target_source_file)) => {
            source_files_or_target_source_file
        }
        Some(NodeOrVecNode::Node(source_files_or_target_source_file)) => get_source_files_to_emit(
            host,
            Some(&*source_files_or_target_source_file),
            Some(force_dts_emit),
        ),
        None => get_source_files_to_emit(host, Option::<&Node>::None, Some(force_dts_emit)),
    };
    let options = ScriptReferenceHost::get_compiler_options(host);
    if out_file(&options)
        .filter(|out_file| !out_file.is_empty())
        .is_some()
    {
        let prepends = host.get_prepend_nodes();
        if !source_files.is_empty() || !prepends.is_empty() {
            let bundle: Gc<Node> = get_factory().create_bundle(
                source_files.into_iter().map(Option::Some).collect(),
                Some(prepends),
            );
            let result = action(
                &get_output_paths_for(&bundle, host, force_dts_emit),
                Some(&*bundle),
            )?;
            if result.is_some() {
                return Ok(result);
            }
        }
    } else {
        if only_build_info != Some(true) {
            for source_file in &source_files {
                let result = action(
                    &get_output_paths_for(source_file, host, force_dts_emit),
                    Some(&**source_file),
                )?;
                if result.is_some() {
                    return Ok(result);
                }
            }
        }
        if include_build_info == Some(true) {
            let build_info_path = get_ts_build_info_emit_output_file_path(&options);
            if let Some(build_info_path) =
                build_info_path.filter(|build_info_path| !build_info_path.is_empty())
            {
                return action(
                    &EmitFileNames {
                        build_info_path: Some(build_info_path),
                        ..Default::default()
                    },
                    None,
                );
            }
        }
    }
    Ok(None)
}

pub fn for_each_emitted_file(
    host: &dyn EmitHost,
    mut action: impl FnMut(&EmitFileNames, Option<&Node /*SourceFile | Bundle*/>),
    source_files_or_target_source_file: Option<impl Into<NodeOrVecNode>>,
    force_dts_emit: Option<bool>,
    only_build_info: Option<bool>,
    include_build_info: Option<bool>,
) {
    try_for_each_emitted_file(
        host,
        |a, b| Ok(action(a, b)),
        source_files_or_target_source_file,
        force_dts_emit,
        only_build_info,
        include_build_info,
    )
    .unwrap()
}

pub fn try_for_each_emitted_file(
    host: &dyn EmitHost,
    mut action: impl FnMut(&EmitFileNames, Option<&Node /*SourceFile | Bundle*/>) -> io::Result<()>,
    source_files_or_target_source_file: Option<impl Into<NodeOrVecNode>>,
    force_dts_emit: Option<bool>,
    only_build_info: Option<bool>,
    include_build_info: Option<bool>,
) -> io::Result<()> {
    try_for_each_emitted_file_returns(
        host,
        |emit_file_names, node| -> io::Result<Option<()>> {
            action(emit_file_names, node)?;
            Ok(None)
        },
        source_files_or_target_source_file,
        force_dts_emit,
        only_build_info,
        include_build_info,
    )?;

    Ok(())
}

pub fn get_ts_build_info_emit_output_file_path(options: &CompilerOptions) -> Option<String> {
    let config_file = options.config_file_path.as_ref();
    if !is_incremental_compilation(options) {
        return None;
    }
    if !is_option_str_empty(options.ts_build_info_file.as_deref()) {
        return options.ts_build_info_file.clone();
    }
    let out_path = out_file(options);
    let build_info_extension_less: String;
    if let Some(out_path) = out_path.filter(|out_path| !out_path.is_empty()) {
        build_info_extension_less = remove_file_extension(out_path).to_owned();
    } else {
        let config_file = config_file?;
        let config_file_extension_less = remove_file_extension(config_file);
        build_info_extension_less = if let Some(options_out_dir) = options
            .out_dir
            .as_ref()
            .filter(|options_out_dir| !options_out_dir.is_empty())
        {
            if let Some(options_root_dir) = options
                .root_dir
                .as_ref()
                .filter(|options_root_dir| !options_root_dir.is_empty())
            {
                resolve_path(
                    options_out_dir,
                    &[Some(&get_relative_path_from_directory(
                        options_root_dir,
                        config_file_extension_less,
                        Option::<fn(&str) -> String>::None,
                        Some(true),
                    ))],
                )
            } else {
                combine_paths(
                    options_out_dir,
                    &[Some(&get_base_file_name(
                        config_file_extension_less,
                        None,
                        None,
                    ))],
                )
            }
        } else {
            config_file_extension_less.to_owned()
        };
    }
    Some(format!(
        "{}{}",
        build_info_extension_less,
        Extension::TsBuildInfo.to_str()
    ))
}

pub(crate) fn get_output_paths_for_bundle(
    options: &CompilerOptions,
    force_dts_paths: bool,
) -> EmitFileNames {
    let out_path = out_file(options).unwrap();
    let js_file_path = if options.emit_declaration_only == Some(true) {
        None
    } else {
        Some(out_path)
    };
    let source_map_file_path = js_file_path
        .filter(|js_file_path| !js_file_path.is_empty())
        .and_then(|js_file_path| get_source_map_file_path(js_file_path, options));
    let declaration_file_path = if force_dts_paths || get_emit_declarations(options) {
        Some(format!(
            "{}{}",
            remove_file_extension(out_path),
            Extension::Dts.to_str()
        ))
    } else {
        None
    };
    let declaration_map_path = declaration_file_path
        .as_ref()
        .filter(|declaration_file_path| {
            !declaration_file_path.is_empty() && get_are_declaration_maps_enabled(options)
        })
        .map(|declaration_file_path| format!("{declaration_file_path}.map"));
    let build_info_path = get_ts_build_info_emit_output_file_path(options);
    EmitFileNames {
        js_file_path: js_file_path.map(ToOwned::to_owned),
        source_map_file_path,
        declaration_file_path,
        declaration_map_path,
        build_info_path,
    }
}

pub(crate) fn get_output_paths_for(
    source_file: &Node, /*SourceFile | Bundle*/
    host: &dyn EmitHost,
    force_dts_paths: bool,
) -> EmitFileNames {
    let options = ScriptReferenceHost::get_compiler_options(host);
    if source_file.kind() == SyntaxKind::Bundle {
        get_output_paths_for_bundle(&options, force_dts_paths)
    } else {
        let source_file_as_source_file = source_file.as_source_file();
        let own_output_file_path = get_own_emit_output_file_path(
            &source_file_as_source_file.file_name(),
            host,
            get_output_extension(&source_file_as_source_file.file_name(), &options).to_str(),
        );
        let is_json_file = is_json_source_file(source_file);
        let is_json_emitted_to_same_location = is_json_file
            && compare_paths(
                &source_file_as_source_file.file_name(),
                &own_output_file_path,
                Some(ScriptReferenceHost::get_current_directory(host)),
                Some(!EmitHost::use_case_sensitive_file_names(host)),
            ) == Comparison::EqualTo;
        let js_file_path =
            if options.emit_declaration_only == Some(true) || is_json_emitted_to_same_location {
                None
            } else {
                Some(own_output_file_path)
            };
        let source_map_file_path = if js_file_path
            .as_ref()
            .filter(|js_file_path| !js_file_path.is_empty())
            .is_none()
            || is_json_source_file(source_file)
        {
            None
        } else {
            get_source_map_file_path(js_file_path.as_ref().unwrap(), &options)
        };
        let declaration_file_path =
            if force_dts_paths || get_emit_declarations(&options) && !is_json_file {
                Some(get_declaration_emit_output_file_path(
                    &source_file_as_source_file.file_name(),
                    host,
                ))
            } else {
                None
            };
        let declaration_map_path = declaration_file_path
            .as_ref()
            .filter(|declaration_file_path| {
                !declaration_file_path.is_empty() && get_are_declaration_maps_enabled(&options)
            })
            .map(|declaration_file_path| format!("{declaration_file_path}.map"));
        EmitFileNames {
            js_file_path,
            source_map_file_path,
            declaration_file_path,
            declaration_map_path,
            build_info_path: None,
        }
    }
}

fn get_source_map_file_path(js_file_path: &str, options: &CompilerOptions) -> Option<String> {
    if options.source_map == Some(true) && options.inline_source_map != Some(true) {
        Some(format!("{js_file_path}.map"))
    } else {
        None
    }
}

pub fn get_output_extension(file_name: &str, options: &CompilerOptions) -> Extension {
    if file_extension_is(file_name, Extension::Json.to_str()) {
        Extension::Json
    } else if options.jsx == Some(JsxEmit::Preserve)
        && file_extension_is_one_of(file_name, &[Extension::Jsx, Extension::Tsx])
    {
        Extension::Jsx
    } else if file_extension_is_one_of(file_name, &[Extension::Mts, Extension::Mjs]) {
        Extension::Mjs
    } else if file_extension_is_one_of(file_name, &[Extension::Cts, Extension::Cjs]) {
        Extension::Cjs
    } else {
        Extension::Js
    }
}

fn get_output_path_without_changing_ext(
    input_file_name: &str,
    config_file: &ParsedCommandLine,
    ignore_case: bool,
    output_dir: Option<&str>,
    get_common_source_directory: Option<&mut impl FnMut() -> String>,
) -> String {
    output_dir.non_empty().map_or_else(
        || input_file_name.to_owned(),
        |output_dir| {
            resolve_path(
                output_dir,
                &[Some(&get_relative_path_from_directory(
                    &get_common_source_directory.map_or_else(
                        || get_common_source_directory_of_config(config_file, ignore_case),
                        |get_common_source_directory| get_common_source_directory(),
                    ),
                    input_file_name,
                    Option::<fn(&str) -> String>::None,
                    Some(ignore_case),
                ))],
            )
        },
    )
}

pub(crate) fn get_output_declaration_file_name(
    input_file_name: &str,
    config_file: &ParsedCommandLine,
    ignore_case: bool,
    get_common_source_directory: Option<&mut impl FnMut() -> String>,
) -> String {
    change_extension(
        &get_output_path_without_changing_ext(
            input_file_name,
            config_file,
            ignore_case,
            config_file
                .options
                .declaration_dir
                .as_deref()
                .non_empty()
                .or_else(|| config_file.options.out_dir.as_deref()),
            get_common_source_directory,
        ),
        get_declaration_emit_extension_for_path(input_file_name),
    )
}

pub(crate) fn get_common_source_directory(
    options: &CompilerOptions,
    mut emitted_files: impl FnMut() -> Vec<String>,
    current_directory: &str,
    get_canonical_file_name: impl FnMut(&str) -> String,
    check_source_files_belong_to_path: Option<impl FnMut(&str)>,
) -> String {
    let mut common_source_directory: String;
    if let Some(options_root_dir) = options
        .root_dir
        .as_ref()
        .filter(|options_root_dir| !options_root_dir.is_empty())
    {
        common_source_directory =
            get_normalized_absolute_path(options_root_dir, Some(current_directory));
        if let Some(mut check_source_files_belong_to_path) = check_source_files_belong_to_path {
            check_source_files_belong_to_path(options_root_dir);
        }
    } else if let Some(options_config_file_path) =
        options
            .config_file_path
            .as_ref()
            .filter(|options_config_file_path| {
                !options_config_file_path.is_empty() && options.composite == Some(true)
            })
    {
        common_source_directory = get_directory_path(&normalize_slashes(options_config_file_path));
        if let Some(mut check_source_files_belong_to_path) = check_source_files_belong_to_path {
            check_source_files_belong_to_path(&common_source_directory);
        }
    } else {
        common_source_directory = compute_common_source_directory_of_filenames(
            &emitted_files(),
            current_directory,
            get_canonical_file_name,
        );
    }

    if !common_source_directory.is_empty()
        && &common_source_directory[common_source_directory.len() - 1..] != directory_separator_str
    {
        common_source_directory.push_str(directory_separator_str);
    }
    common_source_directory
}

pub(crate) fn get_common_source_directory_of_config(
    command_line: &ParsedCommandLine,
    ignore_case: bool,
) -> String {
    let options = &command_line.options;
    let file_names = &command_line.file_names;
    get_common_source_directory(
        options,
        || {
            filter(file_names, |file: &String| {
                !(options.no_emit_for_js_files == Some(true)
                    && file_extension_is_one_of(file, &supported_js_extensions_flat))
                    && !file_extension_is(file, Extension::Dts.to_str())
            })
        },
        &get_directory_path(&normalize_slashes(
            Debug_.check_defined(options.config_file_path.as_deref(), None),
        )),
        create_get_canonical_file_name(!ignore_case),
        Option::<fn(&str)>::None,
    )
}

pub(crate) fn emit_files(
    resolver: Gc<Box<dyn EmitResolver>>,
    host: Gc<Box<dyn EmitHost>>,
    target_source_file: Option<&Node /*SourceFile*/>,
    EmitTransformers {
        script_transformers,
        declaration_transformers,
    }: EmitTransformers,
    emit_only_dts_files: Option<bool>,
    only_build_info: Option<bool>,
    force_dts_emit: Option<bool>,
) -> io::Result<EmitResult> {
    let compiler_options = ScriptReferenceHost::get_compiler_options(&**host);
    let mut source_map_data_list: Option<Vec<SourceMapEmitResult>> = if compiler_options.source_map
        == Some(true)
        || compiler_options.inline_source_map == Some(true)
        || get_are_declaration_maps_enabled(&compiler_options)
    {
        Some(vec![])
    } else {
        None
    };
    let mut emitted_files_list: Option<Vec<String>> =
        if compiler_options.list_emitted_files == Some(true) {
            Some(vec![])
        } else {
            None
        };
    let mut emitter_diagnostics = create_diagnostic_collection();
    let new_line = get_new_line_character(compiler_options.new_line, Some(|| host.get_new_line()));
    let writer = create_text_writer(&new_line);
    // const { enter, exit } = performance.createTimer("printTime", "beforePrint", "afterPrint");
    let mut bundle_build_info: Option<Gc<GcCell<BundleBuildInfo>>> = None;
    let mut emit_skipped = false;
    let mut exported_modules_from_declaration_emit: Option<ExportedModulesFromDeclarationEmit> =
        None;

    // enter();
    try_for_each_emitted_file(
        &**host,
        |emit_file_names, source_file_or_bundle| {
            emit_source_file_or_bundle(
                host.clone(),
                &mut bundle_build_info,
                &mut emitted_files_list,
                emit_only_dts_files,
                &mut emit_skipped,
                target_source_file,
                &mut emitter_diagnostics,
                compiler_options.clone(),
                force_dts_emit,
                resolver.clone(),
                &declaration_transformers,
                &mut exported_modules_from_declaration_emit,
                writer.as_dyn_emit_text_writer(),
                &mut source_map_data_list,
                &new_line,
                &script_transformers,
                emit_file_names,
                source_file_or_bundle,
            )?;

            Ok(())
        },
        Some(get_source_files_to_emit(
            &**host,
            target_source_file,
            force_dts_emit,
        )),
        force_dts_emit,
        only_build_info,
        Some(target_source_file.is_none()),
    )?;
    // exit();

    Ok(EmitResult {
        emit_skipped,
        diagnostics: emitter_diagnostics.get_diagnostics(None),
        emitted_files: emitted_files_list,
        source_maps: source_map_data_list,
        exported_modules_from_declaration_emit,
    })
}

fn emit_source_file_or_bundle(
    host: Gc<Box<dyn EmitHost>>,
    bundle_build_info: &mut Option<Gc<GcCell<BundleBuildInfo>>>,
    emitted_files_list: &mut Option<Vec<String>>,
    emit_only_dts_files: Option<bool>,
    emit_skipped: &mut bool,
    target_source_file: Option<&Node /*SourceFile*/>,
    emitter_diagnostics: &mut DiagnosticCollection,
    compiler_options: Gc<CompilerOptions>,
    force_dts_emit: Option<bool>,
    resolver: Gc<Box<dyn EmitResolver>>,
    declaration_transformers: &[TransformerFactory],
    exported_modules_from_declaration_emit: &mut Option<ExportedModulesFromDeclarationEmit>,
    writer: Gc<Box<dyn EmitTextWriter>>,
    source_map_data_list: &mut Option<Vec<SourceMapEmitResult>>,
    new_line: &str,
    script_transformers: &[TransformerFactory],
    emit_file_names: &EmitFileNames,
    source_file_or_bundle: Option<&Node /*SourceFile | Bundle*/>,
) -> io::Result<()> {
    let js_file_path = emit_file_names.js_file_path.as_deref();
    let source_map_file_path = emit_file_names.source_map_file_path.as_deref();
    let declaration_file_path = emit_file_names.declaration_file_path.as_deref();
    let declaration_map_path = emit_file_names.declaration_map_path.as_deref();
    let build_info_path = emit_file_names.build_info_path.as_deref();
    let mut build_info_directory: Option<String> = None;
    if let Some(build_info_path) =
        build_info_path.filter(|build_info_path| !build_info_path.is_empty())
    {
        if let Some(source_file_or_bundle) =
            source_file_or_bundle.filter(|source_file_or_bundle| is_bundle(source_file_or_bundle))
        {
            build_info_directory = Some(get_directory_path(&get_normalized_absolute_path(
                build_info_path,
                Some(&ScriptReferenceHost::get_current_directory(&**host)),
            )));
            *bundle_build_info = Some(Gc::new(GcCell::new(BundleBuildInfo {
                common_source_directory: relative_to_build_info(
                    build_info_directory.as_ref().unwrap(),
                    &**host,
                    &ScriptReferenceHost::get_current_directory(&**host),
                ),
                source_files: source_file_or_bundle
                    .as_bundle()
                    .source_files
                    .iter()
                    .map(|file: &Option<Gc<Node>>| {
                        let file = file.as_ref().unwrap();
                        relative_to_build_info(
                            build_info_directory.as_ref().unwrap(),
                            &**host,
                            &get_normalized_absolute_path(
                                &file.as_source_file().file_name(),
                                Some(&ScriptReferenceHost::get_current_directory(&**host)),
                            ),
                        )
                    })
                    .collect(),
                js: Default::default(),
                dts: Default::default(),
            })));
        }
    }
    // tracing?.push(tracing.Phase.Emit, "emitJsFileOrBundle", { jsFilePath });
    emit_js_file_or_bundle(
        emit_only_dts_files,
        host.clone(),
        compiler_options.clone(),
        emit_skipped,
        resolver.clone(),
        script_transformers,
        bundle_build_info,
        writer.clone(),
        source_map_data_list,
        new_line,
        emitter_diagnostics,
        source_file_or_bundle,
        js_file_path,
        source_map_file_path,
        Gc::new(Box::new(EmitSourceFileOrBundleRelativeToBuildInfo::new(
            build_info_directory.clone(),
            host.clone(),
        ))),
    )?;
    // tracing?.pop();

    // tracing?.push(tracing.Phase.Emit, "emitDeclarationFileOrBundle", { declarationFilePath });
    emit_declaration_file_or_bundle(
        emit_only_dts_files,
        compiler_options,
        emit_skipped,
        force_dts_emit,
        resolver,
        host.clone(),
        declaration_transformers,
        emitter_diagnostics,
        exported_modules_from_declaration_emit,
        bundle_build_info,
        writer,
        source_map_data_list,
        new_line,
        source_file_or_bundle,
        declaration_file_path,
        declaration_map_path,
        Gc::new(Box::new(EmitSourceFileOrBundleRelativeToBuildInfo::new(
            build_info_directory.clone(),
            host.clone(),
        ))),
    )?;
    // tracing?.pop();

    // tracing?.push(tracing.Phase.Emit, "emitBuildInfo", { buildInfoPath });
    emit_build_info(
        target_source_file,
        emit_skipped,
        host,
        emitter_diagnostics,
        bundle_build_info.clone(),
        build_info_path,
    )?;
    // tracing?.pop();

    if !*emit_skipped {
        if let Some(emitted_files_list) = emitted_files_list.as_mut() {
            if emit_only_dts_files != Some(true) {
                if let Some(js_file_path) =
                    js_file_path.filter(|js_file_path| !js_file_path.is_empty())
                {
                    emitted_files_list.push(js_file_path.to_owned());
                }
                if let Some(source_map_file_path) = source_map_file_path
                    .filter(|source_map_file_path| !source_map_file_path.is_empty())
                {
                    emitted_files_list.push(source_map_file_path.to_owned());
                }
                if let Some(build_info_path) =
                    build_info_path.filter(|build_info_path| !build_info_path.is_empty())
                {
                    emitted_files_list.push(build_info_path.to_owned());
                }
            }
            if let Some(declaration_file_path) = declaration_file_path
                .filter(|declaration_file_path| !declaration_file_path.is_empty())
            {
                emitted_files_list.push(declaration_file_path.to_owned());
            }
            if let Some(declaration_map_path) =
                declaration_map_path.filter(|declaration_map_path| !declaration_map_path.is_empty())
            {
                emitted_files_list.push(declaration_map_path.to_owned());
            }
        }
    }

    Ok(())
}

#[derive(Trace, Finalize)]
struct EmitSourceFileOrBundleRelativeToBuildInfo {
    build_info_directory: Option<String>,
    host: Gc<Box<dyn EmitHost>>,
}

impl EmitSourceFileOrBundleRelativeToBuildInfo {
    fn new(build_info_directory: Option<String>, host: Gc<Box<dyn EmitHost>>) -> Self {
        Self {
            build_info_directory,
            host,
        }
    }
}

impl RelativeToBuildInfo for EmitSourceFileOrBundleRelativeToBuildInfo {
    fn call(&self, path: &str) -> String {
        relative_to_build_info(
            self.build_info_directory.as_ref().unwrap(),
            &**self.host,
            path,
        )
    }
}

fn relative_to_build_info(build_info_directory: &str, host: &dyn EmitHost, path: &str) -> String {
    ensure_path_is_non_module_name(&get_relative_path_from_directory(
        build_info_directory,
        path,
        Some(|path: &str| host.get_canonical_file_name(path)),
        None,
    ))
}

fn emit_build_info(
    target_source_file: Option<&Node /*SourceFile*/>,
    emit_skipped: &mut bool,
    host: Gc<Box<dyn EmitHost>>,
    emitter_diagnostics: &mut DiagnosticCollection,
    bundle: Option<Gc<GcCell<BundleBuildInfo>>>,
    build_info_path: Option<&str>,
) -> io::Result<()> {
    if !build_info_path.is_non_empty() || target_source_file.is_some() || *emit_skipped {
        return Ok(());
    }
    let build_info_path = build_info_path.unwrap();
    let program = host.get_program_build_info();
    if host.is_emit_blocked(build_info_path) {
        *emit_skipped = true;
        return Ok(());
    }
    write_file(
        &EmitHostWriteFileCallback::new(host.clone()),
        emitter_diagnostics,
        build_info_path,
        &get_build_info_text(&BuildInfo {
            bundle: bundle.clone(),
            program: program.clone(),
            version: version.to_owned(),
        }),
        false,
        None,
    )?;

    Ok(())
}

fn emit_js_file_or_bundle(
    emit_only_dts_files: Option<bool>,
    host: Gc<Box<dyn EmitHost>>,
    compiler_options: Gc<CompilerOptions>,
    emit_skipped: &mut bool,
    resolver: Gc<Box<dyn EmitResolver>>,
    script_transformers: &[TransformerFactory],
    bundle_build_info: &mut Option<Gc<GcCell<BundleBuildInfo>>>,
    writer: Gc<Box<dyn EmitTextWriter>>,
    source_map_data_list: &mut Option<Vec<SourceMapEmitResult>>,
    new_line: &str,
    emitter_diagnostics: &mut DiagnosticCollection,
    source_file_or_bundle: Option<&Node /*SourceFile | Bundle*/>,
    js_file_path: Option<&str>,
    source_map_file_path: Option<&str>,
    relative_to_build_info: Gc<Box<dyn RelativeToBuildInfo>>,
) -> io::Result<()> {
    if source_file_or_bundle.is_none()
        || emit_only_dts_files == Some(true)
        || !js_file_path.is_non_empty()
    {
        return Ok(());
    }
    let source_file_or_bundle = source_file_or_bundle.unwrap();
    let js_file_path = js_file_path.unwrap();

    if host.is_emit_blocked(js_file_path) || compiler_options.no_emit == Some(true) {
        *emit_skipped = true;
        return Ok(());
    }
    let transform = transform_nodes(
        Some(resolver.clone()),
        Some(host.clone()),
        get_factory(),
        get_synthetic_factory(),
        compiler_options.clone(),
        &[source_file_or_bundle.node_wrapper()],
        script_transformers,
        false,
    )?;

    let printer_options = PrinterOptions {
        remove_comments: compiler_options.remove_comments,
        new_line: compiler_options.new_line,
        no_emit_helpers: compiler_options.no_emit_helpers,
        module: compiler_options.module,
        target: compiler_options.target,
        source_map: compiler_options.source_map,
        inline_source_map: compiler_options.inline_source_map,
        inline_sources: compiler_options.inline_sources,
        extended_diagnostics: compiler_options.extended_diagnostics,
        write_bundle_file_info: Some(bundle_build_info.is_some()),
        relative_to_build_info: Some(relative_to_build_info),
        ..Default::default()
    };

    let printer = create_printer(
        printer_options,
        Some(Gc::new(Box::new(EmitJsFileOrBundlePrintHandlers::new(
            resolver.clone(),
            transform.clone(),
        )))),
    );

    Debug_.assert(
        transform.transformed().len() == 1,
        Some("Should only see one output from the transform"),
    );
    print_source_file_or_bundle(
        host.clone(),
        writer,
        source_map_data_list,
        new_line,
        emitter_diagnostics,
        &compiler_options,
        js_file_path,
        source_map_file_path,
        &transform.transformed()[0],
        &printer,
        &(&*compiler_options).into(),
    )?;

    transform.dispose();
    if let Some(bundle_build_info) = bundle_build_info.clone() {
        bundle_build_info.borrow_mut().js = printer.maybe_bundle_file_info().clone();
    }

    Ok(())
}

#[derive(Trace, Finalize)]
struct EmitJsFileOrBundlePrintHandlers {
    resolver: Gc<Box<dyn EmitResolver>>,
    transform: Gc<Box<TransformNodesTransformationResult>>,
}

impl EmitJsFileOrBundlePrintHandlers {
    fn new(
        resolver: Gc<Box<dyn EmitResolver>>,
        transform: Gc<Box<TransformNodesTransformationResult>>,
    ) -> Self {
        Self {
            resolver,
            transform,
        }
    }
}

impl PrintHandlers for EmitJsFileOrBundlePrintHandlers {
    fn has_global_name(&self, name: &str) -> Option<bool> {
        Some(self.resolver.has_global_name(name))
    }

    fn is_on_emit_node_supported(&self) -> bool {
        true
    }

    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        self.transform
            .emit_node_with_notification(hint, node, emit_callback)?;

        Ok(())
    }

    fn is_emit_notification_enabled(&self, node: &Node) -> Option<bool> {
        self.transform.is_emit_notification_enabled(node)
    }

    fn is_substitute_node_supported(&self) -> bool {
        true
    }

    fn substitute_node(&self, hint: EmitHint, node: &Node) -> io::Result<Option<Gc<Node>>> {
        Ok(Some(self.transform.substitute_node(hint, node)?))
    }
}

fn emit_declaration_file_or_bundle(
    emit_only_dts_files: Option<bool>,
    compiler_options: Gc<CompilerOptions>,
    emit_skipped: &mut bool,
    force_dts_emit: Option<bool>,
    resolver: Gc<Box<dyn EmitResolver>>,
    host: Gc<Box<dyn EmitHost>>,
    declaration_transformers: &[TransformerFactory],
    emitter_diagnostics: &mut DiagnosticCollection,
    exported_modules_from_declaration_emit: &mut Option<ExportedModulesFromDeclarationEmit>,
    bundle_build_info: &mut Option<Gc<GcCell<BundleBuildInfo>>>,
    writer: Gc<Box<dyn EmitTextWriter>>,
    source_map_data_list: &mut Option<Vec<SourceMapEmitResult>>,
    new_line: &str,
    source_file_or_bundle: Option<&Node /*SourceFile | Bundle*/>,
    declaration_file_path: Option<&str>,
    declaration_map_path: Option<&str>,
    relative_to_build_info: Gc<Box<dyn RelativeToBuildInfo>>,
) -> io::Result<()> {
    if source_file_or_bundle.is_none() {
        return Ok(());
    }
    let source_file_or_bundle = source_file_or_bundle.unwrap();
    if !declaration_file_path.is_non_empty() {
        if emit_only_dts_files == Some(true) || compiler_options.emit_declaration_only == Some(true)
        {
            *emit_skipped = true;
        }
        return Ok(());
    }
    let declaration_file_path = declaration_file_path.unwrap();
    let source_files = if is_source_file(source_file_or_bundle) {
        vec![source_file_or_bundle.node_wrapper()]
    } else {
        source_file_or_bundle
            .as_bundle()
            .source_files
            .iter()
            .cloned()
            .map(Option::unwrap)
            .collect()
    };
    let files_for_emit = if force_dts_emit == Some(true) {
        source_files
    } else {
        filter(&source_files, |source_file: &Gc<Node>| {
            is_source_file_not_json(source_file)
        })
    };
    let input_list_or_bundle = if out_file(&compiler_options).is_non_empty() {
        vec![get_factory().create_bundle(
            files_for_emit.iter().cloned().map(Option::Some).collect(),
            if !is_source_file(source_file_or_bundle) {
                Some(source_file_or_bundle.as_bundle().prepends.clone())
            } else {
                None
            },
        )]
    } else {
        files_for_emit.clone()
    };
    if emit_only_dts_files == Some(true) && !get_emit_declarations(&compiler_options) {
        files_for_emit
            .iter()
            .try_for_each(|file_for_emit| -> io::Result<_> {
                collect_linked_aliases(&**resolver, file_for_emit)?;

                Ok(())
            })?;
    }
    let declaration_transform = transform_nodes(
        Some(resolver.clone()),
        Some(host.clone()),
        get_factory(),
        get_synthetic_factory(),
        compiler_options.clone(),
        &input_list_or_bundle,
        declaration_transformers,
        false,
    )?;

    if length(declaration_transform.diagnostics().as_deref()) > 0 {
        for diagnostic in declaration_transform.diagnostics().unwrap() {
            emitter_diagnostics.add(diagnostic);
        }
    }

    let printer_options = PrinterOptions {
        remove_comments: compiler_options.remove_comments,
        new_line: compiler_options.new_line,
        no_emit_helpers: Some(true),
        module: compiler_options.module,
        target: compiler_options.target,
        source_map: compiler_options.source_map,
        inline_source_map: compiler_options.inline_source_map,
        extended_diagnostics: compiler_options.extended_diagnostics,
        only_print_js_doc_style: Some(true),
        write_bundle_file_info: Some(bundle_build_info.is_some()),
        record_internal_section: Some(bundle_build_info.is_some()),
        relative_to_build_info: Some(relative_to_build_info),
        ..Default::default()
    };

    let declaration_printer = create_printer(
        printer_options,
        Some(Gc::new(Box::new(
            EmitDeclarationFileOrBundlePrintHandlers::new(
                resolver.clone(),
                declaration_transform.clone(),
            ),
        ))),
    );
    let decl_blocked = declaration_transform.diagnostics().is_non_empty()
        || host.is_emit_blocked(declaration_file_path)
        || compiler_options.no_emit == Some(true);
    *emit_skipped = *emit_skipped || decl_blocked;
    if !decl_blocked || force_dts_emit == Some(true) {
        Debug_.assert(
            declaration_transform.transformed().len() == 1,
            Some("Should only see one output from the decl transform"),
        );
        print_source_file_or_bundle(
            host.clone(),
            writer,
            source_map_data_list,
            new_line,
            emitter_diagnostics,
            &compiler_options,
            declaration_file_path,
            declaration_map_path,
            &declaration_transform.transformed()[0],
            &declaration_printer,
            &SourceMapOptions {
                source_map: if force_dts_emit != Some(true) {
                    compiler_options.declaration_map
                } else {
                    Some(false)
                },
                source_root: compiler_options.source_root.clone(),
                map_root: compiler_options.map_root.clone(),
                extended_diagnostics: compiler_options.extended_diagnostics,
                inline_source_map: None,
                inline_sources: None,
            },
        )?;
        if force_dts_emit == Some(true)
            && declaration_transform.transformed()[0].kind() == SyntaxKind::SourceFile
        {
            let source_file = declaration_transform.transformed()[0].clone();
            *exported_modules_from_declaration_emit = source_file
                .as_source_file()
                .maybe_exported_modules_from_declaration_emit()
                .clone();
        }
    }
    declaration_transform.dispose();
    if let Some(bundle_build_info) = bundle_build_info.clone() {
        bundle_build_info.borrow_mut().js = declaration_printer.maybe_bundle_file_info().clone();
    }

    Ok(())
}

#[derive(Trace, Finalize)]
struct EmitDeclarationFileOrBundlePrintHandlers {
    resolver: Gc<Box<dyn EmitResolver>>,
    declaration_transform: Gc<Box<TransformNodesTransformationResult>>,
}

impl EmitDeclarationFileOrBundlePrintHandlers {
    fn new(
        resolver: Gc<Box<dyn EmitResolver>>,
        declaration_transform: Gc<Box<TransformNodesTransformationResult>>,
    ) -> Self {
        Self {
            resolver,
            declaration_transform,
        }
    }
}

impl PrintHandlers for EmitDeclarationFileOrBundlePrintHandlers {
    fn has_global_name(&self, name: &str) -> Option<bool> {
        Some(self.resolver.has_global_name(name))
    }

    fn is_on_emit_node_supported(&self) -> bool {
        true
    }

    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        self.declaration_transform
            .emit_node_with_notification(hint, node, emit_callback)?;

        Ok(())
    }

    fn is_emit_notification_enabled(&self, node: &Node) -> Option<bool> {
        self.declaration_transform
            .is_emit_notification_enabled(node)
    }

    fn is_substitute_node_supported(&self) -> bool {
        true
    }

    fn substitute_node(&self, hint: EmitHint, node: &Node) -> io::Result<Option<Gc<Node>>> {
        Ok(Some(
            self.declaration_transform.substitute_node(hint, node)?,
        ))
    }
}

fn collect_linked_aliases(resolver: &dyn EmitResolver, node: &Node) -> io::Result<()> {
    if is_export_assignment(node) {
        let node_as_export_assignment = node.as_export_assignment();
        if node_as_export_assignment.expression.kind() == SyntaxKind::Identifier {
            resolver.collect_linked_aliases(&node_as_export_assignment.expression, Some(true))?;
        }
        return Ok(());
    } else if is_export_specifier(node) {
        let node_as_export_specifier = node.as_export_specifier();
        resolver.collect_linked_aliases(
            node_as_export_specifier
                .property_name
                .as_deref()
                .unwrap_or(&*node_as_export_specifier.name),
            Some(true),
        )?;
        return Ok(());
    }
    try_for_each_child(
        node,
        |node: &Node| collect_linked_aliases(resolver, node),
        Option::<fn(&NodeArray) -> io::Result<()>>::None,
    )?;

    Ok(())
}

fn print_source_file_or_bundle(
    host: Gc<Box<dyn EmitHost>>,
    writer: Gc<Box<dyn EmitTextWriter>>,
    source_map_data_list: &mut Option<Vec<SourceMapEmitResult>>,
    new_line: &str,
    emitter_diagnostics: &mut DiagnosticCollection,
    compiler_options: &CompilerOptions,
    js_file_path: &str,
    source_map_file_path: Option<&str>,
    source_file_or_bundle: &Node, /*SourceFile | Bundle*/
    printer: &Printer,
    map_options: &SourceMapOptions,
) -> io::Result<()> {
    let bundle = if source_file_or_bundle.kind() == SyntaxKind::Bundle {
        Some(source_file_or_bundle.node_wrapper())
    } else {
        None
    };
    let source_file = if source_file_or_bundle.kind() == SyntaxKind::SourceFile {
        Some(source_file_or_bundle.node_wrapper())
    } else {
        None
    };
    let source_files = if let Some(bundle) = bundle.as_ref() {
        bundle
            .as_bundle()
            .source_files
            .iter()
            .cloned()
            .map(Option::unwrap)
            .collect()
    } else {
        vec![source_file.clone().unwrap()]
    };

    let mut source_map_generator: Option<Gc<Box<dyn SourceMapGenerator>>> = None;
    if should_emit_source_maps(map_options, source_file_or_bundle) {
        source_map_generator = Some(create_source_map_generator(
            host.clone(),
            get_base_file_name(&normalize_slashes(js_file_path), None, None),
            get_source_root(map_options),
            get_source_map_directory(&**host, map_options, js_file_path, source_file.as_deref()),
            &map_options.into(),
        ));
    }

    if let Some(bundle) = bundle.as_ref() {
        printer.write_bundle(bundle, writer.clone(), source_map_generator.clone())?;
    } else {
        printer.write_file(
            source_file.as_ref().unwrap(),
            writer.clone(),
            source_map_generator.clone(),
        )?;
    }

    if let Some(source_map_generator) = source_map_generator {
        if let Some(source_map_data_list) = source_map_data_list.as_mut() {
            source_map_data_list.push(SourceMapEmitResult {
                input_source_file_names: source_map_generator.get_sources(),
                source_map: source_map_generator.to_json(),
            });
        }

        let source_mapping_url = get_source_mapping_url(
            &**host,
            map_options,
            &**source_map_generator,
            js_file_path,
            source_map_file_path,
            source_file.as_deref(),
        );

        if !source_mapping_url.is_empty() {
            if !writer.is_at_start_of_line() {
                writer.raw_write(new_line);
            }
            writer.write_comment(&format!("//# sourceMappingUrl={source_mapping_url}"));
        }

        if let Some(source_map_file_path) = source_map_file_path.non_empty() {
            let source_map = source_map_generator.to_string();
            write_file(
                &EmitHostWriteFileCallback::new(host.clone()),
                emitter_diagnostics,
                source_map_file_path,
                &source_map,
                false,
                Some(&source_files),
            )?;
        }
    } else {
        writer.write_line(None);
    }

    write_file(
        &EmitHostWriteFileCallback::new(host.clone()),
        emitter_diagnostics,
        js_file_path,
        &writer.get_text(),
        compiler_options.emit_bom == Some(true),
        Some(&source_files),
    )?;

    writer.clear();

    Ok(())
}

#[derive(Default)]
pub struct SourceMapOptions {
    pub source_map: Option<bool>,
    pub inline_source_map: Option<bool>,
    pub inline_sources: Option<bool>,
    pub source_root: Option<String>,
    pub map_root: Option<String>,
    pub extended_diagnostics: Option<bool>,
}

impl From<&CompilerOptions> for SourceMapOptions {
    fn from(value: &CompilerOptions) -> Self {
        Self {
            source_map: value.source_map,
            inline_source_map: value.inline_source_map,
            inline_sources: value.inline_sources,
            source_root: value.source_root.clone(),
            map_root: value.map_root.clone(),
            extended_diagnostics: value.extended_diagnostics,
        }
    }
}

fn should_emit_source_maps(
    map_options: &SourceMapOptions,
    source_file_or_bundle: &Node, /*SourceFile | Bundle*/
) -> bool {
    (map_options.source_map == Some(true) || map_options.inline_source_map == Some(true))
        && (source_file_or_bundle.kind() != SyntaxKind::SourceFile
            || !file_extension_is(
                &source_file_or_bundle.as_source_file().file_name(),
                Extension::Json.to_str(),
            ))
}

fn get_source_root(map_options: &SourceMapOptions) -> String {
    let source_root = normalize_slashes(map_options.source_root.as_deref().unwrap_or(""));
    if !source_root.is_empty() {
        ensure_trailing_directory_separator(&source_root)
    } else {
        source_root
    }
}

fn get_source_map_directory(
    host: &dyn EmitHost,
    map_options: &SourceMapOptions,
    file_path: &str,
    source_file: Option<&Node /*SourceFile*/>,
) -> String {
    if map_options.source_root.as_ref().is_non_empty() {
        return ModuleSpecifierResolutionHostAndGetCommonSourceDirectory::get_common_source_directory(host);
    }
    if let Some(map_options_map_root) = map_options.map_root.as_ref().non_empty() {
        let mut source_map_dir = normalize_slashes(map_options_map_root);
        if let Some(source_file) = source_file {
            source_map_dir = get_directory_path(&get_source_file_path_in_new_dir(
                &source_file.as_source_file().file_name(),
                host,
                &source_map_dir,
            ));
        }
        if get_root_length(&source_map_dir) == 0 {
            source_map_dir = combine_paths(
                &ModuleSpecifierResolutionHostAndGetCommonSourceDirectory::get_common_source_directory(host),
                &[Some(&source_map_dir)],
            );
        }
        return source_map_dir;
    }
    get_directory_path(&normalize_path(file_path))
}

fn get_source_mapping_url(
    host: &dyn EmitHost,
    map_options: &SourceMapOptions,
    source_map_generator: &dyn SourceMapGenerator,
    file_path: &str,
    source_map_file_path: Option<&str>,
    source_file: Option<&Node /*SourceFile*/>,
) -> String {
    if map_options.inline_source_map == Some(true) {
        let source_map_text = source_map_generator.to_string();
        let base_64_source_map_text = base64_encode(
            Some(|str_: &str| get_sys().base64_encode(str_)),
            &source_map_text,
        );
        return format!("data:application/json;base64,{base_64_source_map_text}");
    }

    let source_map_file = get_base_file_name(
        &normalize_slashes(Debug_.check_defined(source_map_file_path, None)),
        None,
        None,
    );
    if let Some(map_options_map_root) = map_options.map_root.as_ref().non_empty() {
        let mut source_map_dir = normalize_slashes(map_options_map_root);
        if let Some(source_file) = source_file {
            source_map_dir = get_directory_path(&get_source_file_path_in_new_dir(
                &source_file.as_source_file().file_name(),
                host,
                &source_map_dir,
            ));
        }
        if get_root_length(&source_map_dir) == 0 {
            source_map_dir = combine_paths(
                &ModuleSpecifierResolutionHostAndGetCommonSourceDirectory::get_common_source_directory(host),
                &[Some(&source_map_dir)],
            );
            return encode_uri(&get_relative_path_to_directory_or_url(
                &get_directory_path(&normalize_path(file_path)),
                &combine_paths(&source_map_dir, &[Some(&source_map_file)]),
                &ScriptReferenceHost::get_current_directory(host),
                |file_name| host.get_canonical_file_name(file_name),
                true,
            ));
        } else {
            return encode_uri(&combine_paths(&source_map_dir, &[Some(&source_map_file)]));
        }
    }
    encode_uri(&source_map_file)
}

pub(crate) fn get_build_info_text(build_info: &BuildInfo) -> String {
    serde_json::to_string(build_info).unwrap()
}

pub(crate) fn get_build_info(_build_info_text: &str) -> Gc<BuildInfo> {
    unimplemented!()
}

thread_local! {
    static not_implemented_resolver_: Gc<Box<dyn EmitResolver>> = Gc::new(Box::new(NotImplementedResolver));
}

pub(crate) fn not_implemented_resolver() -> Gc<Box<dyn EmitResolver>> {
    not_implemented_resolver_.with(|not_implemented_resolver| not_implemented_resolver.clone())
}

#[derive(Trace, Finalize)]
struct NotImplementedResolver;

impl EmitResolver for NotImplementedResolver {
    fn has_global_name(&self, _name: &str) -> bool {
        unimplemented!()
    }

    fn get_referenced_export_container(
        &self,
        _node: &Node, /*Identifier*/
        _prefix_locals: Option<bool>,
    ) -> io::Result<Option<Gc<Node /*SourceFile | ModuleDeclaration | EnumDeclaration*/>>> {
        unimplemented!()
    }

    fn get_referenced_import_declaration(
        &self,
        _node: &Node, /*Identifier*/
    ) -> io::Result<Option<Gc<Node /*Declaration*/>>> {
        unimplemented!()
    }

    fn get_referenced_declaration_with_colliding_name(
        &self,
        _node: &Node, /*Identifier*/
    ) -> io::Result<Option<Gc<Node /*Declaration*/>>> {
        unimplemented!()
    }

    fn is_declaration_with_colliding_name(
        &self,
        _node: &Node, /*Declaration*/
    ) -> io::Result<bool> {
        unimplemented!()
    }

    fn is_value_alias_declaration(&self, _node: &Node) -> io::Result<bool> {
        unimplemented!()
    }

    fn is_referenced_alias_declaration(
        &self,
        _node: &Node,
        _check_children: Option<bool>,
    ) -> io::Result<bool> {
        unimplemented!()
    }

    fn is_top_level_value_import_equals_with_entity_name(
        &self,
        _node: &Node, /*ImportEqualsDeclaration*/
    ) -> io::Result<bool> {
        unimplemented!()
    }

    fn get_node_check_flags(&self, _node: &Node) -> crate::NodeCheckFlags {
        unimplemented!()
    }

    fn is_declaration_visible(&self, _node: &Node /*Declaration | AnyImportSyntax*/) -> bool {
        unimplemented!()
    }

    fn is_late_bound(&self, _node: &Node /*Declaration*/) -> io::Result<bool> {
        Ok(false)
    }

    fn collect_linked_aliases(
        &self,
        _node: &Node, /*Identifier*/
        _set_visibility: Option<bool>,
    ) -> io::Result<Option<Vec<Gc<Node>>>> {
        unimplemented!()
    }

    fn is_implementation_of_overload(
        &self,
        _node: &Node, /*SignatureDeclaration*/
    ) -> io::Result<Option<bool>> {
        unimplemented!()
    }

    fn is_required_initialized_parameter(
        &self,
        _node: &Node, /*ParameterDeclaration*/
    ) -> io::Result<bool> {
        unimplemented!()
    }

    fn is_optional_uninitialized_parameter_property(
        &self,
        _node: &Node, /*ParameterDeclaration*/
    ) -> io::Result<bool> {
        unimplemented!()
    }

    fn is_expando_function_declaration(
        &self,
        _node: &Node, /*FunctionDeclaration*/
    ) -> io::Result<bool> {
        unimplemented!()
    }

    fn get_properties_of_container_function(
        &self,
        _node: &Node, /*Declaration*/
    ) -> io::Result<Vec<Id<Symbol>>> {
        unimplemented!()
    }

    fn create_type_of_declaration(
        &self,
        _declaration: &Node, /*AccessorDeclaration | VariableLikeDeclaration | PropertyAccessExpression*/
        _enclosing_declaration: &Node,
        _flags: crate::NodeBuilderFlags,
        _tracker: Gc<Box<dyn crate::SymbolTracker>>,
        _add_undefined: Option<bool>,
    ) -> io::Result<Option<Gc<Node /*TypeNode*/>>> {
        unimplemented!()
    }

    fn create_return_type_of_signature_declaration(
        &self,
        _signature_declaration: &Node, /*SignatureDeclaration*/
        _enclosing_declaration: &Node,
        _flags: crate::NodeBuilderFlags,
        _tracker: Gc<Box<dyn crate::SymbolTracker>>,
    ) -> io::Result<Option<Gc<Node /*TypeNode*/>>> {
        unimplemented!()
    }

    fn create_type_of_expression(
        &self,
        _expr: &Node, /*Expression*/
        _enclosing_declaration: &Node,
        _flags: crate::NodeBuilderFlags,
        _tracker: Gc<Box<dyn crate::SymbolTracker>>,
    ) -> io::Result<Option<Gc<Node /*TypeNode*/>>> {
        unimplemented!()
    }

    fn create_literal_const_value(
        &self,
        _node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
        _tracker: Gc<Box<dyn crate::SymbolTracker>>,
    ) -> io::Result<Gc<Node /*Expression*/>> {
        unimplemented!()
    }

    fn is_symbol_accessible(
        &self,
        _symbol: Id<Symbol>,
        _enclosing_declaration: Option<&Node>,
        _meaning: Option<crate::SymbolFlags>,
        _should_compute_alias_to_mark_visible: bool,
    ) -> io::Result<SymbolAccessibilityResult> {
        unimplemented!()
    }

    fn is_entity_name_visible(
        &self,
        _entity_name: &Node, /*EntityNameOrEntityNameExpression*/
        _enclosing_declaration: &Node,
    ) -> io::Result<SymbolVisibilityResult> {
        unimplemented!()
    }

    fn get_constant_value(
        &self,
        _node: &Node, /*EnumMember | PropertyAccessExpression | ElementAccessExpression*/
    ) -> io::Result<Option<StringOrNumber>> {
        unimplemented!()
    }

    fn get_referenced_value_declaration(
        &self,
        _reference: &Node, /*Identifier*/
    ) -> io::Result<Option<Gc<Node /*Declaration*/>>> {
        unimplemented!()
    }

    fn get_type_reference_serialization_kind(
        &self,
        _type_name: &Node, /*EntityName*/
        _location: Option<&Node>,
    ) -> io::Result<TypeReferenceSerializationKind> {
        unimplemented!()
    }

    fn is_optional_parameter(
        &self,
        _node: &Node, /*ParameterDeclaration*/
    ) -> io::Result<bool> {
        unimplemented!()
    }

    fn module_exports_some_value(
        &self,
        _module_reference_expression: &Node, /*Expression*/
    ) -> io::Result<bool> {
        unimplemented!()
    }

    fn is_arguments_local_binding(&self, _node: &Node /*Identifier*/) -> io::Result<bool> {
        unimplemented!()
    }

    fn get_external_module_file_from_declaration(
        &self,
        _declaration: &Node, /*ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration | ImportTypeNode | ImportCall*/
    ) -> io::Result<Option<Gc<Node /*SourceFile*/>>> {
        unimplemented!()
    }

    fn get_type_reference_directives_for_entity_name(
        &self,
        _name: &Node, /*EntityNameOrEntityNameExpression*/
    ) -> io::Result<Option<Vec<String>>> {
        unimplemented!()
    }

    fn get_type_reference_directives_for_symbol(
        &self,
        _symbol: Id<Symbol>,
        _meaning: Option<crate::SymbolFlags>,
    ) -> io::Result<Option<Vec<String>>> {
        unimplemented!()
    }

    fn is_literal_const_declaration(
        &self,
        _node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
    ) -> io::Result<bool> {
        unimplemented!()
    }

    fn get_jsx_factory_entity(&self, _location: Option<&Node>) -> Option<Gc<Node /*EntityName*/>> {
        unimplemented!()
    }

    fn get_jsx_fragment_factory_entity(
        &self,
        _location: Option<&Node>,
    ) -> Option<Gc<Node /*EntityName*/>> {
        unimplemented!()
    }

    fn get_all_accessor_declarations(
        &self,
        _declaration: &Node, /*AccessorDeclaration*/
    ) -> io::Result<AllAccessorDeclarations> {
        unimplemented!()
    }

    fn get_symbol_of_external_module_specifier(
        &self,
        _node: &Node, /*StringLiteralLike*/
    ) -> io::Result<Option<Id<Symbol>>> {
        unimplemented!()
    }

    fn is_binding_captured_by_node(
        &self,
        _node: &Node,
        _decl: &Node, /*VariableDeclaration | BindingElement*/
    ) -> io::Result<bool> {
        unimplemented!()
    }

    fn get_declaration_statements_for_source_file(
        &self,
        _node: &Node, /*SourceFile*/
        _flags: crate::NodeBuilderFlags,
        _tracker: Gc<Box<dyn crate::SymbolTracker>>,
        _bundled: Option<bool>,
    ) -> io::Result<Option<Vec<Gc<Node /*Statement*/>>>> {
        unimplemented!()
    }

    fn is_import_required_by_augmentation(
        &self,
        _decl: &Node, /*ImportDeclaration*/
    ) -> io::Result<bool> {
        unimplemented!()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum PipelinePhase {
    Notification,
    Substitution,
    Comments,
    SourceMaps,
    Emit,
}

impl PipelinePhase {
    pub fn incremented(&self) -> Self {
        match self {
            Self::Notification => Self::Substitution,
            Self::Substitution => Self::Comments,
            Self::Comments => Self::SourceMaps,
            Self::SourceMaps => Self::Emit,
            Self::Emit => panic!("Called incremented() on PipelinePhase::Emit"),
        }
    }
}

pub fn create_printer(
    printer_options: PrinterOptions,
    handlers: Option<Gc<Box<dyn PrintHandlers>>>,
) -> Gc<Printer> {
    let handlers = handlers.unwrap_or_else(|| Gc::new(Box::new(DummyPrintHandlers)));
    let printer = Gc::new(Printer::new(printer_options, handlers));
    *printer._rc_wrapper.borrow_mut() = Some(printer.clone());
    printer.reset();
    *printer.emit_binary_expression.borrow_mut() =
        Some(Gc::new(printer.create_emit_binary_expression()));
    printer
}

#[derive(Trace, Finalize)]
pub(super) struct DummyPrintHandlers;

impl PrintHandlers for DummyPrintHandlers {
    fn is_on_emit_node_supported(&self) -> bool {
        false
    }

    fn is_substitute_node_supported(&self) -> bool {
        false
    }
}

impl Printer {
    pub fn new(printer_options: PrinterOptions, handlers: Gc<Box<dyn PrintHandlers>>) -> Self {
        let extended_diagnostics = printer_options.extended_diagnostics == Some(true);
        let new_line =
            get_new_line_character(printer_options.new_line, Option::<fn() -> String>::None);
        let module_kind = get_emit_module_kind_from_module_and_target(
            printer_options.module,
            printer_options.target,
        );
        let preserve_source_newlines = printer_options.preserve_source_newlines;
        let bundle_file_info = if printer_options.write_bundle_file_info == Some(true) {
            Some(Gc::new(GcCell::new(BundleFileInfo {
                sections: vec![],
                sources: None,
            })))
        } else {
            None
        };
        let relative_to_build_info = if bundle_file_info.is_some() {
            Some(Debug_.check_defined(printer_options.relative_to_build_info.clone(), None))
        } else {
            None
        };
        let record_internal_section = printer_options.record_internal_section;
        Self {
            _rc_wrapper: Default::default(),
            printer_options,
            handlers,
            extended_diagnostics,
            new_line,
            module_kind,
            current_source_file: Default::default(),
            bundled_helpers: Default::default(),
            node_id_to_generated_name: Default::default(),
            auto_generated_id_to_generated_name: Default::default(),
            generated_names: Default::default(),
            temp_flags_stack: Default::default(),
            temp_flags: Cell::new(TempFlags::Auto),
            reserved_names_stack: Default::default(),
            reserved_names: Default::default(),
            preserve_source_newlines: Cell::new(preserve_source_newlines),
            next_list_element_pos: Default::default(),
            writer: Default::default(),
            own_writer: Default::default(),
            write: Cell::new(Printer::write_base),
            is_own_file_emit: Default::default(),
            bundle_file_info: GcCell::new(bundle_file_info),
            relative_to_build_info,
            record_internal_section,
            source_file_text_pos: Default::default(),
            source_file_text_kind: Cell::new(BundleFileSectionKind::Text),

            source_maps_disabled: Cell::new(true),
            source_map_generator: Default::default(),
            source_map_source: Default::default(),
            source_map_source_index: Cell::new(-1),
            most_recently_added_source_map_source: Default::default(),
            most_recently_added_source_map_source_index: Cell::new(-1),

            container_pos: Cell::new(-1),
            container_end: Cell::new(-1),
            declaration_list_container_end: Cell::new(-1),
            current_line_map: Default::default(),
            detached_comments_info: Default::default(),
            has_written_comment: Default::default(),
            comments_disabled: Default::default(),
            last_substitution: Default::default(),
            current_parenthesizer_rule: Default::default(),
            // const { enter: enterComment, exit: exitComment } = performance.createTimerIf(extendedDiagnostics, "commentTime", "beforeComment", "afterComment");
            parenthesizer: factory.with(|factory_| factory_.parenthesizer()),
            emit_binary_expression: Default::default(),
        }
    }

    pub(super) fn arena(&self) -> &AllArenas {
        unsafe { &*self.arena }
    }

    pub(super) fn symbol(&self, symbol: Id<Symbol>) -> debug_cell::Ref<Symbol> {
        self.arena().symbol(symbol)
    }

    pub(super) fn rc_wrapper(&self) -> Gc<Printer> {
        self._rc_wrapper.borrow().clone().unwrap()
    }

    pub(super) fn maybe_current_source_file(&self) -> Option<Gc<Node>> {
        self.current_source_file.borrow().clone()
    }

    pub(super) fn current_source_file(&self) -> Gc<Node> {
        self.current_source_file.borrow().clone().unwrap()
    }

    pub(super) fn set_current_source_file(&self, current_source_file: Option<Gc<Node>>) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    pub(super) fn bundled_helpers(&self) -> Ref<HashMap<String, bool>> {
        self.bundled_helpers.borrow()
    }

    pub(super) fn bundled_helpers_mut(&self) -> RefMut<HashMap<String, bool>> {
        self.bundled_helpers.borrow_mut()
    }

    pub(super) fn node_id_to_generated_name_mut(&self) -> RefMut<HashMap<NodeId, String>> {
        self.node_id_to_generated_name.borrow_mut()
    }

    pub(super) fn set_node_id_to_generated_name(
        &self,
        node_id_to_generated_name: HashMap<NodeId, String>,
    ) {
        *self.node_id_to_generated_name.borrow_mut() = node_id_to_generated_name;
    }

    pub(super) fn auto_generated_id_to_generated_name_mut(&self) -> RefMut<HashMap<usize, String>> {
        self.auto_generated_id_to_generated_name.borrow_mut()
    }

    pub(super) fn set_auto_generated_id_to_generated_name(
        &self,
        auto_generated_id_to_generated_name: HashMap<usize, String>,
    ) {
        *self.auto_generated_id_to_generated_name.borrow_mut() =
            auto_generated_id_to_generated_name;
    }

    pub(super) fn generated_names(&self) -> Ref<HashSet<String>> {
        self.generated_names.borrow()
    }

    pub(super) fn generated_names_mut(&self) -> RefMut<HashSet<String>> {
        self.generated_names.borrow_mut()
    }

    pub(super) fn set_generated_names(&self, generated_names: HashSet<String>) {
        *self.generated_names.borrow_mut() = generated_names;
    }

    pub(super) fn temp_flags_stack_mut(&self) -> RefMut<Vec<TempFlags>> {
        self.temp_flags_stack.borrow_mut()
    }

    pub(super) fn set_temp_flags_stack(&self, temp_flags_stack: Vec<TempFlags>) {
        *self.temp_flags_stack.borrow_mut() = temp_flags_stack;
    }

    pub(super) fn temp_flags(&self) -> TempFlags {
        self.temp_flags.get()
    }

    pub(super) fn set_temp_flags(&self, temp_flags: TempFlags) {
        self.temp_flags.set(temp_flags);
    }

    pub(super) fn reserved_names_stack(&self) -> Ref<Vec<Option<Rc<RefCell<HashSet<String>>>>>> {
        self.reserved_names_stack.borrow()
    }

    pub(super) fn reserved_names_stack_mut(
        &self,
    ) -> RefMut<Vec<Option<Rc<RefCell<HashSet<String>>>>>> {
        self.reserved_names_stack.borrow_mut()
    }

    pub(super) fn set_reserved_names_stack(
        &self,
        reserved_names_stack: Vec<Option<Rc<RefCell<HashSet<String>>>>>,
    ) {
        *self.reserved_names_stack.borrow_mut() = reserved_names_stack;
    }

    pub(super) fn maybe_reserved_names(&self) -> Option<Rc<RefCell<HashSet<String>>>> {
        self.reserved_names.borrow().clone()
    }

    pub(super) fn maybe_reserved_names_mut(&self) -> RefMut<Option<Rc<RefCell<HashSet<String>>>>> {
        self.reserved_names.borrow_mut()
    }

    pub(super) fn set_reserved_names(&self, reserved_names: Option<Rc<RefCell<HashSet<String>>>>) {
        *self.reserved_names.borrow_mut() = reserved_names;
    }

    pub(super) fn maybe_preserve_source_newlines(&self) -> Option<bool> {
        self.preserve_source_newlines.get()
    }

    pub(super) fn set_preserve_source_newlines(&self, preserve_source_newlines: Option<bool>) {
        self.preserve_source_newlines.set(preserve_source_newlines);
    }

    pub(super) fn maybe_next_list_element_pos(&self) -> Option<isize> {
        self.next_list_element_pos.get()
    }

    pub(super) fn set_next_list_element_pos(&self, next_list_element_pos: Option<isize>) {
        self.next_list_element_pos.set(next_list_element_pos);
    }

    pub(super) fn maybe_writer(&self) -> Option<Gc<Box<dyn EmitTextWriter>>> {
        self.writer.borrow().clone()
    }

    pub(super) fn writer(&self) -> Gc<Box<dyn EmitTextWriter>> {
        self.writer.borrow().clone().unwrap()
    }

    pub(super) fn has_global_name(&self, name: &str) -> Option<bool> {
        self.handlers.has_global_name(name)
    }

    pub(super) fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        Ok(if self.handlers.is_on_emit_node_supported() {
            self.handlers.on_emit_node(hint, node, emit_callback)?
        } else {
            no_emit_notification(hint, node, emit_callback)?
        })
    }

    pub(super) fn is_on_emit_node_no_emit_notification(&self) -> bool {
        !self.handlers.is_on_emit_node_supported()
    }

    pub(super) fn is_emit_notification_enabled(&self, node: &Node) -> Option<bool> {
        self.handlers.is_emit_notification_enabled(node)
    }

    pub(super) fn substitute_node(
        &self,
        hint: EmitHint,
        node: &Node,
    ) -> io::Result<Option<Gc<Node>>> {
        Ok(Some(if self.handlers.is_substitute_node_supported() {
            self.handlers.substitute_node(hint, node)?.unwrap()
        } else {
            no_emit_substitution(hint, node)
        }))
    }

    pub(super) fn is_substitute_node_no_emit_substitution(&self) -> bool {
        !self.handlers.is_substitute_node_supported()
    }

    pub(super) fn on_before_emit_node(&self, node: Option<&Node>) {
        self.handlers.on_before_emit_node(node)
    }

    pub(super) fn on_after_emit_node(&self, node: Option<&Node>) {
        self.handlers.on_after_emit_node(node)
    }

    pub(super) fn on_before_emit_node_array(&self, nodes: Option<&NodeArray>) {
        self.handlers.on_before_emit_node_array(nodes)
    }

    pub(super) fn on_after_emit_node_array(&self, nodes: Option<&NodeArray>) {
        self.handlers.on_after_emit_node_array(nodes)
    }

    pub(super) fn on_before_emit_token(&self, node: Option<&Node>) {
        self.handlers.on_before_emit_token(node)
    }

    pub(super) fn on_after_emit_token(&self, node: Option<&Node>) {
        self.handlers.on_after_emit_token(node)
    }

    pub(super) fn write(&self, text: &str) {
        (self.write.get())(self, text);
    }

    pub(super) fn is_own_file_emit(&self) -> bool {
        self.is_own_file_emit.get()
    }

    pub(super) fn set_is_own_file_emit(&self, is_own_file_emit: bool) {
        self.is_own_file_emit.set(is_own_file_emit);
    }

    pub(super) fn maybe_bundle_file_info(&self) -> Option<Gc<GcCell<BundleFileInfo>>> {
        self.bundle_file_info.borrow().clone()
    }

    pub(super) fn bundle_file_info(&self) -> Gc<GcCell<BundleFileInfo>> {
        self.bundle_file_info.borrow().clone().unwrap()
    }

    pub(super) fn relative_to_build_info(&self, value: &str) -> String {
        self.relative_to_build_info.clone().unwrap().call(value)
    }

    pub(super) fn source_file_text_pos(&self) -> usize {
        self.source_file_text_pos.get()
    }

    pub(super) fn set_source_file_text_pos(&self, source_file_text_pos: usize) {
        self.source_file_text_pos.set(source_file_text_pos);
    }

    pub(super) fn source_file_text_kind(&self) -> BundleFileSectionKind {
        self.source_file_text_kind.get()
    }

    pub(super) fn set_source_file_text_kind(&self, source_file_text_kind: BundleFileSectionKind) {
        self.source_file_text_kind.set(source_file_text_kind);
    }

    pub(super) fn set_source_map_generator(
        &self,
        source_map_generator: Option<Gc<Box<dyn SourceMapGenerator>>>,
    ) {
        *self.source_map_generator.borrow_mut() = source_map_generator;
    }

    pub(super) fn source_maps_disabled(&self) -> bool {
        self.source_maps_disabled.get()
    }

    pub(super) fn set_source_maps_disabled(&self, source_maps_disabled: bool) {
        self.source_maps_disabled.set(source_maps_disabled);
    }

    pub(super) fn source_map_generator(&self) -> Gc<Box<dyn SourceMapGenerator>> {
        self.source_map_generator.borrow().clone().unwrap()
    }

    pub(super) fn maybe_source_map_generator(&self) -> Option<Gc<Box<dyn SourceMapGenerator>>> {
        self.source_map_generator.borrow().clone()
    }

    pub(super) fn maybe_source_map_source(&self) -> Option<Gc<SourceMapSource>> {
        self.source_map_source.borrow().clone()
    }

    pub(super) fn source_map_source(&self) -> Gc<SourceMapSource> {
        self.source_map_source.borrow().clone().unwrap()
    }

    pub(super) fn set_source_map_source_(&self, source_map_source: Option<Gc<SourceMapSource>>) {
        *self.source_map_source.borrow_mut() = source_map_source;
    }

    pub(super) fn source_map_source_index(&self) -> isize {
        self.source_map_source_index.get()
    }

    pub(super) fn set_source_map_source_index(&self, source_map_source_index: isize) {
        self.source_map_source_index.set(source_map_source_index);
    }

    pub(super) fn maybe_most_recently_added_source_map_source(
        &self,
    ) -> Option<Gc<SourceMapSource>> {
        self.most_recently_added_source_map_source.borrow().clone()
    }

    pub(super) fn set_most_recently_added_source_map_source(
        &self,
        most_recently_added_source_map_source: Option<Gc<SourceMapSource>>,
    ) {
        *self.most_recently_added_source_map_source.borrow_mut() =
            most_recently_added_source_map_source;
    }

    pub(super) fn most_recently_added_source_map_source_index(&self) -> isize {
        self.most_recently_added_source_map_source_index.get()
    }

    pub(super) fn set_most_recently_added_source_map_source_index(
        &self,
        most_recently_added_source_map_source_index: isize,
    ) {
        self.most_recently_added_source_map_source_index
            .set(most_recently_added_source_map_source_index);
    }

    pub(super) fn container_pos(&self) -> isize {
        self.container_pos.get()
    }

    pub(super) fn set_container_pos(&self, container_pos: isize) {
        self.container_pos.set(container_pos);
    }

    pub(super) fn container_end(&self) -> isize {
        self.container_end.get()
    }

    pub(super) fn set_container_end(&self, container_end: isize) {
        self.container_end.set(container_end);
    }

    pub(super) fn declaration_list_container_end(&self) -> isize {
        self.declaration_list_container_end.get()
    }

    pub(super) fn set_declaration_list_container_end(&self, declaration_list_container_end: isize) {
        self.declaration_list_container_end
            .set(declaration_list_container_end);
    }

    pub(super) fn maybe_current_line_map(&self) -> Ref<Option<Vec<usize>>> {
        self.current_line_map.borrow()
    }

    pub(super) fn current_line_map(&self) -> Ref<Vec<usize>> {
        Ref::map(self.current_line_map.borrow(), |current_line_map| {
            current_line_map.as_ref().unwrap()
        })
    }

    pub(super) fn set_current_line_map(&self, current_line_map: Option<Vec<usize>>) {
        *self.current_line_map.borrow_mut() = current_line_map;
    }

    pub(super) fn maybe_detached_comments_info(&self) -> Ref<Option<Vec<DetachedCommentInfo>>> {
        self.detached_comments_info.borrow()
    }

    pub(super) fn detached_comments_info(&self) -> Ref<Vec<DetachedCommentInfo>> {
        Ref::map(
            self.detached_comments_info.borrow(),
            |detached_comments_info| detached_comments_info.as_ref().unwrap(),
        )
    }

    pub(super) fn maybe_detached_comments_info_mut(
        &self,
    ) -> RefMut<Option<Vec<DetachedCommentInfo>>> {
        self.detached_comments_info.borrow_mut()
    }

    pub(super) fn detached_comments_info_mut(&self) -> RefMut<Vec<DetachedCommentInfo>> {
        RefMut::map(
            self.detached_comments_info.borrow_mut(),
            |detached_comments_info| detached_comments_info.as_mut().unwrap(),
        )
    }

    pub(super) fn set_detached_comments_info(
        &self,
        detached_comments_info: Option<Vec<DetachedCommentInfo>>,
    ) {
        *self.detached_comments_info.borrow_mut() = detached_comments_info;
    }

    pub(super) fn has_written_comment(&self) -> bool {
        self.has_written_comment.get()
    }

    pub(super) fn set_has_written_comment(&self, has_written_comment: bool) {
        self.has_written_comment.set(has_written_comment);
    }

    pub(super) fn comments_disabled(&self) -> bool {
        self.comments_disabled.get()
    }

    pub(super) fn set_comments_disabled(&self, comments_disabled: bool) {
        self.comments_disabled.set(comments_disabled);
    }

    pub(super) fn maybe_last_substitution(&self) -> Option<Gc<Node>> {
        self.last_substitution.borrow().clone()
    }

    pub(super) fn set_last_substitution(&self, last_substitution: Option<Gc<Node>>) {
        *self.last_substitution.borrow_mut() = last_substitution;
    }

    pub(super) fn maybe_current_parenthesizer_rule(
        &self,
    ) -> Option<Gc<Box<dyn CurrentParenthesizerRule>>> {
        self.current_parenthesizer_rule.borrow().clone()
    }

    pub(super) fn set_current_parenthesizer_rule(
        &self,
        current_parenthesizer_rule: Option<Gc<Box<dyn CurrentParenthesizerRule>>>,
    ) {
        *self.current_parenthesizer_rule.borrow_mut() = current_parenthesizer_rule;
    }

    pub(super) fn parenthesizer(
        &self,
    ) -> Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>> {
        self.parenthesizer.clone()
    }

    pub(super) fn enter_comment(&self) {
        // unimplemented!()
    }

    pub(super) fn exit_comment(&self) {
        // unimplemented!()
    }

    pub(super) fn emit_binary_expression_rc(&self) -> Gc<EmitBinaryExpression> {
        self.emit_binary_expression.borrow().clone().unwrap()
    }

    pub(super) fn emit_binary_expression(&self, node: &Node /*BinaryExpression*/) {
        self.emit_binary_expression_rc()
            .call(node)
            .expect("Don't _think_ this is actually fallible?")
    }

    pub fn print_node(
        &self,
        hint: EmitHint,
        node: &Node,
        source_file: &Node, /*SourceFile*/
    ) -> io::Result<String> {
        match hint {
            EmitHint::SourceFile => {
                Debug_.assert(is_source_file(node), Some("Expected a SourceFile node."));
            }
            EmitHint::IdentifierName => {
                Debug_.assert(is_identifier(node), Some("Expected an Identifier node."));
            }
            EmitHint::Expression => {
                Debug_.assert(is_expression(node), Some("Expected an Expression node."));
            }
            _ => (),
        }
        match node.kind() {
            SyntaxKind::SourceFile => {
                return self.print_file(node);
            }
            SyntaxKind::Bundle => {
                return self.print_bundle(node);
            }
            SyntaxKind::UnparsedSource => {
                return self.print_unparsed_source(node);
            }
            _ => (),
        }
        self.write_node(hint, node, Some(source_file), self.begin_print())?;
        Ok(self.end_print())
    }

    pub fn print_list(
        &self,
        format: ListFormat,
        nodes: &NodeArray,
        source_file: &Node, /*SourceFile*/
    ) -> io::Result<String> {
        self.write_list(format, nodes, Some(source_file), self.begin_print())?;
        Ok(self.end_print())
    }

    pub fn print_bundle(&self, bundle: &Node /*Bundle*/) -> io::Result<String> {
        self.write_bundle(bundle, self.begin_print(), None)?;
        Ok(self.end_print())
    }

    pub fn print_file(&self, source_file: &Node /*SourceFile*/) -> io::Result<String> {
        self.write_file(source_file, self.begin_print(), None)?;
        Ok(self.end_print())
    }

    pub fn print_unparsed_source(
        &self,
        unparsed: &Node, /*UnparsedSource*/
    ) -> io::Result<String> {
        self.write_unparsed_source(unparsed, self.begin_print())?;
        Ok(self.end_print())
    }

    pub fn write_node(
        &self,
        hint: EmitHint,
        node: &Node,
        source_file: Option<&Node /*SourceFile*/>,
        output: Gc<Box<dyn EmitTextWriter>>,
    ) -> io::Result<()> {
        let previous_writer = self.maybe_writer();
        self.set_writer(Some(output), None);
        self.print(hint, node, source_file)?;
        self.reset();
        *self.writer.borrow_mut() = previous_writer;

        Ok(())
    }

    pub fn write_list(
        &self,
        format: ListFormat,
        nodes: &NodeArray,
        source_file: Option<&Node /*SourceFile*/>,
        output: Gc<Box<dyn EmitTextWriter>>,
    ) -> io::Result<()> {
        let previous_writer = self.maybe_writer();
        self.set_writer(Some(output), None);
        if source_file.is_some() {
            self.set_source_file(source_file);
        }
        self.emit_list(Option::<&Node>::None, Some(nodes), format, None, None, None)?;
        self.reset();
        *self.writer.borrow_mut() = previous_writer;

        Ok(())
    }

    pub(super) fn get_text_pos_with_write_line(&self) -> usize {
        self.writer()
            .get_text_pos_with_write_line()
            .unwrap_or_else(|| self.writer().get_text_pos())
    }

    pub(super) fn update_or_push_bundle_file_text_like(
        &self,
        pos: isize,
        end: isize,
        kind: BundleFileSectionKind, /*BundleFileTextLikeKind*/
    ) {
        let bundle_file_info = self.bundle_file_info();
        let mut bundle_file_info = bundle_file_info.borrow_mut();
        let last = last_or_undefined(&bundle_file_info.sections);
        if let Some(last) = last.filter(|last| last.kind() == kind) {
            last.set_end(end);
        } else {
            bundle_file_info
                .sections
                .push(Gc::new(BundleFileSection::new_text_like(
                    kind, None, pos, end,
                )));
        }
    }
}
