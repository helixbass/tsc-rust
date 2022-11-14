pub mod compiler {
    use std::collections::HashMap;
    use std::ptr;
    use std::rc::Rc;
    use typescript_rust::{
        add_related_info, compare_diagnostics, create_compiler_diagnostic, create_program,
        file_extension_is, filter, get_declaration_emit_extension_for_path, get_output_extension,
        get_pre_emit_diagnostics, is_option_str_empty, length, some, Comparison, CompilerOptions,
        CreateProgramOptions, Diagnostic, DiagnosticCategory, DiagnosticMessage,
        DiagnosticRelatedInformation, EmitResult, Extension, ModuleKind, NewLineKind, Node,
        Program, ScriptTarget, SourceFileLike,
    };

    use crate::{collections, documents, fakes, vfs, vpath};

    pub struct CompilationOutput {
        pub inputs: Vec<Rc<documents::TextDocument>>,
        pub js: Option<Rc<documents::TextDocument>>,
        pub dts: Option<Rc<documents::TextDocument>>,
        pub map: Option<Rc<documents::TextDocument>>,
    }

    pub struct CompilationResult {
        pub host: Rc<fakes::CompilerHost>,
        pub program: Option<Rc<Program>>,
        pub result: Option<EmitResult>,
        pub options: Rc<CompilerOptions>,
        pub diagnostics: Vec<Rc<Diagnostic>>,
        pub js: collections::SortedMap<String, Rc<documents::TextDocument>>,
        pub dts: collections::SortedMap<String, Rc<documents::TextDocument>>,
        pub maps: collections::SortedMap<String, Rc<documents::TextDocument>>,
        pub symlinks: Option<vfs::FileSet>,

        _inputs: Vec<Rc<documents::TextDocument>>,
        _inputs_and_outputs: collections::SortedMap<String, Rc<CompilationOutput>>,
    }

    impl CompilationResult {
        pub fn new(
            host: Rc<fakes::CompilerHost>,
            options: Rc<CompilerOptions>,
            program: Option<Rc<Program>>,
            result: Option<EmitResult>,
            diagnostics: Vec<Rc<Diagnostic>>,
        ) -> Self {
            let options = if let Some(program) = program.as_ref() {
                program.get_compiler_options()
            } else {
                options
            };

            let mut js = collections::SortedMap::new(
                collections::SortOptions {
                    comparer: Rc::new({
                        let string_comparer = host.vfs().string_comparer.clone();
                        move |a: &String, b: &String| string_comparer(a, b)
                    }),
                    sort: Some(collections::SortOptionsSort::Insertion),
                },
                Option::<HashMap<String, Rc<documents::TextDocument>>>::None,
            );
            let mut dts = collections::SortedMap::new(
                collections::SortOptions {
                    comparer: Rc::new({
                        let string_comparer = host.vfs().string_comparer.clone();
                        move |a: &String, b: &String| string_comparer(a, b)
                    }),
                    sort: Some(collections::SortOptionsSort::Insertion),
                },
                Option::<HashMap<String, Rc<documents::TextDocument>>>::None,
            );
            let mut maps = collections::SortedMap::new(
                collections::SortOptions {
                    comparer: Rc::new({
                        let string_comparer = host.vfs().string_comparer.clone();
                        move |a: &String, b: &String| string_comparer(a, b)
                    }),
                    sort: Some(collections::SortOptionsSort::Insertion),
                },
                Option::<HashMap<String, Rc<documents::TextDocument>>>::None,
            );
            for document in &*host.outputs() {
                if vpath::is_java_script(&document.file)
                    || file_extension_is(&document.file, Extension::Json.to_str())
                {
                    js.set(document.file.clone(), document.clone());
                } else if vpath::is_declaration(&document.file) {
                    dts.set(document.file.clone(), document.clone());
                } else if vpath::is_source_map(&document.file) {
                    maps.set(document.file.clone(), document.clone());
                }
            }
            let mut ret = Self {
                host: host.clone(),
                program: program.clone(),
                result,
                diagnostics,
                options: options.clone(),
                js,
                dts,
                maps,
                _inputs_and_outputs: collections::SortedMap::new(
                    collections::SortOptions {
                        comparer: Rc::new({
                            let string_comparer = host.vfs().string_comparer.clone();
                            move |a: &String, b: &String| string_comparer(a, b)
                        }),
                        sort: Some(collections::SortOptionsSort::Insertion),
                    },
                    Option::<HashMap<String, Rc<CompilationOutput>>>::None,
                ),
                _inputs: Default::default(),
                symlinks: Default::default(),
            };
            if let Some(program) = program {
                if !is_option_str_empty(options.out.as_deref())
                    || !is_option_str_empty(options.out_file.as_deref())
                {
                    let out_file = vpath::resolve(
                        &ret.vfs().cwd(),
                        &[Some(
                            options
                                .out
                                .as_deref()
                                .filter(|options_out| !options_out.is_empty())
                                .unwrap_or_else(|| options.out_file.as_deref().unwrap()),
                        )],
                    );
                    let mut inputs: Vec<Rc<documents::TextDocument>> = vec![];
                    for source_file in &*program.get_source_files() {
                        // if (sourceFile) {
                        let source_file_as_source_file = source_file.as_source_file();
                        let input = Rc::new(documents::TextDocument::new(
                            source_file_as_source_file.file_name().clone(),
                            source_file_as_source_file.text().clone(),
                            None,
                        ));
                        ret._inputs.push(input.clone());
                        if !vpath::is_declaration(&source_file_as_source_file.file_name()) {
                            inputs.push(input);
                        }
                        // }
                    }

                    let outputs = Rc::new(CompilationOutput {
                        inputs: inputs.clone(),
                        js: ret.js.get(&out_file).cloned(),
                        dts: ret
                            .dts
                            .get(&vpath::change_extension(
                                &out_file,
                                ".d.ts",
                                Option::<&[&str]>::None,
                                None,
                            ))
                            .cloned(),
                        map: ret.maps.get(&format!("{out_file}.map")).cloned(),
                    });

                    if let Some(outputs_js) = outputs.js.as_ref() {
                        ret._inputs_and_outputs
                            .set(outputs_js.file.clone(), outputs.clone());
                    }
                    if let Some(outputs_dts) = outputs.dts.as_ref() {
                        ret._inputs_and_outputs
                            .set(outputs_dts.file.clone(), outputs.clone());
                    }
                    if let Some(outputs_map) = outputs.map.as_ref() {
                        ret._inputs_and_outputs
                            .set(outputs_map.file.clone(), outputs.clone());
                    }

                    for input in inputs {
                        ret._inputs_and_outputs
                            .set(input.file.clone(), outputs.clone());
                    }
                } else {
                    for source_file in &*program.get_source_files() {
                        // if (sourceFile) {
                        let source_file_as_source_file = source_file.as_source_file();
                        let input = Rc::new(documents::TextDocument::new(
                            source_file_as_source_file.file_name().clone(),
                            source_file_as_source_file.text().clone(),
                            None,
                        ));
                        ret._inputs.push(input.clone());
                        if !vpath::is_declaration(&source_file_as_source_file.file_name()) {
                            let extname = get_output_extension(
                                &source_file_as_source_file.file_name(),
                                &ret.options,
                            );
                            let outputs = Rc::new(CompilationOutput {
                                inputs: vec![input],
                                js: ret
                                    .js
                                    .get(&ret.get_output_path(
                                        &source_file_as_source_file.file_name(),
                                        extname.to_str(),
                                    ))
                                    .cloned(),
                                dts: ret
                                    .dts
                                    .get(&ret.get_output_path(
                                        &source_file_as_source_file.file_name(),
                                        get_declaration_emit_extension_for_path(
                                            &source_file_as_source_file.file_name(),
                                        ),
                                    ))
                                    .cloned(),
                                map: ret
                                    .maps
                                    .get(&ret.get_output_path(
                                        &source_file_as_source_file.file_name(),
                                        &format!("{}.map", extname.to_str()),
                                    ))
                                    .cloned(),
                            });

                            ret._inputs_and_outputs.set(
                                source_file_as_source_file.file_name().clone(),
                                outputs.clone(),
                            );
                            if let Some(outputs_js) = outputs.js.as_ref() {
                                ret._inputs_and_outputs
                                    .set(outputs_js.file.clone(), outputs.clone());
                            }
                            if let Some(outputs_dts) = outputs.dts.as_ref() {
                                ret._inputs_and_outputs
                                    .set(outputs_dts.file.clone(), outputs.clone());
                            }
                            if let Some(outputs_map) = outputs.map.as_ref() {
                                ret._inputs_and_outputs
                                    .set(outputs_map.file.clone(), outputs.clone());
                            }
                        }
                        // }
                    }
                }
            }
            ret
        }

        pub fn vfs(&self) -> Rc<vfs::FileSystem> {
            self.host.vfs()
        }

        pub fn get_output_path(&self, path: &str, ext: &str) -> String {
            unimplemented!()
        }
    }

    pub fn compile_files(
        host: Rc<fakes::CompilerHost>,
        root_files: Option<&[String]>,
        compiler_options: &CompilerOptions,
    ) -> CompilationResult {
        let mut compiler_options = compiler_options.clone();
        if compiler_options
            .project
            .as_ref()
            .filter(|compiler_options_project| !compiler_options_project.is_empty())
            .is_some()
            || match root_files {
                None => true,
                Some(root_files) => root_files.is_empty(),
            }
        {
            unimplemented!()
        }

        if compiler_options.target.is_none()
            && !matches!(
                compiler_options.module,
                Some(ModuleKind::Node12) | Some(ModuleKind::NodeNext)
            )
        {
            compiler_options.target = Some(ScriptTarget::ES3);
        }
        if compiler_options.new_line.is_none() {
            compiler_options.new_line = Some(NewLineKind::CarriageReturnLineFeed);
        }
        if compiler_options.skip_default_lib_check.is_none() {
            compiler_options.skip_default_lib_check = Some(true);
        }
        if compiler_options.no_error_truncation.is_none() {
            compiler_options.no_error_truncation = Some(true);
        }

        let skip_error_comparison = length(root_files) >= 100
            || (compiler_options.skip_lib_check == Some(true)
                && compiler_options.declaration == Some(true));

        let pre_program = if !skip_error_comparison {
            Some(create_program(CreateProgramOptions {
                root_names: root_files.map_or_else(|| vec![], ToOwned::to_owned),
                options: {
                    let mut options = compiler_options.clone();
                    options.trace_resolution = Some(false);
                    Rc::new(options)
                },
                host: Some(host.clone()),
                project_references: None,
                old_program: None,
                config_file_parsing_diagnostics: None,
            }))
        } else {
            None
        };
        let pre_errors = pre_program.clone().map(|pre_program| {
            get_pre_emit_diagnostics(&pre_program.into(), Option::<&Node>::None, None)
        });

        let compiler_options = Rc::new(compiler_options);
        let program = create_program(CreateProgramOptions {
            root_names: root_files.map_or_else(|| vec![], ToOwned::to_owned),
            options: compiler_options.clone(),
            host: Some(host.clone()),
            project_references: None,
            old_program: None,
            config_file_parsing_diagnostics: None,
        });

        let emit_result = program.emit(None, None, None, None, None, None);
        let post_errors =
            get_pre_emit_diagnostics(&program.clone().into(), Option::<&Node>::None, None);
        let longer_errors = if length(pre_errors.as_deref()) > post_errors.len() {
            pre_errors.as_ref().unwrap()
        } else {
            &post_errors
        };
        let shorter_errors = if matches!(
            pre_errors.as_ref(),
            Some(pre_errors) if ptr::eq(
                longer_errors,
                pre_errors
            )
        ) {
            Some(&post_errors)
        } else {
            pre_errors.as_ref()
        };
        let errors = if matches!(
            pre_errors.as_ref(),
            Some(pre_errors) if pre_errors.len() != post_errors.len()
        ) {
            let mut errors = shorter_errors.unwrap().clone();
            errors.push({
                let diagnostic: Rc<Diagnostic> = Rc::new(
                    create_compiler_diagnostic(
                        &DiagnosticMessage::new(
                            0, // -1
                            DiagnosticCategory::Error,
                            "-1",
                            format!(
                                "Pre-emit ({}) and post-emit ({}) diagnostic counts do not match! This can indicate that a semantic _error_ was added by the emit resolver - such an error may not be reflected on the command line or in the editor, but may be captured in a baseline here!",
                                pre_errors.as_ref().unwrap().len(),
                                post_errors.len(),
                            ).into(),
                            None,
                        ),
                        None,
                    ).into()
                );
                add_related_info(
                    &diagnostic,
                    {
                        let mut related_info: Vec<Rc<DiagnosticRelatedInformation>> = vec![
                            Rc::new(
                                create_compiler_diagnostic(
                                    &DiagnosticMessage::new(
                                        0, // -1
                                        DiagnosticCategory::Error,
                                        "-1",
                                        "The excess diagnostics are:".into(),
                                        None,
                                    ),
                                    None,
                                ).into(),
                            ),
                        ];
                        related_info.append(
                            &mut filter(
                                &longer_errors.into_iter().map(|error| Rc::new((**error).clone().into())).collect::<Vec<_>>(),
                                |p: &Rc<DiagnosticRelatedInformation>| !some(
                                    shorter_errors.map(|shorter_errors| &**shorter_errors),
                                    Some(|p2: &Rc<Diagnostic>| compare_diagnostics(&**p, &**p2) == Comparison::EqualTo)
                                )
                            )
                        );
                        related_info
                    }
                );
                diagnostic
            });
            errors
        } else {
            post_errors
        };
        CompilationResult::new(
            host,
            compiler_options,
            Some(program),
            Some(emit_result),
            errors,
        )
    }
}
