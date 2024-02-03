pub mod compiler {
    use std::{collections::HashMap, io, ptr};

    use gc::{Finalize, Gc, Trace};
    use typescript_rust::{
        add_related_info, compare_diagnostics, create_compiler_diagnostic, create_program,
        debug_cell, file_extension_is, filter, get_declaration_emit_extension_for_path,
        get_output_extension, get_pre_emit_diagnostics, id_arena::Id, is_option_str_empty, length,
        some, AllArenas, Comparison, CompilerOptions, CreateProgramOptions, Diagnostic,
        DiagnosticCategory, DiagnosticMessage, DiagnosticRelatedInformation, EmitResult, Extension,
        HasArena, InArena, ModuleKind, NewLineKind, NonEmpty, OptionTry, Program, ScriptTarget,
        SourceFileLike,
    };

    use crate::{
        collections, documents, fakes,
        vfs::{self, SortOptionsComparerFromStringComparer},
        vpath, AllArenasHarness, HasArenaHarness, InArenaHarness, SourceMapRecorder,
    };

    #[derive(Trace, Finalize)]
    pub struct CompilationOutput {
        pub inputs: Vec<Id<documents::TextDocument>>,
        pub js: Option<Id<documents::TextDocument>>,
        pub dts: Option<Id<documents::TextDocument>>,
        pub map: Option<Id<documents::TextDocument>>,
    }

    pub struct CompilationResult {
        pub host: Id<Box<dyn typescript_rust::CompilerHost /*fakes::CompilerHost*/>>,
        pub program: Option<Id<Program>>,
        pub result: Option<EmitResult>,
        pub options: Id<CompilerOptions>,
        pub diagnostics: Vec<Id<Diagnostic>>,
        pub js: collections::SortedMap<String, Id<documents::TextDocument>>,
        pub dts: collections::SortedMap<String, Id<documents::TextDocument>>,
        pub maps: collections::SortedMap<String, Id<documents::TextDocument>>,
        pub symlinks: Option<vfs::FileSet>,

        _inputs: Vec<Id<documents::TextDocument>>,
        _inputs_and_outputs: collections::SortedMap<String, Id<CompilationOutput>>,
    }

    impl CompilationResult {
        pub fn new(
            host: Id<Box<dyn typescript_rust::CompilerHost>>,
            options: Id<CompilerOptions>,
            program: Option<Id<Program>>,
            result: Option<EmitResult>,
            diagnostics: Vec<Id<Diagnostic>>,
            arena: &impl HasArenaHarness,
        ) -> io::Result<Self> {
            let host_ref = host.ref_(arena);
            let host_as_fakes_compiler_host = host_ref
                .as_dyn_any()
                .downcast_ref::<fakes::CompilerHost>()
                .unwrap();
            let options = if let Some(program) = program {
                program.ref_(arena).get_compiler_options()
            } else {
                options
            };

            let mut js = collections::SortedMap::new(
                collections::SortOptions {
                    comparer: Gc::new(Box::new(SortOptionsComparerFromStringComparer::new(
                        host_as_fakes_compiler_host.vfs().string_comparer.clone(),
                    ))),
                    sort: Some(collections::SortOptionsSort::Insertion),
                },
                Option::<HashMap<String, Id<documents::TextDocument>>>::None,
            );
            let mut dts = collections::SortedMap::new(
                collections::SortOptions {
                    comparer: Gc::new(Box::new(SortOptionsComparerFromStringComparer::new(
                        host_as_fakes_compiler_host.vfs().string_comparer.clone(),
                    ))),
                    sort: Some(collections::SortOptionsSort::Insertion),
                },
                Option::<HashMap<String, Id<documents::TextDocument>>>::None,
            );
            let mut maps = collections::SortedMap::new(
                collections::SortOptions {
                    comparer: Gc::new(Box::new(SortOptionsComparerFromStringComparer::new(
                        host_as_fakes_compiler_host.vfs().string_comparer.clone(),
                    ))),
                    sort: Some(collections::SortOptionsSort::Insertion),
                },
                Option::<HashMap<String, Id<documents::TextDocument>>>::None,
            );
            for document in &*host_as_fakes_compiler_host.outputs() {
                if vpath::is_java_script(&document.ref_(arena).file)
                    || file_extension_is(&document.ref_(arena).file, Extension::Json.to_str())
                {
                    js.set(document.ref_(arena).file.clone(), document.clone());
                } else if vpath::is_declaration(&document.ref_(arena).file) {
                    dts.set(document.ref_(arena).file.clone(), document.clone());
                } else if vpath::is_source_map(&document.ref_(arena).file) {
                    maps.set(document.ref_(arena).file.clone(), document.clone());
                }
            }
            let mut ret = Self {
                host,
                program,
                result,
                diagnostics,
                options,
                js,
                dts,
                maps,
                _inputs_and_outputs: collections::SortedMap::new(
                    collections::SortOptions {
                        comparer: Gc::new(Box::new(SortOptionsComparerFromStringComparer::new(
                            host_as_fakes_compiler_host.vfs().string_comparer.clone(),
                        ))),
                        sort: Some(collections::SortOptionsSort::Insertion),
                    },
                    Option::<HashMap<String, Id<CompilationOutput>>>::None,
                ),
                _inputs: Default::default(),
                symlinks: Default::default(),
            };
            if let Some(program) = program {
                if !is_option_str_empty(options.ref_(arena).out.as_deref())
                    || !is_option_str_empty(options.ref_(arena).out_file.as_deref())
                {
                    let options_ref = options.ref_(arena);
                    let out_file = vpath::resolve(
                        &ret.vfs().cwd()?,
                        &[Some(
                            options_ref
                                .out
                                .as_deref()
                                .filter(|options_out| !options_out.is_empty())
                                .unwrap_or_else(|| options_ref.out_file.as_deref().unwrap()),
                        )],
                    );
                    let mut inputs: Vec<Id<documents::TextDocument>> = vec![];
                    for &source_file in &*program.ref_(arena).get_source_files() {
                        // if (sourceFile) {
                        let source_file_ref = source_file.ref_(arena);
                        let source_file_as_source_file = source_file_ref.as_source_file();
                        let input = arena.alloc_text_document(documents::TextDocument::new(
                            source_file_as_source_file.file_name().clone(),
                            source_file_as_source_file.text().clone(),
                            source_file_as_source_file.text_as_chars().clone(),
                            None,
                        ));
                        ret._inputs.push(input.clone());
                        if !vpath::is_declaration(&source_file_as_source_file.file_name()) {
                            inputs.push(input);
                        }
                        // }
                    }

                    let outputs = arena.alloc_compilation_output(CompilationOutput {
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

                    if let Some(outputs_js) = outputs.ref_(arena).js.as_ref() {
                        ret._inputs_and_outputs
                            .set(outputs_js.ref_(arena).file.clone(), outputs.clone());
                    }
                    if let Some(outputs_dts) = outputs.ref_(arena).dts.as_ref() {
                        ret._inputs_and_outputs
                            .set(outputs_dts.ref_(arena).file.clone(), outputs.clone());
                    }
                    if let Some(outputs_map) = outputs.ref_(arena).map.as_ref() {
                        ret._inputs_and_outputs
                            .set(outputs_map.ref_(arena).file.clone(), outputs.clone());
                    }

                    for input in inputs {
                        ret._inputs_and_outputs
                            .set(input.ref_(arena).file.clone(), outputs.clone());
                    }
                } else {
                    for source_file in &*program.ref_(arena).get_source_files() {
                        // if (sourceFile) {
                        let source_file_ref = source_file.ref_(arena);
                        let source_file_as_source_file = source_file_ref.as_source_file();
                        let input = arena.alloc_text_document(documents::TextDocument::new(
                            source_file_as_source_file.file_name().clone(),
                            source_file_as_source_file.text().clone(),
                            source_file_as_source_file.text_as_chars().clone(),
                            None,
                        ));
                        ret._inputs.push(input.clone());
                        if !vpath::is_declaration(&source_file_as_source_file.file_name()) {
                            let extname = get_output_extension(
                                &source_file_as_source_file.file_name(),
                                &ret.options.ref_(arena),
                            );
                            let outputs = arena.alloc_compilation_output(CompilationOutput {
                                inputs: vec![input],
                                js: ret
                                    .js
                                    .get(&ret.get_output_path(
                                        &source_file_as_source_file.file_name(),
                                        extname.to_str(),
                                    )?)
                                    .cloned(),
                                dts: ret
                                    .dts
                                    .get(&ret.get_output_path(
                                        &source_file_as_source_file.file_name(),
                                        get_declaration_emit_extension_for_path(
                                            &source_file_as_source_file.file_name(),
                                        ),
                                    )?)
                                    .cloned(),
                                map: ret
                                    .maps
                                    .get(&ret.get_output_path(
                                        &source_file_as_source_file.file_name(),
                                        &format!("{}.map", extname.to_str()),
                                    )?)
                                    .cloned(),
                            });

                            ret._inputs_and_outputs.set(
                                source_file_as_source_file.file_name().clone(),
                                outputs.clone(),
                            );
                            if let Some(outputs_js) = outputs.ref_(arena).js.as_ref() {
                                ret._inputs_and_outputs
                                    .set(outputs_js.ref_(arena).file.clone(), outputs.clone());
                            }
                            if let Some(outputs_dts) = outputs.ref_(arena).dts.as_ref() {
                                ret._inputs_and_outputs
                                    .set(outputs_dts.ref_(arena).file.clone(), outputs.clone());
                            }
                            if let Some(outputs_map) = outputs.ref_(arena).map.as_ref() {
                                ret._inputs_and_outputs
                                    .set(outputs_map.ref_(arena).file.clone(), outputs.clone());
                            }
                        }
                        // }
                    }
                }
            }
            Ok(ret)
        }

        fn host_as_fakes_compiler_host(&self) -> debug_cell::Ref<fakes::CompilerHost> {
            debug_cell::Ref::map(self.host.ref_(self), |host| {
                host.as_dyn_any()
                    .downcast_ref::<fakes::CompilerHost>()
                    .unwrap()
            })
        }

        pub fn vfs_id(&self) -> Id<vfs::FileSystem> {
            self.host_as_fakes_compiler_host().vfs_id()
        }

        pub fn vfs(&self) -> debug_cell::Ref<vfs::FileSystem> {
            self.vfs_id().ref_(self)
        }

        pub fn traces(&self) -> Vec<String> {
            self.host_as_fakes_compiler_host().traces().clone()
        }

        pub fn common_source_directory(&self) -> io::Result<String> {
            let common = self
                .program
                .map(|program| program.ref_(self).get_common_source_directory())
                .unwrap_or_else(|| "".to_owned());
            Ok(if !common.is_empty() {
                vpath::combine(&self.vfs().cwd()?, &[Some(&common)])
            } else {
                common
            })
        }

        pub fn get_source_map_record(&self) -> Option<String> {
            let maps = self.result.as_ref().unwrap().source_maps.as_ref();
            maps.non_empty().map(|maps| {
                SourceMapRecorder::get_source_map_record(
                    maps,
                    &self.program.unwrap().ref_(self),
                    &self
                        .js
                        .values()
                        .filter(|d| {
                            !file_extension_is(&d.ref_(self).file, Extension::Json.to_str())
                        })
                        .cloned()
                        .collect::<Vec<_>>(),
                    &self.dts.values().cloned().collect::<Vec<_>>(),
                    self,
                )
            })
        }

        pub fn get_output_path(&self, path: &str, ext: &str) -> io::Result<String> {
            let mut path = path.to_owned();
            if !is_option_str_empty(self.options.ref_(self).out_file.as_deref())
                || !is_option_str_empty(self.options.ref_(self).out.as_deref())
            {
                let options_ref = self.options.ref_(self);
                path = vpath::resolve(
                    &self.vfs().cwd()?,
                    &[Some(
                        options_ref
                            .out_file
                            .as_deref()
                            .filter(|options_out_file| !options_out_file.is_empty())
                            .unwrap_or_else(|| options_ref.out.as_deref().unwrap()),
                    )],
                );
            } else {
                path = vpath::resolve(&self.vfs().cwd()?, &[Some(&path)]);
                let options_ref = self.options.ref_(self);
                let out_dir = if matches!(ext, ".d.ts" | ".json.d.ts" | ".d.mts" | ".d.cts") {
                    options_ref
                        .declaration_dir
                        .as_deref()
                        .filter(|options_declaration_dir| !options_declaration_dir.is_empty())
                        .or_else(|| options_ref.out_dir.as_deref())
                } else {
                    options_ref.out_dir.as_deref()
                };
                if out_dir.is_non_empty() {
                    let common = self.common_source_directory()?;
                    if !common.is_empty() {
                        path = vpath::relative(
                            &common,
                            &path,
                            Option::<fn(&str) -> String>::None,
                            Some(self.vfs().ignore_case),
                        );
                        path = vpath::combine(
                            &vpath::resolve(
                                &self.vfs().cwd()?,
                                &[self.options.ref_(self).out_dir.as_deref()],
                            ),
                            &[Some(&path)],
                        );
                    }
                }
            }
            Ok(vpath::change_extension(
                &path,
                ext,
                Option::<&[&str]>::None,
                None,
            ))
        }

        pub fn get_number_of_js_files(&self, include_json: bool) -> usize {
            if include_json {
                self.js.len()
            } else {
                let mut count = self.js.len();
                self.js.for_each(|document, _, _| {
                    if file_extension_is(&document.ref_(self).file, Extension::Json.to_str()) {
                        count -= 1;
                    }
                });
                count
            }
        }
    }

    impl HasArena for CompilationResult {
        fn arena(&self) -> &AllArenas {
            unimplemented!()
        }
    }

    impl HasArenaHarness for CompilationResult {
        fn arena_harness(&self) -> &AllArenasHarness {
            unimplemented!()
        }
    }

    pub fn compile_files(
        host: Id<Box<dyn typescript_rust::CompilerHost>>,
        root_files: Option<&[String]>,
        compiler_options: &CompilerOptions,
        arena: &impl HasArenaHarness,
    ) -> io::Result<CompilationResult> {
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
            Some(create_program(
                CreateProgramOptions {
                    root_names: root_files.map_or_else(|| vec![], ToOwned::to_owned),
                    options: {
                        let mut options = compiler_options.clone();
                        options.trace_resolution = Some(false);
                        arena.alloc_compiler_options(options)
                    },
                    host: Some(host),
                    project_references: None,
                    old_program: None,
                    config_file_parsing_diagnostics: None,
                },
                arena,
            )?)
        } else {
            None
        };
        let pre_errors = pre_program.clone().try_map(|pre_program| {
            get_pre_emit_diagnostics(&pre_program.into(), None, None, arena)
        })?;

        let compiler_options = arena.alloc_compiler_options(compiler_options);
        let program = create_program(
            CreateProgramOptions {
                root_names: root_files.map_or_else(|| vec![], ToOwned::to_owned),
                options: compiler_options.clone(),
                host: Some(host),
                project_references: None,
                old_program: None,
                config_file_parsing_diagnostics: None,
            },
            arena,
        )?;

        let emit_result = program
            .ref_(arena)
            .emit(None, None, None, None, None, None)?;
        let post_errors = get_pre_emit_diagnostics(&program.clone().into(), None, None, arena)?;
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
                let diagnostic: Id<Diagnostic> = arena.alloc_diagnostic(
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
                    &diagnostic.ref_(arena),
                    {
                        let mut related_info: Vec<Id<DiagnosticRelatedInformation>> = vec![
                            arena.alloc_diagnostic_related_information(
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
                                &longer_errors.into_iter().map(|error| arena.alloc_diagnostic_related_information(error.ref_(arena).clone().into())).collect::<Vec<_>>(),
                                |p: &Id<DiagnosticRelatedInformation>| !some(
                                    shorter_errors.map(|shorter_errors| &**shorter_errors),
                                    Some(|p2: &Id<Diagnostic>| compare_diagnostics(&*p.ref_(arena), &*p2.ref_(arena), arena) == Comparison::EqualTo)
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
            arena,
        )
    }
}
