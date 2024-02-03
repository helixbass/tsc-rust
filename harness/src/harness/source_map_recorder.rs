pub mod SourceMapRecorder {
    use gc::Gc;
    use typescript_rust::{
        Node, Program, SourceMapEmitResult, _d, compute_line_starts, decode_mappings, ends_with,
        get_line_info, id_arena::Id, is_source_mapping, try_get_source_mapping_url, BoolExt,
        Debug_, Extension, HasArena, InArena, Mapping, MappingsDecoder, RawSourceMap,
        ScriptReferenceHost, SourceFileLike, SourceTextAsChars,
    };

    use crate::{documents, Compiler, Utils};

    struct SourceMapSpanWithDecodeErrors {
        pub source_map_span: Mapping,
        pub decode_errors: Option<Vec<String>>,
    }

    struct SourceMapDecoder {
        decoding_index: usize,
        source_map_mappings: String,
        mappings: Option<MappingsDecoder>,
    }

    impl SourceMapDecoder {
        pub fn initialize_source_map_decoding(source_map: &RawSourceMap) -> Self {
            Self {
                decoding_index: 0,
                source_map_mappings: source_map.mappings.clone(),
                mappings: Some(decode_mappings(&source_map.mappings)),
            }
        }

        pub fn has_completed_decoding(&self) -> bool {
            if self.mappings.is_none() {
                Debug_.fail(Some("not initialized"));
            }
            self.mappings.as_ref().unwrap().pos == self.source_map_mappings.len()
        }

        pub fn get_remaining_decode_string(&self) -> &str {
            &self.source_map_mappings[self.decoding_index..]
        }
    }

    struct SourceMapSpanWriter<'a> {
        source_map_recorder: &'a mut Compiler::WriterAggregator,
        source_map_sources: &'a [String],
        source_map_names: Option<&'a [String]>,
        js_file: Gc<documents::TextDocument>,
        js_line_map: Vec<usize>,
        ts_code: Option<SourceTextAsChars>,
        ts_line_map: Option<Vec<usize>>,
        spans_on_single_line: Vec<SourceMapSpanWithDecodeErrors>,
        prev_written_source_pos: usize,
        next_js_line_to_write: usize,
        span_marker_continues: bool,
        source_map_decoder: SourceMapDecoder,
    }

    impl<'a> SourceMapSpanWriter<'a> {
        pub fn initialize_source_map_span_writer(
            source_map_record_writer: &'a mut Compiler::WriterAggregator,
            source_map: &'a RawSourceMap,
            current_js_file: Gc<documents::TextDocument>,
        ) -> Self {
            let mut ret = Self {
                source_map_recorder: source_map_record_writer,
                source_map_sources: &source_map.sources,
                source_map_names: source_map.names.as_deref(),
                js_file: current_js_file.clone(),
                js_line_map: current_js_file.line_starts().clone(),
                ts_code: _d(),
                ts_line_map: _d(),
                spans_on_single_line: _d(),
                prev_written_source_pos: 0,
                next_js_line_to_write: 0,
                span_marker_continues: false,
                source_map_decoder: SourceMapDecoder::initialize_source_map_decoding(source_map),
            };

            ret.source_map_recorder
                .write_line("===================================================================");
            ret.source_map_recorder
                .write_line(&format!("JsFile: {}", source_map.file));
            ret.source_map_recorder.write_line(&format!(
                "mapUrl: {}",
                try_get_source_mapping_url(&get_line_info(
                    ret.js_file.text_as_chars.clone(),
                    ret.js_line_map.clone()
                ))
                .as_deref()
                .unwrap_or("undefined")
            ));
            ret.source_map_recorder.write_line(&format!(
                "sourceRoot: {}",
                source_map.source_root.as_deref().unwrap_or("undefined")
            ));
            ret.source_map_recorder
                .write_line(&format!("sources: {}", source_map.sources.join(",")));
            if let Some(sources_content) = source_map.sources_content.as_ref() {
                ret.source_map_recorder.write_line(&format!(
                    "sourcesContent: {}",
                    serde_json::to_string(sources_content).unwrap()
                ));
            }
            ret.source_map_recorder
                .write_line("===================================================================");
            ret
        }

        pub fn record_source_map_span(&mut self, source_map_span: &Mapping) {
            unimplemented!()
        }

        pub fn record_new_source_file_span(
            &mut self,
            source_map_span: &Mapping,
            new_source_file_code: SourceTextAsChars,
        ) {
            let mut continues_line = false;
            if !self.spans_on_single_line.is_empty()
                && self.spans_on_single_line[0]
                    .source_map_span
                    .generated_character
                    == source_map_span.generated_line
            {
                self.write_recorded_spans();
                self.spans_on_single_line = _d();
                self.next_js_line_to_write -= 1;
                continues_line = true;
            }

            self.record_source_map_span(source_map_span);

            assert!(self.spans_on_single_line.len() == 1);
            self.source_map_recorder
                .write_line("-------------------------------------------------------------------");
            self.source_map_recorder.write_line(&format!(
                "emittedFile:{}{}",
                self.js_file.file,
                if continues_line {
                    format!(
                        " ({}, {})",
                        source_map_span.generated_line + 1,
                        source_map_span.generated_character + 1,
                    )
                } else {
                    "".to_owned()
                },
            ));
            self.source_map_recorder.write_line(&format!(
                "sourceFile:{}",
                self.source_map_sources[self.spans_on_single_line[0]
                    .source_map_span
                    .source_index
                    .unwrap()]
            ));
            self.source_map_recorder
                .write_line("-------------------------------------------------------------------");

            self.ts_line_map = Some(compute_line_starts(&new_source_file_code));
            self.ts_code = Some(new_source_file_code);
            self.prev_written_source_pos = 0;
        }

        pub fn close(mut self) {
            self.write_recorded_spans();

            if !self.source_map_decoder.has_completed_decoding() {
                self.source_map_recorder.write_line("!!!! **** There are more source map entries in the sourceMap's mapping than what was encoded");
                self.source_map_recorder.write_line(&format!(
                    "!!!! **** Remaining decoded string: {}",
                    self.source_map_decoder.get_remaining_decode_string()
                ));
            }

            self.write_js_file_lines(self.js_line_map.len());
        }

        pub fn get_text_of_line(
            &self,
            line: usize,
            line_map: &[usize],
            code: &SourceTextAsChars,
        ) -> String {
            let start_pos = line_map[line];
            let end_pos = line_map.get(line + 1).copied();
            let text: String = match end_pos {
                None => &code[start_pos..],
                Some(end_pos) => &code[start_pos..end_pos],
            }
            .into_iter()
            .collect();
            if line == 0 {
                Utils::remove_byte_order_mark(text)
            } else {
                text
            }
        }

        pub fn write_js_file_lines(&mut self, end_js_line: usize) {
            while self.next_js_line_to_write < end_js_line {
                self.source_map_recorder.write(&format!(
                    ">>>{}",
                    self.get_text_of_line(
                        self.next_js_line_to_write,
                        &self.js_line_map,
                        &self.js_file.text_as_chars
                    )
                ));
                self.next_js_line_to_write += 1;
            }
        }

        pub fn write_recorded_spans(&mut self) {
            unimplemented!()
        }
    }

    pub fn get_source_map_record(
        source_map_data_list: &[SourceMapEmitResult],
        program: &Program,
        js_files: &[Gc<documents::TextDocument>],
        declaration_files: &[Gc<documents::TextDocument>],
        arena: &impl HasArena,
    ) -> String {
        let mut source_map_recorder = Compiler::WriterAggregator::default();

        for (i, source_map_data) in source_map_data_list.into_iter().enumerate() {
            let mut prev_source_file: Option<Id<Node /*SourceFile*/>> = _d();
            let current_file: Gc<documents::TextDocument>;
            if ends_with(&source_map_data.source_map.file, Extension::Dts.to_str()) {
                if source_map_data_list.len() > js_files.len() {
                    current_file = declaration_files[i / 2].clone();
                } else {
                    current_file = declaration_files[i].clone();
                }
            } else {
                if source_map_data_list.len() > js_files.len() {
                    current_file = js_files[i / 2].clone();
                } else {
                    current_file = js_files[i].clone();
                }
            }

            let mut source_map_span_writer = SourceMapSpanWriter::initialize_source_map_span_writer(
                &mut source_map_recorder,
                &source_map_data.source_map,
                current_file,
            );
            let mapper = decode_mappings(&source_map_data.source_map.mappings);
            for decoded_source_mapping in mapper {
                let current_source_file =
                    is_source_mapping(&decoded_source_mapping).then_and(|| {
                        program.get_source_file(
                            &source_map_data.input_source_file_names
                                [decoded_source_mapping.source_index.unwrap()],
                        )
                    });
                if current_source_file != prev_source_file {
                    if let Some(current_source_file) = current_source_file.as_ref() {
                        source_map_span_writer.record_new_source_file_span(
                            &decoded_source_mapping,
                            current_source_file
                                .ref_(arena)
                                .as_source_file()
                                .text_as_chars()
                                .clone(),
                        );
                    }
                    prev_source_file = current_source_file;
                } else {
                    source_map_span_writer.record_source_map_span(&decoded_source_mapping);
                }
            }
            source_map_span_writer.close();
        }
        source_map_recorder.close();
        source_map_recorder.lines.join("\r\n")
    }
}
