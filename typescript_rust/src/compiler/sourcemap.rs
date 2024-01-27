use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashMap,
};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use crate::{
    combine_paths, get_directory_path, get_relative_path_to_directory_or_url, CharacterCodes,
    Debug_, EmitHost, GetOrInsertDefault, LineAndCharacter, NonEmpty, RawSourceMap,
    ScriptReferenceHost, SourceMapGenerator, SourceMapOptions, SourceTextAsChars,
    HasArena, InArena, AllArenas,
};

pub struct SourceMapGeneratorOptions {
    #[allow(dead_code)]
    extended_diagnostics: Option<bool>,
}

impl From<&SourceMapOptions> for SourceMapGeneratorOptions {
    fn from(value: &SourceMapOptions) -> Self {
        Self {
            extended_diagnostics: value.extended_diagnostics,
        }
    }
}

pub fn create_source_map_generator(
    host: Id<Box<dyn EmitHost>>,
    file: String,
    source_root: String,
    sources_directory_path: String,
    _generator_options: &SourceMapGeneratorOptions,
    arena: &impl HasArena,
) -> Id<Box<dyn SourceMapGenerator>> {
    SourceMapGeneratorConcrete::new(sources_directory_path, file, source_root, host, arena)
}

#[derive(Trace, Finalize)]
struct SourceMapGeneratorConcrete {
    sources_directory_path: String,
    file: String,
    source_root: String,
    host: Id<Box<dyn EmitHost>>,
    #[unsafe_ignore_trace]
    raw_sources: RefCell<Vec<String>>,
    #[unsafe_ignore_trace]
    sources: RefCell<Vec<String>>,
    #[unsafe_ignore_trace]
    source_to_source_index_map: RefCell<HashMap<String, usize>>,
    #[unsafe_ignore_trace]
    sources_content: RefCell<Option<Vec<Option<String>>>>,
    #[unsafe_ignore_trace]
    names: RefCell<Vec<String>>,
    #[unsafe_ignore_trace]
    name_to_name_index_map: RefCell<Option<HashMap<String, usize>>>,
    #[unsafe_ignore_trace]
    mapping_char_codes: RefCell<Vec<char>>,
    #[unsafe_ignore_trace]
    mappings: RefCell<String>,
    #[unsafe_ignore_trace]
    last_generated_line: Cell<usize>,
    #[unsafe_ignore_trace]
    last_generated_character: Cell<usize>,
    #[unsafe_ignore_trace]
    last_source_index: Cell<usize>,
    #[unsafe_ignore_trace]
    last_source_line: Cell<usize>,
    #[unsafe_ignore_trace]
    last_source_character: Cell<usize>,
    #[unsafe_ignore_trace]
    last_name_index: Cell<usize>,
    #[unsafe_ignore_trace]
    has_last: Cell<bool>,
    #[unsafe_ignore_trace]
    pending_generated_line: Cell<usize>,
    #[unsafe_ignore_trace]
    pending_generated_character: Cell<usize>,
    #[unsafe_ignore_trace]
    pending_source_index: Cell<usize>,
    #[unsafe_ignore_trace]
    pending_source_line: Cell<usize>,
    #[unsafe_ignore_trace]
    pending_source_character: Cell<usize>,
    #[unsafe_ignore_trace]
    pending_name_index: Cell<usize>,
    #[unsafe_ignore_trace]
    has_pending: Cell<bool>,
    #[unsafe_ignore_trace]
    has_pending_source: Cell<bool>,
    #[unsafe_ignore_trace]
    has_pending_name: Cell<bool>,
}

impl SourceMapGeneratorConcrete {
    pub fn new(
        sources_directory_path: String,
        file: String,
        source_root: String,
        host: Id<Box<dyn EmitHost>>,
        arena: &impl HasArena,
    ) -> Id<Box<dyn SourceMapGenerator>> {
        // const { enter, exit } = generatorOptions.extendedDiagnostics
        //     ? performance.createTimer("Source Map", "beforeSourcemap", "afterSourcemap")
        //     : performance.nullTimer;
        arena.alloc_source_map_generator(Box::new(Self {
            sources_directory_path,
            file,
            source_root,
            host,
            raw_sources: Default::default(),
            sources: Default::default(),
            source_to_source_index_map: Default::default(),
            sources_content: Default::default(),
            names: Default::default(),
            name_to_name_index_map: Default::default(),
            mapping_char_codes: Default::default(),
            mappings: Default::default(),
            last_generated_line: Default::default(),
            last_generated_character: Default::default(),
            last_source_index: Default::default(),
            last_source_line: Default::default(),
            last_source_character: Default::default(),
            last_name_index: Default::default(),
            has_last: Default::default(),
            pending_generated_line: Default::default(),
            pending_generated_character: Default::default(),
            pending_source_index: Default::default(),
            pending_source_line: Default::default(),
            pending_source_character: Default::default(),
            pending_name_index: Default::default(),
            has_pending: Default::default(),
            has_pending_source: Default::default(),
            has_pending_name: Default::default(),
        }))
    }

    pub fn raw_sources(&self) -> Ref<Vec<String>> {
        self.raw_sources.borrow()
    }

    pub fn raw_sources_mut(&self) -> RefMut<Vec<String>> {
        self.raw_sources.borrow_mut()
    }

    pub fn sources(&self) -> Ref<Vec<String>> {
        self.sources.borrow()
    }

    pub fn sources_mut(&self) -> RefMut<Vec<String>> {
        self.sources.borrow_mut()
    }

    pub fn source_to_source_index_map(&self) -> Ref<HashMap<String, usize>> {
        self.source_to_source_index_map.borrow()
    }

    pub fn source_to_source_index_map_mut(&self) -> RefMut<HashMap<String, usize>> {
        self.source_to_source_index_map.borrow_mut()
    }

    pub fn maybe_sources_content(&self) -> Ref<Option<Vec<Option<String>>>> {
        self.sources_content.borrow()
    }

    pub fn maybe_sources_content_mut(&self) -> RefMut<Option<Vec<Option<String>>>> {
        self.sources_content.borrow_mut()
    }

    pub fn names(&self) -> Ref<Vec<String>> {
        self.names.borrow()
    }

    pub fn names_mut(&self) -> RefMut<Vec<String>> {
        self.names.borrow_mut()
    }

    pub fn maybe_name_to_name_index_map_mut(&self) -> RefMut<Option<HashMap<String, usize>>> {
        self.name_to_name_index_map.borrow_mut()
    }

    pub fn mapping_char_codes(&self) -> Ref<Vec<char>> {
        self.mapping_char_codes.borrow()
    }

    pub fn mapping_char_codes_mut(&self) -> RefMut<Vec<char>> {
        self.mapping_char_codes.borrow_mut()
    }

    pub fn mappings(&self) -> Ref<String> {
        self.mappings.borrow()
    }

    pub fn mappings_mut(&self) -> RefMut<String> {
        self.mappings.borrow_mut()
    }

    pub fn last_generated_line(&self) -> usize {
        self.last_generated_line.get()
    }

    pub fn set_last_generated_line(&self, last_generated_line: usize) {
        self.last_generated_line.set(last_generated_line);
    }

    pub fn last_generated_character(&self) -> usize {
        self.last_generated_character.get()
    }

    pub fn set_last_generated_character(&self, last_generated_character: usize) {
        self.last_generated_character.set(last_generated_character);
    }

    pub fn last_source_index(&self) -> usize {
        self.last_source_index.get()
    }

    pub fn set_last_source_index(&self, last_source_index: usize) {
        self.last_source_index.set(last_source_index);
    }

    pub fn last_source_line(&self) -> usize {
        self.last_source_line.get()
    }

    pub fn set_last_source_line(&self, last_source_line: usize) {
        self.last_source_line.set(last_source_line);
    }

    pub fn last_source_character(&self) -> usize {
        self.last_source_character.get()
    }

    pub fn set_last_source_character(&self, last_source_character: usize) {
        self.last_source_character.set(last_source_character);
    }

    pub fn last_name_index(&self) -> usize {
        self.last_name_index.get()
    }

    pub fn set_last_name_index(&self, last_name_index: usize) {
        self.last_name_index.set(last_name_index);
    }

    pub fn has_last(&self) -> bool {
        self.has_last.get()
    }

    pub fn set_has_last(&self, has_last: bool) {
        self.has_last.set(has_last);
    }

    pub fn pending_generated_line(&self) -> usize {
        self.pending_generated_line.get()
    }

    pub fn set_pending_generated_line(&self, pending_generated_line: usize) {
        self.pending_generated_line.set(pending_generated_line);
    }

    pub fn pending_generated_character(&self) -> usize {
        self.pending_generated_character.get()
    }

    pub fn set_pending_generated_character(&self, pending_generated_character: usize) {
        self.pending_generated_character
            .set(pending_generated_character);
    }

    pub fn pending_source_index(&self) -> usize {
        self.pending_source_index.get()
    }

    pub fn set_pending_source_index(&self, pending_source_index: usize) {
        self.pending_source_index.set(pending_source_index);
    }

    pub fn pending_source_line(&self) -> usize {
        self.pending_source_line.get()
    }

    pub fn set_pending_source_line(&self, pending_source_line: usize) {
        self.pending_source_line.set(pending_source_line);
    }

    pub fn pending_source_character(&self) -> usize {
        self.pending_source_character.get()
    }

    pub fn set_pending_source_character(&self, pending_source_character: usize) {
        self.pending_source_character.set(pending_source_character);
    }

    pub fn pending_name_index(&self) -> usize {
        self.pending_name_index.get()
    }

    pub fn set_pending_name_index(&self, pending_name_index: usize) {
        self.pending_name_index.set(pending_name_index);
    }

    pub fn has_pending(&self) -> bool {
        self.has_pending.get()
    }

    pub fn set_has_pending(&self, has_pending: bool) {
        self.has_pending.set(has_pending);
    }

    pub fn has_pending_source(&self) -> bool {
        self.has_pending_source.get()
    }

    pub fn set_has_pending_source(&self, has_pending_source: bool) {
        self.has_pending_source.set(has_pending_source);
    }

    pub fn has_pending_name(&self) -> bool {
        self.has_pending_name.get()
    }

    pub fn set_has_pending_name(&self, has_pending_name: bool) {
        self.has_pending_name.set(has_pending_name);
    }

    fn is_new_generated_position(&self, generated_line: usize, generated_character: usize) -> bool {
        !self.has_pending()
            || self.pending_generated_line() != generated_line
            || self.pending_generated_character() != generated_character
    }

    fn is_backtracking_source_position(
        &self,
        source_index: Option<usize>,
        source_line: Option<usize>,
        source_character: Option<usize>,
    ) -> bool {
        matches!(
            (
                source_index,
                source_line,
                source_character,
            ),
            (
                Some(source_index),
                Some(source_line),
                Some(source_character),
            ) if self.pending_source_index() == source_index && (
                self.pending_source_line() > source_line ||
                    self.pending_source_line() == source_line &&
                    self.pending_source_character() > source_character
            )
        )
    }

    fn add_mapping_(
        &self,
        generated_line: usize,
        generated_character: usize,
        source_index: Option<usize>,
        source_line: Option<usize>,
        source_character: Option<usize>,
        name_index: Option<usize>,
    ) {
        Debug_.assert(
            generated_line >= self.pending_generated_line(),
            Some("generatedLine cannot backtrack"),
        );
        // Debug_.assert(
        //     generated_character >= 0,
        //     Some("generatedCharacter cannot be negative"),
        // );
        // Debug_.assert(
        //     match source_index {
        //         None => true,
        //         Some(source_index) => source_index >= 0,
        //     },
        //     Some("sourceIndex cannot be negative"),
        // );
        // Debug_.assert(
        //     match source_line {
        //         None => true,
        //         Some(source_line) => source_line >= 0,
        //     },
        //     Some("sourceLine cannot be negative"),
        // );
        // Debug_.assert(
        //     match source_character {
        //         None => true,
        //         Some(source_character) => source_character >= 0,
        //     },
        //     Some("sourceCharacter cannot be negative"),
        // );
        // enter();
        if self.is_new_generated_position(generated_line, generated_character)
            || self.is_backtracking_source_position(source_index, source_line, source_character)
        {
            self.commit_pending_mapping();
            self.set_pending_generated_line(generated_line);
            self.set_pending_generated_character(generated_character);
            self.set_has_pending_source(false);
            self.set_has_pending_name(false);
            self.set_has_pending(true);
        }

        if let (Some(source_index), Some(source_line), Some(source_character)) =
            (source_index, source_line, source_character)
        {
            self.set_pending_source_index(source_index);
            self.set_pending_source_line(source_line);
            self.set_pending_source_character(source_character);
            self.set_has_pending_source(true);
            if let Some(name_index) = name_index {
                self.set_pending_name_index(name_index);
                self.set_has_pending_name(true);
            }
        }
        // exit();
    }

    fn should_commit_mapping(&self) -> bool {
        !self.has_last()
            || self.last_generated_line() != self.pending_generated_line()
            || self.last_generated_character() != self.pending_generated_character()
            || self.last_source_index() != self.pending_source_index()
            || self.last_source_line() != self.pending_source_line()
            || self.last_source_character() != self.pending_source_character()
            || self.last_name_index() != self.pending_name_index()
    }

    fn append_mapping_char_code(&self, char_code: char) {
        self.mapping_char_codes_mut().push(char_code);
        if self.mapping_char_codes().len() >= 1024 {
            self.flush_mapping_buffer();
        }
    }

    fn commit_pending_mapping(&self) {
        if !self.has_pending() || !self.should_commit_mapping() {
            return;
        }

        // enter();

        if self.last_generated_line() < self.pending_generated_line() {
            while {
                self.append_mapping_char_code(CharacterCodes::semicolon);
                self.set_last_generated_line(self.last_generated_line() + 1);
                self.last_generated_line() < self.pending_generated_line()
            } {}
            self.set_last_generated_character(0);
        } else {
            Debug_.assert_equal(
                &self.last_generated_line(),
                &self.pending_generated_line(),
                Some("generatedLine cannot backtrack"),
                None,
            );
            if self.has_last() {
                self.append_mapping_char_code(CharacterCodes::comma);
            }
        }

        self.append_base64_vlq(
            isize::try_from(self.pending_generated_character()).unwrap()
                - isize::try_from(self.last_generated_character()).unwrap(),
        );
        self.set_last_generated_character(self.pending_generated_character());

        if self.has_pending_source() {
            self.append_base64_vlq(
                isize::try_from(self.pending_source_index()).unwrap()
                    - isize::try_from(self.last_source_index()).unwrap(),
            );
            self.set_last_source_index(self.pending_source_index());

            self.append_base64_vlq(
                isize::try_from(self.pending_source_line()).unwrap()
                    - isize::try_from(self.last_source_line()).unwrap(),
            );
            self.set_last_source_line(self.pending_source_line());

            self.append_base64_vlq(
                isize::try_from(self.pending_source_character()).unwrap()
                    - isize::try_from(self.last_source_character()).unwrap(),
            );
            self.set_last_source_character(self.pending_source_character());

            if self.has_pending_name() {
                self.append_base64_vlq(
                    isize::try_from(self.pending_name_index()).unwrap()
                        - isize::try_from(self.last_name_index()).unwrap(),
                );
                self.set_last_name_index(self.pending_name_index());
            }
        }

        self.set_has_last(true);
        // exit();
    }

    fn flush_mapping_buffer(&self) {
        if !self.mapping_char_codes().is_empty() {
            self.mappings_mut().extend(self.mapping_char_codes().iter());
            self.mapping_char_codes_mut().clear();
        }
    }

    fn append_base64_vlq(&self, mut in_value: isize) {
        if in_value < 0 {
            in_value = ((-in_value) << 1) + 1;
        } else {
            in_value = in_value << 1;
        }

        while {
            let mut current_digit = in_value & 31;
            in_value = in_value >> 5;
            if in_value > 0 {
                current_digit = current_digit | 32;
            }
            self.append_mapping_char_code(base64_format_encode(current_digit));
            in_value > 0
        } {}
    }
}

impl SourceMapGenerator for SourceMapGeneratorConcrete {
    fn get_sources(&self) -> Vec<String> {
        self.raw_sources().clone()
    }

    fn to_string(&self) -> String {
        serde_json::to_string(&self.to_json()).unwrap()
    }

    fn add_source(&self, file_name: &str) -> usize {
        // enter();
        let source = get_relative_path_to_directory_or_url(
            &self.sources_directory_path,
            file_name,
            &ScriptReferenceHost::get_current_directory(&**self.host.ref_(self)),
            |file_name: &str| self.host.ref_(self).get_canonical_file_name(file_name),
            true,
        );

        let mut source_index = self.source_to_source_index_map().get(&source).copied();
        if source_index.is_none() {
            source_index = Some(self.sources().len());
            self.sources_mut().push(source.clone());
            self.raw_sources_mut().push(file_name.to_owned());
            self.source_to_source_index_map_mut()
                .insert(source, source_index.unwrap());
        }
        // exit();
        source_index.unwrap()
    }

    fn set_source_content(&self, source_index: usize, content: Option<String>) {
        // enter();
        if let Some(content) = content {
            let mut sources_content = self.maybe_sources_content_mut();
            let sources_content = sources_content.get_or_insert_default_();
            while sources_content.len() < source_index {
                sources_content.push(None);
            }
            if sources_content.len() == source_index {
                sources_content.push(Some(content));
            } else {
                sources_content[source_index] = Some(content);
            }
        }
        // exit();
    }

    fn add_name(&self, name: &str) -> usize {
        // enter();
        let mut name_to_name_index_map = self.maybe_name_to_name_index_map_mut();
        let name_to_name_index_map = name_to_name_index_map.get_or_insert_default_();
        let mut name_index = name_to_name_index_map.get(name).copied();
        if name_index.is_none() {
            name_index = Some(self.names().len());
            self.names_mut().push(name.to_owned());
            name_to_name_index_map.insert(name.to_owned(), name_index.unwrap());
        }
        // exit();
        name_index.unwrap()
    }

    fn add_mapping(
        &self,
        generated_line: usize,
        generated_character: usize,
        source_index: usize,
        source_line: usize,
        source_character: usize,
        name_index: Option<usize>,
    ) {
        self.add_mapping_(
            generated_line,
            generated_character,
            Some(source_index),
            Some(source_line),
            Some(source_character),
            name_index,
        )
    }

    fn append_source_map(
        &self,
        generated_line: usize,
        generated_character: usize,
        map: &RawSourceMap,
        source_map_path: &str,
        start: Option<LineAndCharacter>,
        end: Option<LineAndCharacter>,
    ) {
        Debug_.assert(
            generated_line >= self.pending_generated_line(),
            Some("generatedLine cannot backtrack"),
        );
        // Debug_.assert(
        //     generated_character >= 0,
        //     Some("generatedLine cannot be negative"),
        // );
        // enter();
        let mut source_index_to_new_source_index_map: HashMap<usize, usize> = Default::default();
        let mut name_index_to_new_name_index_map: Option<HashMap<usize, usize>> =
            Default::default();
        let mapping_iterator = decode_mappings(&map.mappings);
        for raw in mapping_iterator {
            if matches!(
                end,
                Some(end) if raw.generated_line > end.line ||
                    raw.generated_line == end.line && raw.generated_character > end.character
            ) {
                break;
            }

            if matches!(
                start,
                Some(start) if raw.generated_line < start.line ||
                    start.line == raw.generated_line && raw.generated_character < start.character
            ) {
                continue;
            }
            let mut new_source_index: Option<usize> = Default::default();
            let mut new_source_line: Option<usize> = Default::default();
            let mut new_source_character: Option<usize> = Default::default();
            let mut new_name_index: Option<usize> = Default::default();
            if let Some(raw_source_index) = raw.source_index {
                new_source_index = source_index_to_new_source_index_map
                    .get(&raw_source_index)
                    .copied();
                if new_source_index.is_none() {
                    let raw_path = &map.sources[raw_source_index];
                    let relative_path =
                        if let Some(map_source_root) = map.source_root.as_ref().non_empty() {
                            combine_paths(map_source_root, &[Some(raw_path)])
                        } else {
                            raw_path.clone()
                        };
                    let combined_path = combine_paths(
                        &get_directory_path(source_map_path),
                        &[Some(&relative_path)],
                    );
                    new_source_index = Some(self.add_source(&combined_path));
                    source_index_to_new_source_index_map
                        .insert(raw_source_index, new_source_index.unwrap());
                    if let Some(map_sources_content) = map.sources_content.as_ref() {
                        if let Some(map_sources_content_raw_source_index) = map_sources_content
                            .get(raw_source_index)
                            .map(Option::as_ref)
                            .flatten()
                        {
                            self.set_source_content(
                                new_source_index.unwrap(),
                                Some(map_sources_content_raw_source_index.clone()),
                            );
                        }
                    }
                }

                new_source_line = raw.source_line;
                new_source_character = raw.source_character;
                if let (Some(map_names), Some(raw_name_index)) =
                    (map.names.as_ref(), raw.name_index)
                {
                    let name_index_to_new_name_index_map =
                        name_index_to_new_name_index_map.get_or_insert_default_();
                    new_name_index = name_index_to_new_name_index_map
                        .get(&raw_name_index)
                        .copied();
                    if new_name_index.is_none() {
                        new_name_index = Some(self.add_name(&map_names[raw_name_index]));
                        name_index_to_new_name_index_map
                            .insert(raw_name_index, new_name_index.unwrap());
                    }
                }
            }

            let raw_generated_line = raw.generated_line - (start.map_or(0, |start| start.line));
            let new_generated_line = raw_generated_line + generated_line;
            let raw_generated_character = start
                .filter(|start| start.line == raw.generated_line)
                .map_or(raw.generated_character, |start| {
                    raw.generated_character - start.character
                });
            let new_generated_character = if raw_generated_line == 0 {
                raw_generated_character + generated_character
            } else {
                raw_generated_character
            };
            self.add_mapping_(
                new_generated_line,
                new_generated_character,
                new_source_index,
                new_source_line,
                new_source_character,
                new_name_index,
            );
        }
        // exit();
    }

    fn to_json(&self) -> RawSourceMap {
        self.commit_pending_mapping();
        self.flush_mapping_buffer();
        RawSourceMap {
            version: 3,
            file: self.file.clone(),
            source_root: Some(self.source_root.clone()),
            sources: self.sources().clone(),
            sources_content: self.maybe_sources_content().clone(),
            mappings: self.mappings().clone(),
            names: Some(self.names().clone()),
        }
    }
}

impl HasArena for SourceMapGeneratorConcrete {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub trait LineInfo {
    fn get_line_count(&self) -> usize;
    fn get_line_text(&self, line: usize) -> String;
}

pub fn get_line_info(text: SourceTextAsChars, line_starts: Vec<usize>) -> LineInfoConcrete {
    LineInfoConcrete { text, line_starts }
}

pub struct LineInfoConcrete {
    text: SourceTextAsChars,
    line_starts: Vec<usize>,
}

impl LineInfo for LineInfoConcrete {
    fn get_line_count(&self) -> usize {
        self.line_starts.len()
    }

    fn get_line_text(&self, line: usize) -> String {
        if line == self.line_starts.len() - 1 {
            self.text[self.line_starts[line]..].into_iter().collect()
        } else {
            self.text[self.line_starts[line]..self.line_starts[line + 1]]
                .into_iter()
                .collect()
        }
    }
}

pub fn try_get_source_mapping_url(_line_info: &impl LineInfo) -> Option<String> {
    unimplemented!()
}

pub fn try_parse_raw_source_map(_text: &str) -> Option<RawSourceMap> {
    unimplemented!()
}

pub struct MappingsDecoder {
    pub pos: usize,
}

impl Iterator for MappingsDecoder {
    type Item = Mapping;

    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}

#[derive(Copy, Clone)]
pub struct Mapping {
    pub generated_line: usize,
    pub generated_character: usize,
    pub source_index: Option<usize>,
    pub source_line: Option<usize>,
    pub source_character: Option<usize>,
    pub name_index: Option<usize>,
}

pub fn decode_mappings(_mappings: &str) -> MappingsDecoder {
    unimplemented!()
}

pub fn is_source_mapping(_mapping: &Mapping) -> bool {
    unimplemented!()
}

fn base64_format_encode(value: isize) -> char {
    if value >= 0 && value < 26 {
        char::from_u32(u32::from(CharacterCodes::A) + u32::try_from(value).unwrap()).unwrap()
    } else if value >= 26 && value < 52 {
        char::from_u32(u32::from(CharacterCodes::a) + u32::try_from(value).unwrap() - 26).unwrap()
    } else if value >= 52 && value < 62 {
        char::from_u32(u32::from(CharacterCodes::_0) + u32::try_from(value).unwrap() - 52).unwrap()
    } else if value == 62 {
        CharacterCodes::plus
    } else if value == 63 {
        CharacterCodes::slash
    } else {
        Debug_.fail(Some(&format!("{value:?}: not a base64 value")))
    }
}
