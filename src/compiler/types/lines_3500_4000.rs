#![allow(non_upper_case_globals)]

use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;

use super::{
    BaseNode, BaseTextRange, Diagnostic, FileReference, LanguageVariant, Node, NodeArray, Path,
    ScriptKind, ScriptTarget, Symbol, TypeCheckerHost,
};
use local_macros::ast_type;

pub type SourceTextAsChars = Vec<char>;

pub fn str_to_source_text_as_chars(str_: &str) -> SourceTextAsChars {
    str_.chars().collect()
}

pub fn text_len(text: &SourceTextAsChars) -> usize {
    text.len()
}

pub fn maybe_text_char_at_index(text: &SourceTextAsChars, index: usize) -> Option<char> {
    text.get(index).map(|ch| *ch)
}

pub fn text_char_at_index(text: &SourceTextAsChars, index: usize) -> char {
    maybe_text_char_at_index(text, index).unwrap()
}

pub fn text_substring(text: &SourceTextAsChars, start: usize, end: usize) -> String {
    text[start..end].into_iter().collect()
}

pub fn text_str_num_chars(text: &str, start: usize, end: usize) -> usize {
    text[start..end].chars().count()
}

pub trait SourceFileLike {
    fn text(&self) -> &str;
    fn text_as_chars(&self) -> &SourceTextAsChars;
    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>>;
    fn line_map(&self) -> Ref<Vec<usize>>;
    fn maybe_get_position_of_line_and_character(
        &self,
        line: usize,
        character: usize,
        allow_edits: Option<bool>,
    ) -> Option<usize>;
}

#[derive(Debug)]
#[ast_type]
pub struct SourceFile {
    _node: BaseNode,
    _symbols_without_a_symbol_table_strong_references: RefCell<Vec<Rc<Symbol>>>,
    pub statements: NodeArray,
    pub end_of_file_token: Rc<Node /*Token<SyntaxFile.EndOfFileToken>*/>,

    file_name: RefCell<String>,
    path: RefCell<Option<Path>>,
    pub text: String,
    pub text_as_chars: SourceTextAsChars,

    language_variant: LanguageVariant,
    is_declaration_file: bool,

    has_no_default_lib: bool,

    language_version: ScriptTarget,

    script_kind: ScriptKind,

    parse_diagnostics: RefCell<Option<Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>>>,

    line_map: RefCell<Option<Vec<usize>>>,
}

impl SourceFile {
    pub fn new(
        base_node: BaseNode,
        statements: NodeArray,
        end_of_file_token: Rc<Node>,
        file_name: String,
        text: String,
        language_version: ScriptTarget,
        language_variant: LanguageVariant,
        script_kind: ScriptKind,
        is_declaration_file: bool,
        has_no_default_lib: bool,
    ) -> Self {
        let text_as_chars = text.chars().collect();
        Self {
            _node: base_node,
            _symbols_without_a_symbol_table_strong_references: RefCell::new(vec![]),
            statements,
            end_of_file_token,
            file_name: RefCell::new(file_name),
            path: RefCell::new(None),
            text,
            text_as_chars,
            parse_diagnostics: RefCell::new(None),
            line_map: RefCell::new(None),
            language_version,
            language_variant,
            script_kind,
            is_declaration_file,
            has_no_default_lib,
        }
    }

    pub fn file_name(&self) -> Ref<String> {
        self.file_name.borrow()
    }

    pub fn set_file_name(&self, file_name: String) {
        *self.file_name.borrow_mut() = file_name;
    }

    pub fn maybe_path(&self) -> Ref<Option<Path>> {
        self.path.borrow()
    }

    pub fn set_path(&self, path: Path) {
        *self.path.borrow_mut() = Some(path);
    }

    pub fn parse_diagnostics(&self) -> Ref<Vec<Rc<Diagnostic>>> {
        Ref::map(self.parse_diagnostics.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_parse_diagnostics(&self, parse_diagnostics: Vec<Rc<Diagnostic>>) {
        *self.parse_diagnostics.borrow_mut() = Some(parse_diagnostics);
    }

    pub fn keep_strong_reference_to_symbol(&self, symbol: Rc<Symbol>) {
        self._symbols_without_a_symbol_table_strong_references
            .borrow_mut()
            .push(symbol);
    }
}

impl SourceFileLike for SourceFile {
    fn text(&self) -> &str {
        &self.text
    }

    fn text_as_chars(&self) -> &SourceTextAsChars {
        &self.text_as_chars
    }

    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>> {
        self.line_map.borrow_mut()
    }

    fn line_map(&self) -> Ref<Vec<usize>> {
        Ref::map(self.line_map.borrow(), |line_map| {
            line_map.as_ref().unwrap()
        })
    }

    fn maybe_get_position_of_line_and_character(
        &self,
        line: usize,
        character: usize,
        allow_edits: Option<bool>,
    ) -> Option<usize> {
        None
    }
}

#[derive(Debug)]
#[ast_type]
pub struct Bundle {
    _node: BaseNode,
    pub prepends: Vec<Rc<Node /*InputFiles | UnparsedSource*/>>,
    pub source_files: Vec<Rc<Node /*SourceFile*/>>,
    pub(crate) synthetic_file_references: Option<Vec<FileReference>>,
    pub(crate) synthetic_type_references: Option<Vec<FileReference>>,
    pub(crate) synthetic_lib_references: Option<Vec<FileReference>>,
    pub(crate) has_no_default_lib: Option<bool>,
}

impl Bundle {
    pub fn new(base_node: BaseNode, prepends: Vec<Rc<Node>>, source_files: Vec<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            prepends,
            source_files,
            synthetic_file_references: None,
            synthetic_type_references: None,
            synthetic_lib_references: None,
            has_no_default_lib: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CommentDirective {
    pub range: BaseTextRange,
    pub type_: CommentDirectiveType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CommentDirectiveType {
    ExpectError,
    Ignore,
}

pub trait Program: TypeCheckerHost {
    fn get_syntactic_diagnostics(&mut self) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>;
    fn get_semantic_diagnostics(&mut self) -> Vec<Rc<Diagnostic>>;
}
