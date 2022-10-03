use regex::{Captures, Regex};
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    file_extension_is_one_of, for_each_child_bool, get_leading_comment_ranges, get_pragma_spec,
    map, to_pragma_name, trim_string, AmdDependency, CheckJsDirective, CommentRange, Debug_,
    DiagnosticMessage, Diagnostics, Extension, FileReference,
    IncrementalParserSyntaxCursorReparseTopLevelAwait, IncrementalParserType, Node, NodeArray,
    NodeInterface, ParserType, PragmaArgument, PragmaArgumentName, PragmaArgumentWithCapturedSpan,
    PragmaArguments, PragmaKindFlags, PragmaName, PragmaPseudoMapEntry, PragmaSpec, PragmaValue,
    ReadonlyPragmaMap, ReadonlyTextRange, ScriptTarget, SourceTextAsChars, SyntaxKind, TextRange,
};

impl IncrementalParserType {
    pub fn create_syntax_cursor(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> IncrementalParserSyntaxCursor {
        IncrementalParserSyntaxCursor::create(source_file.node_wrapper())
    }
}

pub trait IncrementalParserSyntaxCursorInterface {
    fn current_node(&self, parser: &ParserType, position: usize) -> Option<Rc<Node>>;
}

pub enum IncrementalParserSyntaxCursor {
    Created(IncrementalParserSyntaxCursorCreated),
    ReparseTopLevelAwait(IncrementalParserSyntaxCursorReparseTopLevelAwait),
}

impl IncrementalParserSyntaxCursor {
    pub fn create(source_file: Rc<Node>) -> Self {
        Self::Created(IncrementalParserSyntaxCursorCreated::new(source_file))
    }
}

impl IncrementalParserSyntaxCursorInterface for IncrementalParserSyntaxCursor {
    fn current_node(&self, parser: &ParserType, position: usize) -> Option<Rc<Node>> {
        match self {
            IncrementalParserSyntaxCursor::Created(incremental_parser_syntax_cursor) => {
                incremental_parser_syntax_cursor.current_node(parser, position)
            }
            IncrementalParserSyntaxCursor::ReparseTopLevelAwait(
                incremental_parser_syntax_cursor,
            ) => incremental_parser_syntax_cursor.current_node(parser, position),
        }
    }
}

pub struct IncrementalParserSyntaxCursorCreated {
    source_file: Rc<Node /*SourceFile*/>,
    current_array: RefCell<Option<NodeArray>>,
    current_array_index: Cell<Option<usize>>,
    current: RefCell<Option<Rc<Node>>>,
    last_queried_position: Cell<Option<usize>>,
}

impl IncrementalParserSyntaxCursorCreated {
    pub fn new(source_file: Rc<Node>) -> Self {
        let source_file_as_source_file = source_file.as_source_file();
        let current_array = &source_file_as_source_file.statements;
        let current_array_index = 0;

        Debug_.assert(current_array_index < current_array.len(), None);
        Self {
            source_file: source_file.clone(),
            current_array: RefCell::new(Some(current_array.clone())),
            current_array_index: Cell::new(Some(current_array_index)),
            current: RefCell::new(Some(current_array[current_array_index].clone())),
            last_queried_position: Cell::new(None), /*InvalidPosition::Value*/
        }
    }

    fn current_array(&self) -> Ref<NodeArray> {
        Ref::map(self.current_array.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    fn set_current_array(&self, current_array: Option<NodeArray>) {
        *self.current_array.borrow_mut() = current_array;
    }

    fn current_array_index(&self) -> usize {
        self.current_array_index.get().unwrap()
    }

    fn set_current_array_index(&self, current_array_index: Option<usize>) {
        self.current_array_index.set(current_array_index)
    }

    fn maybe_current(&self) -> Option<Rc<Node>> {
        self.current.borrow().as_ref().map(|option| option.clone())
    }

    fn current(&self) -> Rc<Node> {
        self.current.borrow().clone().unwrap()
    }

    fn set_current(&self, current: Option<Rc<Node>>) {
        *self.current.borrow_mut() = current;
    }

    fn maybe_last_queried_position(&self) -> Option<usize> {
        self.last_queried_position.get()
    }

    fn set_last_queried_position(&self, last_queried_position: Option<usize>) {
        self.last_queried_position.set(last_queried_position)
    }

    fn find_highest_list_element_that_starts_at_position(&self, position: usize) {
        self.set_current_array(None);
        self.set_current_array_index(None /*InvalidPosition::Value*/);
        self.set_current(None);

        let position_as_isize = position.try_into().unwrap();

        for_each_child_bool(
            &self.source_file,
            |node: &Node| self.visit_node(position_as_isize, node),
            Some(|array: &NodeArray| self.visit_array(position_as_isize, array)),
        );
    }

    fn visit_node(&self, position_as_isize: isize, node: &Node) -> bool {
        if position_as_isize >= node.pos() && position_as_isize < node.end() {
            for_each_child_bool(
                node,
                |node: &Node| self.visit_node(position_as_isize, node),
                Some(|array: &NodeArray| self.visit_array(position_as_isize, array)),
            );
        }

        true
    }

    fn visit_array(&self, position_as_isize: isize, array: &NodeArray) -> bool {
        if position_as_isize >= array.pos() && position_as_isize < array.end() {
            for (i, child) in array.iter().enumerate() {
                // if (child) {
                if child.pos() == position_as_isize {
                    self.set_current_array(Some(array.clone()));
                    self.set_current_array_index(Some(i));
                    self.set_current(Some(child.clone()));
                    return true;
                } else {
                    if child.pos() < position_as_isize && position_as_isize < child.end() {
                        for_each_child_bool(
                            child,
                            |node: &Node| self.visit_node(position_as_isize, node),
                            Some(|array: &NodeArray| self.visit_array(position_as_isize, array)),
                        );
                        return true;
                    }
                }
                // }
            }
        }

        false
    }
}

impl IncrementalParserSyntaxCursorInterface for IncrementalParserSyntaxCursorCreated {
    fn current_node(&self, _parser: &ParserType, position: usize) -> Option<Rc<Node>> {
        let position_as_isize: isize = position.try_into().unwrap();
        if match self.maybe_last_queried_position() {
            None => true,
            Some(last_queried_position) => position != last_queried_position,
        } {
            if let Some(current) = self.maybe_current() {
                if current.end() == position_as_isize
                    && self.current_array_index() < (self.current_array().len() - 1)
                {
                    self.set_current_array_index(Some(self.current_array_index() + 1));
                    self.set_current(
                        self.current_array()
                            .get(self.current_array_index())
                            .map(Clone::clone),
                    );
                }
            }

            if match self.maybe_current() {
                None => true,
                Some(current) => current.pos() != position_as_isize,
            } {
                self.find_highest_list_element_that_starts_at_position(position);
            }
        }

        self.set_last_queried_position(Some(position));

        Debug_.assert(
            match self.maybe_current() {
                None => true,
                Some(current) => current.pos() == position_as_isize,
            },
            None,
        );
        self.maybe_current()
    }
}

pub(crate) fn is_declaration_file_name(file_name: &str) -> bool {
    file_extension_is_one_of(
        file_name,
        &vec![
            Extension::Dts.to_str(),
            Extension::Dmts.to_str(),
            Extension::Dcts.to_str(),
        ],
    )
}

pub(crate) trait PragmaContext {
    fn language_version(&self) -> ScriptTarget;
    fn maybe_pragmas(&self) -> RefMut<Option<ReadonlyPragmaMap>>;
    fn maybe_check_js_directive(&self) -> RefMut<Option<CheckJsDirective>>;
    fn maybe_referenced_files(&self) -> RefMut<Option<Vec<FileReference>>>;
    fn maybe_type_reference_directives(&self) -> RefMut<Option<Vec<FileReference>>>;
    fn maybe_lib_reference_directives(&self) -> RefMut<Option<Vec<FileReference>>>;
    fn maybe_amd_dependencies(&self) -> RefMut<Option<Vec<AmdDependency>>>;
    fn maybe_has_no_default_lib(&self) -> Option<bool>;
    fn set_has_no_default_lib(&self, has_no_default_lib: bool);
    fn maybe_module_name(&self) -> RefMut<Option<String>>;
}

pub(crate) fn process_comment_pragmas<TContext: PragmaContext>(
    context: &TContext,
    source_text: &str,
    source_text_as_chars: &SourceTextAsChars,
) {
    let mut pragmas: Vec<PragmaPseudoMapEntry> = vec![];

    for range in &get_leading_comment_ranges(source_text_as_chars, 0).unwrap_or_else(|| vec![]) {
        // TODO: should we really be passing a slice of chars into extract_pragmas() instead?
        let comment: String = source_text_as_chars[TryInto::<usize>::try_into(range.pos()).unwrap()
            ..TryInto::<usize>::try_into(range.end()).unwrap()]
            .into_iter()
            .collect();
        extract_pragmas(&mut pragmas, range, &comment);
    }

    let mut context_pragmas = context.maybe_pragmas();
    *context_pragmas = Some(HashMap::new());
    let context_pragmas = context_pragmas.as_mut().unwrap();
    for pragma in pragmas {
        if context_pragmas.contains_key(&pragma.name) {
            let current_value = context_pragmas.get_mut(&pragma.name).unwrap();
            current_value.push(pragma.args.clone());
            continue;
        }
        context_pragmas.insert(pragma.name, vec![pragma.args.clone()]);
    }
}

pub(crate) fn process_pragmas_into_fields<
    TContext: PragmaContext,
    TReportDiagnostic: FnMut(isize, isize, &DiagnosticMessage),
>(
    context: &TContext,
    mut report_diagnostic: TReportDiagnostic,
) {
    let mut context_check_js_directive = context.maybe_check_js_directive();
    *context_check_js_directive = None;
    let mut context_referenced_files = context.maybe_referenced_files();
    *context_referenced_files = Some(vec![]);
    let mut context_type_reference_directives = context.maybe_type_reference_directives();
    *context_type_reference_directives = Some(vec![]);
    let mut context_lib_reference_directives = context.maybe_lib_reference_directives();
    *context_lib_reference_directives = Some(vec![]);
    let mut context_amd_dependencies = context.maybe_amd_dependencies();
    *context_amd_dependencies = Some(vec![]);
    context.set_has_no_default_lib(false);
    for (key, entry_or_list) in context.maybe_pragmas().as_ref().unwrap() {
        match key {
            PragmaName::Reference => {
                let referenced_files = context_referenced_files.as_mut().unwrap();
                let type_reference_directives = context_type_reference_directives.as_mut().unwrap();
                let lib_reference_directives = context_lib_reference_directives.as_mut().unwrap();
                for arg in entry_or_list {
                    let types = arg.arguments.get(&PragmaArgumentName::Types);
                    let lib = arg.arguments.get(&PragmaArgumentName::Lib);
                    let path = arg.arguments.get(&PragmaArgumentName::Path);
                    if matches!(
                        arg.arguments.get(&PragmaArgumentName::NoDefaultLib),
                        Some(value) if !matches!(
                            value,
                            PragmaArgument::WithoutCapturedSpan(value) if value.is_empty()
                        )
                    ) {
                        context.set_has_no_default_lib(true);
                    } else if let Some(types) = types.map(|types| types.as_with_captured_span()) {
                        type_reference_directives.push(FileReference::new(
                            types.pos.try_into().unwrap(),
                            types.end.try_into().unwrap(),
                            types.value.clone(),
                        ));
                    } else if let Some(lib) = lib.map(|lib| lib.as_with_captured_span()) {
                        lib_reference_directives.push(FileReference::new(
                            lib.pos.try_into().unwrap(),
                            lib.end.try_into().unwrap(),
                            lib.value.clone(),
                        ));
                    } else if let Some(path) = path.map(|path| path.as_with_captured_span()) {
                        referenced_files.push(FileReference::new(
                            path.pos.try_into().unwrap(),
                            path.end.try_into().unwrap(),
                            path.value.clone(),
                        ));
                    } else {
                        report_diagnostic(
                            arg.range.pos(),
                            arg.range.end() - arg.range.pos(),
                            &Diagnostics::Invalid_reference_directive_syntax,
                        );
                    }
                }
            }
            PragmaName::AmdDependency => {
                *context_amd_dependencies =
                    Some(map(entry_or_list, |x: &Rc<PragmaValue>, _| AmdDependency {
                        name: x
                            .arguments
                            .get(&PragmaArgumentName::Name)
                            .map(|value| value.as_without_captured_span().clone()),
                        path: x
                            .arguments
                            .get(&PragmaArgumentName::Path)
                            .unwrap()
                            .as_without_captured_span()
                            .clone(),
                    }));
            }
            PragmaName::AmdModule => {
                let mut context_module_name = context.maybe_module_name();
                for entry in entry_or_list {
                    if matches!(
                        context_module_name.as_ref(),
                        Some(context_module_name) if !context_module_name.is_empty()
                    ) {
                        report_diagnostic(
                            entry.range.pos(),
                            entry.range.end() - entry.range.pos(),
                            &Diagnostics::An_AMD_module_cannot_have_multiple_name_assignments,
                        );
                    }
                    *context_module_name = Some(
                        entry
                            .arguments
                            .get(&PragmaArgumentName::Name)
                            .unwrap()
                            .as_without_captured_span()
                            .clone(),
                    );
                }
            }
            PragmaName::TsNocheck | PragmaName::TsCheck => {
                for entry in entry_or_list {
                    if match context_check_js_directive.as_ref() {
                        None => true,
                        Some(context_check_js_directive) => {
                            entry.range.pos() > context_check_js_directive.pos()
                        }
                    } {
                        *context_check_js_directive = Some(CheckJsDirective::new(
                            entry.range.pos(),
                            entry.range.end(),
                            *key == PragmaName::TsCheck,
                        ));
                    }
                }
            }
            PragmaName::Jsx
            | PragmaName::Jsxfrag
            | PragmaName::Jsximportsource
            | PragmaName::Jsxruntime => {
                return;
            }
        }
    }
}

thread_local! {
    static named_arg_reg_ex_cache: RefCell<HashMap<PragmaArgumentName, Rc<Regex>>> = RefCell::new(HashMap::new());
}
fn get_named_arg_reg_ex(name: PragmaArgumentName) -> Rc<Regex> {
    named_arg_reg_ex_cache.with(|named_arg_reg_ex_cache_| {
        let mut named_arg_reg_ex_cache_ = named_arg_reg_ex_cache_.borrow_mut();
        if named_arg_reg_ex_cache_.contains_key(&name) {
            return named_arg_reg_ex_cache_.get(&name).unwrap().clone();
        }
        let result = Rc::new(
            Regex::new(&format!(
                r#"(?i)(?m)(\s{}\s*=\s*)(?:(?:'([^']*)')|(?:"([^"]*)"))"#,
                name.to_str()
            ))
            .unwrap(),
        );
        named_arg_reg_ex_cache_.insert(name, result.clone());
        result
    })
}

lazy_static! {
    static ref triple_slash_xml_comment_start_reg_ex: Regex =
        Regex::new(r"(?i)(?m)///\s*<(\S+)\s*?/>").unwrap();
    static ref single_line_pragma_reg_ex: Regex =
        Regex::new(r"(?i)(?m)///?\s*@(\S+)\s*(.*)\s*$").unwrap();
}

fn extract_pragmas(pragmas: &mut Vec<PragmaPseudoMapEntry>, range: &CommentRange, text: &str) {
    let triple_slash = if range.kind == SyntaxKind::SingleLineCommentTrivia {
        triple_slash_xml_comment_start_reg_ex.captures(text)
    } else {
        None
    };
    if let Some(triple_slash) = triple_slash {
        let name = to_pragma_name(&triple_slash[1].to_lowercase());
        if name.is_none() {
            return;
        }
        let name = name.unwrap();
        let pragma = get_pragma_spec(name);
        if !pragma.kind.intersects(PragmaKindFlags::TripleSlashXML) {
            return;
        }
        if let Some(pragma_args) = pragma.args.as_ref() {
            let mut argument = PragmaArguments::new();
            for arg in pragma_args {
                let matcher = get_named_arg_reg_ex(arg.name);
                let match_result = matcher.captures(text);
                if match_result.is_none() && !arg.optional {
                    return;
                } else if let Some(match_result) = match_result {
                    let value = match_result
                        .get(2)
                        .unwrap_or_else(|| match_result.get(3).unwrap())
                        .as_str()
                        .to_owned();
                    if arg.capture_span {
                        // TODO: this is mixing chars + bytes?
                        let start_pos = TryInto::<usize>::try_into(range.pos()).unwrap()
                            + match_result.get(0).unwrap().start()
                            + (match_result.get(1).unwrap().end()
                                - match_result.get(1).unwrap().start())
                            + 1;
                        let value_len = value.len();
                        argument.insert(
                            arg.name,
                            PragmaArgument::WithCapturedSpan(PragmaArgumentWithCapturedSpan {
                                value,
                                pos: start_pos,
                                end: start_pos + value_len,
                            }),
                        );
                    } else {
                        argument.insert(arg.name, PragmaArgument::WithoutCapturedSpan(value));
                    }
                }
            }
            pragmas.push(PragmaPseudoMapEntry {
                name,
                args: Rc::new(PragmaValue {
                    arguments: argument,
                    range: range.clone(),
                }),
            });
        } else {
            pragmas.push(PragmaPseudoMapEntry {
                name,
                args: Rc::new(PragmaValue {
                    arguments: PragmaArguments::new(),
                    range: range.clone(),
                }),
            });
        }
        return;
    }

    let single_line = if range.kind == SyntaxKind::SingleLineCommentTrivia {
        single_line_pragma_reg_ex.captures(text)
    } else {
        None
    };
    if let Some(single_line) = single_line {
        add_pragma_for_match(pragmas, range, PragmaKindFlags::SingleLine, single_line);
        return;
    }

    if range.kind == SyntaxKind::MultiLineCommentTrivia {
        lazy_static! {
            static ref multi_line_pragma_reg_ex: Regex =
                Regex::new(r"(?i)(?m)@(\S+)(\s+.*)?$").unwrap();
        }
        for multi_line_match in multi_line_pragma_reg_ex.captures_iter(text) {
            add_pragma_for_match(pragmas, range, PragmaKindFlags::MultiLine, multi_line_match);
        }
    }
}

fn add_pragma_for_match(
    pragmas: &mut Vec<PragmaPseudoMapEntry>,
    range: &CommentRange,
    kind: PragmaKindFlags,
    match_: Captures,
) {
    // if (!match) return;
    let name = to_pragma_name(&match_[1].to_lowercase());
    if name.is_none() {
        return;
    }
    let name = name.unwrap();
    let pragma = get_pragma_spec(name);
    if !pragma.kind.intersects(kind) {
        return;
    }
    let args = match_.get(2).map(|value| value.as_str());
    let argument = get_named_pragma_arguments(pragma, args);
    if argument.is_none() {
        return;
    }
    let argument = argument.unwrap();
    pragmas.push(PragmaPseudoMapEntry {
        name,
        args: Rc::new(PragmaValue {
            arguments: argument,
            range: range.clone(),
        }),
    });
}

fn get_named_pragma_arguments(pragma: &PragmaSpec, text: Option<&str>) -> Option<PragmaArguments> {
    let mut arg_map = PragmaArguments::new();
    if text.is_none() {
        return Some(arg_map);
    }
    let text = text.unwrap();
    let pragma_args = pragma.args.as_ref();
    if pragma_args.is_none() {
        return Some(arg_map);
    }
    let pragma_args = pragma_args.unwrap();
    lazy_static! {
        static ref whitespace_regex: Regex = Regex::new(r"\s+").unwrap();
    }
    let args = whitespace_regex
        .split(trim_string(text))
        .collect::<Vec<_>>();
    for i in 0..pragma_args.len() {
        let argument = &pragma_args[i];
        if match args.get(i) {
            None => true,
            Some(arg) => arg.is_empty(),
        } && !argument.optional
        {
            return None;
        }
        if argument.capture_span {
            Debug_.fail(Some(
                "Capture spans not yet implemented for non-xml pragmas",
            ));
        }
        arg_map.insert(
            argument.name,
            PragmaArgument::WithoutCapturedSpan(args.get(i).copied().unwrap().to_owned()),
        );
    }
    Some(arg_map)
}

pub(crate) fn tag_names_are_equivalent(
    lhs: &Node, /*JsxTagNameExpression*/
    rhs: &Node, /*JsxTagNameExpression*/
) -> bool {
    if lhs.kind() != rhs.kind() {
        return false;
    }

    if lhs.kind() == SyntaxKind::Identifier {
        return lhs.as_identifier().escaped_text == rhs.as_identifier().escaped_text;
    }

    if lhs.kind() == SyntaxKind::ThisKeyword {
        return true;
    }

    let lhs_as_property_access_expression = lhs.as_property_access_expression();
    let rhs_as_property_access_expression = rhs.as_property_access_expression();
    lhs_as_property_access_expression
        .name
        .as_member_name()
        .escaped_text()
        == rhs_as_property_access_expression
            .name
            .as_member_name()
            .escaped_text()
        && tag_names_are_equivalent(
            &lhs_as_property_access_expression.expression,
            &rhs_as_property_access_expression.expression,
        )
}
