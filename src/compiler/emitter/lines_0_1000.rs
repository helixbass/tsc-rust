use std::borrow::{Borrow, Cow};
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::{create_brackets_map, TempFlags};
use crate::{
    create_text_writer, factory, file_extension_is, get_emit_flags,
    get_emit_module_kind_from_module_and_target, get_literal_text, get_new_line_character,
    get_parse_tree_node, id_text, is_expression, is_identifier, is_keyword, is_source_file,
    last_or_undefined, no_emit_notification, no_emit_substitution, positions_are_on_same_line,
    skip_trivia, token_to_string, BundleFileInfo, BundleFileSection, BundleFileSectionInterface,
    BundleFileSectionKind, Debug_, EmitFlags, EmitHint, EmitTextWriter, Extension,
    GetLiteralTextFlags, HasTypeArgumentsInterface, HasTypeInterface, ListFormat,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, ParsedCommandLine, PrintHandlers,
    Printer, PrinterOptions, ReadonlyTextRange, SourceFileLike, SourceMapGenerator, Symbol,
    SyntaxKind, TextRange,
};

lazy_static! {
    pub(super) static ref brackets: HashMap<ListFormat, (&'static str, &'static str)> =
        create_brackets_map();
}

pub(crate) fn is_build_info_file(file: &str) -> bool {
    file_extension_is(file, Extension::TsBuildInfo.to_str())
}

pub(crate) fn get_output_declaration_file_name<TGetCommonSourceDirectory: FnMut() -> String>(
    input_file_name: &str,
    config_file: &ParsedCommandLine,
    ignore_case: bool,
    get_common_source_directory: Option<&mut TGetCommonSourceDirectory>,
) -> String {
    unimplemented!()
}

pub(crate) fn get_common_source_directory_of_config(
    command_line: &ParsedCommandLine,
    ignore_case: bool,
) -> String {
    let options = &command_line.options;
    let file_names = &command_line.file_names;
    unimplemented!()
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum PipelinePhase {
    Notification,
    Substitution,
    Comments,
    SourceMaps,
    Emit,
}

pub fn create_printer(
    printer_options: PrinterOptions,
    handlers: Option<Rc<dyn PrintHandlers>>,
) -> Printer {
    let handlers = handlers.unwrap_or_else(|| Rc::new(DummyPrintHandlers));
    let printer = Printer::new(printer_options, handlers);
    printer.reset();
    printer
}

pub(super) struct DummyPrintHandlers;

impl PrintHandlers for DummyPrintHandlers {
    fn is_on_emit_node_supported(&self) -> bool {
        false
    }
}

impl Printer {
    pub fn new(printer_options: PrinterOptions, handlers: Rc<dyn PrintHandlers>) -> Self {
        let extended_diagnostics = printer_options.extended_diagnostics == Some(true);
        let new_line =
            get_new_line_character(printer_options.new_line, Option::<fn() -> String>::None);
        let module_kind = get_emit_module_kind_from_module_and_target(
            printer_options.module,
            printer_options.target,
        );
        let preserve_source_newlines = printer_options.preserve_source_newlines;
        let bundle_file_info = if printer_options.write_bundle_file_info == Some(true) {
            Some(BundleFileInfo {
                sections: vec![],
                sources: None,
            })
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
            printer_options,
            handlers,
            extended_diagnostics,
            new_line,
            module_kind,
            current_source_file: RefCell::new(None),
            bundled_helpers: RefCell::new(HashMap::new()),
            node_id_to_generated_name: RefCell::new(HashMap::new()),
            auto_generated_id_to_generated_name: RefCell::new(HashMap::new()),
            generated_names: RefCell::new(HashSet::new()),
            temp_flags_stack: RefCell::new(vec![]),
            temp_flags: Cell::new(TempFlags::Auto),
            reserved_names_stack: RefCell::new(vec![]),
            reserved_names: RefCell::new(HashSet::new()),
            preserve_source_newlines: Cell::new(preserve_source_newlines),
            next_list_element_pos: Cell::new(None),
            writer: RefCell::new(None),
            own_writer: RefCell::new(None),
            write: Cell::new(Printer::write_base),
            is_own_file_emit: Cell::new(false),
            bundle_file_info: RefCell::new(bundle_file_info),
            relative_to_build_info,
            record_internal_section,
            source_file_text_pos: Cell::new(0),
            source_file_text_kind: Cell::new(BundleFileSectionKind::Text),

            source_maps_disabled: Cell::new(true),
            source_map_generator: RefCell::new(None),
            source_map_source: RefCell::new(None),
            source_map_source_index: Cell::new(-1),
            most_recently_added_source_map_source: RefCell::new(None),
            most_recently_added_source_map_source_index: Cell::new(-1),

            container_pos: Cell::new(-1),
            container_end: Cell::new(-1),
            declaration_list_container_end: Cell::new(-1),
            current_line_map: RefCell::new(None),
            detached_comments_info: RefCell::new(None),
            has_written_comment: Cell::new(false),
            comments_disabled: Cell::new(false),
            last_substitution: RefCell::new(None),
            current_parenthesizer_rule: RefCell::new(None),
            // const { enter: enterComment, exit: exitComment } = performance.createTimerIf(extendedDiagnostics, "commentTime", "beforeComment", "afterComment");
            parenthesizer: factory.with(|factory_| factory_.parenthesizer()),
        }
    }

    pub(super) fn maybe_current_source_file(&self) -> Option<Rc<Node>> {
        self.current_source_file.borrow().clone()
    }

    pub(super) fn maybe_writer(&self) -> Option<Rc<dyn EmitTextWriter>> {
        self.writer.borrow().clone()
    }

    pub(super) fn writer(&self) -> Rc<dyn EmitTextWriter> {
        self.writer.borrow().clone().unwrap()
    }

    pub(super) fn has_global_name(&self, name: &str) -> Option<bool> {
        self.handlers.has_global_name(name)
    }

    pub(super) fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node),
    ) {
        if self.handlers.is_on_emit_node_supported() {
            self.handlers.on_emit_node(hint, node, emit_callback)
        } else {
            no_emit_notification(hint, node, emit_callback)
        }
    }

    pub(super) fn is_on_emit_node_supported(&self) -> bool {
        true
    }

    pub(super) fn is_emit_notification_enabled(&self, node: &Node) -> Option<bool> {
        self.handlers.is_emit_notification_enabled(node)
    }

    pub(super) fn substitute_node(&self, hint: EmitHint, node: &Node) -> Option<Rc<Node>> {
        Some(
            self.handlers
                .substitute_node(hint, node)
                .unwrap_or_else(|| no_emit_substitution(hint, node)),
        )
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

    pub(super) fn bundle_file_info(&self) -> Ref<BundleFileInfo> {
        Ref::map(self.bundle_file_info.borrow(), |bundle_file_info| {
            bundle_file_info.as_ref().unwrap()
        })
    }

    pub(super) fn bundle_file_info_mut(&self) -> RefMut<BundleFileInfo> {
        RefMut::map(self.bundle_file_info.borrow_mut(), |bundle_file_info| {
            bundle_file_info.as_mut().unwrap()
        })
    }

    pub(super) fn enter_comment(&self) {
        // unimplemented!()
    }

    pub(super) fn exit_comment(&self) {
        // unimplemented!()
    }

    pub(super) fn emit_binary_expression(&self) {
        unimplemented!()
    }

    pub fn print_node(
        &self,
        hint: EmitHint,
        node: &Node,
        source_file: &Node, /*SourceFile*/
    ) -> String {
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
        self.write_node(hint, node, Some(source_file), self.begin_print());
        self.end_print()
    }

    pub fn print_list(
        &self,
        format: ListFormat,
        nodes: &NodeArray,
        source_file: &Node, /*SourceFile*/
    ) -> String {
        self.write_list(format, nodes, Some(source_file), self.begin_print());
        self.end_print()
    }

    pub fn print_bundle(&self, bundle: &Node /*Bundle*/) -> String {
        self.write_bundle(bundle, self.begin_print(), None);
        self.end_print()
    }

    pub fn print_file(&self, source_file: &Node /*SourceFile*/) -> String {
        self.write_file(source_file, self.begin_print(), None);
        self.end_print()
    }

    pub fn print_unparsed_source(&self, unparsed: &Node /*UnparsedSource*/) -> String {
        self.write_unparsed_source(unparsed, self.begin_print());
        self.end_print()
    }

    pub fn write_node(
        &self,
        hint: EmitHint,
        node: &Node,
        source_file: Option<&Node /*SourceFile*/>,
        output: Rc<dyn EmitTextWriter>,
    ) {
        let previous_writer = self.maybe_writer();
        self.set_writer(Some(output), None);
        self.print(hint, node, source_file);
        self.reset();
        *self.writer.borrow_mut() = previous_writer;
    }

    pub fn write_list(
        &self,
        format: ListFormat,
        nodes: &NodeArray,
        source_file: Option<&Node /*SourceFile*/>,
        output: Rc<dyn EmitTextWriter>,
    ) {
        let previous_writer = self.maybe_writer();
        self.set_writer(Some(output), None);
        if source_file.is_some() {
            self.set_source_file(source_file);
        }
        self.emit_list(Option::<&Node>::None, Some(nodes), format);
        self.reset();
        *self.writer.borrow_mut() = previous_writer;
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
        let mut bundle_file_info = self.bundle_file_info_mut();
        let last = last_or_undefined(&bundle_file_info.sections);
        if let Some(last) = last.filter(|last| last.kind() == kind) {
            last.set_end(end);
        } else {
            bundle_file_info
                .sections
                .push(BundleFileSection::new_text_like(kind, None, pos, end));
        }
    }
}
