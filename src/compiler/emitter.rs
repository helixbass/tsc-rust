use std::borrow::{Borrow, Cow};
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use crate::{
    create_text_writer, factory, file_extension_is, get_emit_flags,
    get_emit_module_kind_from_module_and_target, get_literal_text, get_new_line_character,
    get_parse_tree_node, id_text, is_expression, is_identifier, is_keyword, is_source_file,
    no_emit_notification, no_emit_substitution, positions_are_on_same_line, skip_trivia,
    token_to_string, BundleFileInfo, BundleFileSectionKind, Debug_, EmitFlags, EmitHint,
    EmitTextWriter, Extension, GetLiteralTextFlags, HasTypeArgumentsInterface, HasTypeInterface,
    ListFormat, NamedDeclarationInterface, Node, NodeArray, NodeInterface, ParsedCommandLine,
    PrintHandlers, Printer, PrinterOptions, ReadonlyTextRange, SourceFileLike, SourceMapGenerator,
    Symbol, SyntaxKind,
};

lazy_static! {
    static ref brackets: HashMap<ListFormat, (&'static str, &'static str)> = create_brackets_map();
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
enum PipelinePhase {
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

struct DummyPrintHandlers;

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

    fn maybe_current_source_file(&self) -> Option<Rc<Node>> {
        self.current_source_file.borrow().clone()
    }

    fn maybe_writer(&self) -> Option<Rc<RefCell<dyn EmitTextWriter>>> {
        self.writer.borrow().clone()
    }

    fn writer_(&self) -> Rc<RefCell<dyn EmitTextWriter>> {
        self.writer.borrow().clone().unwrap()
    }

    fn has_global_name(&self, name: &str) -> Option<bool> {
        self.handlers.has_global_name(name)
    }

    fn on_emit_node(&self, hint: EmitHint, node: &Node, emit_callback: &dyn Fn(EmitHint, &Node)) {
        if self.handlers.is_on_emit_node_supported() {
            self.handlers.on_emit_node(hint, node, emit_callback)
        } else {
            no_emit_notification(hint, node, emit_callback)
        }
    }

    fn is_on_emit_node_supported(&self) -> bool {
        true
    }

    fn is_emit_notification_enabled(&self, node: &Node) -> Option<bool> {
        self.handlers.is_emit_notification_enabled(node)
    }

    fn substitute_node(&self, hint: EmitHint, node: &Node) -> Option<Rc<Node>> {
        Some(
            self.handlers
                .substitute_node(hint, node)
                .unwrap_or_else(|| no_emit_substitution(hint, node)),
        )
    }

    fn on_before_emit_node(&self, node: Option<&Node>) {
        self.handlers.on_before_emit_node(node)
    }

    fn on_after_emit_node(&self, node: Option<&Node>) {
        self.handlers.on_after_emit_node(node)
    }

    fn on_before_emit_node_array(&self, nodes: Option<&NodeArray>) {
        self.handlers.on_before_emit_node_array(nodes)
    }

    fn on_after_emit_node_array(&self, nodes: Option<&NodeArray>) {
        self.handlers.on_after_emit_node_array(nodes)
    }

    fn on_before_emit_token(&self, node: Option<&Node>) {
        self.handlers.on_before_emit_token(node)
    }

    fn on_after_emit_token(&self, node: Option<&Node>) {
        self.handlers.on_after_emit_token(node)
    }

    fn write(&self, text: &str) {
        (self.write.get())(self, text);
    }

    fn enter_comment(&self) {
        // unimplemented!()
    }

    fn exit_comment(&self) {
        // unimplemented!()
    }

    fn emit_binary_expression(&self) {
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
        output: Rc<RefCell<dyn EmitTextWriter>>,
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
        output: Rc<RefCell<dyn EmitTextWriter>>,
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

    pub fn write_bundle(
        &self,
        bundle: &Node, /*Bundle*/
        output: Rc<RefCell<dyn EmitTextWriter>>,
        source_map_generator: Option<Rc<dyn SourceMapGenerator>>,
    ) {
        unimplemented!()
    }

    pub fn write_unparsed_source(
        &self,
        unparsed: &Node, /*UnparsedSource*/
        output: Rc<RefCell<dyn EmitTextWriter>>,
    ) {
        unimplemented!()
    }

    pub fn write_file(
        &self,
        source_file: &Node, /*SourceFile*/
        output: Rc<RefCell<dyn EmitTextWriter>>,
        source_map_generator: Option<Rc<dyn SourceMapGenerator>>,
    ) {
        unimplemented!()
    }

    fn begin_print(&self) -> Rc<RefCell<dyn EmitTextWriter>> {
        self.own_writer
            .borrow_mut()
            .get_or_insert_with(|| Rc::new(RefCell::new(create_text_writer(&self.new_line))))
            .clone()
    }

    fn end_print(&self) -> String {
        let own_writer = self.own_writer.borrow();
        let own_writer = own_writer.as_ref().unwrap();
        let mut own_writer = own_writer.borrow_mut();
        let text = own_writer.get_text();
        own_writer.clear();
        text
    }

    fn print(&self, hint: EmitHint, node: &Node, source_file: Option<&Node /*SourceFile*/>) {
        if let Some(source_file) = source_file {
            self.set_source_file(Some(source_file));
        }

        self.pipeline_emit(hint, node);
    }

    fn set_source_file(&self, source_file: Option<&Node /*SourceFile*/>) {
        *self.current_source_file.borrow_mut() =
            source_file.map(|source_file| source_file.node_wrapper());
    }

    fn set_writer(
        &self,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
        source_map_generator: Option<Rc<dyn SourceMapGenerator>>,
    ) {
        *self.writer.borrow_mut() = writer;
    }

    fn reset(&self) {
        self.set_writer(
            None, None, //TODO this might be wrong
        );
    }

    fn emit(&self, node: Option<&Node>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        self.pipeline_emit(EmitHint::Unspecified, node);
    }

    fn emit_expression(&self, node: &Node /*Expression*/) {
        self.pipeline_emit(EmitHint::Expression, node);
    }

    fn pipeline_emit(&self, emit_hint: EmitHint, node: &Node) {
        let pipeline_phase = self.get_pipeline_phase(PipelinePhase::Notification, emit_hint, node);
        pipeline_phase(self, emit_hint, node);
    }

    fn get_pipeline_phase(
        &self,
        phase: PipelinePhase,
        emit_hint: EmitHint,
        node: &Node,
    ) -> fn(&Printer, EmitHint, &Node) {
        if phase == PipelinePhase::Notification {
            if false {
                unimplemented!()
            }
        }
        if phase == PipelinePhase::Notification || phase == PipelinePhase::Emit {
            return Printer::pipeline_emit_with_hint;
        }
        Debug_.assert_never(phase, None);
    }

    fn pipeline_emit_with_hint(&self, hint: EmitHint, node: &Node) {
        if false {
            unimplemented!()
        } else {
            self.pipeline_emit_with_hint_worker(hint, node);
        }
    }

    fn pipeline_emit_with_hint_worker(&self, mut hint: EmitHint, node: &Node) {
        if hint == EmitHint::Unspecified {
            match node.kind() {
                SyntaxKind::Identifier => return self.emit_identifier(node),
                SyntaxKind::PropertySignature => return self.emit_property_signature(node),
                SyntaxKind::CallSignature => return self.emit_call_signature(node),
                SyntaxKind::TypeReference => return self.emit_type_reference(node),
                SyntaxKind::FunctionType => return self.emit_function_type(node),
                SyntaxKind::ConstructorType => return self.emit_constructor_type(node),
                SyntaxKind::TypeLiteral => return self.emit_type_literal(node),
                SyntaxKind::ArrayType => return self.emit_array_type(node),
                SyntaxKind::TupleType => return self.emit_tuple_type(node),
                SyntaxKind::UnionType => return self.emit_union_type(node),
                SyntaxKind::IntersectionType => return self.emit_intersection_type(node),
                SyntaxKind::ConditionalType => return self.emit_conditional_type(node),
                SyntaxKind::ParenthesizedType => return self.emit_parenthesized_type(node),
                SyntaxKind::TypeOperator => return self.emit_type_operator(node),
                SyntaxKind::IndexedAccessType => return self.emit_indexed_access_type(node),
                SyntaxKind::LiteralType => return self.emit_literal_type(node),
                SyntaxKind::ImportType => return self.emit_import_type(node),
                _ => (),
            }
            if is_expression(node) {
                hint = EmitHint::Expression;
            }
        }
        if hint == EmitHint::Expression {
            match node {
                Node::StringLiteral(_)
                | Node::TemplateLiteralLikeNode(_)
                | Node::NumericLiteral(_)
                | Node::BigIntLiteral(_)
                | Node::RegularExpressionLiteral(_)
                | Node::JsxText(_) => {
                    return self.emit_literal(node, false);
                }
                _ => (),
            }
        }
        if is_keyword(node.kind()) {
            return self.write_token_node(node, Printer::write_keyword);
        }
        unimplemented!("hint: {:?}, kind: {:?}", hint, node.kind());
    }

    fn emit_literal(&self, node: &Node /*LiteralLikeNode*/, jsx_attribute_escape: bool) {
        let text = self.get_literal_text_of_node(node);
        if false {
            unimplemented!()
        } else {
            self.write_string_literal(&text);
        }
    }

    fn emit_identifier(&self, node: &Node /*Identifier*/) {
        let text_of_node = self.get_text_of_node(node, Some(false));
        if let Some(symbol) = node.maybe_symbol() {
            self.write_symbol(&text_of_node, &symbol);
        } else {
            self.write(&text_of_node);
        }
    }

    fn emit_property_signature(&self, node: &Node /*PropertySignature*/) {
        let node_as_property_signature = node.as_property_signature();
        self.emit_node_with_writer(
            Some(&*node_as_property_signature.name()),
            Printer::write_property,
        );
        self.emit_type_annotation(node_as_property_signature.maybe_type());
        self.write_trailing_semicolon();
    }

    fn emit_call_signature(&self, node: &Node /*CallSignature*/) {
        // unimplemented!()
        self.write_punctuation("TODO call signature");
    }

    fn emit_type_reference(&self, node: &Node /*TypeReferenceNode*/) {
        let node_as_type_reference_node = node.as_type_reference_node();
        self.emit(Some(&*node_as_type_reference_node.type_name));
        self.emit_type_arguments(
            node,
            node_as_type_reference_node.maybe_type_arguments().as_ref(),
        );
    }

    fn emit_function_type(&self, node: &Node /*FunctionTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO function type");
    }

    fn emit_constructor_type(&self, node: &Node /*ConstructorTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO constructor type");
    }

    fn emit_type_literal(&self, node: &Node /*TypeLiteralNode*/) {
        self.write_punctuation("{");
        let flags = if true {
            ListFormat::SingleLineTypeLiteralMembers
        } else {
            unimplemented!()
        };
        self.emit_list(
            Some(node),
            Some(&node.as_type_literal_node().members),
            flags | ListFormat::NoSpaceIfEmpty,
        );
        self.write_punctuation("}");
    }

    fn emit_array_type(&self, node: &Node /*ArrayTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO array type");
    }

    fn emit_tuple_type(&self, node: &Node /*TupleTypeNode*/) {
        self.emit_token_with_comment(
            SyntaxKind::OpenBracketToken,
            node.pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        let flags = if get_emit_flags(node).intersects(EmitFlags::SingleLine) {
            ListFormat::SingleLineTupleTypeElements
        } else {
            ListFormat::MultiLineTupleTypeElements
        };
        let node_elements = &node.as_tuple_type_node().elements;
        self.emit_list(
            Some(node),
            Some(&node_elements),
            flags | ListFormat::NoSpaceIfEmpty,
        );
        self.emit_token_with_comment(
            SyntaxKind::CloseBracketToken,
            node_elements.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
    }

    fn emit_union_type(&self, node: &Node /*UnionTypeNode*/) {
        self.emit_list(
            Some(node),
            Some(&node.as_union_or_intersection_type_node().types()),
            ListFormat::UnionTypeConstituents,
        );
    }

    fn emit_intersection_type(&self, node: &Node /*IntersectionTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO intersection type");
    }

    fn emit_conditional_type(&self, node: &Node /*ConditionalTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO conditional type");
    }

    fn emit_parenthesized_type(&self, node: &Node /*ParenthesizedTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO parenthesized type");
    }

    fn emit_type_operator(&self, node: &Node /*TypeOperatorNode*/) {
        let node_as_type_operator_node = node.as_type_operator_node();
        self.write_token_text(
            node_as_type_operator_node.operator,
            |text| self.write_keyword(text),
            None,
        );
        self.write_space();
        self.emit(
            Some(&*node_as_type_operator_node.type_),
            // parenthesizer.parenthesizeMemberOfElementType
        );
    }

    fn emit_indexed_access_type(&self, node: &Node /*IndexedAccessType*/) {
        // unimplemented!()
        self.write_punctuation("TODO indexed access type");
    }

    fn emit_literal_type(&self, node: &Node /*LiteralTypeNode*/) {
        self.emit_expression(&*node.as_literal_type_node().literal);
    }

    fn emit_import_type(&self, node: &Node /*ImportTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO import type");
    }

    fn emit_token_with_comment<TWriter: FnMut(&str)>(
        &self,
        token: SyntaxKind,
        mut pos: isize,
        writer: TWriter,
        context_node: &Node,
        indent_leading: Option<bool>,
    ) -> isize {
        let node = get_parse_tree_node(Some(context_node), Option::<fn(&Node) -> bool>::None);
        let is_similar_node = matches!(
            node.as_ref(),
            Some(node) if node.kind() == context_node.kind()
        );
        let start_pos = pos;
        if is_similar_node {
            if let Some(current_source_file) = self.maybe_current_source_file().as_ref() {
                pos = skip_trivia(
                    &current_source_file.as_source_file().text_as_chars(),
                    pos,
                    None,
                    None,
                    None,
                );
            }
        }
        if is_similar_node && context_node.pos() != start_pos {
            let needs_indent = indent_leading == Some(true)
                && matches!(
                    self.maybe_current_source_file().as_ref(),
                    Some(current_source_file) if !positions_are_on_same_line(
                        start_pos.try_into().unwrap(),
                        pos.try_into().unwrap(),
                        current_source_file,
                    )
                );
            if needs_indent {
                self.increase_indent();
            }
            self.emit_leading_comments_of_position(start_pos);
            if needs_indent {
                self.decrease_indent();
            }
        }
        pos = self.write_token_text(token, writer, Some(pos)).unwrap();
        if is_similar_node && context_node.end() != pos {
            let is_jsx_expr_context = context_node.kind() == SyntaxKind::JsxExpression;
            self.emit_trailing_comments_of_position(
                pos,
                Some(!is_jsx_expr_context),
                Some(is_jsx_expr_context),
            );
        }
        pos
    }

    fn emit_node_with_writer(&self, node: Option<&Node>, writer: fn(&Printer, &str)) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let saved_write = self.write.get();
        self.write.set(writer);
        self.emit(Some(node));
        self.write.set(saved_write);
    }

    fn emit_type_annotation<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef /*TypeNode*/>) {
        if let Some(node) = node {
            let node = node.borrow();
            self.write_punctuation(":");
            self.write_space();
            self.emit(Some(node));
        }
    }

    fn emit_type_arguments(
        &self,
        parent_node: &Node,
        type_arguments: Option<&NodeArray /*<TypeNode>*/>,
    ) {
        self.emit_list(
            Some(parent_node),
            type_arguments,
            ListFormat::TypeArguments,
            // parenthesizer.parenthesizeMemberOfElementType
        );
    }

    fn write_delimiter(&self, format: ListFormat) {
        match format & ListFormat::DelimitersMask {
            ListFormat::None => (),
            ListFormat::CommaDelimited => {
                self.write_punctuation(",");
            }
            ListFormat::BarDelimited => {
                self.write_space();
                self.write_punctuation("|");
            }
            _ => unimplemented!(),
        }
    }

    fn emit_list<TNode: Borrow<Node>>(
        &self,
        parent_node: Option<TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
    ) {
        self.emit_node_list(
            Printer::emit,
            parent_node,
            children,
            format,
            // TODO: this is wrong
            None,
            None,
        );
    }

    fn emit_node_list<TNode: Borrow<Node>>(
        &self,
        emit: fn(&Printer, Option<&Node>),
        parent_node: Option<TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
        start: Option<usize>,
        count: Option<usize>,
    ) {
        let start = start.unwrap_or(0);
        let count = count.unwrap_or_else(|| {
            if let Some(children) = children {
                children.len() - start
            } else {
                0
            }
        });
        let is_undefined = children.is_none();
        if is_undefined && format.intersects(ListFormat::OptionalIfUndefined) {
            return;
        }

        let is_empty = match children {
            None => true,
            Some(children) => start >= children.len(),
        } || count == 0;
        if is_empty && format.intersects(ListFormat::OptionalIfEmpty) {
            // TODO
            return;
        }

        if format.intersects(ListFormat::BracketsMask) {
            self.write_punctuation(get_opening_bracket(format));
            if is_empty {
                if let Some(children) = children {
                    self.emit_trailing_comments_of_position(children.pos(), Some(true), None);
                }
            }
        }

        // TODO

        let children = children.unwrap();
        if is_empty {
            unimplemented!()
        } else {
            if false {
                unimplemented!()
            } else if format.intersects(ListFormat::SpaceBetweenBraces) {
                self.write_space();
            }

            let mut previous_sibling: Option<Rc<Node>> = None;
            let children_iter = children.iter();
            for child in children_iter.skip(start) {
                if false {
                    unimplemented!()
                } else if let Some(previous_sibling) = previous_sibling.as_ref() {
                    self.write_delimiter(format);
                }

                if false {
                    unimplemented!()
                } else if previous_sibling.is_some()
                    && format.intersects(ListFormat::SpaceBetweenSiblings)
                {
                    self.write_space();
                }

                emit(self, Some(&**child));

                previous_sibling = Some(child.clone());
            }

            if false {
                unimplemented!()
            } else if format.intersects(ListFormat::SpaceAfterList | ListFormat::SpaceBetweenBraces)
            {
                self.write_space();
            }
        }
    }

    fn write_base(&self, s: &str) {
        self.writer_().borrow_mut().write(s);
    }

    fn write_string_literal(&self, s: &str) {
        self.writer_().borrow_mut().write_string_literal(s);
    }

    fn write_symbol(&self, s: &str, sym: &Symbol) {
        self.writer_().borrow_mut().write_symbol(s, sym);
    }

    fn write_punctuation(&self, s: &str) {
        self.writer_().borrow_mut().write_punctuation(s);
    }

    fn write_trailing_semicolon(&self) {
        self.writer_().borrow_mut().write_trailing_semicolon(";");
    }

    fn write_keyword(&self, s: &str) {
        self.writer_().borrow_mut().write_keyword(s);
    }

    fn write_space(&self) {
        self.writer_().borrow_mut().write_space(" ");
    }

    fn write_property(&self, s: &str) {
        self.writer_().borrow_mut().write_property(s);
    }

    fn increase_indent(&self) {
        self.writer_().borrow_mut().increase_indent();
    }

    fn decrease_indent(&self) {
        self.writer_().borrow_mut().decrease_indent();
    }

    fn write_token_node(&self, node: &Node, writer: fn(&Printer, &str)) {
        writer(self, token_to_string(node.kind()).unwrap());
    }

    fn write_token_text<TWriter: FnMut(&str)>(
        &self,
        token: SyntaxKind,
        mut writer: TWriter,
        pos: Option<isize>,
    ) -> Option<isize> {
        let token_string = token_to_string(token).unwrap();
        writer(token_string);
        pos.map(|pos| {
            if pos < 0 {
                pos
            } else {
                pos + TryInto::<isize>::try_into(token_string.len()).unwrap()
            }
        })
    }

    fn get_text_of_node(&self, node: &Node, include_trivia: Option<bool>) -> String {
        if false {
            unimplemented!()
        } else if (is_identifier(node) || false) && true {
            return id_text(node);
        }

        unimplemented!()
    }

    fn get_literal_text_of_node(&self, node: &Node) -> Cow<'static, str> {
        let flags = GetLiteralTextFlags::None;

        get_literal_text(node, self.maybe_current_source_file(), flags)
    }

    fn emit_leading_comments_of_position(&self, pos: isize) {
        // unimplemented!()
    }

    fn emit_trailing_comments_of_position(
        &self,
        pos: isize,
        prefix_space: Option<bool>,
        force_no_newline: Option<bool>,
    ) {
        // unimplemented!()
    }
}

fn create_brackets_map() -> HashMap<ListFormat, (&'static str, &'static str)> {
    HashMap::from_iter(IntoIterator::into_iter([
        (ListFormat::Braces, ("{", "}")),
        (ListFormat::Parenthesis, ("(", ")")),
        (ListFormat::AngleBrackets, ("<", ">")),
        (ListFormat::SquareBrackets, ("[", "]")),
    ]))
}

fn get_opening_bracket(format: ListFormat) -> &'static str {
    brackets
        .get(&(format & ListFormat::BracketsMask))
        .unwrap()
        .0
}

fn get_closing_bracket(format: ListFormat) -> &'static str {
    brackets
        .get(&(format & ListFormat::BracketsMask))
        .unwrap()
        .1
}

pub enum TempFlags {
    Auto = 0x00000000,
    CountMask = 0x0FFFFFFF,
    _I = 0x10000000,
}
