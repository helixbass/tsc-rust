use std::borrow::{Borrow, Cow};
use std::cell::{Cell, RefCell};
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    get_emit_flags, get_literal_text, get_parse_tree_node, id_text, is_expression, is_identifier,
    is_keyword, positions_are_on_same_line, skip_trivia, token_to_string, Debug_, EmitFlags,
    EmitHint, EmitTextWriter, GetLiteralTextFlags, HasTypeInterface, ListFormat,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, ParsedCommandLine, Printer,
    PrinterOptions, ReadonlyTextRange, SourceFileLike, Symbol, SyntaxKind,
};

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

#[derive(PartialEq, Eq)]
enum PipelinePhase {
    Notification,
    Emit,
}

pub fn create_printer(printer_options: PrinterOptions) -> Printer {
    let mut printer = Printer::new();
    printer.reset();
    printer
}

impl Printer {
    pub fn new() -> Self {
        Self {
            current_source_file: RefCell::new(None),
            writer: RefCell::new(None),
            write: Cell::new(Printer::write_base),
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

    fn write(&self, text: &str) {
        (self.write.get())(self, text);
    }

    pub fn write_node(
        &self,
        hint: EmitHint,
        node: &Node,
        source_file: Option<Rc<Node /*SourceFile*/>>,
        output: Rc<RefCell<dyn EmitTextWriter>>,
    ) {
        let previous_writer = self.maybe_writer();
        self.set_writer(Some(output));
        self.print(hint, node, source_file);
        self.reset();
        self.set_writer(previous_writer);
    }

    fn print(&self, hint: EmitHint, node: &Node, source_file: Option<Rc<Node /*SourceFile*/>>) {
        if let Some(source_file) = source_file {
            self.set_source_file(Some(source_file));
        }

        self.pipeline_emit(hint, node);
    }

    fn set_source_file(&self, source_file: Option<Rc<Node /*SourceFile*/>>) {
        *self.current_source_file.borrow_mut() = source_file;
    }

    fn set_writer(&self, writer: Option<Rc<RefCell<dyn EmitTextWriter>>>) {
        *self.writer.borrow_mut() = writer;
    }

    fn reset(&self) {
        self.set_writer(None);
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
        self.emit(Some(&*node.as_type_reference_node().type_name));
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
        self.emit_node_list(Printer::emit, parent_node, children, format, None, None);
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
        let children = children.unwrap();
        if false {
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
