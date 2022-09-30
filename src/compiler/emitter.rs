use std::borrow::{Borrow, Cow};
use std::cell::{RefCell, RefMut};
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    get_literal_text, id_text, is_expression, is_identifier, is_keyword, token_to_string, Debug_,
    EmitHint, EmitTextWriter, GetLiteralTextFlags, HasTypeInterface, ListFormat,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, Printer, PrinterOptions, Symbol,
    SyntaxKind,
};

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
            current_source_file: None,
            writer: None,
            write: Printer::write_base,
        }
    }

    fn maybe_writer(&self) -> Option<Rc<RefCell<dyn EmitTextWriter>>> {
        self.writer.clone()
    }

    fn writer_(&self) -> RefMut<dyn EmitTextWriter> {
        match &self.writer {
            None => panic!("Expected writer"),
            Some(writer) => writer.borrow_mut(),
        }
    }

    pub fn write_node(
        &mut self,
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

    fn print(&mut self, hint: EmitHint, node: &Node, source_file: Option<Rc<Node /*SourceFile*/>>) {
        if let Some(source_file) = source_file {
            self.set_source_file(Some(source_file));
        }

        self.pipeline_emit(hint, node);
    }

    fn set_source_file(&mut self, source_file: Option<Rc<Node /*SourceFile*/>>) {
        self.current_source_file = source_file;
    }

    fn set_writer(&mut self, writer: Option<Rc<RefCell<dyn EmitTextWriter>>>) {
        self.writer = writer;
    }

    fn reset(&mut self) {
        self.set_writer(None);
    }

    fn emit(&mut self, node: Option<&Node>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        self.pipeline_emit(EmitHint::Unspecified, node);
    }

    fn emit_expression(&mut self, node: &Node /*Expression*/) {
        self.pipeline_emit(EmitHint::Expression, node);
    }

    fn pipeline_emit(&mut self, emit_hint: EmitHint, node: &Node) {
        let pipeline_phase = self.get_pipeline_phase(PipelinePhase::Notification, emit_hint, node);
        pipeline_phase(self, emit_hint, node);
    }

    fn get_pipeline_phase(
        &self,
        phase: PipelinePhase,
        emit_hint: EmitHint,
        node: &Node,
    ) -> fn(&mut Printer, EmitHint, &Node) {
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

    fn pipeline_emit_with_hint(&mut self, hint: EmitHint, node: &Node) {
        if false {
            unimplemented!()
        } else {
            self.pipeline_emit_with_hint_worker(hint, node);
        }
    }

    fn pipeline_emit_with_hint_worker(&mut self, mut hint: EmitHint, node: &Node) {
        if hint == EmitHint::Unspecified {
            match node.kind() {
                SyntaxKind::Identifier => return self.emit_identifier(node),
                SyntaxKind::PropertySignature => return self.emit_property_signature(node),
                SyntaxKind::TypeReference => return self.emit_type_reference(node),
                SyntaxKind::TypeLiteral => return self.emit_type_literal(node),
                SyntaxKind::TupleType => return self.emit_tuple_type(node),
                SyntaxKind::UnionType => return self.emit_union_type(node),
                SyntaxKind::ConditionalType => return self.emit_conditional_type(node),
                SyntaxKind::TypeOperator => return self.emit_type_operator(node),
                SyntaxKind::IndexedAccessType => return self.emit_indexed_access_type(node),
                SyntaxKind::LiteralType => return self.emit_literal_type(node),
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
            (self.write)(self, &text_of_node);
        }
    }

    fn emit_property_signature(&mut self, node: &Node /*PropertySignature*/) {
        let node_as_property_signature = node.as_property_signature();
        self.emit_node_with_writer(
            Some(&*node_as_property_signature.name()),
            Printer::write_property,
        );
        self.emit_type_annotation(node_as_property_signature.maybe_type());
        self.write_trailing_semicolon();
    }

    fn emit_type_reference(&mut self, node: &Node /*TypeReferenceNode*/) {
        self.emit(Some(&*node.as_type_reference_node().type_name));
    }

    fn emit_type_literal(&mut self, node: &Node /*TypeLiteralNode*/) {
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

    fn emit_tuple_type(&mut self, node: &Node /*TupleTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO");
    }

    fn emit_union_type(&mut self, node: &Node /*UnionTypeNode*/) {
        self.emit_list(
            Some(node),
            Some(&node.as_union_or_intersection_type_node().types()),
            ListFormat::UnionTypeConstituents,
        );
    }

    fn emit_conditional_type(&mut self, node: &Node /*ConditionalTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO");
    }

    fn emit_type_operator(&mut self, node: &Node /*TypeOperatorNode*/) {
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

    fn emit_indexed_access_type(&mut self, node: &Node /*IndexedAccessType*/) {
        // unimplemented!()
        self.write_punctuation("TODO");
    }

    fn emit_literal_type(&mut self, node: &Node /*LiteralTypeNode*/) {
        self.emit_expression(&*node.as_literal_type_node().literal);
    }

    fn emit_node_with_writer(&mut self, node: Option<&Node>, writer: fn(&Printer, &str)) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let saved_write = self.write;
        self.write = writer;
        self.emit(Some(node));
        self.write = saved_write;
    }

    fn emit_type_annotation<TNodeRef: Borrow<Node>>(
        &mut self,
        node: Option<TNodeRef /*TypeNode*/>,
    ) {
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
        &mut self,
        parent_node: Option<TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
    ) {
        self.emit_node_list(Printer::emit, parent_node, children, format, None, None);
    }

    fn emit_node_list<TNode: Borrow<Node>>(
        &mut self,
        emit: fn(&mut Printer, Option<&Node>),
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
        self.writer_().write(s);
    }

    fn write_string_literal(&self, s: &str) {
        self.writer_().write_string_literal(s);
    }

    fn write_symbol(&self, s: &str, sym: &Symbol) {
        self.writer_().write_symbol(s, sym);
    }

    fn write_punctuation(&self, s: &str) {
        self.writer_().write_punctuation(s);
    }

    fn write_trailing_semicolon(&self) {
        self.writer_().write_trailing_semicolon(";");
    }

    fn write_keyword(&self, s: &str) {
        self.writer_().write_keyword(s);
    }

    fn write_space(&self) {
        self.writer_().write_space(" ");
    }

    fn write_property(&self, s: &str) {
        self.writer_().write_property(s);
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

        get_literal_text(node, self.current_source_file.clone(), flags)
    }
}
