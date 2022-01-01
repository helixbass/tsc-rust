use std::borrow::Borrow;
use std::cell::{RefCell, RefMut};
use std::rc::Rc;

use crate::{
    id_text, is_expression, is_identifier, is_keyword, token_to_string, Debug_, EmitHint,
    EmitTextWriter, Expression, HasTypeInterface, Identifier, ListFormat, LiteralTypeNode,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, Printer, PrinterOptions,
    PropertySignature, SourceFile, Symbol, TypeElement, TypeLiteralNode, TypeNode,
    TypeReferenceNode,
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

    pub fn write_node<TNode: NodeInterface>(
        &mut self,
        hint: EmitHint,
        node: &TNode,
        source_file: Option<Rc<SourceFile>>,
        output: Rc<RefCell<dyn EmitTextWriter>>,
    ) {
        let previous_writer = self.maybe_writer();
        self.set_writer(Some(output));
        self.print(hint, node, source_file);
        self.reset();
        self.set_writer(previous_writer);
    }

    fn print<TNode: NodeInterface>(
        &self,
        hint: EmitHint,
        node: &TNode,
        source_file: Option<Rc<SourceFile>>,
    ) {
        if let Some(source_file) = source_file {
            unimplemented!()
        }

        self.pipeline_emit(hint, node);
    }

    fn set_writer(&mut self, writer: Option<Rc<RefCell<dyn EmitTextWriter>>>) {
        self.writer = writer;
    }

    fn reset(&mut self) {
        self.set_writer(None);
    }

    fn emit(&self, node: Option<&Node>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        self.pipeline_emit(EmitHint::Unspecified, node);
    }

    fn emit_expression<TNode: NodeInterface>(&self, node: &TNode /*Expression*/) {
        self.pipeline_emit(EmitHint::Expression, node);
    }

    fn pipeline_emit<TNode: NodeInterface>(&self, emit_hint: EmitHint, node: &TNode) {
        let pipeline_phase = self.get_pipeline_phase(PipelinePhase::Notification, emit_hint, node);
        pipeline_phase(self, emit_hint, node);
    }

    fn get_pipeline_phase<TNode: NodeInterface>(
        &self,
        phase: PipelinePhase,
        emit_hint: EmitHint,
        node: &TNode,
    ) -> fn(&Printer, EmitHint, &TNode) {
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

    fn pipeline_emit_with_hint<TNode: NodeInterface>(&self, hint: EmitHint, node: &TNode) {
        if false {
            unimplemented!()
        } else {
            self.pipeline_emit_with_hint_worker(hint, node);
        }
    }

    fn pipeline_emit_with_hint_worker<TNode: NodeInterface>(
        &self,
        mut hint: EmitHint,
        node: &TNode,
    ) {
        if hint == EmitHint::Unspecified {
            match &*node.node_wrapper() {
                Node::Expression(Expression::Identifier(identifier)) => {
                    return self.emit_identifier(identifier)
                }
                Node::TypeElement(TypeElement::PropertySignature(property_signature)) => {
                    return self.emit_property_signature(property_signature)
                }
                Node::TypeNode(TypeNode::TypeReferenceNode(type_reference_node)) => {
                    return self.emit_type_reference(type_reference_node)
                }
                Node::TypeNode(TypeNode::TypeLiteralNode(type_literal_node)) => {
                    return self.emit_type_literal(type_literal_node)
                }
                Node::TypeNode(TypeNode::LiteralTypeNode(literal_type_node)) => {
                    return self.emit_literal_type(literal_type_node)
                }
                _ => (),
            }
            if is_expression(node) {
                hint = EmitHint::Expression;
            }
        }
        // if hint == EmitHint::Expression {
        // }
        if is_keyword(node.kind()) {
            return self.write_token_node(node, Printer::write_keyword);
        }
        unimplemented!()
    }

    fn emit_identifier(&self, node: &Identifier) {
        let text_of_node = self.get_text_of_node(node, Some(false));
        if let Some(symbol) = node.maybe_symbol() {
            self.write_symbol(&text_of_node, symbol);
        } else {
            (self.write)(self, &text_of_node);
        }
    }

    fn emit_property_signature(&self, node: &PropertySignature) {
        self.emit_node_with_writer(Some(&*node.name()), Printer::write_property);
        self.emit_type_annotation(node.type_());
    }

    fn emit_type_reference(&self, node: &TypeReferenceNode) {
        self.emit(Some(&*node.type_name));
    }

    fn emit_type_literal(&self, node: &TypeLiteralNode) {
        self.write_punctuation("{");
        let flags = if true {
            ListFormat::SingleLineTypeLiteralMembers
        } else {
            unimplemented!()
        };
        self.emit_list(
            Some(node),
            Some(&node.members),
            flags | ListFormat::NoSpaceIfEmpty,
        );
        self.write_punctuation("}");
    }

    fn emit_literal_type(&self, node: &LiteralTypeNode) {
        self.emit_expression(&*node.literal);
    }

    fn emit_node_with_writer(&self, node: Option<&Node>, writer: fn(&Printer, &str)) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let saved_write = self.write;
        self.write = writer;
        self.emit(Some(node));
        self.write = saved_write;
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
            _ => unimplemented!(),
        }
    }

    fn emit_list<TNode: NodeInterface>(
        &self,
        parent_node: Option<&TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
    ) {
        self.emit_node_list(Printer::emit, parent_node, children, format, None, None);
    }

    fn emit_node_list<TNode: NodeInterface>(
        &self,
        emit: fn(&Printer, Option<&Node>),
        parent_node: Option<&TNode>,
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

    fn write_symbol(&self, s: &str, sym: Rc<Symbol>) {
        self.writer_().write_symbol(s, &*sym);
    }

    fn write_punctuation(&self, s: &str) {
        self.writer_().write_punctuation(s);
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

    fn write_token_node<TNode: NodeInterface>(&self, node: &TNode, writer: fn(&Printer, &str)) {
        writer(self, token_to_string(node.kind()).unwrap());
    }

    fn get_text_of_node<TNode: NodeInterface>(
        &self,
        node: &TNode,
        include_trivia: Option<bool>,
    ) -> String {
        if false {
            unimplemented!()
        } else if (is_identifier(node) || false) && true {
            return id_text(node);
        }

        unimplemented!()
    }
}
