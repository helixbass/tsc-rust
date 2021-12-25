use std::cell::{RefCell, RefMut};
use std::rc::Rc;

use crate::{
    is_keyword, token_to_string, Debug_, EmitHint, EmitTextWriter, LiteralTypeNode, Node,
    NodeInterface, Printer, PrinterOptions, SourceFile, TypeNode,
};

#[derive(PartialEq, Eq)]
enum PipelinePhase {
    Notification,
    Emit,
}

pub fn create_printer(printer_options: PrinterOptions) -> Printer {
    let printer = Printer::new();
    printer.reset();
    printer
}

impl Printer {
    pub fn new() -> Self {
        Self { writer: None }
    }

    fn maybe_writer(&self) -> Option<Rc<RefCell<dyn EmitTextWriter>>> {
        self.writer.clone()
    }

    fn writer_(&self) -> RefMut<dyn EmitTextWriter> {
        self.writer.clone().unwrap().borrow_mut()
    }

    pub fn write_node<TNode: NodeInterface>(
        &self,
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

    fn set_writer(&self, writer: Option<Rc<RefCell<dyn EmitTextWriter>>>) {
        self.writer = writer;
    }

    fn reset(&self) {
        self.set_writer(None);
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

    fn pipeline_emit_with_hint_worker<TNode: NodeInterface>(&self, hint: EmitHint, node: &TNode) {
        if hint == EmitHint::Unspecified {
            match &*node.node_wrapper() {
                Node::TypeNode(type_node) => match type_node {
                    TypeNode::LiteralTypeNode(literal_type_node) => {
                        return self.emit_literal_type(literal_type_node)
                    }
                    _ => (),
                },
                _ => (),
            }
        }
        if is_keyword(node.kind()) {
            return self.write_token_node(node, Printer::write_keyword);
        }
        unimplemented!()
    }

    fn emit_literal_type(&self, node: &LiteralTypeNode) {
        self.emit_expression(&*node.literal);
    }

    fn write_keyword(&self, s: &str) {
        self.writer_().write_keyword(s);
    }

    fn write_token_node<TNode: NodeInterface>(&self, node: &TNode, writer: fn(&Printer, &str)) {
        writer(self, token_to_string(node.kind()).unwrap());
    }
}
