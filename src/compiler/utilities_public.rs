use crate::{NodeInterface, SyntaxKind, TextSpan};

fn create_text_span(start: usize, length: usize) -> TextSpan {
    TextSpan { start, length }
}

pub fn create_text_span_from_bounds(start: usize, end: usize) -> TextSpan {
    create_text_span(start, end - start)
}

pub fn is_binding_pattern<TNode: NodeInterface>(node: &TNode) -> bool {
    if true {
        let kind = node.kind();
        return kind == SyntaxKind::ArrayBindingPattern || kind == SyntaxKind::ObjectBindingPattern;
    }

    false
}
