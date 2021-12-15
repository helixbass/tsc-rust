use crate::TextSpan;

fn create_text_span(start: usize, length: usize) -> TextSpan {
    TextSpan { start, length }
}

pub fn create_text_span_from_bounds(start: usize, end: usize) -> TextSpan {
    create_text_span(start, end - start)
}
