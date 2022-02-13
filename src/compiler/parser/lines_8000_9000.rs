use std::rc::Rc;

use crate::{Node, TextChangeRange};

pub fn IncrementalParser() -> IncrementalParserType {
    IncrementalParserType::new()
}

pub struct IncrementalParserType {}

impl IncrementalParserType {
    pub fn new() -> Self {
        Self {}
    }

    pub fn update_source_file(
        &self,
        source_file: &Node, /*SourceFile*/
        new_text: String,
        text_change_range: TextChangeRange,
        aggressive_checks: bool,
    ) -> Rc<Node /*SourceFile*/> {
        unimplemented!()
    }
}
