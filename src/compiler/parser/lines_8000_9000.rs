use std::cell::{Cell, Ref, RefCell};
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    file_extension_is_one_of, for_each_child_bool, Debug_, DiagnosticMessage, Extension,
    IncrementalParserSyntaxCursorReparseTopLevelAwait, Node, NodeArray, NodeInterface, ParserType,
    ReadonlyTextRange, TextChangeRange,
};

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
