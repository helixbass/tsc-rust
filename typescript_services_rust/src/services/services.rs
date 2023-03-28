use gc::Gc;
use std::convert::TryFrom;

use typescript_rust::{
    get_source_file_of_node, position_is_synthesized, CompilerOptions, CompilerOptionsBuilder,
    Debug_, JsxEmit, Node, NodeInterface, ReadonlyTextRange, ScriptTarget, SourceFileLike,
};

pub trait NodeServicesInterface {
    fn assert_has_real_position(&self, message: Option<&str>);
    fn get_source_file(&self) -> Gc<Node /*SourceFile*/>;
    // TODO: this should be updated to return Rc<SourceText> or whatever
    fn get_full_text(&self, source_file: Option<&Node /*SourceFile*/>) -> String;
}

impl NodeServicesInterface for Node {
    fn assert_has_real_position(&self, message: Option<&str>) {
        Debug_.assert(
            !position_is_synthesized(self.pos()) && !position_is_synthesized(self.end()),
            Some(message.unwrap_or("Node must have a real position for this operation")),
        );
    }

    fn get_source_file(&self) -> Gc<Node /*SourceFile*/> {
        get_source_file_of_node(self)
    }

    fn get_full_text(&self, source_file: Option<&Node /*SourceFile*/>) -> String {
        self.assert_has_real_position(None);

        let source_file = source_file.map_or_else(
            || self.get_source_file(),
            |source_file| source_file.node_wrapper(),
        );
        let source_file_text = source_file.as_source_file().text();
        source_file_text[usize::try_from(self.pos()).unwrap()..usize::try_from(self.end()).unwrap()]
            .to_owned()
    }
}

pub fn get_default_compiler_options() -> CompilerOptions {
    CompilerOptionsBuilder::default()
        .target(Some(ScriptTarget::ES5))
        .jsx(Some(JsxEmit::Preserve))
        .build()
        .unwrap()
}
