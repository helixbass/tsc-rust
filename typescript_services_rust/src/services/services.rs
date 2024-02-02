use std::convert::TryFrom;

use typescript_rust::{
    get_source_file_of_node, position_is_synthesized, CompilerOptions, CompilerOptionsBuilder,
    Debug_, JsxEmit, Node, NodeInterface, ReadonlyTextRange, ScriptTarget, SourceFileLike,
    id_arena::Id, HasArena, InArena,
};

pub trait NodeServicesInterface {
    fn assert_has_real_position(&self, message: Option<&str>);
    fn get_source_file(&self, arena: &impl HasArena) -> Id<Node /*SourceFile*/>;
    // TODO: this should be updated to return Rc<SourceText> or whatever
    fn get_full_text(&self, source_file: Option<Id<Node /*SourceFile*/>>, arena: &impl HasArena) -> String;
}

impl NodeServicesInterface for Node {
    fn assert_has_real_position(&self, message: Option<&str>) {
        Debug_.assert(
            !position_is_synthesized(self.pos()) && !position_is_synthesized(self.end()),
            Some(message.unwrap_or("Node must have a real position for this operation")),
        );
    }

    fn get_source_file(&self, arena: &impl HasArena) -> Id<Node /*SourceFile*/> {
        get_source_file_of_node(self.arena_id(), arena)
    }

    fn get_full_text(&self, source_file: Option<Id<Node /*SourceFile*/>>, arena: &impl HasArena) -> String {
        self.assert_has_real_position(None);

        let source_file = source_file.unwrap_or_else(
            || self.get_source_file(arena),
        );
        let source_file_ref = source_file.ref_(arena);
        let source_file_text = source_file_ref.as_source_file().text();
        source_file_text[usize::try_from(self.pos()).unwrap()..usize::try_from(self.end()).unwrap()]
            .to_owned()
    }
}

pub fn get_default_compiler_options() -> CompilerOptions {
    CompilerOptionsBuilder::default()
        .target(ScriptTarget::ES5)
        .jsx(JsxEmit::Preserve)
        .build()
        .unwrap()
}
