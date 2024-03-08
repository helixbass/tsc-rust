use std::convert::TryFrom;

use typescript_rust::{
    get_source_file_of_node, get_token_pos_of_node, id_arena::Id, position_is_synthesized,
    CompilerOptions, CompilerOptionsBuilder, Debug_, HasArena, InArena, JsxEmit, Node,
    NodeInterface, ReadonlyTextRange, ScriptTarget, SourceFileLike,
};

pub trait NodeServicesInterface {
    fn assert_has_real_position(&self, message: Option<&str>);
    fn get_source_file(&self, arena: &impl HasArena) -> Id<Node /*SourceFile*/>;
    fn get_start(
        &self,
        source_file: Option<Id<Node> /*&impl SourceFileLike*/>,
        include_js_doc_comment: Option<bool>,
        arena: &impl HasArena,
    ) -> usize;
    fn get_end(&self) -> usize;
    // TODO: this should be updated to return Rc<SourceText> or whatever
    fn get_full_text(
        &self,
        source_file: Option<Id<Node /*SourceFile*/>>,
        arena: &impl HasArena,
    ) -> String;
    fn get_text(
        &self,
        source_file: Option<Id<Node /*SourceFile*/>>,
        arena: &impl HasArena,
    ) -> String;
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

    fn get_start(
        &self,
        source_file: Option<Id<Node> /*&impl SourceFileLike*/>,
        include_js_doc_comment: Option<bool>,
        arena: &impl HasArena,
    ) -> usize {
        self.assert_has_real_position(None);
        usize::try_from(get_token_pos_of_node(
            self.arena_id(),
            source_file,
            include_js_doc_comment,
            arena,
        ))
        .unwrap()
    }

    fn get_end(&self) -> usize {
        self.assert_has_real_position(None);
        usize::try_from(self.end()).unwrap()
    }

    fn get_full_text(
        &self,
        source_file: Option<Id<Node /*SourceFile*/>>,
        arena: &impl HasArena,
    ) -> String {
        self.assert_has_real_position(None);

        let source_file = source_file.unwrap_or_else(|| self.get_source_file(arena));
        let source_file_ref = source_file.ref_(arena);
        // TODO: this doesn't look right re bytes vs chars (same below in get_text())?
        let source_file_text = source_file_ref.as_source_file().text();
        source_file_text[usize::try_from(self.pos()).unwrap()..usize::try_from(self.end()).unwrap()]
            .to_owned()
    }

    fn get_text(
        &self,
        source_file: Option<Id<Node /*SourceFile*/>>,
        arena: &impl HasArena,
    ) -> String {
        self.assert_has_real_position(None);
        let source_file = source_file.unwrap_or_else(|| self.get_source_file(arena));
        let source_file_ref = source_file.ref_(arena);
        let source_file_text = source_file_ref.as_source_file().text();
        source_file_text[self.get_start(Some(source_file), None, arena)..self.get_end()].to_owned()
    }
}

pub fn get_default_compiler_options() -> CompilerOptions {
    CompilerOptionsBuilder::default()
        .target(ScriptTarget::ES5)
        .jsx(JsxEmit::Preserve)
        .build()
        .unwrap()
}
