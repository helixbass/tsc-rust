use std::rc::Rc;

use super::{ParseJSDocCommentWorker, PropertyLikeParse};
use crate::{
    BaseJSDocTag, DiagnosticMessage, Identifier, JSDocAugmentsTag, JSDocImplementsTag,
    JSDocPropertyLikeTag, Node, StringOrNodeArray, SyntaxKind, TextChangeRange,
};

impl<'parser> ParseJSDocCommentWorker<'parser> {
    pub(super) fn is_next_jsdoc_token_whitespace(&self) -> bool {
        let next = self.parser.next_token_jsdoc();
        matches!(
            next,
            SyntaxKind::WhitespaceTrivia | SyntaxKind::NewLineTrivia
        )
    }

    pub(super) fn parse_jsdoc_link(&self, start: usize) -> Option<Node> {
        unimplemented!()
    }

    pub(super) fn parse_unknown_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocAuthorTag*/ {
        unimplemented!()
    }

    pub(super) fn add_tag(&self, tag: Option<Rc<Node /*JSDocTag*/>>) {
        unimplemented!()
    }

    pub(super) fn parse_parameter_or_property_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        target: PropertyLikeParse,
        indent: usize,
    ) -> JSDocPropertyLikeTag /*JSDocParameterTag | JSDocPropertyTag*/ {
        unimplemented!()
    }

    pub(super) fn parse_return_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocReturnTag*/ {
        unimplemented!()
    }

    pub(super) fn parse_type_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocTypeTag*/ {
        unimplemented!()
    }

    pub(super) fn parse_see_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocSeeTag*/ {
        unimplemented!()
    }

    pub(super) fn parse_author_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocAuthorTag*/ {
        unimplemented!()
    }

    pub(super) fn parse_implements_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> JSDocImplementsTag {
        unimplemented!()
    }

    pub(super) fn parse_augments_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> JSDocAugmentsTag {
        unimplemented!()
    }

    pub(super) fn parse_simple_tag<
        TCreateTag: FnOnce(Option<Rc<Node /*Identifier*/>>, Option<StringOrNodeArray>) -> BaseJSDocTag,
    >(
        &self,
        start: usize,
        create_tag: TCreateTag,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocTag*/ {
        unimplemented!()
    }

    pub(super) fn parse_this_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocThisTag*/ {
        unimplemented!()
    }

    pub(super) fn parse_enum_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocEnumTag*/ {
        unimplemented!()
    }

    pub(super) fn parse_typedef_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocTypedefTag*/ {
        unimplemented!()
    }

    pub(super) fn parse_callback_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocCallbackTag*/ {
        unimplemented!()
    }

    pub(super) fn parse_template_tag(
        &self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocTemplateTag*/ {
        unimplemented!()
    }

    pub(super) fn parse_optional_jsdoc(&self, t: SyntaxKind /*JSDocSyntaxKind*/) -> bool {
        unimplemented!()
    }

    pub(super) fn parse_jsdoc_identifier_name(
        &self,
        message: Option<&DiagnosticMessage>,
    ) -> Identifier {
        unimplemented!()
    }
}

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
