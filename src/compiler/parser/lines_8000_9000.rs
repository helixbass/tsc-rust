use std::convert::TryInto;
use std::rc::Rc;

use super::{ParseJSDocCommentWorker, PropertyLikeParse};
use crate::{
    append, is_identifier, is_type_reference_node, token_is_identifier_or_keyword, BaseJSDocTag,
    DiagnosticMessage, Identifier, JSDocAugmentsTag, JSDocImplementsTag, JSDocPropertyLikeTag,
    JSDocTypeExpression, Node, NodeInterface, ReadonlyTextRange, StringOrNodeArray, SyntaxKind,
    TextChangeRange,
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
        let link_type = self.parser.try_parse(|| self.parse_jsdoc_link_prefix())?;
        self.parser.next_token_jsdoc();
        self.skip_whitespace();
        let p2 = self.parser.get_node_pos();
        let mut name: Option<Rc<Node /*EntityName | JSDocMemberName*/>> =
            if token_is_identifier_or_keyword(self.parser.token()) {
                Some(self.parser.parse_entity_name(true, None).wrap())
            } else {
                None
            };
        if name.is_some() {
            while self.parser.token() == SyntaxKind::PrivateIdentifier {
                self.parser.re_scan_hash_token();
                self.parser.next_token_jsdoc();
                name = Some(
                    self.parser
                        .finish_node(
                            self.parser.factory.create_jsdoc_member_name(
                                self.parser,
                                name.clone().unwrap(),
                                self.parser.parse_identifier(None, None).wrap(),
                            ),
                            p2,
                            None,
                        )
                        .into(),
                );
            }
        }
        let mut text = vec![];
        while !matches!(
            self.parser.token(),
            SyntaxKind::CloseBraceToken | SyntaxKind::NewLineTrivia | SyntaxKind::EndOfFileToken
        ) {
            text.push(self.parser.scanner().get_token_text());
            self.parser.next_token_jsdoc();
        }
        let create = |name, text| -> Node {
            if link_type == "link" {
                self.parser
                    .factory
                    .create_jsdoc_link(self.parser, name, text)
                    .into()
            } else if link_type == "linkcode" {
                self.parser
                    .factory
                    .create_jsdoc_link_code(self.parser, name, text)
                    .into()
            } else {
                self.parser
                    .factory
                    .create_jsdoc_link_plain(self.parser, name, text)
                    .into()
            }
        };
        Some(self.parser.finish_node(
            create(name, text.join("")),
            start.try_into().unwrap(),
            Some(self.parser.scanner().get_text_pos().try_into().unwrap()),
        ))
    }

    pub(super) fn parse_jsdoc_link_prefix(&self) -> Option<String> {
        self.skip_whitespace_or_asterisk();
        if self.parser.token() == SyntaxKind::OpenBraceToken
            && self.parser.next_token_jsdoc() == SyntaxKind::AtToken
            && token_is_identifier_or_keyword(self.parser.next_token_jsdoc())
        {
            let kind = self.parser.scanner().get_token_value();
            if matches!(&*kind, "link" | "linkcode" | "linkplain") {
                return Some(kind);
            }
        }
        None
    }

    pub(super) fn parse_unknown_tag(
        &mut self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocAuthorTag*/ {
        self.parser.finish_node(
            self.parser.factory.create_jsdoc_unknown_tag(
                self.parser,
                tag_name,
                self.parse_trailing_tag_comments(
                    start,
                    self.parser.get_node_pos().try_into().unwrap(),
                    indent,
                    indent_text,
                ),
            ),
            start.try_into().unwrap(),
            None,
        )
    }

    pub(super) fn add_tag(&mut self, tag: Option<Rc<Node /*JSDocTag*/>>) {
        if tag.is_none() {
            return;
        }
        let tag = tag.unwrap();
        let tag_end = tag.end();
        if self.tags.is_none() {
            let tag_pos = tag.pos();
            self.tags = Some(vec![tag]);
            self.tags_pos = Some(tag_pos);
        } else {
            self.tags.as_mut().unwrap().push(tag);
        }
        self.tags_end = Some(tag_end);
    }

    pub(super) fn try_parse_type_expression(&self) -> Option<Rc<Node>> {
        self.skip_whitespace_or_asterisk();
        if self.parser.token() == SyntaxKind::OpenBraceToken {
            Some(self.parser.JSDocParser_parse_jsdoc_type_expression(None))
        } else {
            None
        }
    }

    pub(super) fn parse_bracket_name_in_property_and_param_tag(
        &self,
    ) -> ParseBracketNameInPropertyAndParamTagReturn {
        let is_bracketed = self.parse_optional_jsdoc(SyntaxKind::OpenBracketToken);
        if is_bracketed {
            self.skip_whitespace();
        }
        let is_backquoted = self.parse_optional_jsdoc(SyntaxKind::BacktickToken);
        let name = self.parse_jsdoc_entity_name();
        if is_backquoted {
            self.parser
                .parse_expected_token_jsdoc(SyntaxKind::BacktickToken);
        }
        if is_bracketed {
            self.skip_whitespace();
            if self
                .parser
                .parse_optional_token(SyntaxKind::EqualsToken)
                .is_some()
            {
                self.parser.parse_expression();
            }

            self.parser
                .parse_expected(SyntaxKind::CloseBracketToken, None, None);
        }

        ParseBracketNameInPropertyAndParamTagReturn { name, is_bracketed }
    }

    pub(super) fn is_object_or_object_array_type_reference(
        &self,
        node: &Node, /*TypeNode*/
    ) -> bool {
        match node.kind() {
            SyntaxKind::ObjectKeyword => true,
            SyntaxKind::ArrayType => self
                .is_object_or_object_array_type_reference(&node.as_array_type_node().element_type),
            _ => {
                if !is_type_reference_node(node) {
                    return false;
                }
                let node_as_type_reference_node = node.as_type_reference_node();
                is_identifier(&node_as_type_reference_node.type_name)
                    && &*node_as_type_reference_node
                        .type_name
                        .as_identifier()
                        .escaped_text
                        == "Object"
                    && node_as_type_reference_node.type_arguments.is_none()
            }
        }
    }

    pub(super) fn parse_parameter_or_property_tag(
        &mut self,
        start: usize,
        tag_name: Rc<Node /*Identifier*/>,
        target: PropertyLikeParse,
        indent: usize,
    ) -> JSDocPropertyLikeTag /*JSDocParameterTag | JSDocPropertyTag*/ {
        let mut type_expression = self.try_parse_type_expression();
        let mut is_name_first = type_expression.is_none();
        self.skip_whitespace_or_asterisk();

        let ParseBracketNameInPropertyAndParamTagReturn { name, is_bracketed } =
            self.parse_bracket_name_in_property_and_param_tag();
        let name = name.wrap();
        let indent_text = self.skip_whitespace_or_asterisk();

        if is_name_first
            && !self
                .parser
                .look_ahead_bool(|| self.parse_jsdoc_link_prefix().is_some())
        {
            type_expression = self.try_parse_type_expression();
        }

        let comment = self.parse_trailing_tag_comments(
            start,
            self.parser.get_node_pos().try_into().unwrap(),
            indent,
            &indent_text,
        );

        let nested_type_literal = if target != PropertyLikeParse::CallbackParameter {
            self.parse_nested_type_literal(type_expression.clone(), &name, target, indent)
        } else {
            None
        };
        if let Some(nested_type_literal) = nested_type_literal {
            type_expression = Some(nested_type_literal.into());
            is_name_first = true;
        }
        let result = if target == PropertyLikeParse::Property {
            self.parser.factory.create_jsdoc_property_tag(
                self.parser,
                Some(tag_name),
                name,
                is_bracketed,
                type_expression,
                Some(is_name_first),
                comment,
            )
        } else {
            self.parser.factory.create_jsdoc_parameter_tag(
                self.parser,
                Some(tag_name),
                name,
                is_bracketed,
                type_expression,
                Some(is_name_first),
                comment,
            )
        };
        self.parser
            .finish_node(result, start.try_into().unwrap(), None)
    }

    pub(super) fn parse_nested_type_literal(
        &self,
        type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
        name: &Node, /*EntityName*/
        target: PropertyLikeParse,
        indent: usize,
    ) -> Option<JSDocTypeExpression> {
        if let Some(type_expression) = type_expression.filter(|type_expression| {
            self.is_object_or_object_array_type_reference(
                &type_expression.as_jsdoc_type_expression().type_,
            )
        }) {
            let pos = self.parser.get_node_pos();
            let mut child: Option<Node> = None;
            let mut children: Option<Vec<Rc<Node /*JSDocPropertyLikeTag*/>>> = None;
            while let Some(child) = {
                child = self.parser.try_parse(|| {
                    self.parse_child_parameter_or_property_tag(target, indent, Some(name))
                });
                child
            } {
                if matches!(
                    child.kind(),
                    SyntaxKind::JSDocParameterTag | SyntaxKind::JSDocPropertyTag
                ) {
                    if children.is_none() {
                        children = Some(vec![]);
                    }
                    append(children.as_mut().unwrap(), Some(child.wrap()));
                }
            }
            if let Some(children) = children {
                let literal = self.parser.finish_node(
                    self.parser.factory.create_jsdoc_type_literal(
                        self.parser,
                        Some(children),
                        Some(
                            type_expression.as_jsdoc_type_expression().type_.kind()
                                == SyntaxKind::ArrayType,
                        ),
                    ),
                    pos,
                    None,
                );
                return Some(
                    self.parser.finish_node(
                        self.parser
                            .factory
                            .create_jsdoc_type_expression(self.parser, literal.into()),
                        pos,
                        None,
                    ),
                );
            }
        }
        None
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

    pub(super) fn parse_child_parameter_or_property_tag(
        &self,
        target: PropertyLikeParse,
        indent: usize,
        name: Option<&Node /*EntityName*/>,
    ) -> Option<Node /*JSDocTypeTag | JSDocPropertyTag | JSDocParameterTag*/> {
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

    pub(super) fn parse_jsdoc_entity_name(&self) -> Node /*EntityName*/ {
        let mut entity: Node /*EntityName*/ = self.parse_jsdoc_identifier_name(None).into();
        if self.parser.parse_optional(SyntaxKind::OpenBracketToken) {
            self.parser
                .parse_expected(SyntaxKind::CloseBracketToken, None, None);
        }
        while self.parser.parse_optional(SyntaxKind::DotToken) {
            let name = self.parse_jsdoc_identifier_name(None);
            if self.parser.parse_optional(SyntaxKind::OpenBracketToken) {
                self.parser
                    .parse_expected(SyntaxKind::CloseBracketToken, None, None);
            }
            entity = self
                .parser
                .create_qualified_name(entity.wrap(), name.into())
                .into();
        }
        entity
    }

    pub(super) fn parse_jsdoc_identifier_name(
        &self,
        message: Option<&DiagnosticMessage>,
    ) -> Identifier {
        unimplemented!()
    }
}

pub(super) struct ParseBracketNameInPropertyAndParamTagReturn {
    pub name: Node, /*EntityName*/
    pub is_bracketed: bool,
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
