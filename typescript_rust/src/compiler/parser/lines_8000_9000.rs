use gc::Gc;
use std::convert::TryInto;
use std::rc::Rc;

use super::{ParseJSDocCommentWorker, PropertyLikeParse};
use crate::{
    add_related_info, append, concatenate, create_detached_diagnostic, is_identifier,
    is_jsdoc_return_tag, is_jsdoc_type_tag, is_type_reference_node, last_or_undefined,
    node_is_missing, some, token_is_identifier_or_keyword, BaseJSDocTag, BaseJSDocTypeLikeTag,
    Debug_, DiagnosticMessage, Diagnostics, ExpressionWithTypeArguments, HasTypeArgumentsInterface,
    Identifier, JSDocAugmentsTag, JSDocCallbackTag, JSDocImplementsTag, JSDocPropertyLikeTag,
    JSDocSeeTag, JSDocTemplateTag, JSDocText, JSDocTypeExpression, JSDocTypedefTag, Node,
    NodeArray, NodeFlags, NodeInterface, ReadonlyTextRange, StringOrNodeArray, SyntaxKind,
    TextChangeRange, TypeParameterDeclaration,
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
        let mut name: Option<Gc<Node /*EntityName | JSDocMemberName*/>> =
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
                            self.parser.factory().create_jsdoc_member_name(
                                self.parser,
                                name.clone().unwrap(),
                                self.parser.parse_identifier(None, None).wrap(),
                            ),
                            p2,
                            None,
                        )
                        .wrap(),
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
                    .factory()
                    .create_jsdoc_link(self.parser, name, text)
                    .into()
            } else if link_type == "linkcode" {
                self.parser
                    .factory()
                    .create_jsdoc_link_code(self.parser, name, text)
                    .into()
            } else {
                self.parser
                    .factory()
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
            let scanner = self.parser.scanner();
            let kind = scanner.get_token_value();
            if matches!(&**kind, "link" | "linkcode" | "linkplain") {
                return Some(kind.clone());
            }
        }
        None
    }

    pub(super) fn parse_unknown_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocAuthorTag*/ {
        self.parser.finish_node(
            self.parser.factory().create_jsdoc_unknown_tag(
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

    pub(super) fn add_tag(&mut self, tag: Option<Gc<Node /*JSDocTag*/>>) {
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

    pub(super) fn try_parse_type_expression(&self) -> Option<Gc<Node>> {
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
                    && node_as_type_reference_node.maybe_type_arguments().is_none()
            }
        }
    }

    pub(super) fn parse_parameter_or_property_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
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
            type_expression = Some(nested_type_literal.wrap());
            is_name_first = true;
        }
        let result = if target == PropertyLikeParse::Property {
            self.parser.factory().create_jsdoc_property_tag(
                self.parser,
                Some(tag_name),
                name,
                is_bracketed,
                type_expression,
                Some(is_name_first),
                comment,
            )
        } else {
            self.parser.factory().create_jsdoc_parameter_tag(
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
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
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
            let mut children: Option<Vec<Gc<Node /*JSDocPropertyLikeTag*/>>> = None;
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
                    self.parser.factory().create_jsdoc_type_literal(
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
                            .factory()
                            .create_jsdoc_type_expression(self.parser, literal.wrap()),
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
        tag_name: Gc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTypeLikeTag /*JSDocReturnTag*/ {
        if some(
            self.tags.as_deref(),
            Some(|tag: &Gc<Node>| is_jsdoc_return_tag(tag)),
        ) {
            self.parser.parse_error_at(
                tag_name.pos(),
                self.parser.scanner().get_token_pos().try_into().unwrap(),
                &Diagnostics::_0_tag_already_specified,
                Some(vec![(&*tag_name.as_identifier().escaped_text).to_owned()]),
            );
        }

        let type_expression = self.try_parse_type_expression();
        self.parser.finish_node(
            self.parser.factory().create_jsdoc_return_tag(
                self.parser,
                Some(tag_name),
                type_expression,
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

    pub(super) fn parse_type_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
        indent: Option<usize>,
        indent_text: Option<&str>,
    ) -> BaseJSDocTypeLikeTag /*JSDocTypeTag*/ {
        if some(
            self.tags.as_deref(),
            Some(|tag: &Gc<Node>| is_jsdoc_type_tag(tag)),
        ) {
            self.parser.parse_error_at(
                tag_name.pos(),
                self.parser.scanner().get_token_pos().try_into().unwrap(),
                &Diagnostics::_0_tag_already_specified,
                Some(vec![(&*tag_name.as_identifier().escaped_text).to_owned()]),
            );
        }

        let type_expression = self
            .parser
            .JSDocParser_parse_jsdoc_type_expression(Some(true));
        let comments = match (indent, indent_text) {
            (Some(indent), Some(indent_text)) => self.parse_trailing_tag_comments(
                start,
                self.parser.get_node_pos().try_into().unwrap(),
                indent,
                indent_text,
            ),
            _ => None,
        };
        self.parser.finish_node(
            self.parser.factory().create_jsdoc_type_tag(
                self.parser,
                Some(tag_name),
                Some(type_expression),
                comments,
            ),
            start.try_into().unwrap(),
            None,
        )
    }

    pub(super) fn parse_see_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
        indent: Option<usize>,
        indent_text: Option<&str>,
    ) -> JSDocSeeTag /*JSDocSeeTag*/ {
        let is_markdown_or_jsdoc_link = self.parser.token() == SyntaxKind::OpenBracketToken
            || self.parser.look_ahead_bool(|| {
                self.parser.next_token_jsdoc() == SyntaxKind::AtToken
                    && token_is_identifier_or_keyword(self.parser.next_token_jsdoc())
                    && &*self.parser.scanner().get_token_value() == "link"
            });
        let name_expression = if is_markdown_or_jsdoc_link {
            None
        } else {
            Some(self.parser.JSDocParser_parse_jsdoc_name_reference())
        };
        let comments = match (indent, indent_text) {
            (Some(indent), Some(indent_text)) => self.parse_trailing_tag_comments(
                start,
                self.parser.get_node_pos().try_into().unwrap(),
                indent,
                indent_text,
            ),
            _ => None,
        };
        self.parser.finish_node(
            self.parser.factory().create_jsdoc_see_tag(
                self.parser,
                Some(tag_name),
                name_expression,
                comments,
            ),
            start.try_into().unwrap(),
            None,
        )
    }

    pub(super) fn parse_author_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocAuthorTag*/ {
        let comment_start = self.parser.get_node_pos();
        let text_only = self.parse_author_name_and_email();
        let mut comment_end = self.parser.scanner().get_start_pos();
        let comments = self.parse_trailing_tag_comments(start, comment_end, indent, indent_text);
        if comments.is_none() {
            comment_end = self.parser.scanner().get_start_pos();
        }
        let all_parts: StringOrNodeArray = match comments {
            Some(StringOrNodeArray::NodeArray(comments)) => self
                .parser
                .create_node_array(
                    concatenate(
                        vec![self
                            .parser
                            .finish_node(
                                text_only,
                                comment_start,
                                Some(comment_end.try_into().unwrap()),
                            )
                            .wrap()],
                        comments.to_vec(),
                    ),
                    comment_start,
                    None,
                    None,
                )
                .into(),
            None => self
                .parser
                .create_node_array(
                    concatenate(
                        vec![self
                            .parser
                            .finish_node(
                                text_only,
                                comment_start,
                                Some(comment_end.try_into().unwrap()),
                            )
                            .wrap()],
                        vec![],
                    ),
                    comment_start,
                    None,
                    None,
                )
                .into(),
            Some(StringOrNodeArray::String(comments)) => {
                format!("{}{}", text_only.text, comments).into()
            }
        };
        self.parser.finish_node(
            self.parser.factory().create_jsdoc_author_tag(
                self.parser,
                Some(tag_name),
                Some(all_parts),
            ),
            start.try_into().unwrap(),
            None,
        )
    }

    pub(super) fn parse_author_name_and_email(&self) -> JSDocText {
        let mut comments = vec![];
        let mut in_email = false;
        let mut token = self.parser.scanner().get_token();
        while !matches!(
            token,
            SyntaxKind::EndOfFileToken | SyntaxKind::NewLineTrivia
        ) {
            if token == SyntaxKind::LessThanToken {
                in_email = true;
            } else if token == SyntaxKind::AtToken && !in_email {
                break;
            } else if token == SyntaxKind::GreaterThanToken && in_email {
                comments.push(self.parser.scanner().get_token_text());
                let token_pos = self.parser.scanner().get_token_pos();
                self.parser.scanner_mut().set_text_pos(token_pos + 1);
                break;
            }
            comments.push(self.parser.scanner().get_token_text());
            token = self.parser.next_token_jsdoc();
        }

        self.parser
            .factory()
            .create_jsdoc_text(self.parser, comments.join(""))
    }

    pub(super) fn parse_implements_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
        margin: usize,
        indent_text: &str,
    ) -> JSDocImplementsTag {
        let class_name = self.parse_expression_with_type_arguments_for_augments();
        self.parser.finish_node(
            self.parser.factory().create_jsdoc_implements_tag(
                self.parser,
                Some(tag_name),
                class_name.wrap(),
                self.parse_trailing_tag_comments(
                    start,
                    self.parser.get_node_pos().try_into().unwrap(),
                    margin,
                    indent_text,
                ),
            ),
            start.try_into().unwrap(),
            None,
        )
    }

    pub(super) fn parse_augments_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
        margin: usize,
        indent_text: &str,
    ) -> JSDocAugmentsTag {
        let class_name = self.parse_expression_with_type_arguments_for_augments();
        self.parser.finish_node(
            self.parser.factory().create_jsdoc_augments_tag(
                self.parser,
                Some(tag_name),
                class_name.wrap(),
                self.parse_trailing_tag_comments(
                    start,
                    self.parser.get_node_pos().try_into().unwrap(),
                    margin,
                    indent_text,
                ),
            ),
            start.try_into().unwrap(),
            None,
        )
    }

    pub(super) fn parse_expression_with_type_arguments_for_augments(
        &self,
    ) -> ExpressionWithTypeArguments /* & { expression: Identifier | PropertyAccessEntityNameExpression }*/
    {
        let used_brace = self.parser.parse_optional(SyntaxKind::OpenBraceToken);
        let pos = self.parser.get_node_pos();
        let expression = self.parse_property_access_entity_name_expression();
        let type_arguments = self.parser.try_parse_type_arguments();
        let node = self.parser.factory().create_expression_with_type_arguments(
            self.parser,
            expression.wrap(),
            type_arguments,
        );
        let res = self.parser.finish_node(node, pos, None);
        if used_brace {
            self.parser
                .parse_expected(SyntaxKind::CloseBraceToken, None, None);
        }
        res
    }

    pub(super) fn parse_property_access_entity_name_expression(&self) -> Node /* Identifier | PropertyAccessEntityNameExpression */
    {
        let pos = self.parser.get_node_pos();
        let mut node: Node = self.parse_jsdoc_identifier_name(None);
        while self.parser.parse_optional(SyntaxKind::DotToken) {
            let name: Gc<Node> = self.parse_jsdoc_identifier_name(None).wrap();
            node = self
                .parser
                .finish_node(
                    self.parser.factory().create_property_access_expression(
                        self.parser,
                        node.wrap(),
                        name,
                    ),
                    pos,
                    None,
                )
                .into();
        }
        node
    }

    pub(super) fn parse_simple_tag<
        TCreateTag: FnOnce(Option<Gc<Node /*Identifier*/>>, Option<StringOrNodeArray>) -> BaseJSDocTag,
    >(
        &self,
        start: usize,
        create_tag: TCreateTag,
        tag_name: Gc<Node /*Identifier*/>,
        margin: usize,
        indent_text: &str,
    ) -> BaseJSDocTag /*JSDocTag*/ {
        self.parser.finish_node(
            create_tag(
                Some(tag_name),
                self.parse_trailing_tag_comments(
                    start,
                    self.parser.get_node_pos().try_into().unwrap(),
                    margin,
                    indent_text,
                ),
            ),
            start.try_into().unwrap(),
            None,
        )
    }

    pub(super) fn parse_this_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
        margin: usize,
        indent_text: &str,
    ) -> BaseJSDocTypeLikeTag /*JSDocThisTag*/ {
        let type_expression = self
            .parser
            .JSDocParser_parse_jsdoc_type_expression(Some(true));
        self.skip_whitespace();
        self.parser.finish_node(
            self.parser.factory().create_jsdoc_this_tag(
                self.parser,
                Some(tag_name),
                Some(type_expression),
                self.parse_trailing_tag_comments(
                    start,
                    self.parser.get_node_pos().try_into().unwrap(),
                    margin,
                    indent_text,
                ),
            ),
            start.try_into().unwrap(),
            None,
        )
    }

    pub(super) fn parse_enum_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
        margin: usize,
        indent_text: &str,
    ) -> BaseJSDocTypeLikeTag /*JSDocEnumTag*/ {
        let type_expression = self
            .parser
            .JSDocParser_parse_jsdoc_type_expression(Some(true));
        self.skip_whitespace();
        self.parser.finish_node(
            self.parser.factory().create_jsdoc_enum_tag(
                self.parser,
                Some(tag_name),
                Some(type_expression),
                self.parse_trailing_tag_comments(
                    start,
                    self.parser.get_node_pos().try_into().unwrap(),
                    margin,
                    indent_text,
                ),
            ),
            start.try_into().unwrap(),
            None,
        )
    }

    pub(super) fn parse_typedef_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> JSDocTypedefTag {
        let mut type_expression: Option<Gc<Node /*JSDocTypeExpression | JSDocTypeLiteral*/>> =
            self.try_parse_type_expression();
        self.skip_whitespace_or_asterisk();

        let full_name: Option<Gc<Node>> = self
            .parse_jsdoc_type_name_with_namespace(None)
            .map(Node::wrap);
        self.skip_whitespace();
        let mut comment = self.parse_tag_comments(indent, None);

        let mut end: Option<isize> = None;
        if match type_expression.as_ref() {
            None => true,
            Some(type_expression) => self.is_object_or_object_array_type_reference(
                &type_expression.as_jsdoc_type_expression().type_,
            ),
        } {
            let mut child_type_tag: Option<Gc<Node /*JSDocTypeTag*/>> = None;
            let mut js_doc_property_tags: Option<Vec<Gc<Node /*JSDocPropertyTag*/>>> = None;
            let mut has_children = false;
            while let Some(child) = self
                .parser
                .try_parse(|| self.parse_child_property_tag(indent).map(Node::wrap))
            {
                has_children = true;
                if child.kind() == SyntaxKind::JSDocTypeTag {
                    if child_type_tag.is_some() {
                        self.parser.parse_error_at_current_token(&Diagnostics::A_JSDoc_typedef_comment_may_not_contain_multiple_type_tags, None);
                        let parse_diagnostics = self.parser.parse_diagnostics();
                        let last_error = last_or_undefined(&parse_diagnostics);
                        if let Some(last_error) = last_error {
                            add_related_info(
                                last_error,
                                vec![Gc::new(
                                    create_detached_diagnostic(
                                        &self.parser.file_name(),
                                        0,
                                        0,
                                        &Diagnostics::The_tag_was_first_specified_here,
                                        None,
                                    )
                                    .into(),
                                )],
                            );
                        }
                        break;
                    } else {
                        child_type_tag = Some(child);
                    }
                } else {
                    if js_doc_property_tags.is_none() {
                        js_doc_property_tags = Some(vec![]);
                    }
                    append(js_doc_property_tags.as_mut().unwrap(), Some(child));
                }
            }
            if has_children {
                let is_array_type = matches!(type_expression, Some(type_expression) if type_expression.as_jsdoc_type_expression().type_.kind() == SyntaxKind::ArrayType);
                let jsdoc_type_literal = self.parser.factory().create_jsdoc_type_literal(
                    self.parser,
                    js_doc_property_tags,
                    Some(is_array_type),
                );
                type_expression = Some(match child_type_tag {
                    Some(child_type_tag)
                        if matches!(
                            child_type_tag.as_base_jsdoc_type_like_tag().type_expression.as_ref(),
                            Some(type_expression) if !self.is_object_or_object_array_type_reference(&type_expression.as_jsdoc_type_expression().type_)
                        ) =>
                    {
                        child_type_tag
                            .as_base_jsdoc_type_like_tag()
                            .type_expression
                            .clone()
                            .unwrap()
                    }
                    _ => self
                        .parser
                        .finish_node(jsdoc_type_literal, start.try_into().unwrap(), None)
                        .wrap(),
                });
                end = Some(type_expression.as_ref().unwrap().end());
            }
        }

        end = Some(
            if matches!(end, Some(end) if end != 0) || comment.is_some() {
                self.parser.get_node_pos()
            } else {
                full_name
                    .as_ref()
                    .unwrap_or_else(|| type_expression.as_ref().unwrap_or_else(|| &tag_name))
                    .end()
            },
        );
        let end = end.unwrap();

        if comment.is_none() {
            comment = self.parse_trailing_tag_comments(
                start,
                end.try_into().unwrap(),
                indent,
                indent_text,
            );
        }

        let typedef_tag = self.parser.factory().create_jsdoc_typedef_tag(
            self.parser,
            Some(tag_name),
            type_expression,
            full_name,
            comment,
        );
        self.parser
            .finish_node(typedef_tag, start.try_into().unwrap(), Some(end))
    }

    pub(super) fn parse_jsdoc_type_name_with_namespace(
        &self,
        nested: Option<bool>,
    ) -> Option<Node> {
        let nested = nested.unwrap_or(false);
        let pos = self.parser.scanner().get_token_pos();
        if !token_is_identifier_or_keyword(self.parser.token()) {
            return None;
        }
        let mut type_name_or_namespace_name = self.parse_jsdoc_identifier_name(None);
        if self.parser.parse_optional(SyntaxKind::DotToken) {
            let body = self.parse_jsdoc_type_name_with_namespace(Some(true));
            let js_doc_namespace_node = self.parser.factory().create_module_declaration(
                self.parser,
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                type_name_or_namespace_name.wrap(),
                body.map(Node::wrap),
                if nested {
                    Some(NodeFlags::NestedNamespace)
                } else {
                    None
                },
            );
            return Some(
                self.parser
                    .finish_node(js_doc_namespace_node, pos.try_into().unwrap(), None)
                    .into(),
            );
        }

        if nested {
            type_name_or_namespace_name
                .as_identifier()
                .set_is_in_jsdoc_namespace(Some(true));
        }
        Some(type_name_or_namespace_name)
    }

    pub(super) fn parse_callback_tag_parameters(&self, indent: usize) -> Gc<NodeArray> /*<JSDocParameterTag>*/
    {
        let pos = self.parser.get_node_pos();
        let mut parameters: Option<Vec<Gc<Node>>> = None;
        while let Some(child) = self.parser.try_parse(|| {
            self.parse_child_parameter_or_property_tag(
                PropertyLikeParse::CallbackParameter,
                indent,
                None,
            )
            .map(Node::wrap)
        }) {
            if parameters.is_none() {
                parameters = Some(vec![]);
                append(parameters.as_mut().unwrap(), Some(child));
            }
        }
        self.parser
            .create_node_array(parameters.unwrap_or_else(|| vec![]), pos, None, None)
    }

    pub(super) fn parse_callback_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> JSDocCallbackTag {
        let full_name = self
            .parse_jsdoc_type_name_with_namespace(None)
            .map(Node::wrap);
        self.skip_whitespace();
        let mut comment = self.parse_tag_comments(indent, None);
        let parameters = self.parse_callback_tag_parameters(indent);
        let return_tag = self.parser.try_parse(|| {
            if self.parse_optional_jsdoc(SyntaxKind::AtToken) {
                let tag = self.parse_tag(indent);
                if
                /*tag &&*/
                tag.kind() == SyntaxKind::JSDocReturnTag {
                    return Some(tag.wrap());
                }
            }
            None
        });
        let type_expression = self.parser.finish_node(
            self.parser.factory().create_jsdoc_signature(
                self.parser,
                Option::<Gc<NodeArray>>::None,
                parameters,
                return_tag,
            ),
            start.try_into().unwrap(),
            None,
        );
        if comment.is_none() {
            comment = self.parse_trailing_tag_comments(
                start,
                self.parser.get_node_pos().try_into().unwrap(),
                indent,
                indent_text,
            );
        }
        self.parser.finish_node(
            self.parser.factory().create_jsdoc_callback_tag(
                self.parser,
                Some(tag_name),
                type_expression.wrap(),
                full_name,
                comment,
            ),
            start.try_into().unwrap(),
            None,
        )
    }

    pub(super) fn escaped_texts_equal(
        &self,
        a: &Node, /*EntityName*/
        b: &Node, /*EntityName*/
    ) -> bool {
        let mut a = a.node_wrapper();
        let mut b = b.node_wrapper();
        while !is_identifier(&a) || !is_identifier(&b) {
            if !is_identifier(&a)
                && !is_identifier(&b)
                && a.as_qualified_name().right.as_identifier().escaped_text
                    == b.as_qualified_name().right.as_identifier().escaped_text
            {
                a = a.as_qualified_name().left.clone();
                b = b.as_qualified_name().left.clone();
            } else {
                return false;
            }
        }
        a.as_identifier().escaped_text == b.as_identifier().escaped_text
    }

    pub(super) fn parse_child_property_tag(&self, indent: usize) -> Option<Node> {
        self.parse_child_parameter_or_property_tag(PropertyLikeParse::Property, indent, None)
    }

    pub(super) fn parse_child_parameter_or_property_tag(
        &self,
        target: PropertyLikeParse,
        indent: usize,
        name: Option<&Node /*EntityName*/>,
    ) -> Option<Node /*JSDocTypeTag | JSDocPropertyTag | JSDocParameterTag*/> {
        let mut can_parse_tag = true;
        let mut seen_asterisk = false;
        loop {
            match self.parser.next_token_jsdoc() {
                SyntaxKind::AtToken => {
                    if can_parse_tag {
                        let child = self.try_parse_child_tag(target, indent);
                        if matches!(
                            child.as_ref(),
                            Some(child) if matches!(child.kind(), SyntaxKind::JSDocParameterTag | SyntaxKind::JSDocPropertyTag) &&
                                target != PropertyLikeParse::CallbackParameter &&
                                matches!(
                                    name,
                                    Some(name) if is_identifier(&child.as_jsdoc_property_like_tag().name) || !self.escaped_texts_equal(name, &child.as_jsdoc_property_like_tag().name.as_qualified_name().left)
                                )
                        ) {
                            return None;
                        }
                        return child;
                    }
                    seen_asterisk = false;
                }
                SyntaxKind::NewLineTrivia => {
                    can_parse_tag = true;
                    seen_asterisk = false;
                }
                SyntaxKind::AsteriskToken => {
                    if seen_asterisk {
                        can_parse_tag = false;
                    }
                    seen_asterisk = true;
                }
                SyntaxKind::Identifier => {
                    can_parse_tag = false;
                }
                SyntaxKind::EndOfFileToken => {
                    return None;
                }
                _ => (),
            }
        }
    }

    pub(super) fn try_parse_child_tag(
        &self,
        target: PropertyLikeParse,
        indent: usize,
    ) -> Option<Node /*JSDocTypeTag | JSDocPropertyTag | JSDocParameterTag*/> {
        Debug_.assert(self.parser.token() == SyntaxKind::AtToken, None);
        let start = self.parser.scanner().get_start_pos();
        self.parser.next_token_jsdoc();

        let tag_name = self.parse_jsdoc_identifier_name(None);
        self.skip_whitespace();
        let t: PropertyLikeParse;
        match &*tag_name.as_identifier().escaped_text {
            "type" => {
                return if target == PropertyLikeParse::Property {
                    Some(
                        self.parse_type_tag(start, tag_name.wrap(), None, None)
                            .into(),
                    )
                } else {
                    None
                };
            }
            "prop" | "property" => {
                t = PropertyLikeParse::Property;
            }
            "arg" | "argument" | "param" => {
                t = PropertyLikeParse::Parameter | PropertyLikeParse::CallbackParameter;
            }
            _ => {
                return None;
            }
        }
        if !target.intersects(t) {
            return None;
        }
        Some(
            self.parse_parameter_or_property_tag(start, tag_name.wrap(), target, indent)
                .into(),
        )
    }

    pub(super) fn parse_template_tag_type_parameter(&self) -> Option<TypeParameterDeclaration> {
        let type_parameter_pos = self.parser.get_node_pos();
        let is_bracketed = self.parse_optional_jsdoc(SyntaxKind::OpenBracketToken);
        if is_bracketed {
            self.skip_whitespace();
        }
        let name: Gc<Node> = self.parse_jsdoc_identifier_name(Some(
            &Diagnostics::Unexpected_token_A_type_parameter_name_was_expected_without_curly_braces,
        )).wrap();

        let mut default_type: Option<Gc<Node /*TypeNode*/>> = None;
        if is_bracketed {
            self.skip_whitespace();
            self.parser
                .parse_expected(SyntaxKind::EqualsToken, None, None);
            default_type = Some(
                self.parser
                    .do_inside_of_context(NodeFlags::JSDoc, || self.parser.parse_jsdoc_type()),
            );
            self.parser
                .parse_expected(SyntaxKind::CloseBracketToken, None, None);
        }

        if node_is_missing(Some(&*name)) {
            return None;
        }
        Some(self.parser.finish_node(
            self.parser.factory().create_type_parameter_declaration(
                self.parser,
                name,
                None,
                default_type,
            ),
            type_parameter_pos,
            None,
        ))
    }

    pub(super) fn parse_template_tag_type_parameters(&self) -> Gc<NodeArray> /*<TypeParameterDeclaration>*/
    {
        let pos = self.parser.get_node_pos();
        let mut type_parameters: Vec<Gc<Node>> = vec![];
        while {
            self.skip_whitespace();
            let node = self
                .parse_template_tag_type_parameter()
                .map(NodeInterface::wrap);
            if let Some(node) = node {
                type_parameters.push(node);
            }
            self.skip_whitespace_or_asterisk();
            self.parse_optional_jsdoc(SyntaxKind::CommaToken)
        } {}
        self.parser
            .create_node_array(type_parameters, pos, None, None)
    }

    pub(super) fn parse_template_tag(
        &self,
        start: usize,
        tag_name: Gc<Node /*Identifier*/>,
        indent: usize,
        indent_text: &str,
    ) -> JSDocTemplateTag {
        let constraint: Option<Gc<Node>> = if self.parser.token() == SyntaxKind::OpenBraceToken {
            Some(self.parser.JSDocParser_parse_jsdoc_type_expression(None))
        } else {
            None
        };
        let type_parameters = self.parse_template_tag_type_parameters();
        self.parser.finish_node(
            self.parser.factory().create_jsdoc_template_tag(
                self.parser,
                Some(tag_name),
                constraint,
                type_parameters,
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

    pub(super) fn parse_optional_jsdoc(&self, t: SyntaxKind /*JSDocSyntaxKind*/) -> bool {
        if self.parser.token() == t {
            self.parser.next_token_jsdoc();
            return true;
        }
        false
    }

    pub(super) fn parse_jsdoc_entity_name(&self) -> Node /*EntityName*/ {
        let mut entity: Node /*EntityName*/ = self.parse_jsdoc_identifier_name(None);
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
                .create_qualified_name(entity.wrap(), name.wrap())
                .into();
        }
        entity
    }

    pub(super) fn parse_jsdoc_identifier_name(&self, message: Option<&DiagnosticMessage>) -> Node {
        if !token_is_identifier_or_keyword(self.parser.token()) {
            return self.parser.create_missing_node(
                SyntaxKind::Identifier,
                message.is_none(),
                Some(message.unwrap_or(&Diagnostics::Identifier_expected)),
                None,
            );
        }

        self.parser.increment_identifier_count();
        let pos = self.parser.scanner().get_token_pos();
        let end = self.parser.scanner().get_text_pos();
        let original_keyword_kind = self.parser.token();
        let text = self
            .parser
            .intern_identifier(&self.parser.scanner().get_token_value());
        let result: Node = self
            .parser
            .finish_node(
                self.parser.factory().create_identifier(
                    self.parser,
                    &text,
                    Option::<Gc<NodeArray>>::None,
                    Some(original_keyword_kind),
                ),
                pos.try_into().unwrap(),
                Some(end.try_into().unwrap()),
            )
            .into();
        self.parser.next_token_jsdoc();
        result
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
    ) -> Gc<Node /*SourceFile*/> {
        unimplemented!()
    }
}
