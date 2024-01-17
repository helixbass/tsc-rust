use std::{borrow::Borrow, io};

use gc::Gc;
use id_arena::Id;

use super::{
    create_spread_segment, ES2015SubstitutionFlags, HierarchyFacts, SpreadSegment,
    SpreadSegmentKind, TransformES2015,
};
use crate::{
    first_or_undefined, is_array_literal_expression, is_call_to_helper, is_expression,
    is_identifier, is_packed_array_literal, is_static, node_is_synthesized, return_default_if_none,
    single_or_undefined, try_map, try_process_tagged_template_expression, try_visit_node,
    try_visit_nodes, FunctionLikeDeclarationInterface, GeneratedIdentifierFlags,
    LiteralLikeNodeInterface, Matches, Node, NodeArray, NodeExt, NodeInterface, ProcessLevel,
    SignatureDeclarationInterface, SyntaxKind, TokenFlags, VisitResult,
};

impl TransformES2015 {
    pub(super) fn visit_span_of_spreads(
        &self,
        chunk: &[Id<Node /*Expression*/>],
    ) -> io::Result<Vec<SpreadSegment>> {
        try_map(chunk, |node: &Id<Node>, _| {
            self.visit_expression_of_spread(node)
        })
    }

    pub(super) fn visit_expression_of_spread(
        &self,
        node: Id<Node>, /*SpreadElement*/
    ) -> io::Result<SpreadSegment> {
        let node_as_spread_element = node.as_spread_element();
        let mut expression = try_visit_node(
            &node_as_spread_element.expression,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;

        let is_call_to_read_helper = is_call_to_helper(&expression, "___read");
        let mut kind = if is_call_to_read_helper || is_packed_array_literal(&expression) {
            SpreadSegmentKind::PackedSpread
        } else {
            SpreadSegmentKind::UnpackedSpread
        };

        if self.compiler_options.downlevel_iteration == Some(true)
            && kind == SpreadSegmentKind::UnpackedSpread
            && !is_array_literal_expression(&expression)
            && !is_call_to_read_helper
        {
            expression = self
                .emit_helpers()
                .create_read_helper(expression.clone(), None);
            kind = SpreadSegmentKind::PackedSpread;
        }

        Ok(create_spread_segment(kind, expression))
    }

    pub(super) fn visit_span_of_non_spreads(
        &self,
        chunk: Vec<Id<Node /*Expression*/>>,
        multi_line: bool,
        has_trailing_comma: bool,
    ) -> io::Result<SpreadSegment> {
        let expression = self.factory.create_array_literal_expression(
            Some(try_visit_nodes(
                &self
                    .factory
                    .create_node_array(Some(chunk), Some(has_trailing_comma)),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                None,
                None,
            )?),
            Some(multi_line),
        );

        Ok(create_spread_segment(SpreadSegmentKind::None, expression))
    }

    pub(super) fn visit_spread_element(
        &self,
        node: Id<Node>, /*SpreadElement*/
    ) -> io::Result<VisitResult> {
        let node_as_spread_element = node.as_spread_element();
        Ok(Some(
            try_visit_node(
                &node_as_spread_element.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?
            .into(),
        ))
    }

    pub(super) fn visit_template_literal(
        &self,
        node: Id<Node>, /*LiteralExpression*/
    ) -> Id<Node /*LeftHandSideExpression*/> {
        self.factory
            .create_string_literal(node.as_literal_like_node().text().clone(), None, None)
            .set_text_range(Some(node))
    }

    pub(super) fn visit_string_literal(
        &self,
        node: Id<Node>, /*StringLiteral*/
    ) -> VisitResult {
        let node_as_string_literal = node.as_string_literal();
        if node_as_string_literal.has_extended_unicode_escape() == Some(true) {
            return Some(
                self.factory
                    .create_string_literal(node_as_string_literal.text().clone(), None, None)
                    .set_text_range(Some(node))
                    .into(),
            );
        }
        Some(node.node_wrapper().into())
    }

    pub(super) fn visit_numeric_literal(
        &self,
        node: Id<Node>, /*NumericLiteral*/
    ) -> VisitResult {
        let node_as_numeric_literal = node.as_numeric_literal();
        if node_as_numeric_literal
            .numeric_literal_flags
            .intersects(TokenFlags::BinaryOrOctalSpecifier)
        {
            return Some(
                self.factory
                    .create_numeric_literal(node_as_numeric_literal.text().clone(), None)
                    .set_text_range(Some(node))
                    .into(),
            );
        }
        Some(node.node_wrapper().into())
    }

    pub(super) fn visit_tagged_template_expression(
        &self,
        node: Id<Node>, /*TaggedTemplateExpression*/
    ) -> io::Result<VisitResult> {
        Ok(Some(
            try_process_tagged_template_expression(
                &**self.context,
                node,
                |node: Id<Node>| self.visitor(node),
                &self.current_source_file(),
                |node: Id<Node>| self.record_tagged_template_string(node),
                ProcessLevel::All,
            )?
            .into(),
        ))
    }

    pub(super) fn visit_template_expression(
        &self,
        node: Id<Node>, /*TemplateExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_as_template_expression = node.as_template_expression();
        let mut expression: Id<Node /*Expression*/> = self.factory.create_string_literal(
            node_as_template_expression
                .head
                .as_template_literal_like_node()
                .text()
                .clone(),
            None,
            None,
        );
        for span in &node_as_template_expression.template_spans {
            let span_as_template_span = span.as_template_span();
            let mut args = vec![try_visit_node(
                &span_as_template_span.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?];

            if !span_as_template_span
                .literal
                .as_template_literal_like_node()
                .text()
                .is_empty()
            {
                args.push(
                    self.factory.create_string_literal(
                        span_as_template_span
                            .literal
                            .as_template_literal_like_node()
                            .text()
                            .clone(),
                        None,
                        None,
                    ),
                );
            }

            expression = self.factory.create_call_expression(
                self.factory
                    .create_property_access_expression(expression, "concat"),
                Option::<Gc<NodeArray>>::None,
                Some(args),
            );
        }

        Ok(expression.set_text_range(Some(node)))
    }

    pub(super) fn visit_super_keyword(
        &self,
        is_expression_of_call: bool,
    ) -> Id<Node /*LeftHandSideExpression*/> {
        if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::NonStaticClassElement)
            && !is_expression_of_call
        {
            self.factory.create_property_access_expression(
                self.factory.create_unique_name(
                    "super",
                    Some(
                        GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel,
                    ),
                ),
                "prototype",
            )
        } else {
            self.factory.create_unique_name(
                "super",
                Some(GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel),
            )
        }
    }

    pub(super) fn visit_meta_property(&self, node: Id<Node> /*MetaProperty*/) -> VisitResult {
        let node_as_meta_property = node.as_meta_property();
        if node_as_meta_property.keyword_token == SyntaxKind::NewKeyword
            && node_as_meta_property.name.as_identifier().escaped_text == "target"
        {
            self.set_hierarchy_facts(Some(
                self.maybe_hierarchy_facts().unwrap_or_default() | HierarchyFacts::NewTarget,
            ));
            return Some(
                self.factory
                    .create_unique_name(
                        "_newTarget",
                        Some(
                            GeneratedIdentifierFlags::Optimistic
                                | GeneratedIdentifierFlags::FileLevel,
                        ),
                    )
                    .into(),
            );
        }
        Some(node.node_wrapper().into())
    }

    pub(super) fn enable_substitutions_for_block_scoped_bindings(&self) {
        if !self
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ES2015SubstitutionFlags::BlockScopedBindings)
        {
            self.set_enabled_substitutions(Some(
                self.maybe_enabled_substitutions().unwrap_or_default()
                    | ES2015SubstitutionFlags::BlockScopedBindings,
            ));
            self.context.enable_substitution(SyntaxKind::Identifier);
        }
    }

    pub(super) fn enable_substitutions_for_captured_this(&self) {
        if !self
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ES2015SubstitutionFlags::CapturedThis)
        {
            self.set_enabled_substitutions(Some(
                self.maybe_enabled_substitutions().unwrap_or_default()
                    | ES2015SubstitutionFlags::CapturedThis,
            ));
            self.context.enable_substitution(SyntaxKind::ThisKeyword);
            self.context
                .enable_emit_notification(SyntaxKind::Constructor);
            self.context
                .enable_emit_notification(SyntaxKind::MethodDeclaration);
            self.context
                .enable_emit_notification(SyntaxKind::GetAccessor);
            self.context
                .enable_emit_notification(SyntaxKind::SetAccessor);
            self.context
                .enable_emit_notification(SyntaxKind::ArrowFunction);
            self.context
                .enable_emit_notification(SyntaxKind::FunctionExpression);
            self.context
                .enable_emit_notification(SyntaxKind::FunctionDeclaration);
        }
    }

    pub(super) fn get_class_member_prefix(
        &self,
        node: Id<Node>,   /*ClassExpression | ClassDeclaration*/
        member: Id<Node>, /*ClassElement*/
    ) -> Id<Node> {
        if is_static(member, self) {
            self.factory.get_internal_name(node, None, None)
        } else {
            self.factory.create_property_access_expression(
                self.factory.get_internal_name(node, None, None),
                "prototype",
            )
        }
    }

    pub(super) fn has_synthesized_default_super_call(
        &self,
        constructor: Option<Id<Node /*ConstructorDeclaration*/>>,
        has_extends_clause: bool,
    ) -> bool {
        let constructor = return_default_if_none!(constructor);
        let constructor: Id<Node> = constructor.borrow();
        let constructor_as_constructor_declaration = constructor.as_constructor_declaration();
        if !has_extends_clause {
            return false;
        }

        if !constructor_as_constructor_declaration
            .parameters()
            .is_empty()
        {
            return false;
        }

        let statement = first_or_undefined(
            &constructor_as_constructor_declaration
                .maybe_body()
                .unwrap()
                .as_block()
                .statements,
        )
        .cloned();
        if statement.as_ref().is_none_or_matches(|statement| {
            !node_is_synthesized(&**statement)
                || statement.kind() != SyntaxKind::ExpressionStatement
        }) {
            return false;
        }
        let statement = statement.unwrap();
        let statement_as_expression_statement = statement.as_expression_statement();

        let statement_expression = &statement_as_expression_statement.expression;
        if !node_is_synthesized(&**statement_expression)
            || statement_expression.kind() != SyntaxKind::CallExpression
        {
            return false;
        }
        let statement_expression_as_call_expression = statement_expression.as_call_expression();

        let call_target = &statement_expression_as_call_expression.expression;
        if !node_is_synthesized(&**call_target) || call_target.kind() != SyntaxKind::SuperKeyword {
            return false;
        }

        let call_argument =
            single_or_undefined(Some(&statement_expression_as_call_expression.arguments)).cloned();
        if call_argument.as_ref().is_none_or_matches(|call_argument| {
            !node_is_synthesized(&**call_argument)
                || call_argument.kind() != SyntaxKind::SpreadElement
        }) {
            return false;
        }
        let call_argument = call_argument.unwrap();
        let call_argument_as_spread_element = call_argument.as_spread_element();

        let expression = &call_argument_as_spread_element.expression;
        is_identifier(expression) && expression.as_identifier().escaped_text == "arguments"
    }
}
