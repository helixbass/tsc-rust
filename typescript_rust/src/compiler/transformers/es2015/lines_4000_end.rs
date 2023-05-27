use std::borrow::Borrow;

use gc::Gc;

use super::{ES2015SubstitutionFlags, HierarchyFacts, TransformES2015};
use crate::{
    first_or_undefined, is_identifier, is_static, node_is_synthesized, return_default_if_none,
    single_or_undefined, FunctionLikeDeclarationInterface, GeneratedIdentifierFlags, Matches, Node,
    NodeInterface, SignatureDeclarationInterface, SyntaxKind, VisitResult,
};

impl TransformES2015 {
    pub(super) fn visit_spread_element(&self, _node: &Node /*SpreadElement*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_template_literal(
        &self,
        _node: &Node, /*LiteralExpression*/
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }

    pub(super) fn visit_string_literal(&self, _node: &Node /*StringLiteral*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_numeric_literal(
        &self,
        _node: &Node, /*NumericLiteral*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_tagged_template_expression(
        &self,
        _node: &Node, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_template_expression(
        &self,
        _node: &Node, /*TemplateExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_super_keyword(
        &self,
        is_expression_of_call: bool,
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::NonStaticClassElement)
            && !is_expression_of_call
        {
            self.factory
                .create_property_access_expression(
                    self.factory.create_unique_name(
                        "super",
                        Some(
                            GeneratedIdentifierFlags::Optimistic
                                | GeneratedIdentifierFlags::FileLevel,
                        ),
                    ),
                    "prototype",
                )
                .wrap()
        } else {
            self.factory.create_unique_name(
                "super",
                Some(GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel),
            )
        }
    }

    pub(super) fn visit_meta_property(&self, node: &Node /*MetaProperty*/) -> VisitResult {
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
        node: &Node,   /*ClassExpression | ClassDeclaration*/
        member: &Node, /*ClassElement*/
    ) -> Gc<Node> {
        if is_static(member) {
            self.factory.get_internal_name(node, None, None)
        } else {
            self.factory
                .create_property_access_expression(
                    self.factory.get_internal_name(node, None, None),
                    "prototype",
                )
                .wrap()
        }
    }

    pub(super) fn has_synthesized_default_super_call(
        &self,
        constructor: Option<impl Borrow<Node /*ConstructorDeclaration*/>>,
        has_extends_clause: bool,
    ) -> bool {
        let constructor = return_default_if_none!(constructor);
        let constructor: &Node = constructor.borrow();
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
