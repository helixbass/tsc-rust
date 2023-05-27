use std::{borrow::Borrow, io, ptr};

use gc::Gc;

use super::{ES2015SubstitutionFlags, HierarchyFacts, TransformES2015};
use crate::{
    first_or_undefined, get_enclosing_block_scope_container, get_name_of_declaration,
    get_parse_tree_node, is_class_element, is_class_like, is_identifier, is_internal_name,
    is_static, node_is_synthesized, return_default_if_none, single_or_undefined,
    FunctionLikeDeclarationInterface, GeneratedIdentifierFlags, Matches, Node, NodeExt,
    NodeInterface, ReadonlyTextRange, SignatureDeclarationInterface, SyntaxKind, VisitResult,
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
        _is_expression_of_call: bool,
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }

    pub(super) fn visit_meta_property(&self, _node: &Node /*MetaProperty*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn enable_substitutions_for_block_scoped_bindings(&self) {
        unimplemented!()
    }

    pub(super) fn enable_substitutions_for_captured_this(&self) {
        unimplemented!()
    }

    pub(super) fn substitute_expression(
        &self,
        node: &Node, /*Identifier*/
    ) -> io::Result<Gc<Node>> {
        match node.kind() {
            SyntaxKind::Identifier => {
                return self.substitute_expression_identifier(node);
            }
            SyntaxKind::ThisKeyword => {
                return Ok(self.substitute_this_keyword(node));
            }
            _ => (),
        }

        Ok(node.node_wrapper())
    }

    pub(super) fn substitute_expression_identifier(
        &self,
        node: &Node, /*Identifier*/
    ) -> io::Result<Gc<Node /*Identifier*/>> {
        if self
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ES2015SubstitutionFlags::BlockScopedBindings)
            && !is_internal_name(node)
        {
            let declaration = self
                .resolver
                .get_referenced_declaration_with_colliding_name(node)?;
            if let Some(declaration) = declaration.filter(|declaration| {
                !(is_class_like(declaration) && self.is_part_of_class_body(declaration, node))
            }) {
                return Ok(self
                    .factory
                    .get_generated_name_for_node(get_name_of_declaration(Some(declaration)), None)
                    .set_text_range(Some(node)));
            }
        }

        Ok(node.node_wrapper())
    }

    pub(super) fn is_part_of_class_body(
        &self,
        declaration: &Node, /*ClassLikeDeclaration*/
        node: &Node,        /*Identifier*/
    ) -> bool {
        let mut current_node = get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None);
        if current_node.as_ref().is_none_or_matches(|current_node| {
            ptr::eq(&**current_node, declaration)
                || current_node.end() <= declaration.pos()
                || current_node.pos() >= declaration.end()
        }) {
            return false;
        }
        let ref block_scope = get_enclosing_block_scope_container(declaration).unwrap();
        while let Some(current_node_present) = current_node.as_ref() {
            if Gc::ptr_eq(current_node_present, block_scope)
                || ptr::eq(&**current_node_present, declaration)
            {
                return false;
            }
            if is_class_element(current_node_present)
                && ptr::eq(&*current_node_present.parent(), declaration)
            {
                return true;
            }
            current_node = current_node_present.maybe_parent();
        }
        false
    }

    pub(super) fn substitute_this_keyword(
        &self,
        node: &Node, /*PrimaryExpression*/
    ) -> Gc<Node /*PrimaryExpression*/> {
        if self
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ES2015SubstitutionFlags::CapturedThis)
            && self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::CapturesThis)
        {
            return self
                .factory
                .create_unique_name(
                    "_this",
                    Some(
                        GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel,
                    ),
                )
                .set_text_range(Some(node));
        }
        node.node_wrapper()
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
