#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::propagate_child_flags;
use crate::{
    add_range, append_if_unique, create_base_node_factory, create_parenthesizer_rules,
    escape_leading_underscores, is_call_chain, is_import_keyword, is_named_declaration,
    is_omitted_expression, is_outer_expression, is_property_name, is_super_property,
    last_or_undefined, null_parenthesizer_rules, pseudo_big_int_to_string, set_text_range,
    ArrayLiteralExpression, ArrayTypeNode, BaseBindingLikeDeclaration, BaseFunctionLikeDeclaration,
    BaseGenericNamedDeclaration, BaseInterfaceOrClassLikeDeclaration, BaseLiteralLikeNode,
    BaseNamedDeclaration, BaseNode, BaseNodeFactory, BaseNodeFactoryConcrete,
    BaseSignatureDeclaration, BaseVariableLikeDeclaration, BigIntLiteral, BinaryExpression, Block,
    CallExpression, Debug_, EmitFlags, EmitNode, EmptyStatement, Expression, ExpressionStatement,
    FunctionDeclaration, Identifier, IfStatement, InterfaceDeclaration, IntersectionTypeNode,
    LiteralLikeNode, LiteralLikeNodeInterface, LiteralTypeNode, Node, NodeArray, NodeArrayOrVec,
    NodeFactory, NodeFlags, NodeInterface, NumericLiteral, ObjectLiteralExpression,
    OuterExpressionKinds, ParameterDeclaration, ParenthesizedExpression, ParenthesizedTypeNode,
    ParenthesizerRules, PrefixUnaryExpression, PropertyAssignment, PropertySignature, PseudoBigInt,
    ReadonlyTextRange, ReturnStatement, ShorthandPropertyAssignment, SourceFile, SourceMapRange,
    Statement, StringLiteral, SyntaxKind, TemplateExpression, TemplateLiteralLikeNode,
    TemplateSpan, TokenFlags, TransformFlags, TypeAliasDeclaration, TypeLiteralNode, TypeNode,
    TypeParameterDeclaration, TypePredicateNode, TypeReferenceNode, UnionTypeNode,
    VariableDeclaration, VariableDeclarationList, VariableStatement,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_property_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node>,
        initializer: Rc<Node>,
    ) -> PropertyAssignment {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertyAssignment,
            None,
            None,
            Some(name),
        );
        let node = PropertyAssignment::new(node, initializer);
        node
    }

    pub fn create_shorthand_property_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node>,
        object_assignment_initializer: Option<Rc<Node>>,
    ) -> ShorthandPropertyAssignment {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertyAssignment,
            None,
            None,
            Some(name),
        );
        let mut node = ShorthandPropertyAssignment::new(
            node,
            object_assignment_initializer.map(|object_assignment_initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(
                        base_factory,
                        &object_assignment_initializer,
                    )
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(
                node.object_assignment_initializer
                    .as_ref()
                    .map(|rc| rc.clone()),
            ) | TransformFlags::ContainsES2015,
        );
        node
    }

    pub fn create_source_file<TNodes: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: TNodes,
    ) -> SourceFile {
        let node = base_factory.create_base_source_file_node(SyntaxKind::SourceFile);
        let node = SourceFile::new(
            node,
            self.create_node_array(Some(statements), None),
            "".to_string(),
            "".to_string(),
        );
        node
    }

    fn is_ignorable_paren(&self, node: &Node /*Expression*/) -> bool {
        unimplemented!()
    }

    pub fn restore_outer_expressions<TOuterExpression: Borrow<Node>>(
        &self,
        outer_expression: Option<TOuterExpression /*Expression*/>,
        inner_expression: &Node, /*Expression*/
        kinds: Option<OuterExpressionKinds>,
    ) -> Rc<Node /*Expression*/> {
        let kinds = kinds.unwrap_or(OuterExpressionKinds::All);
        if let Some(outer_expression) = outer_expression.filter(|outer_expression| {
            let outer_expression = outer_expression.borrow();
            is_outer_expression(outer_expression, Some(kinds))
                && !self.is_ignorable_paren(outer_expression)
        }) {
            unimplemented!()
        }
        inner_expression.node_wrapper()
    }
}
