#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use super::CheckMode;
use crate::{
    for_each, get_combined_node_flags, get_effective_initializer, is_binding_element,
    is_function_or_module_block, is_private_identifier, map, maybe_for_each, parse_pseudo_big_int,
    ArrayTypeNode, Block, DiagnosticMessage, Diagnostics, Expression, ExpressionStatement,
    HasTypeParametersInterface, IfStatement, InterfaceDeclaration, LiteralLikeNode,
    LiteralLikeNodeInterface, NamedDeclarationInterface, Node, NodeArray, NodeFlags, NodeInterface,
    PrefixUnaryExpression, PropertyAssignment, PropertySignature, PseudoBigInt, SymbolInterface,
    SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, TypeParameterDeclaration,
    TypeReferenceNode, UnionOrIntersectionTypeInterface, VariableDeclaration,
    VariableLikeDeclarationInterface, VariableStatement,
};
use local_macros::{enum_unwrapped, node_unwrapped};

impl TypeChecker {
    pub(super) fn check_arithmetic_operand_type(
        &self,
        operand: /*&Node*/ &Expression,
        type_: &Type,
        diagnostic: &DiagnosticMessage,
    ) -> bool {
        if !self.is_type_assignable_to(type_, &self.number_or_big_int_type()) {
            self.error_and_maybe_suggest_await(operand, diagnostic, None);
            return false;
        }
        true
    }

    pub(super) fn check_prefix_unary_expression(&self, node: &PrefixUnaryExpression) -> Rc<Type> {
        let operand_expression = node.operand.as_expression();
        let operand_type = self.check_expression(operand_expression, None);
        match node.operator {
            SyntaxKind::PlusPlusToken => {
                self.check_arithmetic_operand_type(operand_expression, &operand_type, &Diagnostics::An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type);
                return self.get_unary_result_type(&operand_type);
            }
            _ => {
                unimplemented!();
            }
        }
    }

    pub(super) fn get_unary_result_type(&self, operand_type: &Type) -> Rc<Type> {
        self.number_type()
    }

    pub(super) fn maybe_type_of_kind(&self, type_: &Type, kind: TypeFlags) -> bool {
        if type_.flags().intersects(kind) {
            return true;
        }
        if let Type::UnionOrIntersectionType(type_) = type_ {
            for t in type_.types() {
                if self.maybe_type_of_kind(&**t, kind) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn check_expression_cached(
        &self,
        node: &Expression,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let links = self.get_node_links(node);
        let mut links_ref = links.borrow_mut();
        if links_ref.resolved_type.is_none() {
            if let Some(check_mode) = check_mode {
                if check_mode != CheckMode::Normal {
                    return self.check_expression(node, Some(check_mode));
                }
            }
            links_ref.resolved_type = Some(self.check_expression(node, check_mode));
        }
        links_ref.resolved_type.clone().unwrap()
    }

    pub(super) fn check_declaration_initializer<TTypeRef: Borrow<Type>>(
        &self,
        declaration: &Node, /*HasExpressionInitializer*/
        contextual_type: Option<TTypeRef>,
    ) -> Rc<Type> {
        let initializer = get_effective_initializer(declaration).unwrap();
        let initializer_as_expression = initializer.as_expression();
        let type_ = self
            .get_quick_type_of_expression(initializer_as_expression)
            .unwrap_or_else(|| {
                if let Some(contextual_type) = contextual_type {
                    unimplemented!()
                } else {
                    self.check_expression_cached(initializer_as_expression, None)
                }
            });
        if false {
            unimplemented!()
        } else {
            type_
        }
    }

    pub(super) fn widen_type_inferred_from_initializer(
        &self,
        declaration: &Node, /*HasExpressionInitializer*/
        type_: &Type,
    ) -> Rc<Type> {
        let widened = if get_combined_node_flags(declaration).intersects(NodeFlags::Const) {
            type_.type_wrapper()
        } else {
            self.get_widened_literal_type(type_)
        };
        widened
    }

    pub(super) fn is_literal_of_contextual_type<TTypeRef: Borrow<Type>>(
        &self,
        candidate_type: &Type,
        contextual_type: Option<TTypeRef>,
    ) -> bool {
        if let Some(contextual_type) = contextual_type {
            let contextual_type = contextual_type.borrow();
            if let Type::UnionOrIntersectionType(union_or_intersection_type) = contextual_type {
                let types = union_or_intersection_type.types();
                // return some(
                //     types,
                //     Some(Box::new(|t| {
                //         self.is_literal_of_contextual_type(candidate_type, Some(t.clone()))
                //     })),
                // );
                return types
                    .iter()
                    .any(|t| self.is_literal_of_contextual_type(candidate_type, Some(&**t)));
            }
            return contextual_type.flags().intersects(
                TypeFlags::StringLiteral
                    | TypeFlags::Index
                    | TypeFlags::TemplateLiteral
                    | TypeFlags::StringMapping,
            ) && self.maybe_type_of_kind(&*candidate_type, TypeFlags::StringLiteral)
                || contextual_type.flags().intersects(TypeFlags::NumberLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::NumberLiteral)
                || contextual_type.flags().intersects(TypeFlags::BigIntLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::BigIntLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::BooleanLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::BooleanLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::UniqueESSymbol)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::UniqueESSymbol);
        }
        false
    }

    pub(super) fn check_expression_for_mutable_location<TTypeRef: Borrow<Type>>(
        &self,
        node: &Expression,
        check_mode: Option<CheckMode>,
        contextual_type: Option<TTypeRef>,
    ) -> Rc<Type> {
        let type_ = self.check_expression(node, check_mode);
        if false {
            unimplemented!()
        } else {
            self.get_widened_literal_like_type_for_contextual_type(
                &type_,
                self.instantiate_contextual_type(
                    if contextual_type.is_none() {
                        self.get_contextual_type(node)
                    } else {
                        Some(contextual_type.unwrap().borrow().type_wrapper())
                    },
                    node,
                ),
            )
        }
    }

    pub(super) fn check_property_assignment(
        &self,
        node: &PropertyAssignment,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_expression_for_mutable_location(
            node.initializer.as_expression(),
            check_mode,
            Option::<&Type>::None,
        )
    }

    pub(super) fn get_quick_type_of_expression(&self, node: &Expression) -> Option<Rc<Type>> {
        let expr = node;
        if false {
            unimplemented!()
        } else if matches!(
            node.kind(),
            SyntaxKind::NumericLiteral
                | SyntaxKind::StringLiteral
                | SyntaxKind::TrueKeyword
                | SyntaxKind::FalseKeyword
        ) {
            return Some(self.check_expression(node, None));
        }
        None
    }

    pub(super) fn check_expression(
        &self,
        node: &Expression,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_expression_worker(node, check_mode)
    }

    pub(super) fn check_expression_worker(
        &self,
        node: &Expression,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        match node {
            Expression::Identifier(identifier) => self.check_identifier(identifier, check_mode),
            Expression::TokenExpression(token_expression) => match token_expression.kind() {
                SyntaxKind::TrueKeyword => self.true_type(),
                _ => unimplemented!(),
            },
            Expression::ObjectLiteralExpression(object_literal_expression) => {
                self.check_object_literal(object_literal_expression)
            }
            Expression::PrefixUnaryExpression(prefix_unary_expression) => {
                self.check_prefix_unary_expression(prefix_unary_expression)
            }
            // Expression::BinaryExpression(binary_expression) => {
            //     return self.check_binary_expression(binary_expression);
            // }
            Expression::LiteralLikeNode(LiteralLikeNode::StringLiteral(string_literal)) => {
                let type_: Rc<Type> = self.get_string_literal_type(string_literal.text());
                self.get_fresh_type_of_literal_type(&type_)
            }
            Expression::LiteralLikeNode(LiteralLikeNode::NumericLiteral(numeric_literal)) => {
                self.check_grammar_numeric_literal(numeric_literal);
                let type_: Rc<Type> = self.get_number_literal_type(numeric_literal.text().into());
                self.get_fresh_type_of_literal_type(&type_)
            }
            Expression::LiteralLikeNode(LiteralLikeNode::BigIntLiteral(big_int_literal)) => {
                let type_: Rc<Type> = self
                    .get_big_int_literal_type(PseudoBigInt::new(
                        false,
                        parse_pseudo_big_int(big_int_literal.text()),
                    ))
                    .into();
                self.get_fresh_type_of_literal_type(&type_)
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn check_type_parameter(&self, node: &TypeParameterDeclaration) {
        // TODO
    }

    pub(super) fn check_property_declaration(&mut self, node: &PropertySignature) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_property_signature(&mut self, node: &PropertySignature) {
        if is_private_identifier(&*node.name()) {
            self.error(
                Some(node.node_wrapper()),
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                None,
            );
        }
        self.check_property_declaration(node)
    }

    pub(super) fn get_effective_type_arguments(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
        type_parameters: &[Rc<Type /*TypeParameter*/>],
    ) -> Vec<Rc<Type>> {
        self.fill_missing_type_arguments(
            map(
                Some(node.as_has_type_arguments().maybe_type_arguments().unwrap()),
                |type_argument, _| self.get_type_from_type_node(type_argument),
            ),
            Some(type_parameters),
        )
        .unwrap()
    }

    pub(super) fn check_type_reference_node(&mut self, node: &TypeReferenceNode) {
        maybe_for_each(node.type_arguments.as_ref(), |type_argument, _| {
            self.check_source_element(Some(&**type_argument));
            Option::<()>::None
        });
        let type_ = self.get_type_from_type_reference(node);
    }

    pub(super) fn check_array_type(&mut self, node: &ArrayTypeNode) {
        self.check_source_element(Some(&*node.element_type));
    }

    pub(super) fn check_union_or_intersection_type(
        &mut self,
        node: &Node, /*UnionOrIntersectionTypeNode*/
    ) {
        for_each(
            node.as_union_or_intersection_type_node().types(),
            |type_, _| {
                self.check_source_element(Some(&**type_));
                Option::<()>::None
            },
        );
        self.get_type_from_type_node(node);
    }

    pub(super) fn check_block(&mut self, node: &Block) {
        if is_function_or_module_block(node) {
            for_each(&node.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        } else {
            for_each(&node.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        }
    }

    pub(super) fn convert_auto_to_any(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn check_variable_like_declaration<TNode: VariableLikeDeclarationInterface>(
        &mut self,
        node: &TNode,
    ) {
        if !is_binding_element(node) {
            self.check_source_element(node.type_());
        }

        let symbol = self.get_symbol_of_node(node).unwrap();

        let type_ = self.convert_auto_to_any(&self.get_type_of_symbol(&*symbol));
        let value_declaration = symbol.maybe_value_declaration();
        let wrapper = node.node_wrapper();
        if value_declaration.is_some()
            && Rc::ptr_eq(
                &wrapper,
                &value_declaration.as_ref().unwrap().upgrade().unwrap(),
            )
        {
            let initializer = get_effective_initializer(node);
            if let Some(initializer) = initializer {
                if true {
                    let initializer_type =
                        self.check_expression_cached(initializer.as_expression(), None);
                    self.check_type_assignable_to_and_optionally_elaborate(
                        &initializer_type,
                        &type_,
                        Some(&*wrapper),
                        Some(initializer.as_expression()),
                        None,
                    );
                }
            }
        } else {
            unimplemented!()
        }
    }

    pub(super) fn check_variable_declaration(&mut self, node: &VariableDeclaration) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_variable_statement(&mut self, node: &VariableStatement) {
        for_each(
            &node
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
            |declaration, _| Some(self.check_source_element(Some(&**declaration))),
        );
    }

    pub(super) fn check_expression_statement(&mut self, node: &ExpressionStatement) {
        let expression = enum_unwrapped!(&*node.expression, [Node, Expression]);
        self.check_expression(expression, None);
    }

    pub(super) fn check_if_statement(&mut self, node: &IfStatement) {
        let type_ = self.check_truthiness_expression(
            enum_unwrapped!(&*node.expression, [Node, Expression]),
            None,
        );
        self.check_source_element(Some(&*node.then_statement));

        if node.then_statement.kind() == SyntaxKind::EmptyStatement {
            self.error(
                Some(&*node.then_statement),
                &Diagnostics::The_body_of_an_if_statement_cannot_be_the_empty_statement,
                None,
            );
        }

        self.check_source_element(node.else_statement.clone());
    }

    pub(super) fn check_truthiness_of_type<TNode: NodeInterface>(
        &self,
        type_: &Type,
        node: &TNode,
    ) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Void) {
            self.error(
                Some(node.node_wrapper()),
                &Diagnostics::An_expression_of_type_void_cannot_be_tested_for_truthiness,
                None,
            );
        }

        type_.type_wrapper()
    }

    pub(super) fn check_truthiness_expression(
        &self,
        node: &Expression,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_truthiness_of_type(&self.check_expression(node, check_mode), node)
    }

    pub(super) fn check_type_parameters(
        &self,
        type_parameter_declarations: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
    ) {
        if let Some(type_parameter_declarations) = type_parameter_declarations {
            for node in type_parameter_declarations {
                self.check_type_parameter(enum_unwrapped!(
                    &**node,
                    [Node, TypeParameterDeclaration]
                ));
            }
        }
    }

    pub(super) fn check_interface_declaration(&mut self, node: &InterfaceDeclaration) {
        self.check_type_parameters(node.maybe_type_parameters());
        for_each(&node.members, |member, _| {
            self.check_source_element(Some(&**member));
            Option::<()>::None
        });
    }
}
