#![allow(non_upper_case_globals)]

use std::rc::Rc;

use crate::{
    for_each, get_effective_initializer, is_binding_element, is_private_identifier, maybe_for_each,
    ArrayTypeNode, DiagnosticMessage, Diagnostics, Expression, ExpressionStatement,
    HasTypeParametersInterface, InterfaceDeclaration, LiteralLikeNode, LiteralLikeNodeInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, PrefixUnaryExpression,
    PropertyAssignment, PropertySignature, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeParameterDeclaration, TypeReferenceNode, UnionOrIntersectionTypeInterface,
    VariableDeclaration, VariableLikeDeclarationInterface, VariableStatement,
};

impl TypeChecker {
    pub(super) fn check_arithmetic_operand_type(
        &self,
        operand: /*&Node*/ &Expression,
        type_: Rc<Type>,
        diagnostic: &DiagnosticMessage,
    ) -> bool {
        if !self.is_type_assignable_to(type_, self.number_or_big_int_type()) {
            self.error_and_maybe_suggest_await(operand, diagnostic);
            return false;
        }
        true
    }

    pub(super) fn check_prefix_unary_expression(&self, node: &PrefixUnaryExpression) -> Rc<Type> {
        let operand_expression = match &*node.operand {
            Node::Expression(expression) => expression,
            _ => panic!("Expected Expression"),
        };
        let operand_type = self.check_expression(operand_expression);
        match node.operator {
            SyntaxKind::PlusPlusToken => {
                self.check_arithmetic_operand_type(operand_expression, operand_type.clone(), &Diagnostics::An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type);
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

    pub(super) fn check_expression_cached(&mut self, node: &Expression) -> Rc<Type> {
        self.check_expression(node)
    }

    pub(super) fn is_literal_of_contextual_type(
        &self,
        candidate_type: Rc<Type>,
        contextual_type: Option<Rc<Type>>,
    ) -> bool {
        if let Some(contextual_type) = contextual_type {
            if let Type::UnionOrIntersectionType(union_or_intersection_type) = &*contextual_type {
                let types = union_or_intersection_type.types();
                // return some(
                //     types,
                //     Some(Box::new(|t| {
                //         self.is_literal_of_contextual_type(candidate_type, Some(t.clone()))
                //     })),
                // );
                return types.iter().any(|t| {
                    self.is_literal_of_contextual_type(candidate_type.clone(), Some(t.clone()))
                });
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

    pub(super) fn check_expression_for_mutable_location(
        &self,
        node: &Expression,
        contextual_type: Option<Rc<Type>>,
    ) -> Rc<Type> {
        let type_ = self.check_expression(node);
        if false {
            unimplemented!()
        } else {
            self.get_widened_literal_like_type_for_contextual_type(
                type_,
                self.instantiate_contextual_type(
                    if contextual_type.is_none() {
                        self.get_contextual_type(node)
                    } else {
                        Some(contextual_type.unwrap())
                    },
                    node,
                ),
            )
        }
    }

    pub(super) fn check_property_assignment(&self, node: &PropertyAssignment) -> Rc<Type> {
        self.check_expression_for_mutable_location(
            match &*node.initializer {
                Node::Expression(expression) => expression,
                _ => panic!("Expected Expression"),
            },
            None,
        )
    }

    pub(super) fn check_expression(&self, node: &Expression) -> Rc<Type> {
        self.check_expression_worker(node)
    }

    pub(super) fn check_expression_worker(&self, node: &Expression) -> Rc<Type> {
        match node {
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
            Expression::LiteralLikeNode(LiteralLikeNode::NumericLiteral(numeric_literal)) => {
                self.check_grammar_numeric_literal(numeric_literal);
                let type_: Rc<Type> = self.get_number_literal_type(numeric_literal.text().into());
                self.get_fresh_type_of_literal_type(type_)
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn check_type_parameter(&self, node: &TypeParameterDeclaration) {}

    pub(super) fn check_property_declaration(&mut self, node: &PropertySignature) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_property_signature(&mut self, node: &PropertySignature) {
        if is_private_identifier(&*node.name()) {
            self.error(
                Some(node),
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
            );
        }
        self.check_property_declaration(node)
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

    pub(super) fn convert_auto_to_any(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    pub(super) fn check_variable_like_declaration<TNode: VariableLikeDeclarationInterface>(
        &mut self,
        node: &TNode,
    ) {
        if !is_binding_element(node) {
            self.check_source_element(node.type_());
        }

        let symbol = self.get_symbol_of_node(node).unwrap();

        let type_ = self.convert_auto_to_any(self.get_type_of_symbol(&*symbol));
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
                    let initializer_type = self.check_expression_cached(match &*initializer {
                        Node::Expression(expression) => expression,
                        _ => panic!("Expected Expression"),
                    });
                    self.check_type_assignable_to_and_optionally_elaborate(
                        initializer_type,
                        type_,
                        Some(&*wrapper),
                        Some(match &*initializer {
                            Node::Expression(expression) => expression,
                            _ => panic!("Expected Expression"),
                        }),
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
            &match &*node.declaration_list {
                Node::VariableDeclarationList(variable_declaration_list) => {
                    variable_declaration_list
                }
                _ => panic!("Expected VariableDeclarationList"),
            }
            .declarations,
            |declaration, _| Some(self.check_source_element(Some(&**declaration))),
        );
    }

    pub(super) fn check_expression_statement(&mut self, node: &ExpressionStatement) {
        let expression = match &*node.expression {
            Node::Expression(expression) => expression,
            _ => panic!("Expected Expression"),
        };
        self.check_expression(expression);
    }

    pub(super) fn check_type_parameters(
        &self,
        type_parameter_declarations: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
    ) {
        if let Some(type_parameter_declarations) = type_parameter_declarations {
            for node in type_parameter_declarations {
                self.check_type_parameter(match &**node {
                    Node::TypeParameterDeclaration(type_parameter_declaration) => {
                        type_parameter_declaration
                    }
                    _ => panic!("Expected TypeParameterDeclaration"),
                });
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
