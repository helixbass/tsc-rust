#![allow(non_upper_case_globals)]

mod lines_0_8000;
pub use lines_0_8000::*;
mod lines_8000_16000;
pub use lines_8000_16000::*;
mod lines_16000_24000;
pub use lines_16000_24000::*;
mod lines_24000_32000;
pub use lines_24000_32000::*;

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

use crate::{
    TypeNode, UnionOrIntersectionType, UnionOrIntersectionTypeInterface, UnionType,
    VariableDeclaration, VariableStatement, __String, bind_source_file, chain_diagnostic_messages,
    create_diagnostic_collection, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, create_printer, create_symbol_table,
    create_text_writer, declaration_name_to_string, escape_leading_underscores, every, factory,
    first_defined, first_or_undefined, for_each, get_effective_initializer,
    get_effective_type_annotation_node, get_first_identifier, get_name_of_declaration,
    get_object_flags, get_source_file_of_node, get_synthetic_factory, has_dynamic_name,
    has_initializer, is_binding_element, is_external_or_common_js_module, is_identifier_text,
    is_object_literal_expression, is_private_identifier, is_property_assignment,
    is_property_declaration, is_property_signature, is_variable_declaration, node_is_missing,
    object_allocator, unescape_leading_underscores, using_single_line_string_writer, ArrayTypeNode,
    BaseInterfaceType, BaseIntrinsicType, BaseLiteralType, BaseNodeFactorySynthetic,
    BaseObjectType, BaseType, BaseUnionOrIntersectionType, CharacterCodes, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, EmitHint,
    EmitTextWriter, Expression, ExpressionStatement, FreshableIntrinsicType,
    HasExpressionInitializerInterface, InterfaceDeclaration, InterfaceType, IntrinsicType,
    KeywordTypeNode, LiteralLikeNode, LiteralLikeNodeInterface, LiteralType, LiteralTypeInterface,
    NamedDeclarationInterface, Node, NodeInterface, Number, NumberLiteralType, NumericLiteral,
    ObjectFlags, ObjectFlagsTypeInterface, ObjectLiteralExpression, PrefixUnaryExpression,
    PrinterOptions, PropertyAssignment, PropertySignature, RelationComparisonResult,
    ResolvableTypeInterface, ResolvedTypeInterface, SourceFile, Statement, StringLiteralType,
    Symbol, SymbolFlags, SymbolFormatFlags, SymbolTable, SymbolTracker, SyntaxKind, Ternary, Type,
    TypeChecker, TypeCheckerHost, TypeElement, TypeFlags, TypeInterface, TypeReferenceNode,
    UnionReduction, VariableLikeDeclarationInterface,
};

impl TypeChecker {
    fn check_arithmetic_operand_type(
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

    fn check_prefix_unary_expression(&self, node: &PrefixUnaryExpression) -> Rc<Type> {
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

    fn get_unary_result_type(&self, operand_type: &Type) -> Rc<Type> {
        self.number_type()
    }

    fn maybe_type_of_kind(&self, type_: &Type, kind: TypeFlags) -> bool {
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

    fn check_expression_cached(&mut self, node: &Expression) -> Rc<Type> {
        self.check_expression(node)
    }

    fn is_literal_of_contextual_type(
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

    fn check_expression_for_mutable_location(
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

    fn check_property_assignment(&self, node: &PropertyAssignment) -> Rc<Type> {
        self.check_expression_for_mutable_location(
            match &*node.initializer {
                Node::Expression(expression) => expression,
                _ => panic!("Expected Expression"),
            },
            None,
        )
    }

    fn check_expression(&self, node: &Expression) -> Rc<Type> {
        self.check_expression_worker(node)
    }

    fn check_expression_worker(&self, node: &Expression) -> Rc<Type> {
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

    fn check_property_declaration(&mut self, node: &PropertySignature) {
        self.check_variable_like_declaration(node);
    }

    fn check_property_signature(&mut self, node: &PropertySignature) {
        if is_private_identifier(&*node.name()) {
            self.error(
                Some(node),
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
            );
        }
        self.check_property_declaration(node)
    }

    fn check_type_reference_node(&mut self, node: &TypeReferenceNode) {
        let type_ = self.get_type_from_type_reference(node);
    }

    fn check_array_type(&mut self, node: &ArrayTypeNode) {
        self.check_source_element(Some(&*node.element_type));
    }

    fn convert_auto_to_any(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    fn check_variable_like_declaration<TNode: VariableLikeDeclarationInterface>(
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

    fn check_variable_declaration(&mut self, node: &VariableDeclaration) {
        self.check_variable_like_declaration(node);
    }

    fn check_variable_statement(&mut self, node: &VariableStatement) {
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

    fn check_expression_statement(&mut self, node: &ExpressionStatement) {
        let expression = match &*node.expression {
            Node::Expression(expression) => expression,
            _ => panic!("Expected Expression"),
        };
        self.check_expression(expression);
    }

    fn check_interface_declaration(&mut self, node: &InterfaceDeclaration) {
        for_each(&node.members, |member, _| {
            self.check_source_element(Some(&**member));
            Option::<()>::None
        });
    }

    fn check_source_element<TNodeRef: Borrow<Node>>(&mut self, node: Option<TNodeRef>) {
        if let Some(node) = node {
            let node = node.borrow();
            self.check_source_element_worker(node);
        }
    }

    fn check_source_element_worker(&mut self, node: &Node) {
        match node {
            Node::TypeElement(TypeElement::PropertySignature(property_signature)) => {
                self.check_property_signature(property_signature)
            }
            Node::TypeNode(TypeNode::TypeReferenceNode(type_reference_node)) => {
                self.check_type_reference_node(type_reference_node)
            }
            Node::TypeNode(TypeNode::KeywordTypeNode(_)) => (),
            Node::TypeNode(TypeNode::ArrayTypeNode(array_type_node)) => {
                self.check_array_type(array_type_node)
            }
            Node::Statement(Statement::VariableStatement(variable_statement)) => {
                self.check_variable_statement(variable_statement)
            }
            Node::Statement(Statement::ExpressionStatement(expression_statement)) => {
                self.check_expression_statement(expression_statement)
            }
            Node::VariableDeclaration(variable_declaration) => {
                self.check_variable_declaration(variable_declaration)
            }
            Node::Statement(Statement::InterfaceDeclaration(interface_declaration)) => {
                self.check_interface_declaration(interface_declaration)
            }
            _ => unimplemented!("{:?}", node.kind()),
        };
    }

    fn check_source_file(&mut self, source_file: &SourceFile) {
        self.check_source_file_worker(source_file)
    }

    fn check_source_file_worker(&mut self, node: &SourceFile) {
        if true {
            for_each(&node.statements, |statement, _index| {
                self.check_source_element(Some(&**statement));
                Option::<()>::None
            });
        }
    }

    pub fn get_diagnostics(&mut self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    fn get_diagnostics_worker(&mut self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self.diagnostics().get_diagnostics(&source_file.file_name);

        semantic_diagnostics
    }

    fn initialize_type_checker<TTypeCheckerHost: TypeCheckerHost>(
        &mut self,
        host: &TTypeCheckerHost,
    ) {
        for file in host.get_source_files() {
            bind_source_file(&*file);
            println!("post-binding: {:#?}", file);
        }

        for file in host.get_source_files() {
            if !is_external_or_common_js_module(match &*file {
                Node::SourceFile(source_file) => source_file,
                _ => panic!("Expected SourceFile"),
            }) {
                self.merge_symbol_table(&mut *self.globals(), &*file.locals(), None);
            }
        }

        // self.global_array_type = self.get_global_type(__String::new("Array".to_string()));
    }

    fn check_grammar_numeric_literal(&self, node: &NumericLiteral) -> bool {
        false
    }
}
