#![allow(non_upper_case_globals)]

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
    pub(super) fn check_source_element<TNodeRef: Borrow<Node>>(&mut self, node: Option<TNodeRef>) {
        if let Some(node) = node {
            let node = node.borrow();
            self.check_source_element_worker(node);
        }
    }

    pub(super) fn check_source_element_worker(&mut self, node: &Node) {
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

    pub(super) fn check_source_file(&mut self, source_file: &SourceFile) {
        self.check_source_file_worker(source_file)
    }

    pub(super) fn check_source_file_worker(&mut self, node: &SourceFile) {
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

    pub(super) fn get_diagnostics_worker(
        &mut self,
        source_file: &SourceFile,
    ) -> Vec<Rc<Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self.diagnostics().get_diagnostics(&source_file.file_name);

        semantic_diagnostics
    }

    pub(super) fn initialize_type_checker<TTypeCheckerHost: TypeCheckerHost>(
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

    pub(super) fn check_grammar_numeric_literal(&self, node: &NumericLiteral) -> bool {
        false
    }
}
