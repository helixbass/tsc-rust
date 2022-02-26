#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use crate::{
    bind_source_file, for_each, is_external_or_common_js_module, Diagnostic, Node, NodeInterface,
    SourceFile, TypeChecker, TypeCheckerHost,
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
            Node::PropertySignature(_) => self.check_property_signature(node),
            Node::TypeReferenceNode(_) => self.check_type_reference_node(node),
            Node::KeywordTypeNode(_) | Node::LiteralTypeNode(_) => (),
            Node::ArrayTypeNode(_) => self.check_array_type(node),
            Node::UnionTypeNode(_) => self.check_union_or_intersection_type(node),
            Node::Block(_) => self.check_block(node),
            Node::VariableStatement(_) => self.check_variable_statement(node),
            Node::ExpressionStatement(_) => self.check_expression_statement(node),
            Node::IfStatement(_) => self.check_if_statement(node),
            Node::VariableDeclaration(_) => self.check_variable_declaration(node),
            Node::InterfaceDeclaration(_) => self.check_interface_declaration(node),
            Node::TypeAliasDeclaration(_) => self.check_type_alias_declaration(node),
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

        let semantic_diagnostics = self
            .diagnostics()
            .get_diagnostics(Some(&source_file.file_name()));

        semantic_diagnostics
    }

    pub(super) fn initialize_type_checker<TTypeCheckerHost: TypeCheckerHost>(
        &mut self,
        host: &TTypeCheckerHost,
    ) {
        for file in host.get_source_files() {
            bind_source_file(&*file, self.compiler_options.clone());
            println!("post-binding: {:#?}", file);
        }

        for file in host.get_source_files() {
            if !is_external_or_common_js_module(&file) {
                self.merge_symbol_table(&mut *self.globals(), &*file.locals(), None);
            }
        }

        // self.global_array_type = self.get_global_type(__String::new("Array".to_string()));
    }

    pub(super) fn check_grammar_numeric_literal(
        &self,
        node: &Node, /*NumericLiteral*/
    ) -> bool {
        false
    }
}
