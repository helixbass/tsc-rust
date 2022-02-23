#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use super::{BinderType, ContainerFlags, ModuleInstanceState};
use crate::{
    DiagnosticMessage, Symbol, SyntaxKind, __String, create_symbol_table, is_binding_pattern,
    is_block_or_catch_scoped, set_parent, InternalSymbolName, NamedDeclarationInterface, Node,
    NodeInterface, SymbolFlags,
};

impl BinderType {
    pub(super) fn declare_module_symbol(
        &self,
        node: &Node, /*ModuleDeclaration*/
    ) -> ModuleInstanceState {
        unimplemented!()
    }

    pub(super) fn bind_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) {
        self.bind_anonymous_declaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::Object(),
        );
    }

    pub(super) fn bind_anonymous_declaration(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        name: __String,
    ) -> Rc<Symbol> {
        let symbol = self.create_symbol(symbol_flags, name).wrap();
        self.file()
            .as_source_file()
            .keep_strong_reference_to_symbol(symbol.clone());
        self.add_declaration_to_symbol(&symbol, node, symbol_flags);
        symbol
    }

    pub(super) fn bind_block_scoped_declaration(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        let block_scope_container = self.block_scope_container();
        {
            let mut block_scope_container_locals = block_scope_container.maybe_locals();
            if block_scope_container_locals.is_none() {
                *block_scope_container_locals = Some(create_symbol_table(None));
            }
        }
        self.declare_symbol(
            &mut *block_scope_container.locals(),
            Option::<&Symbol>::None,
            node,
            symbol_flags,
            symbol_excludes,
            None,
            None,
        );
    }

    pub(super) fn delayed_bind_jsdoc_typedef_tag(&self) {
        // unimplemented!()
    }

    pub(super) fn error_on_first_token(
        &self,
        node: &Node,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        unimplemented!()
    }

    pub(super) fn error_or_suggestion_on_node(
        &self,
        is_error: bool,
        node: &Node,
        message: &DiagnosticMessage,
    ) {
        unimplemented!()
    }

    pub(super) fn bind<TNode: Borrow<Node>>(&self, node: Option<TNode>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let node = node.borrow();
        set_parent(node, self.maybe_parent());

        self.bind_worker(node);

        if node.kind() > SyntaxKind::LastToken {
            let save_parent = self.maybe_parent();
            self.set_parent(Some(node.node_wrapper()));
            let container_flags = self.get_container_flags(node);
            if container_flags == ContainerFlags::None {
                self.bind_children(node);
            } else {
                self.bind_container(node, container_flags);
            }
            self.set_parent(save_parent);
        }
    }

    pub(super) fn bind_jsdoc(&self, node: &Node) {
        // unimplemented!()
    }

    pub(super) fn bind_worker(&self, node: &Node) {
        match node {
            Node::TypeParameterDeclaration(_) => self.bind_type_parameter(node),
            Node::ParameterDeclaration(_) => self.bind_parameter(node),
            Node::VariableDeclaration(_) => self.bind_variable_declaration_or_binding_element(node),
            Node::PropertySignature(_) => self.bind_property_worker(node),
            Node::PropertyAssignment(_) => self.bind_property_or_method_or_accessor(
                node,
                SymbolFlags::Property,
                SymbolFlags::PropertyExcludes,
            ),
            Node::FunctionDeclaration(_) => self.bind_function_declaration(node),
            Node::ObjectLiteralExpression(_) => self.bind_object_literal_expression(node),
            Node::InterfaceDeclaration(_) => self.bind_block_scoped_declaration(
                node,
                SymbolFlags::Interface,
                SymbolFlags::InterfaceExcludes,
            ),
            Node::TypeAliasDeclaration(_) => self.bind_block_scoped_declaration(
                node,
                SymbolFlags::TypeAlias,
                SymbolFlags::TypeAliasExcludes,
            ),
            _ => (),
        }
    }

    pub(super) fn bind_property_worker(&self, node: &Node /*PropertySignature*/) {
        self.bind_property_or_method_or_accessor(
            node,
            SymbolFlags::Property
                | if false {
                    unimplemented!()
                } else {
                    SymbolFlags::None
                },
            SymbolFlags::PropertyExcludes,
        )
    }

    pub(super) fn is_top_level_namespace_assignment(
        &self,
        property_access: &Node, /*BindableAccessExpression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn bind_variable_declaration_or_binding_element(
        &self,
        node: &Node, /*VariableDeclaration*/
    ) {
        let node_as_variable_declaration = node.as_variable_declaration();
        if !is_binding_pattern(Some(node_as_variable_declaration.name())) {
            if false {
                unimplemented!()
            } else if is_block_or_catch_scoped(node) {
                self.bind_block_scoped_declaration(
                    node,
                    SymbolFlags::BlockScopedVariable,
                    SymbolFlags::BlockScopedVariableExcludes,
                );
            } else {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::FunctionScopedVariable,
                    SymbolFlags::FunctionScopedVariableExcludes,
                );
            }
        }
    }

    pub(super) fn bind_parameter(&self, node: &Node /*ParameterDeclaration*/) {
        if is_binding_pattern(Some(node.as_parameter_declaration().name())) {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::FunctionScopedVariable,
                SymbolFlags::ParameterExcludes,
            );
        }
    }

    pub(super) fn bind_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::Function,
                SymbolFlags::FunctionExcludes,
            );
        }
    }

    pub(super) fn bind_property_or_method_or_accessor(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(node, symbol_flags, symbol_excludes);
        }
    }

    pub(super) fn bind_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::TypeParameter,
                SymbolFlags::TypeParameterExcludes,
            );
        }
    }

    pub(super) fn check_unreachable(&self, node: &Node) -> bool {
        false
        // unimplemented!()
    }
}
