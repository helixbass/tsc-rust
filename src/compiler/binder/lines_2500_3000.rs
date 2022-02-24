#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use super::BinderType;
use crate::{
    __String, is_assignment_expression, is_binding_pattern, is_block_or_catch_scoped,
    is_exports_identifier, is_identifier, is_module_exports_access_expression, is_source_file,
    is_variable_declaration, HasInitializerInterface, NamedDeclarationInterface, Node,
    NodeInterface, Symbol, SymbolFlags, SymbolInterface,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ElementKind {
    Property = 1,
    Accessor = 2,
}

impl BinderType {
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

    pub(super) fn bind_potentially_missing_namespaces<TNamespaceSymbol: Borrow<Symbol>>(
        &self,
        namespace_symbol: Option<TNamespaceSymbol>,
        entity_name: &Node, /*BindableStaticNameExpression*/
        is_top_level: bool,
        is_prototype_property: bool,
        container_is_class: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
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

pub fn is_exports_or_module_exports_or_alias(
    source_file: &Node, /*SourceFile*/
    node: &Node,        /*Expression*/
) -> bool {
    let mut node = node.node_wrapper();
    let mut i = 0;
    let mut q = vec![node];
    while !q.is_empty() && i < 100 {
        i += 1;
        node = q.remove(0);
        if is_exports_identifier(&node) || is_module_exports_access_expression(&node) {
            return true;
        } else if is_identifier(&node) {
            let symbol = lookup_symbol_for_name(&source_file, &node.as_identifier().escaped_text);
            if let Some(symbol) = symbol {
                if let Some(symbol_value_declaration) =
                    symbol
                        .maybe_value_declaration()
                        .filter(|value_declaration| {
                            is_variable_declaration(&value_declaration)
                                && value_declaration
                                    .as_variable_declaration()
                                    .maybe_initializer()
                                    .is_some()
                        })
                {
                    let init = symbol_value_declaration
                        .as_variable_declaration()
                        .maybe_initializer()
                        .unwrap();
                    q.push(init.clone());
                    if is_assignment_expression(&init, Some(true)) {
                        let init_as_binary_expression = init.as_binary_expression();
                        q.push(init_as_binary_expression.left.clone());
                        q.push(init_as_binary_expression.right.clone());
                    }
                }
            }
        }
    }
    false
}

fn lookup_symbol_for_name(container: &Node, name: &__String) -> Option<Rc<Symbol>> {
    let container_locals = container.maybe_locals();
    let local = container_locals
        .as_ref()
        .and_then(|locals| locals.get(name));
    if let Some(local) = local {
        return Some(local.maybe_export_symbol().unwrap_or(local.clone()));
    }
    if is_source_file(container) {
        let container_as_source_file = container.as_source_file();
        if let Some(container_js_global_augmentations) = container_as_source_file
            .maybe_js_global_augmentations()
            .as_ref()
        {
            let container_js_global_augmentations = container_js_global_augmentations.borrow_mut(); // TODO: doesn't actually need to be mut
            if container_js_global_augmentations.contains_key(name) {
                return container_js_global_augmentations
                    .get(name)
                    .map(Clone::clone);
            }
        }
    }
    container
        .maybe_symbol()
        .and_then(|symbol| symbol.maybe_exports().clone())
        .and_then(|exports| exports.borrow_mut().get(name).map(Clone::clone)) // TODO: same here doesn't need to be mut
}
