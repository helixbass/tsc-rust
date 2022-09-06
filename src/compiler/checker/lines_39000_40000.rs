#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::CheckTypeContainingMessageChain;
use crate::{
    __String, chain_diagnostic_messages, create_diagnostic_for_node_from_message_chain,
    escape_leading_underscores, first, for_each, get_check_flags,
    get_class_like_declaration_of_symbol, get_declaration_modifier_flags_from_symbol,
    get_effective_base_type_node, get_name_of_declaration, get_text_of_property_name,
    has_abstract_modifier, has_ambient_modifier, has_effective_modifier, has_override_modifier,
    has_syntactic_modifier, is_binary_expression, is_constructor_declaration, is_identifier,
    is_in_js_file, is_parameter_property_declaration, is_property_declaration, is_static, length,
    maybe_filter, maybe_for_each, some, symbol_name, unescape_leading_underscores, CheckFlags,
    Debug_, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, HasInitializerInterface,
    HasTypeParametersInterface, InterfaceTypeInterface, MemberOverrideStatus, ModifierFlags,
    NamedDeclarationInterface, Node, NodeFlags, NodeInterface, SignatureDeclarationInterface,
    SignatureKind, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, TransientSymbolInterface,
    Type, TypeChecker, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_property_initialization(&self, node: &Node /*ClassLikeDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn is_property_without_initializer(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_property_initialized_in_static_blocks(
        &self,
        prop_name: &Node, /*Identifier | PrivateIdentifier*/
        prop_type: &Type,
        static_blocks: &[Rc<Node /*ClassStaticBlockDeclaration*/>],
        start_pos: isize,
        end_pos: isize,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_property_initialized_in_constructor(
        &self,
        prop_name: &Node, /*Identifier | PrivateIdentifier*/
        prop_type: &Type,
        constructor: &Node, /*ConstructorDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_interface_declaration(&self, node: &Node /*InterfaceDeclaration*/) {
        let node_as_interface_declaration = node.as_interface_declaration();
        self.check_type_parameters(
            node_as_interface_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        for_each(&node_as_interface_declaration.members, |member, _| {
            self.check_source_element(Some(&**member));
            Option::<()>::None
        });
    }

    pub(super) fn check_type_alias_declaration(&self, node: &Node /*TypeAliasDeclaration*/) {
        let node_as_type_alias_declaration = node.as_type_alias_declaration();
        self.check_type_parameters(
            node_as_type_alias_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        if false {
            unimplemented!()
        } else {
            self.check_source_element(Some(&*node_as_type_alias_declaration.type_));
        }
    }

    pub(super) fn check_alias_symbol(
        &self,
        node: &Node, /*ImportEqualsDeclaration | VariableDeclaration | ImportClause | NamespaceImport | ImportSpecifier | ExportSpecifier | NamespaceExport*/
    ) {
        unimplemented!()
    }
}
