#![allow(non_upper_case_globals)]

use std::rc::Rc;

use crate::{
    for_each, DiagnosticMessage, HasTypeParametersInterface, Node, Symbol, Type, TypeChecker,
};

impl TypeChecker {
    pub(super) fn check_members_for_override_modifier(
        &self,
        node: &Node,  /*ClassLikeDeclaration*/
        type_: &Type, /*InterfaceType*/
        type_with_this: &Type,
        static_type: &Type, /*ObjectType*/
    ) {
        unimplemented!()
    }

    pub(super) fn issue_member_specific_error(
        &self,
        node: &Node, /*ClassLikeDeclaration*/
        type_with_this: &Type,
        base_with_this: &Type,
        broad_diag: &DiagnosticMessage,
    ) {
        unimplemented!()
    }

    pub(super) fn check_base_type_accessibility(
        &self,
        type_: &Type,
        node: &Node, /*ExpressionWithTypeArguments*/
    ) {
        unimplemented!()
    }

    pub(super) fn get_target_symbol(&self, s: &Symbol) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn get_class_or_interface_declarations_of_symbol(
        &self,
        s: &Symbol,
    ) -> Option<Vec<Rc<Node>>> {
        unimplemented!()
    }

    pub(super) fn check_kinds_of_property_member_overrides(
        &self,
        type_: &Type,     /*InterfaceType*/
        base_type: &Type, /*BaseType*/
    ) {
        unimplemented!()
    }

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
