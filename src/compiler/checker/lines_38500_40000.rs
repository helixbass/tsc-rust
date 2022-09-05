#![allow(non_upper_case_globals)]

use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use crate::{
    are_option_rcs_equal, declaration_name_to_string, find_ancestor, for_each, for_each_bool,
    for_each_child, get_class_extends_heritage_element, get_declaration_of_kind,
    get_effective_base_type_node, get_effective_constraint_of_type_parameter,
    get_effective_implements_type_nodes, get_effective_type_parameter_declarations,
    get_name_of_declaration, get_object_flags, get_source_file_of_node,
    get_span_of_token_at_position, get_text_of_node, has_static_modifier, has_syntactic_modifier,
    is_class_like, is_entity_name_expression, is_function_like, is_identifier, is_optional_chain,
    is_private_identifier, is_private_identifier_class_element_declaration, is_static, length,
    maybe_for_each, some, ClassLikeDeclarationInterface, DiagnosticMessage, Diagnostics,
    ExternalEmitHelpers, FindAncestorCallbackReturn, HasTypeParametersInterface, IndexInfo,
    InterfaceTypeInterface, ModifierFlags, ModuleKind, NamedDeclarationInterface, Node, NodeArray,
    NodeFlags, NodeInterface, ObjectFlags, ReadonlyTextRange, ScriptTarget, Signature,
    SignatureFlags, SignatureKind, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type,
    TypeChecker, __String, for_each_key, get_effective_type_annotation_node, get_root_declaration,
    HasInitializerInterface, TypeFlags, TypeInterface,
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
