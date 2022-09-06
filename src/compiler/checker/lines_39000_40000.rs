#![allow(non_upper_case_globals)]

use std::ptr;
use std::rc::Rc;

use super::intrinsic_type_kinds;
use crate::{
    declaration_name_to_string, factory, for_each, get_declaration_of_kind,
    get_effective_modifier_flags, get_interface_base_type_nodes, has_abstract_modifier,
    is_entity_name_expression, is_identifier, is_optional_chain, is_private_identifier, is_static,
    length, maybe_for_each, set_parent, synthetic_factory, Diagnostics,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasTypeParametersInterface,
    InterfaceTypeInterface, ModifierFlags, NamedDeclarationInterface, Node, NodeFlags,
    NodeInterface, ReadonlyTextRange, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_property_initialization(&self, node: &Node /*ClassLikeDeclaration*/) {
        if !self.strict_null_checks
            || !self.strict_property_initialization
            || node.flags().intersects(NodeFlags::Ambient)
        {
            return;
        }
        let constructor = self.find_constructor_declaration(node);
        for member in node.as_class_like_declaration().members() {
            if get_effective_modifier_flags(member).intersects(ModifierFlags::Ambient) {
                continue;
            }
            if !is_static(member) && self.is_property_without_initializer(member) {
                let member_as_property_declaration = member.as_property_declaration();
                let prop_name = member_as_property_declaration.name();
                if is_identifier(&prop_name) || is_private_identifier(&prop_name) {
                    let type_ = self.get_type_of_symbol(&self.get_symbol_of_node(member).unwrap());
                    if !(type_.flags().intersects(TypeFlags::AnyOrUnknown)
                        || self
                            .get_falsy_flags(&type_)
                            .intersects(TypeFlags::Undefined))
                    {
                        if match constructor.as_ref() {
                            None => true,
                            Some(constructor) => !self.is_property_initialized_in_constructor(
                                &prop_name,
                                &type_,
                                constructor,
                            ),
                        } {
                            self.error(
                                member_as_property_declaration.maybe_name(),
                                &Diagnostics::Property_0_has_no_initializer_and_is_not_definitely_assigned_in_the_constructor,
                                Some(vec![
                                    declaration_name_to_string(Some(&*prop_name)).into_owned()
                                ])
                            );
                        }
                    }
                }
            }
        }
    }

    pub(super) fn is_property_without_initializer(&self, node: &Node) -> bool {
        node.kind() == SyntaxKind::PropertyDeclaration && !has_abstract_modifier(node) && {
            let node_as_property_declaration = node.as_property_declaration();
            node_as_property_declaration.exclamation_token.is_none()
                && node_as_property_declaration.maybe_initializer().is_none()
        }
    }

    pub(super) fn is_property_initialized_in_static_blocks(
        &self,
        prop_name: &Node, /*Identifier | PrivateIdentifier*/
        prop_type: &Type,
        static_blocks: &[Rc<Node /*ClassStaticBlockDeclaration*/>],
        start_pos: isize,
        end_pos: isize,
    ) -> bool {
        for static_block in static_blocks {
            if static_block.pos() >= start_pos && static_block.pos() <= end_pos {
                let reference: Rc<Node> = factory.with(|factory_| {
                    synthetic_factory.with(|synthetic_factory_| {
                        factory_
                            .create_property_access_expression(
                                synthetic_factory_,
                                factory_.create_this(synthetic_factory_).into(),
                                prop_name.node_wrapper(),
                            )
                            .into()
                    })
                });
                set_parent(
                    &reference.as_property_access_expression().expression,
                    Some(&*reference),
                );
                set_parent(&reference, Some(&**static_block));
                *reference.maybe_flow_node() = static_block
                    .as_class_static_block_declaration()
                    .maybe_return_flow_node();
                let flow_type = self.get_flow_type_of_reference(
                    &reference,
                    prop_type,
                    Some(self.get_optional_type_(prop_type, None)),
                    Option::<&Node>::None,
                );
                if !self
                    .get_falsy_flags(&flow_type)
                    .intersects(TypeFlags::Undefined)
                {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn is_property_initialized_in_constructor(
        &self,
        prop_name: &Node, /*Identifier | PrivateIdentifier*/
        prop_type: &Type,
        constructor: &Node, /*ConstructorDeclaration*/
    ) -> bool {
        let reference: Rc<Node> = factory.with(|factory_| {
            synthetic_factory.with(|synthetic_factory_| {
                factory_
                    .create_property_access_expression(
                        synthetic_factory_,
                        factory_.create_this(synthetic_factory_).into(),
                        prop_name.node_wrapper(),
                    )
                    .into()
            })
        });
        set_parent(
            &reference.as_property_access_expression().expression,
            Some(&*reference),
        );
        set_parent(&reference, Some(constructor));
        *reference.maybe_flow_node() = constructor
            .as_constructor_declaration()
            .maybe_return_flow_node();
        let flow_type = self.get_flow_type_of_reference(
            &reference,
            prop_type,
            Some(self.get_optional_type_(prop_type, None)),
            Option::<&Node>::None,
        );
        !self
            .get_falsy_flags(&flow_type)
            .intersects(TypeFlags::Undefined)
    }

    pub(super) fn check_interface_declaration(&self, node: &Node /*InterfaceDeclaration*/) {
        if !self.check_grammar_decorators_and_modifiers(node) {
            self.check_grammar_interface_declaration(node);
        }

        let node_as_interface_declaration = node.as_interface_declaration();
        self.check_type_parameters(
            node_as_interface_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        if self.produce_diagnostics {
            self.check_type_name_is_reserved(
                &node_as_interface_declaration.name(),
                &Diagnostics::Interface_name_cannot_be_0,
            );

            self.check_exports_on_merged_declarations(node);
            let symbol = self.get_symbol_of_node(node).unwrap();
            self.check_type_parameter_lists_identical(&symbol);

            let first_interface_decl =
                get_declaration_of_kind(&symbol, SyntaxKind::InterfaceDeclaration);
            if matches!(
                first_interface_decl.as_ref(),
                Some(first_interface_decl) if ptr::eq(node, &**first_interface_decl)
            ) {
                let type_ = self.get_declared_type_of_symbol(&symbol);
                let type_with_this =
                    self.get_type_with_this_argument(&type_, Option::<&Type>::None, None);
                let type_as_interface_type = type_.as_interface_type();
                if self.check_inherited_properties_are_identical(
                    &type_,
                    &node_as_interface_declaration.name(),
                ) {
                    for base_type in &self.get_base_types(&type_) {
                        self.check_type_assignable_to(
                            &type_with_this,
                            &self.get_type_with_this_argument(
                                base_type,
                                type_as_interface_type.maybe_this_type(),
                                None,
                            ),
                            node_as_interface_declaration.maybe_name(),
                            Some(&Diagnostics::Interface_0_incorrectly_extends_interface_1),
                            None,
                            None,
                        );
                    }
                    self.check_index_constraints(&type_, &symbol, None);
                }
            }
            self.check_object_type_for_duplicate_declarations(node);
        }
        maybe_for_each(
            get_interface_base_type_nodes(node).as_ref(),
            |heritage_element: &Rc<Node>, _| -> Option<()> {
                let heritage_element_as_expression_with_type_arguments =
                    heritage_element.as_expression_with_type_arguments();
                if !is_entity_name_expression(heritage_element)
                    || is_optional_chain(
                        &heritage_element_as_expression_with_type_arguments.expression,
                    )
                {
                    self.error(
                        Some(&*heritage_element_as_expression_with_type_arguments.expression),
                        &Diagnostics::An_interface_can_only_extend_an_identifier_Slashqualified_name_with_optional_type_arguments,
                        None,
                    );
                }
                self.check_type_reference_node(heritage_element);
                None
            },
        );

        for_each(&node_as_interface_declaration.members, |member, _| {
            self.check_source_element(Some(&**member));
            Option::<()>::None
        });

        if self.produce_diagnostics {
            self.check_type_for_duplicate_index_signatures(node);
            self.register_for_unused_identifiers_check(node);
        }
    }

    pub(super) fn check_type_alias_declaration(&self, node: &Node /*TypeAliasDeclaration*/) {
        self.check_grammar_decorators_and_modifiers(node);
        self.check_type_name_is_reserved(node, &Diagnostics::Type_alias_name_cannot_be_0);
        self.check_exports_on_merged_declarations(node);
        let node_as_type_alias_declaration = node.as_type_alias_declaration();
        self.check_type_parameters(
            node_as_type_alias_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        if node_as_type_alias_declaration.type_.kind() == SyntaxKind::IntrinsicKeyword {
            if !intrinsic_type_kinds.contains_key(
                &&*node_as_type_alias_declaration
                    .name()
                    .as_identifier()
                    .escaped_text,
            ) || length(
                node_as_type_alias_declaration
                    .maybe_type_parameters()
                    .as_deref(),
            ) != 1
            {
                self.error(
                    Some(&*node_as_type_alias_declaration.type_),
                    &Diagnostics::The_intrinsic_keyword_can_only_be_used_to_declare_compiler_provided_intrinsic_types,
                    None,
                );
            }
        } else {
            self.check_source_element(Some(&*node_as_type_alias_declaration.type_));
            self.register_for_unused_identifiers_check(node);
        }
    }

    pub(super) fn check_alias_symbol(
        &self,
        node: &Node, /*ImportEqualsDeclaration | VariableDeclaration | ImportClause | NamespaceImport | ImportSpecifier | ExportSpecifier | NamespaceExport*/
    ) {
        unimplemented!()
    }
}
