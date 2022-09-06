#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use super::CheckTypeContainingMessageChain;
use crate::{
    chain_diagnostic_messages, escape_leading_underscores, first, for_each,
    get_class_like_declaration_of_symbol, get_effective_base_type_node, get_text_of_property_name,
    has_abstract_modifier, has_ambient_modifier, has_effective_modifier, has_override_modifier,
    has_syntactic_modifier, is_constructor_declaration, is_in_js_file,
    is_parameter_property_declaration, is_static, some, symbol_name, unescape_leading_underscores,
    DiagnosticMessage, DiagnosticMessageChain, Diagnostics, HasTypeParametersInterface,
    InterfaceTypeInterface, MemberOverrideStatus, ModifierFlags, Node, NodeFlags, NodeInterface,
    SignatureDeclarationInterface, SignatureKind, Symbol, SymbolInterface, Type, TypeChecker,
    TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_members_for_override_modifier(
        &self,
        node: &Node,  /*ClassLikeDeclaration*/
        type_: &Type, /*InterfaceType*/
        type_with_this: &Type,
        static_type: &Type, /*ObjectType*/
    ) {
        let base_type_node = get_effective_base_type_node(node);
        let base_types = if base_type_node.is_some() {
            Some(self.get_base_types(type_))
        } else {
            None
        };
        let base_with_this = base_types
            .as_ref()
            .filter(|base_types| !base_types.is_empty())
            .map(|base_types| {
                self.get_type_with_this_argument(
                    &*first(base_types),
                    type_.as_interface_type().maybe_this_type(),
                    None,
                )
            });
        let base_static_type = self.get_base_constructor_type_of_class(type_);

        for member in node.as_class_like_declaration().members() {
            if has_ambient_modifier(member) {
                continue;
            }

            if is_constructor_declaration(member) {
                for_each(
                    member.as_constructor_declaration().parameters(),
                    |param: &Rc<Node>, _| -> Option<()> {
                        if is_parameter_property_declaration(param, member) {
                            self.check_existing_member_for_override_modifier(
                                node,
                                static_type,
                                &base_static_type,
                                base_with_this.as_deref(),
                                type_,
                                type_with_this,
                                param,
                                true,
                                None,
                            );
                        }
                        None
                    },
                );
            }
            self.check_existing_member_for_override_modifier(
                node,
                static_type,
                &base_static_type,
                base_with_this.as_deref(),
                type_,
                type_with_this,
                member,
                false,
                None,
            );
        }
    }

    pub(super) fn check_existing_member_for_override_modifier<TBaseWithThis: Borrow<Type>>(
        &self,
        node: &Node,        /*ClassLikeDeclaration*/
        static_type: &Type, /*ObjectType*/
        base_static_type: &Type,
        base_with_this: Option<TBaseWithThis>,
        type_: &Type, /*InterfaceType*/
        type_with_this: &Type,
        member: &Node, /*ClassElement | ParameterPropertyDeclaration*/
        member_is_parameter_property: bool,
        report_errors: Option<bool>,
    ) -> MemberOverrideStatus {
        let report_errors = report_errors.unwrap_or(true);
        let declared_prop =
            if let Some(member_name) = member.as_named_declaration().maybe_name().as_ref() {
                self.get_symbol_at_location_(member_name, None)
            } else {
                self.get_symbol_at_location_(member, None)
            };
        if declared_prop.is_none() {
            return MemberOverrideStatus::Ok;
        }
        let declared_prop = declared_prop.unwrap();

        self.check_member_for_override_modifier(
            node,
            static_type,
            base_static_type,
            base_with_this,
            type_,
            type_with_this,
            has_override_modifier(member),
            has_abstract_modifier(member),
            is_static(member),
            member_is_parameter_property,
            &symbol_name(&declared_prop),
            if report_errors { Some(member) } else { None },
        )
    }

    pub(super) fn check_member_for_override_modifier<
        TBaseWithThis: Borrow<Type>,
        TErrorNode: Borrow<Node>,
    >(
        &self,
        node: &Node,        /*ClassLikeDeclaration*/
        static_type: &Type, /*ObjectType*/
        base_static_type: &Type,
        base_with_this: Option<TBaseWithThis>,
        type_: &Type, /*InterfaceType*/
        type_with_this: &Type,
        member_has_override_modifier: bool,
        member_has_abstract_modifier: bool,
        member_is_static: bool,
        member_is_parameter_property: bool,
        member_name: &str,
        error_node: Option<TErrorNode>,
    ) -> MemberOverrideStatus {
        let is_js = is_in_js_file(Some(node));
        let node_in_ambient_context = node.flags().intersects(NodeFlags::Ambient);
        if let Some(base_with_this) = base_with_this.filter(|_| {
            member_has_override_modifier || self.compiler_options.no_implicit_override == Some(true)
        }) {
            let base_with_this: &Type = base_with_this.borrow();
            let member_escaped_name = escape_leading_underscores(member_name);
            let this_type = if member_is_static {
                static_type
            } else {
                type_with_this
            };
            let base_type = if member_is_static {
                base_static_type
            } else {
                base_with_this
            };
            let prop = self.get_property_of_type_(this_type, &member_escaped_name, None);
            let base_prop = self.get_property_of_type_(base_type, &member_escaped_name, None);

            let base_class_name =
                self.type_to_string_(base_with_this, Option::<&Node>::None, None, None);
            if let Some(prop) = prop
                .as_ref()
                .filter(|_| base_prop.is_none() && member_has_override_modifier)
            {
            } else if let (Some(prop), Some(base_prop_declarations)) = (
                prop.as_ref(),
                base_prop
                    .as_ref()
                    .and_then(|base_prop| base_prop.maybe_declarations().clone())
                    .as_ref()
                    .filter(|_| {
                        self.compiler_options.no_implicit_override == Some(true)
                            && !node_in_ambient_context
                    }),
            ) {
                let base_has_abstract = some(
                    Some(base_prop_declarations),
                    Some(|declaration: &Rc<Node>| has_abstract_modifier(declaration)),
                );
                if member_has_override_modifier {
                    return MemberOverrideStatus::Ok;
                }

                if !base_has_abstract {
                    if error_node.is_some() {
                        let diag = if member_is_parameter_property {
                            if is_js {
                                &*Diagnostics::This_parameter_property_must_have_a_JSDoc_comment_with_an_override_tag_because_it_overrides_a_member_in_the_base_class_0
                            } else {
                                &*Diagnostics::This_parameter_property_must_have_an_override_modifier_because_it_overrides_a_member_in_base_class_0
                            }
                        } else {
                            if is_js {
                                &*Diagnostics::This_member_must_have_a_JSDoc_comment_with_an_override_tag_because_it_overrides_a_member_in_the_base_class_0
                            } else {
                                &*Diagnostics::This_member_must_have_an_override_modifier_because_it_overrides_a_member_in_the_base_class_0
                            }
                        };
                        self.error(error_node, diag, Some(vec![base_class_name]));
                    }
                    return MemberOverrideStatus::NeedsOverride;
                } else if member_has_abstract_modifier && base_has_abstract {
                    if error_node.is_some() {
                        self.error(
                            error_node,
                            &Diagnostics::This_member_must_have_an_override_modifier_because_it_overrides_an_abstract_method_that_is_declared_in_the_base_class_0,
                            Some(vec![
                                base_class_name
                            ])
                        );
                    }
                    return MemberOverrideStatus::NeedsOverride;
                }
            }
        } else if member_has_override_modifier {
            if error_node.is_some() {
                let class_name = self.type_to_string_(type_, Option::<&Node>::None, None, None);
                self.error(
                    error_node,
                    if is_js {
                        &*Diagnostics::This_member_cannot_have_a_JSDoc_comment_with_an_override_tag_because_its_containing_class_0_does_not_extend_another_class
                    } else {
                        &*Diagnostics::This_member_cannot_have_an_override_modifier_because_its_containing_class_0_does_not_extend_another_class
                    },
                    Some(vec![
                        class_name
                    ])
                );
            }
            return MemberOverrideStatus::HasInvalidOverride;
        }

        MemberOverrideStatus::Ok
    }

    pub(super) fn issue_member_specific_error(
        &self,
        node: &Node, /*ClassLikeDeclaration*/
        type_with_this: &Type,
        base_with_this: &Type,
        broad_diag: &'static DiagnosticMessage,
    ) {
        let mut issued_member_error = false;
        let node_as_class_like_declaration = node.as_class_like_declaration();
        for member in node_as_class_like_declaration.members() {
            if is_static(member) {
                continue;
            }
            let member_as_named_declaration = member.as_named_declaration();
            let declared_prop = member_as_named_declaration
                .maybe_name()
                .as_ref()
                .and_then(|member_name| self.get_symbol_at_location_(member_name, None))
                .or_else(|| self.get_symbol_at_location_(member, None));
            if let Some(declared_prop) = declared_prop.as_ref() {
                let prop =
                    self.get_property_of_type_(type_with_this, declared_prop.escaped_name(), None);
                let base_prop =
                    self.get_property_of_type_(base_with_this, declared_prop.escaped_name(), None);
                if let (Some(prop), Some(base_prop)) = (prop.as_ref(), base_prop.as_ref()) {
                    if !self.check_type_assignable_to(
                        &self.get_type_of_symbol(prop),
                        &self.get_type_of_symbol(base_prop),
                        Some(
                            member_as_named_declaration
                                .maybe_name()
                                .unwrap_or_else(|| member.clone()),
                        ),
                        None,
                        Some(Rc::new(
                            IssueMemberSpecificErrorContainingMessageChain::new(
                                self.rc_wrapper(),
                                declared_prop.clone(),
                                type_with_this.type_wrapper(),
                                base_with_this.type_wrapper(),
                            ),
                        )),
                        None,
                    ) {
                        issued_member_error = true;
                    }
                }
            }
        }
        if !issued_member_error {
            self.check_type_assignable_to(
                type_with_this,
                base_with_this,
                Some(
                    node_as_class_like_declaration
                        .maybe_name()
                        .unwrap_or_else(|| node.node_wrapper()),
                ),
                Some(broad_diag),
                None,
                None,
            );
        }
    }

    pub(super) fn check_base_type_accessibility(
        &self,
        type_: &Type,
        node: &Node, /*ExpressionWithTypeArguments*/
    ) {
        let signatures = self.get_signatures_of_type(type_, SignatureKind::Construct);
        if !signatures.is_empty() {
            let declaration = signatures[0].declaration.as_ref();
            if matches!(
                declaration,
                Some(declaration) if has_effective_modifier(declaration, ModifierFlags::Private)
            ) {
                let type_class_declaration =
                    get_class_like_declaration_of_symbol(&type_.symbol()).unwrap();
                if !self.is_node_within_class(node, &type_class_declaration) {
                    self.error(
                        Some(node),
                        &Diagnostics::Cannot_extend_a_class_0_Class_constructor_is_marked_as_private,
                        Some(vec![
                            self.get_fully_qualified_name(
                                &type_.symbol(),
                                Option::<&Node>::None
                            )
                        ])
                    );
                }
            }
        }
    }

    pub(super) fn get_member_override_modifier_status(
        &self,
        node: &Node,   /*ClassLikeDeclaration*/
        member: &Node, /*ClassElement*/
    ) -> MemberOverrideStatus {
        let member_name = member.as_named_declaration().maybe_name();
        if member_name.is_none() {
            return MemberOverrideStatus::Ok;
        }
        let member_name = member_name.unwrap();

        let symbol = self.get_symbol_of_node(node).unwrap();
        let type_ = self.get_declared_type_of_symbol(&symbol);
        let type_with_this = self.get_type_with_this_argument(&type_, Option::<&Type>::None, None);
        let static_type = self.get_type_of_symbol(&symbol);

        let base_type_node = get_effective_base_type_node(node);
        let base_types = if base_type_node.is_some() {
            Some(self.get_base_types(&type_))
        } else {
            None
        };
        let base_with_this = base_types
            .as_ref()
            .filter(|base_types| !base_types.is_empty())
            .map(|base_types| {
                self.get_type_with_this_argument(
                    &*first(base_types),
                    type_.as_interface_type().maybe_this_type(),
                    None,
                )
            });
        let base_static_type = self.get_base_constructor_type_of_class(&type_);

        let member_has_override_modifier = if member.maybe_parent().is_some() {
            has_override_modifier(member)
        } else {
            has_syntactic_modifier(member, ModifierFlags::Override)
        };

        let member_name = unescape_leading_underscores(&get_text_of_property_name(&member_name));

        self.check_member_for_override_modifier(
            node,
            &static_type,
            &base_static_type,
            base_with_this,
            &type_,
            &type_with_this,
            member_has_override_modifier,
            has_abstract_modifier(member),
            is_static(member),
            false,
            &member_name,
            Option::<&Node>::None,
        )
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

struct IssueMemberSpecificErrorContainingMessageChain {
    type_checker: Rc<TypeChecker>,
    declared_prop: Rc<Symbol>,
    type_with_this: Rc<Type>,
    base_with_this: Rc<Type>,
}

impl IssueMemberSpecificErrorContainingMessageChain {
    pub fn new(
        type_checker: Rc<TypeChecker>,
        declared_prop: Rc<Symbol>,
        type_with_this: Rc<Type>,
        base_with_this: Rc<Type>,
    ) -> Self {
        Self {
            type_checker,
            declared_prop,
            type_with_this,
            base_with_this,
        }
    }
}

impl CheckTypeContainingMessageChain for IssueMemberSpecificErrorContainingMessageChain {
    fn get(&self) -> Option<Rc<RefCell<DiagnosticMessageChain>>> {
        Some(Rc::new(RefCell::new(
            chain_diagnostic_messages(
                None,
                &Diagnostics::Property_0_in_type_1_is_not_assignable_to_the_same_property_in_base_type_2,
                Some(vec![
                    self.type_checker.symbol_to_string_(
                        &self.declared_prop,
                        Option::<&Node>::None,
                        None, None, None,
                    ),
                    self.type_checker.type_to_string_(
                        &self.type_with_this,
                        Option::<&Node>::None,
                        None, None,
                    ),
                    self.type_checker.type_to_string_(
                        &self.base_with_this,
                        Option::<&Node>::None,
                        None, None,
                    ),
                ])
            )
        )))
    }
}
