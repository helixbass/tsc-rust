#![allow(non_upper_case_globals)]

use gc::{Finalize, Gc, Trace};
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
    InterfaceTypeInterface, MemberOverrideStatus, ModifierFlags, NamedDeclarationInterface, Node,
    NodeFlags, NodeInterface, SignatureDeclarationInterface, SignatureKind, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeInterface,
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
                    |param: &Gc<Node>, _| -> Option<()> {
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
                    Some(|declaration: &Gc<Node>| has_abstract_modifier(declaration)),
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

        let text_of_property_name = get_text_of_property_name(&member_name);
        let member_name = unescape_leading_underscores(&text_of_property_name);

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

    pub(super) fn get_target_symbol(&self, s: &Symbol) -> Gc<Symbol> {
        if get_check_flags(s).intersects(CheckFlags::Instantiated) {
            (*s.as_transient_symbol().symbol_links())
                .borrow()
                .target
                .clone()
                .unwrap()
        } else {
            s.symbol_wrapper()
        }
    }

    pub(super) fn get_class_or_interface_declarations_of_symbol(
        &self,
        symbol: &Symbol,
    ) -> Option<Vec<Gc<Node>>> {
        maybe_filter(symbol.maybe_declarations().as_deref(), |d: &Gc<Node>| {
            matches!(
                d.kind(),
                SyntaxKind::ClassDeclaration | SyntaxKind::InterfaceDeclaration
            )
        })
    }

    pub(super) fn check_kinds_of_property_member_overrides(
        &self,
        type_: &Type,     /*InterfaceType*/
        base_type: &Type, /*BaseType*/
    ) {
        let base_properties = self.get_properties_of_type(base_type);
        'base_property_check: for base_property in &base_properties {
            let base = self.get_target_symbol(base_property);

            if base.flags().intersects(SymbolFlags::Prototype) {
                continue;
            }
            let base_symbol = self.get_property_of_object_type(type_, base.escaped_name());
            if base_symbol.is_none() {
                continue;
            }
            let base_symbol = base_symbol.unwrap();
            let derived = self.get_target_symbol(&base_symbol);
            let base_declaration_flags = get_declaration_modifier_flags_from_symbol(&base, None);

            // Debug.assert(!!derived, "derived should point at something, even if it is the base class' declaration.");

            if Rc::ptr_eq(&derived, &base) {
                let derived_class_decl =
                    get_class_like_declaration_of_symbol(&type_.symbol()).unwrap();

                if base_declaration_flags.intersects(ModifierFlags::Abstract) && /* !derivedClassDecl ||*/ !has_syntactic_modifier(&derived_class_decl, ModifierFlags::Abstract)
                {
                    for other_base_type in &self.get_base_types(type_) {
                        if ptr::eq(&**other_base_type, base_type) {
                            continue;
                        }
                        let base_symbol =
                            self.get_property_of_object_type(other_base_type, base.escaped_name());
                        let derived_elsewhere = base_symbol
                            .as_ref()
                            .map(|base_symbol| self.get_target_symbol(base_symbol));
                        if matches!(
                            derived_elsewhere.as_ref(),
                            Some(derived_elsewhere) if !Rc::ptr_eq(derived_elsewhere, &base)
                        ) {
                            continue 'base_property_check;
                        }
                    }

                    if derived_class_decl.kind() == SyntaxKind::ClassExpression {
                        self.error(
                            Some(&*derived_class_decl),
                            &Diagnostics::Non_abstract_class_expression_does_not_implement_inherited_abstract_member_0_from_class_1,
                            Some(vec![
                                self.symbol_to_string_(
                                    base_property,
                                    Option::<&Node>::None,
                                    None, None, None
                                ),
                                self.type_to_string_(
                                    base_type,
                                    Option::<&Node>::None,
                                    None, None,
                                ),
                            ])
                        );
                    } else {
                        self.error(
                            Some(&*derived_class_decl),
                            &Diagnostics::Non_abstract_class_0_does_not_implement_inherited_abstract_member_1_from_class_2,
                            Some(vec![
                                self.type_to_string_(
                                    type_,
                                    Option::<&Node>::None,
                                    None, None,
                                ),
                                self.symbol_to_string_(
                                    base_property,
                                    Option::<&Node>::None,
                                    None, None, None
                                ),
                                self.type_to_string_(
                                    base_type,
                                    Option::<&Node>::None,
                                    None, None,
                                ),
                            ])
                        );
                    }
                }
            } else {
                let derived_declaration_flags =
                    get_declaration_modifier_flags_from_symbol(&derived, None);
                if base_declaration_flags.intersects(ModifierFlags::Private)
                    || derived_declaration_flags.intersects(ModifierFlags::Private)
                {
                    continue;
                }

                let error_message: &DiagnosticMessage;
                let base_property_flags = base.flags() & SymbolFlags::PropertyOrAccessor;
                let derived_property_flags = derived.flags() & SymbolFlags::PropertyOrAccessor;
                if base_property_flags != SymbolFlags::None
                    && derived_property_flags != SymbolFlags::None
                {
                    if base_declaration_flags.intersects(ModifierFlags::Abstract)
                        && !matches!(
                            base.maybe_value_declaration().as_ref(),
                            Some(base_value_declaration) if is_property_declaration(base_value_declaration) &&
                                base_value_declaration.as_property_declaration().maybe_initializer().is_some()
                        )
                        || matches!(
                            base.maybe_value_declaration().as_ref(),
                            Some(base_value_declaration) if base_value_declaration.parent().kind() == SyntaxKind::InterfaceDeclaration
                        )
                        || matches!(
                            derived.maybe_value_declaration().as_ref(),
                            Some(derived_value_declaration) if is_binary_expression(derived_value_declaration)
                        )
                    {
                        continue;
                    }

                    let overridden_instance_property = base_property_flags != SymbolFlags::Property
                        && derived_property_flags == SymbolFlags::Property;
                    let overridden_instance_accessor = base_property_flags == SymbolFlags::Property
                        && derived_property_flags != SymbolFlags::Property;
                    if overridden_instance_property || overridden_instance_accessor {
                        let error_message = if overridden_instance_property {
                            &*Diagnostics::_0_is_defined_as_an_accessor_in_class_1_but_is_overridden_here_in_2_as_an_instance_property
                        } else {
                            &*Diagnostics::_0_is_defined_as_a_property_in_class_1_but_is_overridden_here_in_2_as_an_accessor
                        };
                        self.error(
                            get_name_of_declaration(derived.maybe_value_declaration())
                                .or_else(|| derived.maybe_value_declaration()),
                            error_message,
                            Some(vec![
                                self.symbol_to_string_(
                                    &base,
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                    None,
                                ),
                                self.type_to_string_(base_type, Option::<&Node>::None, None, None),
                                self.type_to_string_(type_, Option::<&Node>::None, None, None),
                            ]),
                        );
                    } else if self.use_define_for_class_fields {
                        let uninitialized = derived.maybe_declarations().as_ref().and_then(
                            |derived_declarations| {
                                derived_declarations
                                    .into_iter()
                                    .find(|d| {
                                        d.kind() == SyntaxKind::PropertyDeclaration
                                            && d.as_property_declaration()
                                                .maybe_initializer()
                                                .is_none()
                                    })
                                    .cloned()
                            },
                        );
                        if let Some(uninitialized) = uninitialized.as_ref() {
                            if !derived.flags().intersects(SymbolFlags::Transient)
                                && !base_declaration_flags.intersects(ModifierFlags::Abstract)
                                && !derived_declaration_flags.intersects(ModifierFlags::Abstract)
                                && !matches!(
                                    derived.maybe_declarations().as_ref(),
                                    Some(derived_declarations) if derived_declarations.into_iter().any(|d| d.flags().intersects(NodeFlags::Ambient))
                                )
                            {
                                let constructor = self.find_constructor_declaration(
                                    &get_class_like_declaration_of_symbol(&type_.symbol()).unwrap(),
                                );
                                let uninitialized_as_property_declaration =
                                    uninitialized.as_property_declaration();
                                let prop_name = uninitialized_as_property_declaration.name();
                                if uninitialized_as_property_declaration
                                    .exclamation_token
                                    .is_some()
                                    || constructor.is_none()
                                    || !is_identifier(&prop_name)
                                    || !self.strict_null_checks
                                    || !self.is_property_initialized_in_constructor(
                                        &prop_name,
                                        type_,
                                        constructor.as_ref().unwrap(),
                                    )
                                {
                                    let error_message = &Diagnostics::Property_0_will_overwrite_the_base_property_in_1_If_this_is_intentional_add_an_initializer_Otherwise_add_a_declare_modifier_or_remove_the_redundant_declaration;
                                    self.error(
                                        get_name_of_declaration(derived.maybe_value_declaration())
                                            .or_else(|| derived.maybe_value_declaration()),
                                        error_message,
                                        Some(vec![
                                            self.symbol_to_string_(
                                                &base,
                                                Option::<&Node>::None,
                                                None,
                                                None,
                                                None,
                                            ),
                                            self.type_to_string_(
                                                &base_type,
                                                Option::<&Node>::None,
                                                None,
                                                None,
                                            ),
                                        ]),
                                    );
                                }
                            }
                        }
                    }

                    continue;
                } else if self.is_prototype_property(&base) {
                    if self.is_prototype_property(&derived)
                        || derived.flags().intersects(SymbolFlags::Property)
                    {
                        continue;
                    } else {
                        Debug_.assert(derived.flags().intersects(SymbolFlags::Accessor), None);
                        error_message = &Diagnostics::Class_0_defines_instance_member_function_1_but_extended_class_2_defines_it_as_instance_member_accessor;
                    }
                } else if base.flags().intersects(SymbolFlags::Accessor) {
                    error_message = &Diagnostics::Class_0_defines_instance_member_accessor_1_but_extended_class_2_defines_it_as_instance_member_function;
                } else {
                    error_message = &Diagnostics::Class_0_defines_instance_member_property_1_but_extended_class_2_defines_it_as_instance_member_function;
                }

                self.error(
                    get_name_of_declaration(derived.maybe_value_declaration())
                        .or_else(|| derived.maybe_value_declaration()),
                    error_message,
                    Some(vec![
                        self.type_to_string_(base_type, Option::<&Node>::None, None, None),
                        self.symbol_to_string_(&base, Option::<&Node>::None, None, None, None),
                        self.type_to_string_(type_, Option::<&Node>::None, None, None),
                    ]),
                );
            }
        }
    }

    pub(super) fn get_non_interhited_properties(
        &self,
        type_: &Type, /*InterfaceType*/
        base_types: &[Gc<Type /*BaseType*/>],
        properties: &[Gc<Symbol>],
    ) -> Vec<Gc<Symbol>> {
        if length(Some(base_types)) == 0 {
            return properties.to_owned();
        }
        let mut seen: HashMap<__String, Gc<Symbol>> = HashMap::new();
        for_each(properties, |p: &Gc<Symbol>, _| -> Option<()> {
            seen.insert(p.escaped_name().to_owned(), p.clone());
            None
        });

        let type_as_interface_type = type_.as_interface_type();
        for base in base_types {
            let properties = self.get_properties_of_type(&self.get_type_with_this_argument(
                base,
                type_as_interface_type.maybe_this_type(),
                None,
            ));
            for prop in &properties {
                let existing = seen.get(prop.escaped_name());
                if matches!(
                    existing,
                    Some(existing) if !self.is_property_identical_to(
                        existing,
                        prop
                    )
                ) {
                    seen.remove(prop.escaped_name());
                }
            }
        }

        seen.into_values().collect()
    }

    pub(super) fn check_inherited_properties_are_identical(
        &self,
        type_: &Type, /*InterfaceType*/
        type_node: &Node,
    ) -> bool {
        let base_types = self.get_base_types(type_);
        if base_types.len() < 2 {
            return true;
        }

        let mut seen: HashMap<__String, InheritanceInfoMap> = HashMap::new();
        maybe_for_each(
            self.resolve_declared_members(type_)
                .as_interface_type_with_declared_members()
                .maybe_declared_properties()
                .as_ref(),
            |p: &Gc<Symbol>, _| -> Option<()> {
                seen.insert(
                    p.escaped_name().to_owned(),
                    InheritanceInfoMap {
                        prop: p.clone(),
                        containing_type: type_.type_wrapper(),
                    },
                );
                None
            },
        );
        let mut ok = true;

        let type_as_interface_type = type_.as_interface_type();
        for base in &base_types {
            let properties = self.get_properties_of_type(&self.get_type_with_this_argument(
                base,
                type_as_interface_type.maybe_this_type(),
                None,
            ));
            for prop in &properties {
                let existing = seen.get(prop.escaped_name());
                match existing {
                    None => {
                        seen.insert(
                            prop.escaped_name().to_owned(),
                            InheritanceInfoMap {
                                prop: prop.clone(),
                                containing_type: base.clone(),
                            },
                        );
                    }
                    Some(existing) => {
                        let is_inherited_property = !ptr::eq(&*existing.containing_type, type_);
                        if is_inherited_property
                            && !self.is_property_identical_to(&existing.prop, prop)
                        {
                            ok = false;

                            let type_name1 = self.type_to_string_(
                                &existing.containing_type,
                                Option::<&Node>::None,
                                None,
                                None,
                            );
                            let type_name2 =
                                self.type_to_string_(base, Option::<&Node>::None, None, None);

                            let mut error_info = chain_diagnostic_messages(
                                None,
                                &Diagnostics::Named_property_0_of_types_1_and_2_are_not_identical,
                                Some(vec![
                                    self.symbol_to_string_(
                                        prop,
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                        None,
                                    ),
                                    type_name1.clone(),
                                    type_name2.clone(),
                                ]),
                            );
                            error_info = chain_diagnostic_messages(
                                Some(error_info),
                                &Diagnostics::Interface_0_cannot_simultaneously_extend_types_1_and_2,
                                Some(vec![
                                    self.type_to_string_(
                                        type_,
                                        Option::<&Node>::None,
                                        None, None,
                                    ),
                                    type_name1,
                                    type_name2,
                                ])
                            );
                            self.diagnostics().add(Rc::new(
                                create_diagnostic_for_node_from_message_chain(
                                    type_node, error_info, None,
                                )
                                .into(),
                            ));
                        }
                    }
                }
            }
        }

        ok
    }
}

#[derive(Trace, Finalize)]
struct IssueMemberSpecificErrorContainingMessageChain {
    type_checker: Gc<TypeChecker>,
    declared_prop: Gc<Symbol>,
    type_with_this: Gc<Type>,
    base_with_this: Gc<Type>,
}

impl IssueMemberSpecificErrorContainingMessageChain {
    pub fn new(
        type_checker: Gc<TypeChecker>,
        declared_prop: Gc<Symbol>,
        type_with_this: Gc<Type>,
        base_with_this: Gc<Type>,
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

struct InheritanceInfoMap {
    pub prop: Gc<Symbol>,
    pub containing_type: Gc<Type>,
}
