use std::{cell::RefCell, collections::HashMap, io, rc::Rc};

use id_arena::Id;

use super::CheckTypeContainingMessageChain;
use crate::{
    __String, chain_diagnostic_messages, continue_if_none,
    create_diagnostic_for_node_from_message_chain, escape_leading_underscores, first, for_each,
    get_check_flags, get_class_like_declaration_of_symbol,
    get_declaration_modifier_flags_from_symbol, get_effective_base_type_node,
    get_name_of_declaration, get_text_of_property_name, has_abstract_modifier,
    has_ambient_modifier, has_effective_modifier, has_override_modifier, has_syntactic_modifier,
    impl_has_arena, is_binary_expression, is_constructor_declaration, is_identifier, is_in_js_file,
    is_parameter_property_declaration, is_property_declaration, is_static, length, maybe_filter,
    maybe_for_each, released, some, symbol_name, try_for_each, unescape_leading_underscores,
    AllArenas, CheckFlags, Debug_, DiagnosticMessage, DiagnosticMessageChain, Diagnostics,
    HasArena, HasInitializerInterface, InArena, InterfaceTypeInterface, Matches,
    MemberOverrideStatus, ModifierFlags, NamedDeclarationInterface, Node, NodeFlags, NodeInterface,
    OptionTry, SignatureDeclarationInterface, SignatureKind, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_members_for_override_modifier(
        &self,
        node: Id<Node>,  /*ClassLikeDeclaration*/
        type_: Id<Type>, /*InterfaceType*/
        type_with_this: Id<Type>,
        static_type: Id<Type>, /*ObjectType*/
    ) -> io::Result<()> {
        let base_type_node = get_effective_base_type_node(node, self);
        let base_types = if base_type_node.is_some() {
            Some(self.get_base_types(type_)?)
        } else {
            None
        };
        let base_with_this = base_types
            .as_ref()
            .filter(|base_types| !base_types.is_empty())
            .try_map(|base_types| {
                self.get_type_with_this_argument(
                    *first(base_types),
                    type_.ref_(self).as_interface_type().maybe_this_type(),
                    None,
                )
            })?;
        let base_static_type = self.get_base_constructor_type_of_class(type_)?;

        for &member in &*released!(node
            .ref_(self)
            .as_class_like_declaration()
            .members()
            .ref_(self)
            .clone())
        {
            if has_ambient_modifier(member, self) {
                continue;
            }

            if is_constructor_declaration(&member.ref_(self)) {
                try_for_each(
                    &*released!(member.ref_(self).as_constructor_declaration().parameters())
                        .ref_(self),
                    |&param: &Id<Node>, _| -> io::Result<Option<()>> {
                        if is_parameter_property_declaration(param, member, self) {
                            self.check_existing_member_for_override_modifier(
                                node,
                                static_type,
                                base_static_type,
                                base_with_this,
                                type_,
                                type_with_this,
                                param,
                                true,
                                None,
                            )?;
                        }
                        Ok(None)
                    },
                )?;
            }
            self.check_existing_member_for_override_modifier(
                node,
                static_type,
                base_static_type,
                base_with_this,
                type_,
                type_with_this,
                member,
                false,
                None,
            )?;
        }

        Ok(())
    }

    pub(super) fn check_existing_member_for_override_modifier(
        &self,
        node: Id<Node>,        /*ClassLikeDeclaration*/
        static_type: Id<Type>, /*ObjectType*/
        base_static_type: Id<Type>,
        base_with_this: Option<Id<Type>>,
        type_: Id<Type>, /*InterfaceType*/
        type_with_this: Id<Type>,
        member: Id<Node>, /*ClassElement | ParameterPropertyDeclaration*/
        member_is_parameter_property: bool,
        report_errors: Option<bool>,
    ) -> io::Result<MemberOverrideStatus> {
        let report_errors = report_errors.unwrap_or(true);
        let Some(declared_prop) =
            (if let Some(member_name) = member.ref_(self).as_named_declaration().maybe_name() {
                self.get_symbol_at_location_(member_name, None)?
            } else {
                self.get_symbol_at_location_(member, None)?
            })
        else {
            return Ok(MemberOverrideStatus::Ok);
        };

        self.check_member_for_override_modifier(
            node,
            static_type,
            base_static_type,
            base_with_this,
            type_,
            type_with_this,
            has_override_modifier(member, self),
            has_abstract_modifier(member, self),
            is_static(member, self),
            member_is_parameter_property,
            &symbol_name(declared_prop, self),
            if report_errors { Some(member) } else { None },
        )
    }

    pub(super) fn check_member_for_override_modifier(
        &self,
        node: Id<Node>,        /*ClassLikeDeclaration*/
        static_type: Id<Type>, /*ObjectType*/
        base_static_type: Id<Type>,
        base_with_this: Option<Id<Type>>,
        type_: Id<Type>, /*InterfaceType*/
        type_with_this: Id<Type>,
        member_has_override_modifier: bool,
        member_has_abstract_modifier: bool,
        member_is_static: bool,
        member_is_parameter_property: bool,
        member_name: &str,
        error_node: Option<Id<Node>>,
    ) -> io::Result<MemberOverrideStatus> {
        let is_js = is_in_js_file(Some(&node.ref_(self)));
        let node_in_ambient_context = node.ref_(self).flags().intersects(NodeFlags::Ambient);
        if let Some(base_with_this) = base_with_this.filter(|_| {
            member_has_override_modifier
                || self.compiler_options.ref_(self).no_implicit_override == Some(true)
        }) {
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
            let prop = self.get_property_of_type_(this_type, &member_escaped_name, None)?;
            let base_prop = self.get_property_of_type_(base_type, &member_escaped_name, None)?;

            let base_class_name =
                self.type_to_string_(base_with_this, Option::<Id<Node>>::None, None, None)?;
            if prop
                .as_ref()
                .matches(|_| base_prop.is_none() && member_has_override_modifier)
            {
                unimplemented!()
            } else if let (Some(_prop), Some(base_prop_declarations)) = (
                prop.as_ref(),
                base_prop
                    .and_then(|base_prop| base_prop.ref_(self).maybe_declarations().clone())
                    .as_ref()
                    .filter(|_| {
                        self.compiler_options.ref_(self).no_implicit_override == Some(true)
                            && !node_in_ambient_context
                    }),
            ) {
                let base_has_abstract = some(
                    Some(base_prop_declarations),
                    Some(|&declaration: &Id<Node>| has_abstract_modifier(declaration, self)),
                );
                if member_has_override_modifier {
                    return Ok(MemberOverrideStatus::Ok);
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
                    return Ok(MemberOverrideStatus::NeedsOverride);
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
                    return Ok(MemberOverrideStatus::NeedsOverride);
                }
            }
        } else if member_has_override_modifier {
            if error_node.is_some() {
                let class_name =
                    self.type_to_string_(type_, Option::<Id<Node>>::None, None, None)?;
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
            return Ok(MemberOverrideStatus::HasInvalidOverride);
        }

        Ok(MemberOverrideStatus::Ok)
    }

    pub(super) fn issue_member_specific_error(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration*/
        type_with_this: Id<Type>,
        base_with_this: Id<Type>,
        broad_diag: &'static DiagnosticMessage,
    ) -> io::Result<()> {
        let mut issued_member_error = false;
        for &member in &*released!(node
            .ref_(self)
            .as_class_like_declaration()
            .members()
            .ref_(self)
            .clone())
        {
            if is_static(member, self) {
                continue;
            }
            let declared_prop = member
                .ref_(self)
                .as_named_declaration()
                .maybe_name()
                .try_and_then(|member_name| self.get_symbol_at_location_(member_name, None))?
                .try_or_else(|| self.get_symbol_at_location_(member, None))?;
            if let Some(declared_prop) = declared_prop {
                let prop = self.get_property_of_type_(
                    type_with_this,
                    declared_prop.ref_(self).escaped_name(),
                    None,
                )?;
                let base_prop = self.get_property_of_type_(
                    base_with_this,
                    declared_prop.ref_(self).escaped_name(),
                    None,
                )?;
                if let (Some(prop), Some(base_prop)) = (prop, base_prop) {
                    if !self.check_type_assignable_to(
                        self.get_type_of_symbol(prop)?,
                        self.get_type_of_symbol(base_prop)?,
                        Some(released!(member
                            .ref_(self)
                            .as_named_declaration()
                            .maybe_name()
                            .unwrap_or_else(|| member.clone()))),
                        None,
                        Some(self.alloc_check_type_containing_message_chain(Box::new(
                            IssueMemberSpecificErrorContainingMessageChain::new(
                                self.arena_id(),
                                declared_prop.clone(),
                                type_with_this,
                                base_with_this,
                                self,
                            ),
                        ))),
                        None,
                    )? {
                        issued_member_error = true;
                    }
                }
            }
        }
        if !issued_member_error {
            self.check_type_assignable_to(
                type_with_this,
                base_with_this,
                Some(released!(node
                    .ref_(self)
                    .as_class_like_declaration()
                    .maybe_name()
                    .unwrap_or(node))),
                Some(broad_diag),
                None,
                None,
            )?;
        }

        Ok(())
    }

    pub(super) fn check_base_type_accessibility(
        &self,
        type_: Id<Type>,
        node: Id<Node>, /*ExpressionWithTypeArguments*/
    ) -> io::Result<()> {
        let signatures = self.get_signatures_of_type(type_, SignatureKind::Construct)?;
        if !signatures.is_empty() {
            let declaration = signatures[0].ref_(self).declaration;
            if matches!(
                declaration,
                Some(declaration) if has_effective_modifier(declaration, ModifierFlags::Private, self)
            ) {
                let type_class_declaration =
                    get_class_like_declaration_of_symbol(type_.ref_(self).symbol(), self).unwrap();
                if !self.is_node_within_class(node, type_class_declaration) {
                    self.error(
                        Some(node),
                        &Diagnostics::Cannot_extend_a_class_0_Class_constructor_is_marked_as_private,
                        Some(vec![
                            self.get_fully_qualified_name(
                                type_.ref_(self).symbol(),
                                Option::<Id<Node>>::None
                            )?
                        ])
                    );
                }
            }
        }

        Ok(())
    }

    pub fn get_member_override_modifier_status(
        &self,
        node: Id<Node>,   /*ClassLikeDeclaration*/
        member: Id<Node>, /*ClassElement*/
    ) -> io::Result<MemberOverrideStatus> {
        let Some(member_name) = member.ref_(self).as_named_declaration().maybe_name() else {
            return Ok(MemberOverrideStatus::Ok);
        };

        let symbol = self.get_symbol_of_node(node)?.unwrap();
        let type_ = self.get_declared_type_of_symbol(symbol)?;
        let type_with_this = self.get_type_with_this_argument(type_, None, None)?;
        let static_type = self.get_type_of_symbol(symbol)?;

        let base_type_node = get_effective_base_type_node(node, self);
        let base_types = if base_type_node.is_some() {
            Some(self.get_base_types(type_)?)
        } else {
            None
        };
        let base_with_this = base_types
            .as_ref()
            .filter(|base_types| !base_types.is_empty())
            .try_map(|base_types| {
                self.get_type_with_this_argument(
                    *first(base_types),
                    type_.ref_(self).as_interface_type().maybe_this_type(),
                    None,
                )
            })?;
        let base_static_type = self.get_base_constructor_type_of_class(type_)?;

        let member_has_override_modifier = if member.ref_(self).maybe_parent().is_some() {
            has_override_modifier(member, self)
        } else {
            has_syntactic_modifier(member, ModifierFlags::Override, self)
        };

        let text_of_property_name = get_text_of_property_name(member_name, self);
        let member_name = unescape_leading_underscores(&text_of_property_name);

        self.check_member_for_override_modifier(
            node,
            static_type,
            base_static_type,
            base_with_this,
            type_,
            type_with_this,
            member_has_override_modifier,
            has_abstract_modifier(member, self),
            is_static(member, self),
            false,
            &member_name,
            Option::<Id<Node>>::None,
        )
    }

    pub(super) fn get_target_symbol(&self, s: Id<Symbol>) -> Id<Symbol> {
        if get_check_flags(&s.ref_(self)).intersects(CheckFlags::Instantiated) {
            s.ref_(self)
                .as_transient_symbol()
                .symbol_links()
                .ref_(self)
                .target
                .unwrap()
        } else {
            s
        }
    }

    pub(super) fn get_class_or_interface_declarations_of_symbol(
        &self,
        symbol: Id<Symbol>,
    ) -> Option<Vec<Id<Node>>> {
        maybe_filter(
            symbol.ref_(self).maybe_declarations().as_deref(),
            |d: &Id<Node>| {
                matches!(
                    d.ref_(self).kind(),
                    SyntaxKind::ClassDeclaration | SyntaxKind::InterfaceDeclaration
                )
            },
        )
    }

    pub(super) fn check_kinds_of_property_member_overrides(
        &self,
        type_: Id<Type>,     /*InterfaceType*/
        base_type: Id<Type>, /*BaseType*/
    ) -> io::Result<()> {
        let base_properties = self.get_properties_of_type(base_type)?;
        'base_property_check: for base_property in base_properties {
            let base = self.get_target_symbol(base_property);

            if base.ref_(self).flags().intersects(SymbolFlags::Prototype) {
                continue;
            }
            let base_symbol = continue_if_none!(self.get_property_of_object_type(
                type_,
                &released!(base.ref_(self).escaped_name().to_owned())
            )?);
            let derived = self.get_target_symbol(base_symbol);
            let base_declaration_flags =
                get_declaration_modifier_flags_from_symbol(base, None, self);

            // Debug.assert(!!derived, "derived should point at something, even if it is the base class' declaration.");

            if derived == base {
                let derived_class_decl =
                    get_class_like_declaration_of_symbol(type_.ref_(self).symbol(), self).unwrap();

                if base_declaration_flags.intersects(ModifierFlags::Abstract) &&
                    /* !derivedClassDecl ||*/ !has_syntactic_modifier(derived_class_decl, ModifierFlags::Abstract, self)
                {
                    for &other_base_type in &self.get_base_types(type_)? {
                        if other_base_type == base_type {
                            continue;
                        }
                        let base_symbol = self.get_property_of_object_type(
                            other_base_type,
                            base.ref_(self).escaped_name(),
                        )?;
                        let derived_elsewhere =
                            base_symbol.map(|base_symbol| self.get_target_symbol(base_symbol));
                        if matches!(
                            derived_elsewhere,
                            Some(derived_elsewhere) if derived_elsewhere != base
                        ) {
                            continue 'base_property_check;
                        }
                    }

                    if derived_class_decl.ref_(self).kind() == SyntaxKind::ClassExpression {
                        self.error(
                            Some(derived_class_decl),
                            &Diagnostics::Non_abstract_class_expression_does_not_implement_inherited_abstract_member_0_from_class_1,
                            Some(vec![
                                self.symbol_to_string_(
                                    base_property,
                                    Option::<Id<Node>>::None,
                                    None, None, None
                                )?,
                                self.type_to_string_(
                                    base_type,
                                    Option::<Id<Node>>::None,
                                    None, None,
                                )?,
                            ])
                        );
                    } else {
                        self.error(
                            Some(derived_class_decl),
                            &Diagnostics::Non_abstract_class_0_does_not_implement_inherited_abstract_member_1_from_class_2,
                            Some(vec![
                                self.type_to_string_(
                                    type_,
                                    Option::<Id<Node>>::None,
                                    None, None,
                                )?,
                                self.symbol_to_string_(
                                    base_property,
                                    Option::<Id<Node>>::None,
                                    None, None, None
                                )?,
                                self.type_to_string_(
                                    base_type,
                                    Option::<Id<Node>>::None,
                                    None, None,
                                )?,
                            ])
                        );
                    }
                }
            } else {
                let derived_declaration_flags =
                    get_declaration_modifier_flags_from_symbol(derived, None, self);
                if base_declaration_flags.intersects(ModifierFlags::Private)
                    || derived_declaration_flags.intersects(ModifierFlags::Private)
                {
                    continue;
                }

                let error_message: &DiagnosticMessage;
                let base_property_flags = base.ref_(self).flags() & SymbolFlags::PropertyOrAccessor;
                let derived_property_flags =
                    derived.ref_(self).flags() & SymbolFlags::PropertyOrAccessor;
                if base_property_flags != SymbolFlags::None
                    && derived_property_flags != SymbolFlags::None
                {
                    if base_declaration_flags.intersects(ModifierFlags::Abstract)
                        && !matches!(
                            base.ref_(self).maybe_value_declaration(),
                            Some(base_value_declaration) if is_property_declaration(&base_value_declaration.ref_(self)) &&
                                base_value_declaration.ref_(self).as_property_declaration().maybe_initializer().is_some()
                        )
                        || matches!(
                            base.ref_(self).maybe_value_declaration(),
                            Some(base_value_declaration) if base_value_declaration.ref_(self).parent().ref_(self).kind() == SyntaxKind::InterfaceDeclaration
                        )
                        || matches!(
                            derived.ref_(self).maybe_value_declaration(),
                            Some(derived_value_declaration) if is_binary_expression(&derived_value_declaration.ref_(self))
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
                            get_name_of_declaration(
                                derived.ref_(self).maybe_value_declaration(),
                                self,
                            )
                            .or_else(|| derived.ref_(self).maybe_value_declaration()),
                            error_message,
                            Some(vec![
                                self.symbol_to_string_(
                                    base,
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                    None,
                                )?,
                                self.type_to_string_(
                                    base_type,
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                )?,
                                self.type_to_string_(type_, Option::<Id<Node>>::None, None, None)?,
                            ]),
                        );
                    } else if self.use_define_for_class_fields {
                        let uninitialized =
                            derived.ref_(self).maybe_declarations().as_ref().and_then(
                                |derived_declarations| {
                                    derived_declarations
                                        .into_iter()
                                        .find(|d| {
                                            d.ref_(self).kind() == SyntaxKind::PropertyDeclaration
                                                && d.ref_(self)
                                                    .as_property_declaration()
                                                    .maybe_initializer()
                                                    .is_none()
                                        })
                                        .copied()
                                },
                            );
                        if let Some(uninitialized) = uninitialized {
                            if !derived
                                .ref_(self)
                                .flags()
                                .intersects(SymbolFlags::Transient)
                                && !base_declaration_flags.intersects(ModifierFlags::Abstract)
                                && !derived_declaration_flags.intersects(ModifierFlags::Abstract)
                                && !matches!(
                                    derived.ref_(self).maybe_declarations().as_ref(),
                                    Some(derived_declarations) if derived_declarations.into_iter().any(|d| d.ref_(self).flags().intersects(NodeFlags::Ambient))
                                )
                            {
                                let constructor = self.find_constructor_declaration(
                                    get_class_like_declaration_of_symbol(
                                        type_.ref_(self).symbol(),
                                        self,
                                    )
                                    .unwrap(),
                                );
                                let uninitialized_ref = uninitialized.ref_(self);
                                let uninitialized_as_property_declaration =
                                    uninitialized_ref.as_property_declaration();
                                let prop_name = uninitialized_as_property_declaration.name();
                                if uninitialized_as_property_declaration
                                    .exclamation_token
                                    .is_some()
                                    || constructor.is_none()
                                    || !is_identifier(&prop_name.ref_(self))
                                    || !self.strict_null_checks
                                    || !self.is_property_initialized_in_constructor(
                                        prop_name,
                                        type_,
                                        constructor.unwrap(),
                                    )?
                                {
                                    let error_message = &Diagnostics::Property_0_will_overwrite_the_base_property_in_1_If_this_is_intentional_add_an_initializer_Otherwise_add_a_declare_modifier_or_remove_the_redundant_declaration;
                                    self.error(
                                        get_name_of_declaration(
                                            derived.ref_(self).maybe_value_declaration(),
                                            self,
                                        )
                                        .or_else(|| derived.ref_(self).maybe_value_declaration()),
                                        error_message,
                                        Some(vec![
                                            self.symbol_to_string_(
                                                base,
                                                Option::<Id<Node>>::None,
                                                None,
                                                None,
                                                None,
                                            )?,
                                            self.type_to_string_(
                                                base_type,
                                                Option::<Id<Node>>::None,
                                                None,
                                                None,
                                            )?,
                                        ]),
                                    );
                                }
                            }
                        }
                    }

                    continue;
                } else if self.is_prototype_property(base) {
                    if self.is_prototype_property(derived)
                        || derived.ref_(self).flags().intersects(SymbolFlags::Property)
                    {
                        continue;
                    } else {
                        Debug_.assert(
                            derived.ref_(self).flags().intersects(SymbolFlags::Accessor),
                            None,
                        );
                        error_message = &Diagnostics::Class_0_defines_instance_member_function_1_but_extended_class_2_defines_it_as_instance_member_accessor;
                    }
                } else if base.ref_(self).flags().intersects(SymbolFlags::Accessor) {
                    error_message = &Diagnostics::Class_0_defines_instance_member_accessor_1_but_extended_class_2_defines_it_as_instance_member_function;
                } else {
                    error_message = &Diagnostics::Class_0_defines_instance_member_property_1_but_extended_class_2_defines_it_as_instance_member_function;
                }

                self.error(
                    get_name_of_declaration(derived.ref_(self).maybe_value_declaration(), self)
                        .or_else(|| derived.ref_(self).maybe_value_declaration()),
                    error_message,
                    Some(vec![
                        self.type_to_string_(base_type, Option::<Id<Node>>::None, None, None)?,
                        self.symbol_to_string_(base, Option::<Id<Node>>::None, None, None, None)?,
                        self.type_to_string_(type_, Option::<Id<Node>>::None, None, None)?,
                    ]),
                );
            }
        }

        Ok(())
    }

    pub(super) fn get_non_interhited_properties(
        &self,
        type_: Id<Type>, /*InterfaceType*/
        base_types: &[Id<Type /*BaseType*/>],
        properties: &[Id<Symbol>],
    ) -> io::Result<Vec<Id<Symbol>>> {
        if length(Some(base_types)) == 0 {
            return Ok(properties.to_owned());
        }
        let mut seen: HashMap<__String, Id<Symbol>> = Default::default();
        for_each(properties, |&p, _| -> Option<()> {
            seen.insert(p.ref_(self).escaped_name().to_owned(), p);
            None
        });

        for &base in base_types {
            let properties = self.get_properties_of_type(self.get_type_with_this_argument(
                base,
                released!(type_.ref_(self).as_interface_type().maybe_this_type()),
                None,
            )?)?;
            for prop in properties {
                let existing = seen.get(prop.ref_(self).escaped_name());
                if matches!(
                    existing,
                    Some(&existing) if !self.is_property_identical_to(
                        existing,
                        prop
                    )?
                ) {
                    seen.remove(prop.ref_(self).escaped_name());
                }
            }
        }

        Ok(seen.into_values().collect())
    }

    pub(super) fn check_inherited_properties_are_identical(
        &self,
        type_: Id<Type>, /*InterfaceType*/
        type_node: Id<Node>,
    ) -> io::Result<bool> {
        let base_types = self.get_base_types(type_)?;
        if base_types.len() < 2 {
            return Ok(true);
        }

        let mut seen: HashMap<__String, InheritanceInfoMap> = HashMap::new();
        maybe_for_each(
            self.resolve_declared_members(type_)?
                .ref_(self)
                .as_interface_type_with_declared_members()
                .maybe_declared_properties()
                .as_ref(),
            |&p: &Id<Symbol>, _| -> Option<()> {
                seen.insert(
                    p.ref_(self).escaped_name().to_owned(),
                    InheritanceInfoMap {
                        prop: p.clone(),
                        containing_type: type_,
                    },
                );
                None
            },
        );
        let mut ok = true;

        for &base in &base_types {
            let properties = self.get_properties_of_type(self.get_type_with_this_argument(
                base,
                {
                    let this_type = type_.ref_(self).as_interface_type().maybe_this_type();
                    this_type
                },
                None,
            )?)?;
            for prop in properties {
                let existing = seen.get(prop.ref_(self).escaped_name());
                match existing {
                    None => {
                        seen.insert(
                            prop.ref_(self).escaped_name().to_owned(),
                            InheritanceInfoMap {
                                prop: prop.clone(),
                                containing_type: base.clone(),
                            },
                        );
                    }
                    Some(existing) => {
                        let is_inherited_property = existing.containing_type != type_;
                        if is_inherited_property
                            && !self.is_property_identical_to(existing.prop, prop)?
                        {
                            ok = false;

                            let type_name1 = self.type_to_string_(
                                existing.containing_type,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                            )?;
                            let type_name2 =
                                self.type_to_string_(base, Option::<Id<Node>>::None, None, None)?;

                            let mut error_info = chain_diagnostic_messages(
                                None,
                                &Diagnostics::Named_property_0_of_types_1_and_2_are_not_identical,
                                Some(vec![
                                    self.symbol_to_string_(
                                        prop,
                                        Option::<Id<Node>>::None,
                                        None,
                                        None,
                                        None,
                                    )?,
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
                                        Option::<Id<Node>>::None,
                                        None, None,
                                    )?,
                                    type_name1,
                                    type_name2,
                                ])
                            );
                            self.diagnostics().add(
                                self.alloc_diagnostic(
                                    create_diagnostic_for_node_from_message_chain(
                                        type_node, error_info, None, self,
                                    )
                                    .into(),
                                ),
                            );
                        }
                    }
                }
            }
        }

        Ok(ok)
    }
}

struct IssueMemberSpecificErrorContainingMessageChain {
    arena: *const AllArenas,
    type_checker: Id<TypeChecker>,
    declared_prop: Id<Symbol>,
    type_with_this: Id<Type>,
    base_with_this: Id<Type>,
}

impl IssueMemberSpecificErrorContainingMessageChain {
    pub fn new(
        type_checker: Id<TypeChecker>,
        declared_prop: Id<Symbol>,
        type_with_this: Id<Type>,
        base_with_this: Id<Type>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            type_checker,
            declared_prop,
            type_with_this,
            base_with_this,
        }
    }
}

impl CheckTypeContainingMessageChain for IssueMemberSpecificErrorContainingMessageChain {
    fn get(&self) -> io::Result<Option<Rc<RefCell<DiagnosticMessageChain>>>> {
        Ok(Some(Rc::new(RefCell::new(
            chain_diagnostic_messages(
                None,
                &Diagnostics::Property_0_in_type_1_is_not_assignable_to_the_same_property_in_base_type_2,
                Some(vec![
                    self.type_checker.ref_(self).symbol_to_string_(
                        self.declared_prop,
                        Option::<Id<Node>>::None,
                        None, None, None,
                    )?,
                    self.type_checker.ref_(self).type_to_string_(
                        self.type_with_this,
                        Option::<Id<Node>>::None,
                        None, None,
                    )?,
                    self.type_checker.ref_(self).type_to_string_(
                        self.base_with_this,
                        Option::<Id<Node>>::None,
                        None, None,
                    )?,
                ])
            )
        ))))
    }
}

impl_has_arena!(IssueMemberSpecificErrorContainingMessageChain);

struct InheritanceInfoMap {
    pub prop: Id<Symbol>,
    pub containing_type: Id<Type>,
}
