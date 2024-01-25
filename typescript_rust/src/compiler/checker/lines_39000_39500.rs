use std::{io, ptr};

use gc::Gc;
use id_arena::Id;

use super::{intrinsic_type_kinds, is_instantiated_module};
use crate::{
    try_for_each, try_maybe_for_each, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface, __String, are_option_gcs_equal, cast_present, declaration_name_to_string,
    escape_leading_underscores, factory, for_each, get_declaration_of_kind,
    get_effective_modifier_flags, get_enclosing_block_scope_container, get_factory,
    get_interface_base_type_nodes, get_name_of_declaration, get_text_of_identifier_or_literal,
    get_text_of_property_name, has_abstract_modifier, is_ambient_module, is_binding_pattern,
    is_computed_non_literal_name, is_entity_name_expression, is_enum_const, is_enum_declaration,
    is_external_module_augmentation, is_external_module_name_relative, is_finite,
    is_global_scope_augmentation, is_identifier, is_infinity_or_nan_string, is_literal_expression,
    is_nan, is_optional_chain, is_private_identifier, is_static, is_string_literal_like, length,
    maybe_for_each, maybe_get_source_file_of_node, node_is_missing, node_is_present, set_parent,
    should_preserve_const_enums, AsDoubleDeref, Diagnostics, EnumKind,
    FunctionLikeDeclarationInterface, HasArena, HasInitializerInterface,
    HasTypeParametersInterface, InArena, InterfaceTypeInterface, ModifierFlags,
    NamedDeclarationInterface, Node, NodeCheckFlags, NodeFlags, NodeInterface, Number, OptionTry,
    ReadonlyTextRange, StringOrNumber, Symbol, SymbolFlags,
    OptionInArena,
};

impl TypeChecker {
    pub(super) fn check_property_initialization(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration*/
    ) -> io::Result<()> {
        if !self.strict_null_checks
            || !self.strict_property_initialization
            || node.ref_(self).flags().intersects(NodeFlags::Ambient)
        {
            return Ok(());
        }
        let constructor = self.find_constructor_declaration(node);
        for &member in &node.ref_(self).as_class_like_declaration().members() {
            if get_effective_modifier_flags(member, self).intersects(ModifierFlags::Ambient) {
                continue;
            }
            if !is_static(member, self) && self.is_property_without_initializer(member) {
                let member_ref = member.ref_(self);
                let member_as_property_declaration = member_ref.as_property_declaration();
                let prop_name = member_as_property_declaration.name();
                if is_identifier(&prop_name.ref_(self)) || is_private_identifier(&prop_name.ref_(self)) {
                    let type_ =
                        self.get_type_of_symbol(self.get_symbol_of_node(member)?.unwrap())?;
                    if !(type_.ref_(self).flags().intersects(TypeFlags::AnyOrUnknown)
                        || self.get_falsy_flags(type_).intersects(TypeFlags::Undefined))
                    {
                        if match constructor {
                            None => true,
                            Some(constructor) => !self.is_property_initialized_in_constructor(
                                prop_name,
                                type_,
                                constructor,
                            )?,
                        } {
                            self.error(
                                member_as_property_declaration.maybe_name(),
                                &Diagnostics::Property_0_has_no_initializer_and_is_not_definitely_assigned_in_the_constructor,
                                Some(vec![
                                    declaration_name_to_string(Some(prop_name), self).into_owned()
                                ])
                            );
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub(super) fn is_property_without_initializer(&self, node: Id<Node>) -> bool {
        node.ref_(self).kind() == SyntaxKind::PropertyDeclaration && !has_abstract_modifier(node, self) && {
            let node_ref = node.ref_(self);
            let node_as_property_declaration = node_ref.as_property_declaration();
            node_as_property_declaration.exclamation_token.is_none()
                && node_as_property_declaration.maybe_initializer().is_none()
        }
    }

    pub(super) fn is_property_initialized_in_static_blocks(
        &self,
        prop_name: Id<Node>, /*Identifier | PrivateIdentifier*/
        prop_type: Id<Type>,
        static_blocks: impl IntoIterator<Item = Id<Node /*ClassStaticBlockDeclaration*/>>,
        start_pos: isize,
        end_pos: isize,
    ) -> io::Result<bool> {
        for static_block in static_blocks {
            if static_block.ref_(self).pos() >= start_pos && static_block.ref_(self).pos() <= end_pos {
                let reference: Id<Node> = factory.with(|factory_| {
                    factory_.create_property_access_expression(
                        factory_.create_this(),
                        prop_name,
                    )
                });
                set_parent(
                    &reference.ref_(self).as_property_access_expression().expression.ref_(self),
                    Some(reference),
                );
                set_parent(&reference.ref_(self), Some(static_block));
                *reference.ref_(self).maybe_flow_node_mut() = static_block
                    .ref_(self).as_class_static_block_declaration()
                    .maybe_return_flow_node();
                let flow_type = self.get_flow_type_of_reference(
                    reference,
                    prop_type,
                    Some(self.get_optional_type_(prop_type, None)?),
                    Option::<Id<Node>>::None,
                )?;
                if !self
                    .get_falsy_flags(flow_type)
                    .intersects(TypeFlags::Undefined)
                {
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    pub(super) fn is_property_initialized_in_constructor(
        &self,
        prop_name: Id<Node>, /*Identifier | PrivateIdentifier*/
        prop_type: Id<Type>,
        constructor: Id<Node>, /*ConstructorDeclaration*/
    ) -> io::Result<bool> {
        let reference = get_factory().create_property_access_expression(
            get_factory().create_this(),
            prop_name,
        );
        set_parent(
            &reference.ref_(self).as_property_access_expression().expression.ref_(self),
            Some(reference),
        );
        set_parent(&reference.ref_(self), Some(constructor));
        *reference.ref_(self).maybe_flow_node_mut() = constructor
            .ref_(self).as_constructor_declaration()
            .maybe_return_flow_node();
        let flow_type = self.get_flow_type_of_reference(
            reference,
            prop_type,
            Some(self.get_optional_type_(prop_type, None)?),
            Option::<Id<Node>>::None,
        )?;
        Ok(!self
            .get_falsy_flags(flow_type)
            .intersects(TypeFlags::Undefined))
    }

    pub(super) fn check_interface_declaration(
        &self,
        node: Id<Node>, /*InterfaceDeclaration*/
    ) -> io::Result<()> {
        if !self.check_grammar_decorators_and_modifiers(node) {
            self.check_grammar_interface_declaration(node);
        }

        let node_ref = node.ref_(self);
        let node_as_interface_declaration = node_ref.as_interface_declaration();
        self.check_type_parameters(
            node_as_interface_declaration
                .maybe_type_parameters()
                .as_double_deref(),
        )?;
        if self.produce_diagnostics {
            self.check_type_name_is_reserved(
                node_as_interface_declaration.name(),
                &Diagnostics::Interface_name_cannot_be_0,
            );

            self.check_exports_on_merged_declarations(node)?;
            let symbol = self.get_symbol_of_node(node)?.unwrap();
            self.check_type_parameter_lists_identical(symbol)?;

            let first_interface_decl =
                get_declaration_of_kind(symbol, SyntaxKind::InterfaceDeclaration, self);
            if first_interface_decl == Some(node) {
                let type_ = self.get_declared_type_of_symbol(symbol)?;
                let type_with_this = self.get_type_with_this_argument(type_, None, None)?;
                if self.check_inherited_properties_are_identical(
                    type_,
                    node_as_interface_declaration.name(),
                )? {
                    for &base_type in &self.get_base_types(type_)? {
                        self.check_type_assignable_to(
                            type_with_this,
                            self.get_type_with_this_argument(
                                base_type,
                                {
                                    let this_type =
                                        type_.ref_(self).as_interface_type().maybe_this_type();
                                    this_type
                                },
                                None,
                            )?,
                            node_as_interface_declaration.maybe_name(),
                            Some(&Diagnostics::Interface_0_incorrectly_extends_interface_1),
                            None,
                            None,
                        )?;
                    }
                    self.check_index_constraints(type_, symbol, None)?;
                }
            }
            self.check_object_type_for_duplicate_declarations(node);
        }
        try_maybe_for_each(
            get_interface_base_type_nodes(node, self).as_ref(),
            |&heritage_element: &Id<Node>, _| -> io::Result<Option<()>> {
                let heritage_element_ref = heritage_element.ref_(self);
                let heritage_element_as_expression_with_type_arguments = heritage_element_ref.as_expression_with_type_arguments();
                if !is_entity_name_expression(
                    heritage_element_as_expression_with_type_arguments.expression,
                    self,
                ) || is_optional_chain(
                    &heritage_element_as_expression_with_type_arguments.expression.ref_(self),
                ) {
                    self.error(
                        Some(heritage_element_as_expression_with_type_arguments.expression),
                        &Diagnostics::An_interface_can_only_extend_an_identifier_Slashqualified_name_with_optional_type_arguments,
                        None,
                    );
                }
                self.check_type_reference_node(heritage_element)?;
                Ok(None)
            },
        )?;

        try_for_each(
            &node_as_interface_declaration.members,
            |&member, _| -> io::Result<_> {
                self.check_source_element(Some(member))?;
                Ok(Option::<()>::None)
            },
        )?;

        if self.produce_diagnostics {
            self.check_type_for_duplicate_index_signatures(node)?;
            self.register_for_unused_identifiers_check(node);
        }

        Ok(())
    }

    pub(super) fn check_type_alias_declaration(
        &self,
        node: Id<Node>, /*TypeAliasDeclaration*/
    ) -> io::Result<()> {
        self.check_grammar_decorators_and_modifiers(node);
        let node_ref = node.ref_(self);
        let node_as_type_alias_declaration = node_ref.as_type_alias_declaration();
        self.check_type_name_is_reserved(
            node_as_type_alias_declaration.name(),
            &Diagnostics::Type_alias_name_cannot_be_0,
        );
        self.check_exports_on_merged_declarations(node)?;
        self.check_type_parameters(
            node_as_type_alias_declaration
                .maybe_type_parameters()
                .as_double_deref(),
        )?;
        if node_as_type_alias_declaration.type_.ref_(self).kind() == SyntaxKind::IntrinsicKeyword {
            if !intrinsic_type_kinds.contains_key(
                &&*node_as_type_alias_declaration
                    .name()
                    .ref_(self).as_identifier()
                    .escaped_text,
            ) || length(
                node_as_type_alias_declaration
                    .maybe_type_parameters()
                    .as_double_deref(),
            ) != 1
            {
                self.error(
                    Some(node_as_type_alias_declaration.type_),
                    &Diagnostics::The_intrinsic_keyword_can_only_be_used_to_declare_compiler_provided_intrinsic_types,
                    None,
                );
            }
        } else {
            self.check_source_element(Some(node_as_type_alias_declaration.type_))?;
            self.register_for_unused_identifiers_check(node);
        }

        Ok(())
    }

    pub(super) fn compute_enum_member_values(
        &self,
        node: Id<Node>, /*EnumDeclaration*/
    ) -> io::Result<()> {
        let node_links = self.get_node_links(node);
        if !(*node_links)
            .borrow()
            .flags
            .intersects(NodeCheckFlags::EnumValuesComputed)
        {
            node_links.borrow_mut().flags |= NodeCheckFlags::EnumValuesComputed;
            let mut auto_value: Option<Number> = Some(Number::new(0.0));
            for &member in &node.ref_(self).as_enum_declaration().members {
                let value = self.compute_member_value(member, auto_value)?;
                self.get_node_links(member).borrow_mut().enum_member_value = value.clone();
                auto_value = if let Some(StringOrNumber::Number(value)) = value.as_ref() {
                    Some(*value + Number::new(1.0))
                } else {
                    None
                };
            }
        }

        Ok(())
    }

    pub(super) fn compute_member_value(
        &self,
        member: Id<Node>, /*EnumMember*/
        auto_value: Option<Number>,
    ) -> io::Result<Option<StringOrNumber>> {
        let member_ref = member.ref_(self);
        let member_as_enum_member = member_ref.as_enum_member();
        if is_computed_non_literal_name(member_as_enum_member.name, self) {
            self.error(
                Some(member_as_enum_member.name),
                &Diagnostics::Computed_property_names_are_not_allowed_in_enums,
                None,
            );
        } else {
            let text = get_text_of_property_name(member_as_enum_member.name, self);
            if self.is_numeric_literal_name(&text) && !is_infinity_or_nan_string(&text) {
                self.error(
                    Some(member_as_enum_member.name),
                    &Diagnostics::An_enum_member_cannot_have_a_numeric_name,
                    None,
                );
            }
        }
        if member_as_enum_member.initializer.is_some() {
            return self.compute_constant_value(member);
        }
        if member.ref_(self).parent().ref_(self).flags().intersects(NodeFlags::Ambient)
            && !is_enum_const(member.ref_(self).parent(), self)
            && self.get_enum_kind(self.get_symbol_of_node(member.ref_(self).parent())?.unwrap())?
                == EnumKind::Numeric
        {
            return Ok(None);
        }
        if auto_value.is_some() {
            return Ok(auto_value.map(Into::into));
        }
        self.error(
            Some(member_as_enum_member.name),
            &Diagnostics::Enum_member_must_have_initializer,
            None,
        );
        Ok(None)
    }

    pub(super) fn compute_constant_value(
        &self,
        member: Id<Node>, /*EnumMember*/
    ) -> io::Result<Option<StringOrNumber>> {
        let enum_kind = self.get_enum_kind(self.get_symbol_of_node(member.ref_(self).parent())?.unwrap())?;
        let is_const_enum = is_enum_const(member.ref_(self).parent(), self);
        let member_ref = member.ref_(self);
        let member_as_enum_member = member_ref.as_enum_member();
        let initializer = member_as_enum_member.initializer.unwrap();
        let value = if enum_kind == EnumKind::Literal && !self.is_literal_enum_member(member)? {
            None
        } else {
            self.evaluate(member, initializer)?
        };
        if let Some(value) = value.as_ref() {
            if is_const_enum {
                if let StringOrNumber::Number(value) = value {
                    if !is_finite(value) {
                        self.error(
                            Some(initializer),
                            if is_nan(value) {
                                &*Diagnostics::const_enum_member_initializer_was_evaluated_to_disallowed_value_NaN
                            } else {
                                &*Diagnostics::const_enum_member_initializer_was_evaluated_to_a_non_finite_value
                            },
                            None,
                        );
                    }
                }
            }
        } else if enum_kind == EnumKind::Literal {
            self.error(
                Some(initializer),
                &Diagnostics::Computed_values_are_not_permitted_in_an_enum_with_string_valued_members,
                None,
            );
            return Ok(Some(StringOrNumber::Number(Number::new(0.0))));
        } else if is_const_enum {
            self.error(
                Some(initializer),
                &Diagnostics::const_enum_member_initializers_can_only_contain_literal_values_and_other_computed_enum_values,
                None,
            );
        } else if member.ref_(self).parent().ref_(self).flags().intersects(NodeFlags::Ambient) {
            self.error(
                Some(initializer),
                &Diagnostics::In_ambient_enum_declarations_member_initializer_must_be_constant_expression,
                None,
            );
        } else {
            let source = self.check_expression(initializer, None, None)?;
            if !self.is_type_assignable_to_kind(source, TypeFlags::NumberLike, None)? {
                self.error(
                    Some(initializer),
                    &Diagnostics::Only_numeric_enums_can_have_computed_members_but_this_expression_has_type_0_If_you_do_not_need_exhaustiveness_checks_consider_using_an_object_literal_instead,
                    Some(vec![
                        self.type_to_string_(
                            source,
                            Option::<Id<Node>>::None,
                            None, None,
                        )?
                    ])
                );
            } else {
                self.check_type_assignable_to(
                    source,
                    self.get_declared_type_of_symbol(
                        self.get_symbol_of_node(member.ref_(self).parent())?.unwrap(),
                    )?,
                    Some(initializer),
                    None,
                    None,
                    None,
                )?;
            }
        }
        Ok(value)
    }

    pub(super) fn evaluate(
        &self,
        member: Id<Node>,
        expr: Id<Node>, /*Expression*/
    ) -> io::Result<Option<StringOrNumber>> {
        match expr.ref_(self).kind() {
            SyntaxKind::PrefixUnaryExpression => {
                let expr_ref = expr.ref_(self);
                let expr_as_prefix_unary_expression = expr_ref.as_prefix_unary_expression();
                let value = self.evaluate(member, expr_as_prefix_unary_expression.operand)?;
                if let Some(StringOrNumber::Number(value)) = value.as_ref() {
                    match expr_as_prefix_unary_expression.operator {
                        SyntaxKind::PlusToken => {
                            return Ok(Some(StringOrNumber::Number(*value)));
                        }
                        SyntaxKind::MinusToken => {
                            return Ok(Some(StringOrNumber::Number(-*value)));
                        }
                        SyntaxKind::TildeToken => {
                            return Ok(Some(StringOrNumber::Number(!*value)));
                        }
                        _ => (),
                    }
                }
            }
            SyntaxKind::BinaryExpression => {
                let expr_ref = expr.ref_(self);
                let expr_as_binary_expression = expr_ref.as_binary_expression();
                let left = self.evaluate(member, expr_as_binary_expression.left)?;
                let right = self.evaluate(member, expr_as_binary_expression.right)?;
                if let (Some(StringOrNumber::Number(left)), Some(StringOrNumber::Number(right))) =
                    (left.as_ref(), right.as_ref())
                {
                    match expr_as_binary_expression.operator_token.ref_(self).kind() {
                        SyntaxKind::BarToken => {
                            return Ok(Some(StringOrNumber::Number(*left | *right)));
                        }
                        SyntaxKind::AmpersandToken => {
                            return Ok(Some(StringOrNumber::Number(*left & *right)));
                        }
                        SyntaxKind::GreaterThanGreaterThanToken => {
                            return Ok(Some(StringOrNumber::Number(*left >> *right)));
                        }
                        SyntaxKind::GreaterThanGreaterThanGreaterThanToken => {
                            // per https://stackoverflow.com/a/70212287/732366
                            return Ok(Some(StringOrNumber::Number(Number::new(
                                ((left.integer_value() as u64) >> right.integer_value()) as f64,
                            ))));
                        }
                        SyntaxKind::LessThanLessThanToken => {
                            return Ok(Some(StringOrNumber::Number(*left << *right)));
                        }
                        SyntaxKind::CaretToken => {
                            return Ok(Some(StringOrNumber::Number(*left ^ *right)));
                        }
                        SyntaxKind::AsteriskToken => {
                            return Ok(Some(StringOrNumber::Number(*left * *right)));
                        }
                        SyntaxKind::SlashToken => {
                            return Ok(Some(StringOrNumber::Number(*left / *right)));
                        }
                        SyntaxKind::PlusToken => {
                            return Ok(Some(StringOrNumber::Number(*left + *right)));
                        }
                        SyntaxKind::MinusToken => {
                            return Ok(Some(StringOrNumber::Number(*left - *right)));
                        }
                        SyntaxKind::PercentToken => {
                            return Ok(Some(StringOrNumber::Number(*left % *right)));
                        }
                        SyntaxKind::AsteriskAsteriskToken => {
                            return Ok(Some(StringOrNumber::Number(Number::new(
                                left.value().powf(right.value()),
                            ))));
                        }
                        _ => (),
                    }
                } else if let (
                    Some(StringOrNumber::String(left)),
                    Some(StringOrNumber::String(right)),
                ) = (left.as_ref(), right.as_ref())
                {
                    return Ok(Some(StringOrNumber::String(format!("{}{}", left, right))));
                }
            }
            SyntaxKind::StringLiteral | SyntaxKind::NoSubstitutionTemplateLiteral => {
                return Ok(Some(StringOrNumber::String(
                    expr.ref_(self).as_literal_like_node().text().clone(),
                )));
            }
            SyntaxKind::NumericLiteral => {
                self.check_grammar_numeric_literal(expr);
                return Ok(Some(StringOrNumber::Number(
                    (&**expr.ref_(self).as_literal_like_node().text()).into(),
                )));
            }
            SyntaxKind::ParenthesizedExpression => {
                return self.evaluate(member, expr.ref_(self).as_parenthesized_expression().expression);
            }
            SyntaxKind::Identifier => {
                let expr_ref = expr.ref_(self);
                let identifier = expr_ref.as_identifier();
                if is_infinity_or_nan_string(&identifier.escaped_text) {
                    unimplemented!("Infinity/NaN not implemented yet")
                }
                return Ok(if node_is_missing(Some(&expr.ref_(self))) {
                    Some(StringOrNumber::Number(Number::new(0.0)))
                } else {
                    self.evaluate_enum_member(
                        member,
                        expr,
                        self.get_symbol_of_node(member.ref_(self).parent())?.unwrap(),
                        &identifier.escaped_text,
                    )?
                });
            }
            SyntaxKind::ElementAccessExpression | SyntaxKind::PropertyAccessExpression => {
                let ex = expr;
                if self.is_constant_member_access(ex) {
                    let type_ =
                        self.get_type_of_expression(ex.ref_(self).as_has_expression().expression())?;
                    if let Some(type_symbol) =
                        type_.ref_(self).maybe_symbol().filter(|&type_symbol| {
                            type_symbol.ref_(self).flags().intersects(SymbolFlags::Enum)
                        })
                    {
                        let name: __String;
                        if ex.ref_(self).kind() == SyntaxKind::PropertyAccessExpression {
                            name = ex
                                .ref_(self).as_property_access_expression()
                                .name
                                .ref_(self).as_member_name()
                                .escaped_text()
                                .to_owned();
                        } else {
                            name = escape_leading_underscores(
                                &cast_present(
                                    ex.ref_(self).as_element_access_expression().argument_expression,
                                    |expression: &Id<Node>| is_literal_expression(&expression.ref_(self)),
                                )
                                .ref_(self).as_literal_like_node()
                                .text(),
                            )
                            .into_owned();
                        }
                        return self.evaluate_enum_member(member, expr, type_symbol, &name);
                    }
                }
            }
            _ => (),
        }
        Ok(None)
    }

    pub(super) fn evaluate_enum_member(
        &self,
        member: Id<Node>,
        expr: Id<Node>, /*Expression*/
        enum_symbol: Id<Symbol>,
        name: &str, /*__String*/
    ) -> io::Result<Option<StringOrNumber>> {
        let member_symbol = (*enum_symbol.ref_(self).maybe_exports().clone().unwrap())
            .borrow()
            .get(name)
            .cloned();
        if let Some(member_symbol) = member_symbol {
            let declaration = member_symbol.ref_(self).maybe_value_declaration();
            if declaration != Some(member) {
                if let Some(declaration) = declaration.try_filter(|&declaration| {
                    self.is_block_scoped_name_declared_before_use(declaration, member)
                })? {
                    return self.get_enum_member_value(declaration);
                }
                self.error(
                    Some(expr),
                    &Diagnostics::A_member_initializer_in_a_enum_declaration_cannot_reference_members_declared_after_it_including_members_defined_in_other_enums,
                    None,
                );
                return Ok(Some(StringOrNumber::Number(Number::new(0.0))));
            } else {
                self.error(
                    Some(expr),
                    &Diagnostics::Property_0_is_used_before_being_assigned,
                    Some(vec![self.symbol_to_string_(
                        member_symbol,
                        Option::<Id<Node>>::None,
                        None,
                        None,
                        None,
                    )?]),
                );
            }
        }
        Ok(None)
    }

    pub(super) fn is_constant_member_access(&self, node: Id<Node> /*Expression*/) -> bool {
        node.ref_(self).kind() == SyntaxKind::Identifier
            || node.ref_(self).kind() == SyntaxKind::PropertyAccessExpression
                && self.is_constant_member_access(node.ref_(self).as_property_access_expression().expression)
            || node.ref_(self).kind() == SyntaxKind::ElementAccessExpression
                && self.is_constant_member_access(node.ref_(self).as_element_access_expression().expression)
                && is_string_literal_like(&node.ref_(self).as_element_access_expression().argument_expression.ref_(self))
    }

    pub(super) fn check_enum_declaration(
        &self,
        node: Id<Node>, /*EnumDeclaration*/
    ) -> io::Result<()> {
        if !self.produce_diagnostics {
            return Ok(());
        }

        self.check_grammar_decorators_and_modifiers(node);

        let node_ref = node.ref_(self);
        let node_as_enum_declaration = node_ref.as_enum_declaration();
        self.check_collisions_for_declaration_name(node, node_as_enum_declaration.maybe_name());
        self.check_exports_on_merged_declarations(node)?;
        for &member in &node_as_enum_declaration.members {
            self.check_enum_member(member);
        }

        self.compute_enum_member_values(node)?;

        let enum_symbol = self.get_symbol_of_node(node)?.unwrap();
        let first_declaration = get_declaration_of_kind(enum_symbol, node.ref_(self).kind(), self);
        if first_declaration == Some(node) {
            if let Some(enum_symbol_declarations) = enum_symbol
                .ref_(self)
                .maybe_declarations()
                .as_ref()
                .filter(|enum_symbol_declarations| enum_symbol_declarations.len() > 1)
            {
                let enum_is_const = is_enum_const(node, self);
                for_each(
                    enum_symbol_declarations,
                    |&decl: &Id<Node>, _| -> Option<()> {
                        if is_enum_declaration(&decl.ref_(self)) && is_enum_const(decl, self) != enum_is_const {
                            self.error(
                                get_name_of_declaration(Some(decl), self),
                                &Diagnostics::Enum_declarations_must_all_be_const_or_non_const,
                                None,
                            );
                        }
                        None
                    },
                );
            }

            let mut seen_enum_missing_initial_initializer = false;
            maybe_for_each(
                enum_symbol.ref_(self).maybe_declarations().as_ref(),
                |declaration: &Id<Node>, _| -> Option<()> {
                    if declaration.ref_(self).kind() != SyntaxKind::EnumDeclaration {
                        return None;
                    }

                    let declaration_ref = declaration.ref_(self);
                    let enum_declaration = declaration_ref.as_enum_declaration();
                    if enum_declaration.members.is_empty() {
                        return None;
                    }

                    let first_enum_member = enum_declaration.members[0];
                    let first_enum_member_ref = first_enum_member.ref_(self);
                    let first_enum_member_as_enum_member = first_enum_member_ref.as_enum_member();
                    if first_enum_member_as_enum_member.initializer.is_none() {
                        if seen_enum_missing_initial_initializer {
                            self.error(
                                Some(first_enum_member_as_enum_member.name),
                                &Diagnostics::In_an_enum_with_multiple_declarations_only_one_declaration_can_omit_an_initializer_for_its_first_enum_element,
                                None,
                            );
                        } else {
                            seen_enum_missing_initial_initializer = true;
                        }
                    }
                    None
                },
            );
        }

        Ok(())
    }

    pub(super) fn check_enum_member(&self, node: Id<Node> /*EnumMember*/) {
        if is_private_identifier(&node.ref_(self).as_enum_member().name.ref_(self)) {
            self.error(
                Some(node),
                &Diagnostics::An_enum_member_cannot_be_named_with_a_private_identifier,
                None,
            );
        }
    }

    pub(super) fn get_first_non_ambient_class_or_function_declaration(
        &self,
        symbol: Id<Symbol>,
    ) -> Option<Id<Node /*Declaration*/>> {
        let symbol_ref = symbol.ref_(self);
        let declarations = symbol_ref.maybe_declarations();
        if let Some(declarations) = declarations.as_ref() {
            for &declaration in declarations {
                if (declaration.ref_(self).kind() == SyntaxKind::ClassDeclaration
                    || declaration.ref_(self).kind() == SyntaxKind::FunctionDeclaration
                        && node_is_present(declaration.ref_(self).as_function_declaration().maybe_body().refed(self).as_deref()))
                    && !declaration.ref_(self).flags().intersects(NodeFlags::Ambient)
                {
                    return Some(declaration);
                }
            }
        }
        None
    }

    pub(super) fn in_same_lexical_scope(&self, node1: Id<Node>, node2: Id<Node>) -> bool {
        let container1 = get_enclosing_block_scope_container(node1, self).unwrap();
        let container2 = get_enclosing_block_scope_container(node2, self).unwrap();
        if self.is_global_source_file(container1) {
            self.is_global_source_file(container2)
        } else if self.is_global_source_file(container2) {
            false
        } else {
            container1 == container2
        }
    }

    pub(super) fn check_module_declaration(
        &self,
        node: Id<Node>, /*ModuleDeclaration*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_module_declaration = node_ref.as_module_declaration();
        if self.produce_diagnostics {
            let is_global_augmentation = is_global_scope_augmentation(&node.ref_(self));
            let in_ambient_context = node.ref_(self).flags().intersects(NodeFlags::Ambient);
            if is_global_augmentation && !in_ambient_context {
                self.error(
                    Some(node_as_module_declaration.name),
                    &Diagnostics::Augmentations_for_the_global_scope_should_have_declare_modifier_unless_they_appear_in_already_ambient_context,
                    None,
                );
            }

            let is_ambient_external_module = is_ambient_module(node, self);
            let context_error_message = if is_ambient_external_module {
                &*Diagnostics::An_ambient_module_declaration_is_only_allowed_at_the_top_level_in_a_file
            } else {
                &*Diagnostics::A_namespace_declaration_is_only_allowed_in_a_namespace_or_module
            };
            if self.check_grammar_module_element_context(node, context_error_message) {
                return Ok(());
            }

            if !self.check_grammar_decorators_and_modifiers(node) {
                if !in_ambient_context
                    && node_as_module_declaration.name.ref_(self).kind() == SyntaxKind::StringLiteral
                {
                    self.grammar_error_on_node(
                        node_as_module_declaration.name,
                        &Diagnostics::Only_ambient_modules_can_use_quoted_names,
                        None,
                    );
                }
            }

            if is_identifier(&node_as_module_declaration.name.ref_(self)) {
                self.check_collisions_for_declaration_name(
                    node,
                    Some(node_as_module_declaration.name),
                );
            }

            self.check_exports_on_merged_declarations(node)?;
            let symbol = self.get_symbol_of_node(node)?.unwrap();

            if symbol
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::ValueModule)
                && !in_ambient_context
                && matches!(
                    symbol.ref_(self).maybe_declarations().as_ref(),
                    Some(symbol_declarations) if symbol_declarations.len() > 1
                )
                && is_instantiated_module(node, should_preserve_const_enums(&self.compiler_options.ref_(self)), self)
            {
                let first_non_ambient_class_or_func =
                    self.get_first_non_ambient_class_or_function_declaration(symbol);
                if let Some(first_non_ambient_class_or_func) =
                    first_non_ambient_class_or_func
                {
                    if maybe_get_source_file_of_node(Some(node), self) !=
                        maybe_get_source_file_of_node(Some(first_non_ambient_class_or_func), self)
                    {
                        self.error(
                            Some(node_as_module_declaration.name),
                            &Diagnostics::A_namespace_declaration_cannot_be_in_a_different_file_from_a_class_or_function_with_which_it_is_merged,
                            None,
                        );
                    } else if node.ref_(self).pos() < first_non_ambient_class_or_func.ref_(self).pos() {
                        self.error(
                            Some(node_as_module_declaration.name),
                            &Diagnostics::A_namespace_declaration_cannot_be_located_prior_to_a_class_or_function_with_which_it_is_merged,
                            None,
                        );
                    }
                }

                let merged_class =
                    get_declaration_of_kind(symbol, SyntaxKind::ClassDeclaration, self);
                if matches!(
                    merged_class,
                    Some(merged_class) if self.in_same_lexical_scope(
                        node,
                        merged_class
                    )
                ) {
                    self.get_node_links(node).borrow_mut().flags |=
                        NodeCheckFlags::LexicalModuleMergesWithClass;
                }
            }

            if is_ambient_external_module {
                if is_external_module_augmentation(node, self) {
                    let check_body = is_global_augmentation
                        || self
                            .get_symbol_of_node(node)?
                            .unwrap()
                            .ref_(self)
                            .flags()
                            .intersects(SymbolFlags::Transient);
                    if check_body {
                        if let Some(node_body) = node_as_module_declaration.body {
                            for &statement in &node_body.ref_(self).as_module_block().statements {
                                self.check_module_augmentation_element(
                                    statement,
                                    is_global_augmentation,
                                )?;
                            }
                        }
                    }
                } else if self.is_global_source_file(node.ref_(self).parent()) {
                    if is_global_augmentation {
                        self.error(
                            Some(node_as_module_declaration.name),
                            &Diagnostics::Augmentations_for_the_global_scope_can_only_be_directly_nested_in_external_modules_or_ambient_module_declarations,
                            None,
                        );
                    } else if is_external_module_name_relative(&get_text_of_identifier_or_literal(
                        &node_as_module_declaration.name.ref_(self),
                    )) {
                        self.error(
                            Some(node_as_module_declaration.name),
                            &Diagnostics::Ambient_module_declaration_cannot_specify_relative_module_name,
                            None,
                        );
                    }
                } else {
                    if is_global_augmentation {
                        self.error(
                            Some(node_as_module_declaration.name),
                            &Diagnostics::Augmentations_for_the_global_scope_can_only_be_directly_nested_in_external_modules_or_ambient_module_declarations,
                            None,
                        );
                    } else {
                        self.error(
                            Some(node_as_module_declaration.name),
                            &Diagnostics::Ambient_modules_cannot_be_nested_in_other_modules_or_namespaces,
                            None,
                        );
                    }
                }
            }
        }

        if let Some(node_body) = node_as_module_declaration.body {
            self.check_source_element(Some(node_body))?;
            if !is_global_scope_augmentation(&node.ref_(self)) {
                self.register_for_unused_identifiers_check(node);
            }
        }

        Ok(())
    }

    pub(super) fn check_module_augmentation_element(
        &self,
        node: Id<Node>,
        is_global_augmentation: bool,
    ) -> io::Result<()> {
        match node.ref_(self).kind() {
            SyntaxKind::VariableStatement => {
                for &decl in &node
                    .ref_(self).as_variable_statement()
                    .declaration_list
                    .ref_(self).as_variable_declaration_list()
                    .declarations
                {
                    self.check_module_augmentation_element(decl, is_global_augmentation)?;
                }
            }
            SyntaxKind::ExportAssignment | SyntaxKind::ExportDeclaration => {
                self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::Exports_and_export_assignments_are_not_permitted_in_module_augmentations,
                    None,
                );
            }
            SyntaxKind::ImportEqualsDeclaration | SyntaxKind::ImportDeclaration => {
                self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::Imports_are_not_permitted_in_module_augmentations_Consider_moving_them_to_the_enclosing_external_module,
                    None,
                );
            }
            SyntaxKind::BindingElement | SyntaxKind::VariableDeclaration => {
                let name = node.ref_(self).as_named_declaration().maybe_name();
                if is_binding_pattern(name.refed(self).as_deref()) {
                    for &el in &name.unwrap().ref_(self).as_has_elements().elements() {
                        self.check_module_augmentation_element(el, is_global_augmentation)?;
                    }
                }
            }
            SyntaxKind::ClassDeclaration
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::TypeAliasDeclaration => {
                if is_global_augmentation {
                    return Ok(());
                }
                let symbol = self.get_symbol_of_node(node)?;
                if let Some(symbol) = symbol {
                    let mut report_error =
                        !symbol.ref_(self).flags().intersects(SymbolFlags::Transient);
                    #[allow(unused_assignments)]
                    if !report_error {
                        report_error = matches!(
                            symbol.ref_(self).maybe_parent().and_then(|symbol_parent| {
                                symbol_parent.ref_(self).maybe_declarations().clone()
                            }).as_ref(),
                            Some(symbol_parent_declarations) if is_external_module_augmentation(symbol_parent_declarations[0], self)
                        );
                    }
                }
            }
            _ => (),
        }

        Ok(())
    }
}
