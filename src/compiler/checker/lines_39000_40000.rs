#![allow(non_upper_case_globals)]

use std::ptr;
use std::rc::Rc;

use super::intrinsic_type_kinds;
use crate::{
    cast_present, declaration_name_to_string, factory, for_each, get_declaration_of_kind,
    get_effective_modifier_flags, get_interface_base_type_nodes, get_text_of_property_name,
    has_abstract_modifier, is_computed_non_literal_name, is_entity_name_expression, is_enum_const,
    is_finite, is_identifier, is_infinity_or_nan_string, is_literal_expression, is_nan,
    is_optional_chain, is_private_identifier, is_static, is_string_literal_like, length,
    maybe_for_each, node_is_missing, set_parent, synthetic_factory, Diagnostics, EnumKind,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasTypeParametersInterface,
    InterfaceTypeInterface, ModifierFlags, NamedDeclarationInterface, Node, NodeCheckFlags,
    NodeFlags, NodeInterface, Number, ReadonlyTextRange, StringOrNumber, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, __String,
    escape_leading_underscores,
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

    pub(super) fn compute_enum_member_values(&self, node: &Node /*EnumDeclaration*/) {
        let node_links = self.get_node_links(node);
        if !(*node_links)
            .borrow()
            .flags
            .intersects(NodeCheckFlags::EnumValuesComputed)
        {
            node_links.borrow_mut().flags |= NodeCheckFlags::EnumValuesComputed;
            let mut auto_value: Option<Number> = Some(Number::new(0.0));
            for member in &node.as_enum_declaration().members {
                let value = self.compute_member_value(member, auto_value);
                self.get_node_links(member).borrow_mut().enum_member_value = value.clone();
                auto_value = if let Some(StringOrNumber::Number(value)) = value.as_ref() {
                    Some(*value + Number::new(1.0))
                } else {
                    None
                };
            }
        }
    }

    pub(super) fn compute_member_value(
        &self,
        member: &Node, /*EnumMember*/
        auto_value: Option<Number>,
    ) -> Option<StringOrNumber> {
        let member_as_enum_member = member.as_enum_member();
        if is_computed_non_literal_name(&member_as_enum_member.name) {
            self.error(
                Some(&*member_as_enum_member.name),
                &Diagnostics::Computed_property_names_are_not_allowed_in_enums,
                None,
            );
        } else {
            let text = get_text_of_property_name(&member_as_enum_member.name);
            if self.is_numeric_literal_name(&text) && !is_infinity_or_nan_string(&text) {
                self.error(
                    Some(&*member_as_enum_member.name),
                    &Diagnostics::An_enum_member_cannot_have_a_numeric_name,
                    None,
                );
            }
        }
        if member_as_enum_member.initializer.is_some() {
            return self.compute_constant_value(member);
        }
        if member.parent().flags().intersects(NodeFlags::Ambient)
            && !is_enum_const(&member.parent())
            && self.get_enum_kind(&self.get_symbol_of_node(&member.parent()).unwrap())
                == EnumKind::Numeric
        {
            return None;
        }
        if auto_value.is_some() {
            return auto_value.map(Into::into);
        }
        self.error(
            Some(&*member_as_enum_member.name),
            &Diagnostics::Enum_member_must_have_initializer,
            None,
        );
        None
    }

    pub(super) fn compute_constant_value(
        &self,
        member: &Node, /*EnumMember*/
    ) -> Option<StringOrNumber> {
        let enum_kind = self.get_enum_kind(&self.get_symbol_of_node(&member.parent()).unwrap());
        let is_const_enum = is_enum_const(&member.parent());
        let member_as_enum_member = member.as_enum_member();
        let initializer = member_as_enum_member.initializer.as_ref().unwrap();
        let value = if enum_kind == EnumKind::Literal && !self.is_literal_enum_member(member) {
            None
        } else {
            self.evaluate(member, initializer)
        };
        if let Some(value) = value.as_ref() {
            if is_const_enum {
                if let StringOrNumber::Number(value) = value {
                    if !is_finite(value) {
                        self.error(
                            Some(&**initializer),
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
                Some(&**initializer),
                &Diagnostics::Computed_values_are_not_permitted_in_an_enum_with_string_valued_members,
                None,
            );
            return Some(StringOrNumber::Number(Number::new(0.0)));
        } else if is_const_enum {
            self.error(
                Some(&**initializer),
                &Diagnostics::const_enum_member_initializers_can_only_contain_literal_values_and_other_computed_enum_values,
                None,
            );
        } else if member.parent().flags().intersects(NodeFlags::Ambient) {
            self.error(
                Some(&**initializer),
                &Diagnostics::In_ambient_enum_declarations_member_initializer_must_be_constant_expression,
                None,
            );
        } else {
            let source = self.check_expression(initializer, None, None);
            if !self.is_type_assignable_to_kind(&source, TypeFlags::NumberLike, None) {
                self.error(
                    Some(&**initializer),
                    &Diagnostics::Only_numeric_enums_can_have_computed_members_but_this_expression_has_type_0_If_you_do_not_need_exhaustiveness_checks_consider_using_an_object_literal_instead,
                    Some(vec![
                        self.type_to_string_(
                            &source,
                            Option::<&Node>::None,
                            None, None,
                        )
                    ])
                );
            } else {
                self.check_type_assignable_to(
                    &source,
                    &self.get_declared_type_of_symbol(
                        &self.get_symbol_of_node(&member.parent()).unwrap(),
                    ),
                    Some(&**initializer),
                    None,
                    None,
                    None,
                );
            }
        }
        value
    }

    pub(super) fn evaluate(
        &self,
        member: &Node,
        expr: &Node, /*Expression*/
    ) -> Option<StringOrNumber> {
        match expr.kind() {
            SyntaxKind::PrefixUnaryExpression => {
                let expr_as_prefix_unary_expression = expr.as_prefix_unary_expression();
                let value = self.evaluate(member, &expr_as_prefix_unary_expression.operand);
                if let Some(StringOrNumber::Number(value)) = value.as_ref() {
                    match expr_as_prefix_unary_expression.operator {
                        SyntaxKind::PlusToken => {
                            return Some(StringOrNumber::Number(*value));
                        }
                        SyntaxKind::MinusToken => {
                            return Some(StringOrNumber::Number(-*value));
                        }
                        SyntaxKind::TildeToken => {
                            return Some(StringOrNumber::Number(!*value));
                        }
                        _ => (),
                    }
                }
            }
            SyntaxKind::BinaryExpression => {
                let expr_as_binary_expression = expr.as_binary_expression();
                let left = self.evaluate(member, &expr_as_binary_expression.left);
                let right = self.evaluate(member, &expr_as_binary_expression.right);
                if let (Some(StringOrNumber::Number(left)), Some(StringOrNumber::Number(right))) =
                    (left.as_ref(), right.as_ref())
                {
                    match expr_as_binary_expression.operator_token.kind() {
                        SyntaxKind::BarToken => {
                            return Some(StringOrNumber::Number(*left | *right));
                        }
                        SyntaxKind::AmpersandToken => {
                            return Some(StringOrNumber::Number(*left & *right));
                        }
                        SyntaxKind::GreaterThanGreaterThanToken => {
                            return Some(StringOrNumber::Number(*left >> *right));
                        }
                        SyntaxKind::GreaterThanGreaterThanGreaterThanToken => {
                            // per https://stackoverflow.com/a/70212287/732366
                            return Some(StringOrNumber::Number(Number::new(
                                ((left.integer_value() as u64) >> right.integer_value()) as f64,
                            )));
                        }
                        SyntaxKind::LessThanLessThanToken => {
                            return Some(StringOrNumber::Number(*left << *right));
                        }
                        SyntaxKind::CaretToken => {
                            return Some(StringOrNumber::Number(*left ^ *right));
                        }
                        SyntaxKind::AsteriskToken => {
                            return Some(StringOrNumber::Number(*left * *right));
                        }
                        SyntaxKind::SlashToken => {
                            return Some(StringOrNumber::Number(*left / *right));
                        }
                        SyntaxKind::PlusToken => {
                            return Some(StringOrNumber::Number(*left + *right));
                        }
                        SyntaxKind::MinusToken => {
                            return Some(StringOrNumber::Number(*left - *right));
                        }
                        SyntaxKind::PercentToken => {
                            return Some(StringOrNumber::Number(*left % *right));
                        }
                        SyntaxKind::AsteriskAsteriskToken => {
                            return Some(StringOrNumber::Number(Number::new(
                                left.value().powf(right.value()),
                            )));
                        }
                        _ => (),
                    }
                } else if let (
                    Some(StringOrNumber::String(left)),
                    Some(StringOrNumber::String(right)),
                ) = (left.as_ref(), right.as_ref())
                {
                    return Some(StringOrNumber::String(format!("{}{}", left, right)));
                }
            }
            SyntaxKind::StringLiteral | SyntaxKind::NoSubstitutionTemplateLiteral => {
                return Some(StringOrNumber::String(
                    expr.as_literal_like_node().text().clone(),
                ));
            }
            SyntaxKind::NumericLiteral => {
                self.check_grammar_numeric_literal(expr);
                return Some(StringOrNumber::Number(
                    (&**expr.as_literal_like_node().text()).into(),
                ));
            }
            SyntaxKind::ParenthesizedExpression => {
                return self.evaluate(member, &expr.as_parenthesized_expression().expression);
            }
            SyntaxKind::Identifier => {
                let identifier = expr.as_identifier();
                if is_infinity_or_nan_string(&identifier.escaped_text) {
                    unimplemented!("Infinity/NaN not implemented yet")
                }
                return if node_is_missing(Some(expr)) {
                    Some(StringOrNumber::Number(Number::new(0.0)))
                } else {
                    self.evaluate_enum_member(
                        member,
                        expr,
                        &self.get_symbol_of_node(&member.parent()).unwrap(),
                        &identifier.escaped_text,
                    )
                };
            }
            SyntaxKind::ElementAccessExpression | SyntaxKind::PropertyAccessExpression => {
                let ex = expr;
                if self.is_constant_member_access(ex) {
                    let type_ = self.get_type_of_expression(&ex.as_has_expression().expression());
                    if let Some(type_symbol) = type_
                        .maybe_symbol()
                        .as_ref()
                        .filter(|type_symbol| type_symbol.flags().intersects(SymbolFlags::Enum))
                    {
                        let name: __String;
                        if ex.kind() == SyntaxKind::PropertyAccessExpression {
                            name = ex
                                .as_property_access_expression()
                                .name
                                .as_member_name()
                                .escaped_text();
                        } else {
                            name = escape_leading_underscores(
                                &cast_present(
                                    &ex.as_element_access_expression().argument_expression,
                                    |expression: &&Rc<Node>| is_literal_expression(expression),
                                )
                                .as_literal_like_node()
                                .text(),
                            );
                        }
                        return self.evaluate_enum_member(member, expr, type_symbol, &name);
                    }
                }
            }
            _ => (),
        }
        None
    }

    pub(super) fn evaluate_enum_member(
        &self,
        member: &Node,
        expr: &Node, /*Expression*/
        enum_symbol: &Symbol,
        name: &__String,
    ) -> Option<StringOrNumber> {
        let member_symbol = (*enum_symbol.maybe_exports().clone().unwrap())
            .borrow()
            .get(name)
            .cloned();
        if let Some(member_symbol) = member_symbol.as_ref() {
            let declaration = member_symbol.maybe_value_declaration();
            if !matches!(
                declaration.as_ref(),
                Some(declaration) if ptr::eq(
                    &**declaration,
                    member
                )
            ) {
                if let Some(declaration) = declaration.as_ref().filter(|declaration| {
                    self.is_block_scoped_name_declared_before_use(declaration, member)
                }) {
                    return self.get_enum_member_value(declaration);
                }
                self.error(
                    Some(expr),
                    &Diagnostics::A_member_initializer_in_a_enum_declaration_cannot_reference_members_declared_after_it_including_members_defined_in_other_enums,
                    None,
                );
                return Some(StringOrNumber::Number(Number::new(0.0)));
            } else {
                self.error(
                    Some(expr),
                    &Diagnostics::Property_0_is_used_before_being_assigned,
                    Some(vec![self.symbol_to_string_(
                        member_symbol,
                        Option::<&Node>::None,
                        None,
                        None,
                        None,
                    )]),
                );
            }
        }
        None
    }

    pub(super) fn is_constant_member_access(&self, node: &Node /*Expression*/) -> bool {
        node.kind() == SyntaxKind::Identifier
            || node.kind() == SyntaxKind::PropertyAccessExpression
                && self.is_constant_member_access(&node.as_property_access_expression().expression)
            || node.kind() == SyntaxKind::ElementAccessExpression
                && self.is_constant_member_access(&node.as_element_access_expression().expression)
                && is_string_literal_like(&node.as_element_access_expression().argument_expression)
    }

    pub(super) fn check_alias_symbol(
        &self,
        node: &Node, /*ImportEqualsDeclaration | VariableDeclaration | ImportClause | NamespaceImport | ImportSpecifier | ExportSpecifier | NamespaceExport*/
    ) {
        unimplemented!()
    }
}
