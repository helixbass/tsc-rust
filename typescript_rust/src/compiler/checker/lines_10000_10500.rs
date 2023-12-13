use std::{borrow::Borrow, collections::HashMap, io, ptr};

use gc::Gc;
use id_arena::Id;

use crate::{
    concatenate, create_symbol_table, every, get_effective_constraint_of_type_parameter,
    get_effective_return_type_node, get_effective_type_annotation_node,
    get_effective_type_parameter_declarations, get_interface_base_type_nodes, has_initializer,
    is_computed_property_name, is_element_access_expression, is_entity_name_expression,
    is_jsdoc_type_alias, is_named_declaration, is_private_identifier_class_element_declaration,
    is_static, is_string_literal_like, is_type_alias, node_is_missing, try_every,
    BaseInterfaceType, Debug_, Diagnostics, EnumKind, GenericableTypeInterface,
    HasTypeArgumentsInterface, InterfaceTypeInterface, InterfaceTypeWithDeclaredMembersInterface,
    InternalSymbolName, Node, NodeFlags, NodeInterface, Number, ObjectFlags,
    ObjectFlagsTypeInterface, OptionTry, Symbol, SymbolFlags, SymbolInterface, SymbolTable,
    SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    TypeReferenceInterface, TypeSystemPropertyName, UnionReduction,
};

impl TypeChecker {
    pub(super) fn are_all_outer_type_parameters_applied(
        &self,
        type_: Id<Type>,
    ) -> io::Result<bool> {
        let type_ref = self.type_(type_);
        let outer_type_parameters = type_ref
            .maybe_as_interface_type()
            .and_then(|type_| type_.maybe_outer_type_parameters());
        if let Some(outer_type_parameters) = outer_type_parameters {
            let last = outer_type_parameters.len() - 1;
            let type_arguments = self.get_type_arguments(type_)?;
            return Ok(!match (
                self.type_(outer_type_parameters[last]).maybe_symbol(),
                self.type_(type_arguments[last]).maybe_symbol(),
            ) {
                (None, None) => true,
                (Some(symbol_a), Some(symbol_b)) => symbol_a == symbol_b,
                _ => false,
            });
        }
        Ok(true)
    }

    pub(super) fn is_valid_base_type(&self, type_: Id<Type>) -> io::Result<bool> {
        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::TypeParameter)
        {
            if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::TypeParameter)
            {
                let constraint = self.get_base_constraint_of_type(type_)?;
                if let Some(constraint) = constraint {
                    return self.is_valid_base_type(constraint);
                }
            }
        }
        Ok(self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Object | TypeFlags::NonPrimitive | TypeFlags::Any)
            && !self.is_generic_mapped_type(type_)?
            || self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Intersection)
                && try_every(
                    self.type_(type_)
                        .as_union_or_intersection_type_interface()
                        .types(),
                    |&type_: &Id<Type>, _| self.is_valid_base_type(type_),
                )?)
    }

    pub(super) fn resolve_base_types_of_interface(
        &self,
        type_: Id<Type>, /*InterfaceType*/
    ) -> io::Result<()> {
        if self
            .type_(type_)
            .as_interface_type()
            .maybe_resolved_base_types()
            .is_none()
        {
            *self
                .type_(type_)
                .as_interface_type()
                .maybe_resolved_base_types() = Some(Gc::new(vec![]));
        }
        if let Some(type_symbol_declarations) = {
            let type_symbol_declarations = self
                .symbol(self.type_(type_).symbol())
                .maybe_declarations()
                .clone();
            type_symbol_declarations
        }
        .as_deref()
        {
            for declaration in type_symbol_declarations {
                if declaration.kind() == SyntaxKind::InterfaceDeclaration
                    && get_interface_base_type_nodes(declaration).is_some()
                {
                    for node in get_interface_base_type_nodes(declaration)
                        .as_deref()
                        .unwrap()
                    {
                        let base_type =
                            self.get_reduced_type(self.get_type_from_type_node_(node)?)?;
                        if !self.is_error_type(base_type) {
                            if self.is_valid_base_type(base_type)? {
                                if type_ != base_type
                                    && !self.has_base_type(base_type, Some(type_))?
                                {
                                    let mut resolved_base_types = Vec::clone(
                                        self.type_(type_)
                                            .as_interface_type()
                                            .maybe_resolved_base_types()
                                            .as_ref()
                                            .unwrap(),
                                    );
                                    resolved_base_types.push(base_type);
                                    *self
                                        .type_(type_)
                                        .as_interface_type()
                                        .maybe_resolved_base_types() =
                                        Some(Gc::new(resolved_base_types));
                                } else {
                                    self.report_circular_base_type(declaration, type_)?;
                                }
                            } else {
                                self.error(
                                    Some(&**node),
                                    &Diagnostics::An_interface_can_only_extend_an_object_type_or_intersection_of_object_types_with_statically_known_members,
                                    None,
                                );
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub(super) fn is_thisless_interface(&self, symbol: Id<Symbol>) -> io::Result<bool> {
        let symbol_declarations = self.symbol(symbol).maybe_declarations();
        if symbol_declarations.is_none() {
            return Ok(true);
        }
        let symbol_declarations = symbol_declarations.as_deref().unwrap();
        for declaration in symbol_declarations {
            if declaration.kind() == SyntaxKind::InterfaceDeclaration {
                if declaration.flags().intersects(NodeFlags::ContainsThis) {
                    return Ok(false);
                }
                let base_type_nodes = get_interface_base_type_nodes(declaration);
                if let Some(base_type_nodes) = base_type_nodes {
                    for node in &*base_type_nodes {
                        let node_as_expression_with_type_arguments =
                            node.as_expression_with_type_arguments();
                        if is_entity_name_expression(
                            &node_as_expression_with_type_arguments.expression,
                        ) {
                            let base_symbol = self.resolve_entity_name(
                                &node_as_expression_with_type_arguments.expression,
                                SymbolFlags::Type,
                                Some(true),
                                None,
                                Option::<&Node>::None,
                            )?;
                            if match base_symbol {
                                None => true,
                                Some(base_symbol) => {
                                    !self.symbol(base_symbol).flags().intersects(SymbolFlags::Interface)
                                        || self
                                            .type_(self.get_declared_type_of_class_or_interface(
                                                base_symbol,
                                            )?)
                                            .as_interface_type()
                                            .maybe_this_type()
                                            .is_some()
                                }
                            } {
                                return Ok(false);
                            }
                        }
                    }
                }
            }
        }
        Ok(true)
    }

    pub(super) fn get_declared_type_of_class_or_interface(
        &self,
        mut symbol: Id<Symbol>,
    ) -> io::Result<Id<Type /*InterfaceType*/>> {
        let mut links = self.get_symbol_links(symbol);
        let original_links = links.clone();
        if (*links).borrow().declared_type.is_none() {
            let kind = if symbol.flags().intersects(SymbolFlags::Class) {
                ObjectFlags::Class
            } else {
                ObjectFlags::Interface
            };
            let merged =
                self.merge_js_symbols(
                    &symbol,
                    symbol.maybe_value_declaration().as_ref().try_and_then(
                        |value_declaration| self.get_assigned_class_symbol(value_declaration),
                    )?,
                )?;
            if let Some(merged) = merged {
                symbol = merged.clone();
                links = self.symbol(merged).as_transient_symbol().symbol_links();
            }

            let temporary_type_to_avoid_infinite_recursion_in_is_thisless_interface =
                self.create_object_type(kind, Some(&*symbol));
            let temporary_type_to_avoid_infinite_recursion_in_is_thisless_interface = self
                .alloc_type(
                    BaseInterfaceType::new(
                        temporary_type_to_avoid_infinite_recursion_in_is_thisless_interface,
                        None,
                        None,
                        None,
                        None,
                    )
                    .into(),
                );
            original_links.borrow_mut().declared_type =
                Some(temporary_type_to_avoid_infinite_recursion_in_is_thisless_interface.clone());
            links.borrow_mut().declared_type =
                Some(temporary_type_to_avoid_infinite_recursion_in_is_thisless_interface.clone());
            let type_ = self.create_object_type(kind, Some(&*symbol));
            let outer_type_parameters =
                self.get_outer_type_parameters_of_class_or_interface(&symbol)?;
            let local_type_parameters =
                self.get_local_type_parameters_of_class_or_interface_or_type_alias(&symbol)?;
            let mut need_to_set_constraint = false;
            let type_ = self.alloc_type(
                if outer_type_parameters.is_some()
                    || local_type_parameters.is_some()
                    || kind == ObjectFlags::Class
                    || !self.is_thisless_interface(&symbol)?
                {
                    need_to_set_constraint = true;
                    type_.set_object_flags(type_.object_flags() | ObjectFlags::Reference);
                    let mut this_type = self.create_type_parameter(Some(symbol.clone()));
                    this_type.is_this_type = Some(true);
                    BaseInterfaceType::new(
                        type_,
                        Some(concatenate(
                            outer_type_parameters.clone().unwrap_or_else(|| vec![]),
                            local_type_parameters.clone().unwrap_or_else(|| vec![]),
                        )),
                        outer_type_parameters,
                        local_type_parameters,
                        Some(self.alloc_type(this_type.into())),
                    )
                } else {
                    BaseInterfaceType::new(type_, None, None, None, None)
                }
                .into(),
            );
            if need_to_set_constraint {
                *self
                    .type_(
                        self.type_(type_)
                            .as_interface_type()
                            .maybe_this_type_mut()
                            .unwrap(),
                    )
                    .as_type_parameter()
                    .constraint
                    .borrow_mut() = Some(type_.clone());
            }
            let mut instantiations: HashMap<String, Id<Type /*TypeReference*/>> = HashMap::new();
            instantiations.insert(
                self.get_type_list_id(
                    self.type_(type_)
                        .as_interface_type()
                        .maybe_type_parameters(),
                ),
                type_.clone(),
            );
            self.type_(type_)
                .as_interface_type()
                .genericize(instantiations);
            self.type_(type_)
                .as_interface_type()
                .set_target(type_.clone());
            *self
                .type_(type_)
                .as_interface_type()
                .maybe_resolved_type_arguments_mut() = self
                .type_(type_)
                .as_interface_type()
                .maybe_type_parameters()
                .map(ToOwned::to_owned);
            original_links.borrow_mut().declared_type = Some(type_.clone());
            links.borrow_mut().declared_type = Some(type_.clone());
        }
        let ret = (*links).borrow().declared_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_declared_type_of_type_alias(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<Id<Type>> {
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().declared_type.is_none() {
            if !self.push_type_resolution(
                &symbol.into(),
                TypeSystemPropertyName::DeclaredType,
            ) {
                return Ok(self.error_type());
            }

            let declaration = Debug_.check_defined(
                self.symbol(symbol)
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|declarations| {
                        declarations
                            .iter()
                            .find(|declaration| is_type_alias(declaration))
                            .map(|rc| rc.clone())
                    }),
                Some("Type alias symbol with no valid declaration found"),
            );
            let type_node = if is_jsdoc_type_alias(&declaration) {
                declaration.as_jsdoc_type_like_tag().maybe_type_expression()
            } else {
                Some(declaration.as_type_alias_declaration().type_.clone())
            };
            let mut type_ = type_node.try_map_or_else(
                || Ok(self.error_type()),
                |type_node| self.get_type_from_type_node_(&type_node),
            )?;

            if self.pop_type_resolution() {
                let type_parameters =
                    self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol)?;
                if let Some(type_parameters) = type_parameters {
                    let mut links = links.borrow_mut();
                    let mut instantiations: HashMap<String, Id<Type>> = HashMap::new();
                    instantiations.insert(
                        self.get_type_list_id(Some(&*type_parameters)),
                        type_.clone(),
                    );
                    links.type_parameters = Some(type_parameters);
                    links.instantiations = Some(instantiations);
                }
            } else {
                type_ = self.error_type();
                if declaration.kind() == SyntaxKind::JSDocEnumTag {
                    self.error(
                        Some(
                            &*declaration
                                .as_jsdoc_type_like_tag()
                                .type_expression()
                                .as_jsdoc_type_expression()
                                .type_,
                        ),
                        &Diagnostics::Type_alias_0_circularly_references_itself,
                        Some(vec![self.symbol_to_string_(
                            symbol,
                            Option::<&Node>::None,
                            None,
                            None,
                            None,
                        )?]),
                    );
                } else {
                    self.error(
                        Some(if is_named_declaration(&declaration) {
                            declaration
                                .as_named_declaration()
                                .maybe_name()
                                .unwrap_or_else(|| declaration.clone())
                        } else {
                            declaration.clone()
                        }),
                        &Diagnostics::Type_alias_0_circularly_references_itself,
                        Some(vec![self.symbol_to_string_(
                            symbol,
                            Option::<&Node>::None,
                            None,
                            None,
                            None,
                        )?]),
                    );
                }
            }
            links.borrow_mut().declared_type = Some(type_);
        }
        let ret = (*links).borrow().declared_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn is_string_concat_expression(&self, expr: &Node) -> bool {
        if is_string_literal_like(expr) {
            return true;
        } else if expr.kind() == SyntaxKind::BinaryExpression {
            let expr_as_binary_expression = expr.as_binary_expression();
            return self.is_string_concat_expression(&expr_as_binary_expression.left)
                && self.is_string_concat_expression(&expr_as_binary_expression.right);
        }
        false
    }

    pub(super) fn is_literal_enum_member(
        &self,
        member: &Node, /*EnumMember*/
    ) -> io::Result<bool> {
        let member_as_enum_member = member.as_enum_member();
        let expr = member_as_enum_member.initializer.as_deref();
        if expr.is_none() {
            return Ok(!member.flags().intersects(NodeFlags::Ambient));
        }
        let expr = expr.unwrap();
        Ok(match expr.kind() {
            SyntaxKind::StringLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::NoSubstitutionTemplateLiteral => true,
            SyntaxKind::PrefixUnaryExpression => {
                let expr_as_prefix_unary_expression = expr.as_prefix_unary_expression();
                expr_as_prefix_unary_expression.operator == SyntaxKind::MinusToken
                    && expr_as_prefix_unary_expression.operand.kind() == SyntaxKind::NumericLiteral
            }
            SyntaxKind::Identifier => {
                node_is_missing(Some(expr))
                    || (*self.symbol(self
                        .get_symbol_of_node(&member.parent())?
                        .unwrap())
                        .exports())
                    .borrow()
                    .get(&expr.as_identifier().escaped_text)
                    .is_some()
            }
            SyntaxKind::BinaryExpression => self.is_string_concat_expression(expr),
            _ => false,
        })
    }

    pub(super) fn get_enum_kind(&self, symbol: Id<Symbol>) -> io::Result<EnumKind> {
        let links = self.get_symbol_links(symbol);
        if let Some(links_enum_kind) = (*links).borrow().enum_kind {
            return Ok(links_enum_kind);
        }
        let mut has_non_literal_member = false;
        if let Some(symbol_declarations) = self.symbol(symbol).maybe_declarations().as_deref() {
            for declaration in symbol_declarations {
                if declaration.kind() == SyntaxKind::EnumDeclaration {
                    for member in &declaration.as_enum_declaration().members {
                        if matches!(member.as_enum_member().initializer.as_ref(), Some(initializer) if is_string_literal_like(initializer))
                        {
                            let ret = EnumKind::Literal;
                            links.borrow_mut().enum_kind = Some(ret);
                            return Ok(ret);
                        }
                        if !self.is_literal_enum_member(member)? {
                            has_non_literal_member = true;
                        }
                    }
                }
            }
        }
        let ret = if has_non_literal_member {
            EnumKind::Numeric
        } else {
            EnumKind::Literal
        };
        links.borrow_mut().enum_kind = Some(ret);
        Ok(ret)
    }

    pub(super) fn get_base_type_of_enum_literal_type(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Id<Type>> {
        Ok(
            if self.type_(type_).flags().intersects(TypeFlags::EnumLiteral)
                && !self.type_(type_).flags().intersects(TypeFlags::Union)
            {
                self.get_declared_type_of_symbol(
                    self
                        .get_parent_of_symbol(self.type_(type_).symbol())?
                        .unwrap(),
                )?
            } else {
                type_
            },
        )
    }

    pub(super) fn get_declared_type_of_enum(&self, symbol: Id<Symbol>) -> io::Result<Id<Type>> {
        let links = self.get_symbol_links(symbol);
        if let Some(links_declared_type) = (*links).borrow().declared_type.clone() {
            return Ok(links_declared_type);
        }
        if self.get_enum_kind(symbol)? == EnumKind::Literal {
            self.increment_enum_count();
            let mut member_type_list: Vec<Id<Type>> = vec![];
            if let Some(symbol_declarations) = self.symbol(symbol).maybe_declarations().as_deref() {
                for declaration in symbol_declarations {
                    if declaration.kind() == SyntaxKind::EnumDeclaration {
                        for member in &declaration.as_enum_declaration().members {
                            let value = self.get_enum_member_value(member)?;
                            let member_type =
                                self.get_fresh_type_of_literal_type(self.get_enum_literal_type(
                                    value.unwrap_or_else(|| Number::new(0.0).into()),
                                    self.enum_count(),
                                    self.get_symbol_of_node(member)?.unwrap(),
                                ));
                            self.get_symbol_links(&self.get_symbol_of_node(member)?.unwrap())
                                .borrow_mut()
                                .declared_type = Some(member_type.clone());
                            member_type_list
                                .push(self.get_regular_type_of_literal_type(member_type));
                        }
                    }
                }
            }
            if !member_type_list.is_empty() {
                let enum_type = self.get_union_type(
                    &member_type_list,
                    Some(UnionReduction::Literal),
                    Some(symbol),
                    None,
                    None,
                )?;
                if self.type_(enum_type).flags().intersects(TypeFlags::Union) {
                    self.type_(enum_type)
                        .set_flags(self.type_(enum_type).flags() | TypeFlags::EnumLiteral);
                    self.type_(enum_type)
                        .set_symbol(Some(symbol));
                }
                links.borrow_mut().declared_type = Some(enum_type.clone());
                return Ok(enum_type);
            }
        }
        let enum_type = self.alloc_type(self.create_type(TypeFlags::Enum).into());
        self.type_(enum_type).set_symbol(Some(symbol));
        links.borrow_mut().declared_type = Some(enum_type.clone());
        Ok(enum_type)
    }

    pub(super) fn get_declared_type_of_enum_member(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<Id<Type>> {
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().declared_type.is_none() {
            let enum_type =
                self.get_declared_type_of_enum(self.get_parent_of_symbol(symbol)?.unwrap())?;
            let mut links = links.borrow_mut();
            if links.declared_type.is_none() {
                links.declared_type = Some(enum_type);
            }
        }
        let ret = (*links).borrow().declared_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_declared_type_of_type_parameter(
        &self,
        symbol: Id<Symbol>,
    ) -> Id<Type /*TypeParameter*/> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.declared_type.is_none() {
            links.declared_type =
                Some(self.alloc_type(self.create_type_parameter(Some(symbol)).into()));
        }
        links.declared_type.clone().unwrap()
    }

    pub(super) fn get_declared_type_of_alias(&self, symbol: Id<Symbol>) -> io::Result<Id<Type>> {
        let links = self.get_symbol_links(symbol);
        if let Some(links_declared_type) = (*links).borrow().declared_type.clone() {
            return Ok(links_declared_type);
        }
        let declared_type = self.get_declared_type_of_symbol(self.resolve_alias(symbol)?)?;
        links.borrow_mut().declared_type = Some(declared_type.clone());
        Ok(declared_type)
    }

    pub(super) fn get_declared_type_of_symbol(&self, symbol: Id<Symbol>) -> io::Result<Id<Type>> {
        Ok(self
            .try_get_declared_type_of_symbol(symbol)?
            .unwrap_or_else(|| self.error_type()))
    }

    pub(super) fn try_get_declared_type_of_symbol(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<Option<Id<Type>>> {
        if self.symbol(symbol)
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return Ok(Some(self.get_declared_type_of_class_or_interface(symbol)?));
        }
        if self
            .symbol(symbol)
            .flags()
            .intersects(SymbolFlags::TypeAlias)
        {
            return Ok(Some(self.get_declared_type_of_type_alias(symbol)?));
        }
        if self
            .symbol(symbol)
            .flags()
            .intersects(SymbolFlags::TypeParameter)
        {
            return Ok(Some(self.get_declared_type_of_type_parameter(symbol)));
        }
        if self.symbol(symbol).flags().intersects(SymbolFlags::Enum) {
            return Ok(Some(self.get_declared_type_of_enum(symbol)?));
        }
        if self
            .symbol(symbol)
            .flags()
            .intersects(SymbolFlags::EnumMember)
        {
            return Ok(Some(self.get_declared_type_of_enum_member(symbol)?));
        }
        if self.symbol(symbol).flags().intersects(SymbolFlags::Alias) {
            return Ok(Some(self.get_declared_type_of_alias(symbol)?));
        }
        Ok(None)
    }

    pub(super) fn is_thisless_type(&self, node: &Node /*TypeNode*/) -> bool {
        match node.kind() {
            SyntaxKind::AnyKeyword
            | SyntaxKind::UnknownKeyword
            | SyntaxKind::StringKeyword
            | SyntaxKind::NumberKeyword
            | SyntaxKind::BigIntKeyword
            | SyntaxKind::BooleanKeyword
            | SyntaxKind::SymbolKeyword
            | SyntaxKind::ObjectKeyword
            | SyntaxKind::VoidKeyword
            | SyntaxKind::UndefinedKeyword
            | SyntaxKind::NeverKeyword
            | SyntaxKind::LiteralType => true,
            SyntaxKind::ArrayType => self.is_thisless_type(&node.as_array_type_node().element_type),
            SyntaxKind::TypeReference => {
                match node
                    .as_type_reference_node()
                    .maybe_type_arguments()
                    .as_deref()
                {
                    None => true,
                    Some(type_arguments) => every(type_arguments, |type_argument: &Gc<Node>, _| {
                        self.is_thisless_type(type_argument)
                    }),
                }
            }
            _ => false,
        }
    }

    pub(super) fn is_thisless_type_parameter(
        &self,
        node: &Node, /*TypeParameterDeclaration*/
    ) -> bool {
        let constraint = get_effective_constraint_of_type_parameter(node);
        match constraint {
            None => true,
            Some(constraint) => self.is_thisless_type(&constraint),
        }
    }

    pub(super) fn is_thisless_variable_like_declaration(
        &self,
        node: &Node, /*VariableLikeDeclaration*/
    ) -> bool {
        let type_node = get_effective_type_annotation_node(node);
        if let Some(type_node) = type_node {
            self.is_thisless_type(&type_node)
        } else {
            !has_initializer(node)
        }
    }

    pub(super) fn is_thisless_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> bool {
        let return_type = get_effective_return_type_node(node);
        let type_parameters = get_effective_type_parameter_declarations(node);
        (node.kind() == SyntaxKind::Constructor
            || matches!(return_type, Some(return_type) if self.is_thisless_type(&return_type)))
            && node
                .as_signature_declaration()
                .parameters()
                .iter()
                .all(|parameter: &Gc<Node>| self.is_thisless_variable_like_declaration(parameter))
            && type_parameters
                .iter()
                .all(|type_parameter: &Gc<Node>| self.is_thisless_type_parameter(type_parameter))
    }

    pub(super) fn is_thisless(&self, symbol: Id<Symbol>) -> bool {
        if let Some(symbol_declarations) = self.symbol(symbol).maybe_declarations().as_deref() {
            if symbol_declarations.len() == 1 {
                let declaration = &symbol_declarations[0];
                // if (declaration) {
                match declaration.kind() {
                    SyntaxKind::PropertyDeclaration | SyntaxKind::PropertySignature => {
                        return self.is_thisless_variable_like_declaration(declaration);
                    }
                    SyntaxKind::MethodDeclaration
                    | SyntaxKind::MethodSignature
                    | SyntaxKind::Constructor
                    | SyntaxKind::GetAccessor
                    | SyntaxKind::SetAccessor => {
                        return self.is_thisless_function_like_declaration(declaration);
                    }
                    _ => (),
                }
                // }
            }
        }
        false
    }

    pub(super) fn create_instantiated_symbol_table(
        &self,
        symbols: impl IntoIterator<Item = impl Borrow<Id<Symbol>>>,
        mapper: Id<TypeMapper>,
        mapping_this_only: bool,
    ) -> io::Result<SymbolTable> {
        let mut result = create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None);
        for symbol in symbols {
            result.insert(
                self.symbol(symbol).escaped_name().to_owned(),
                if mapping_this_only && self.is_thisless(symbol) {
                    symbol.clone()
                } else {
                    self.instantiate_symbol(symbol, mapper.clone())?
                },
            );
        }
        Ok(result)
    }

    pub(super) fn add_inherited_members(
        &self,
        symbols: &mut SymbolTable,
        base_symbols: impl IntoIterator<Item = Id<Symbol>>,
    ) {
        for s in base_symbols {
            if !symbols.contains_key(self.symbol(s).escaped_name())
                && !self.is_static_private_identifier_property(s)
            {
                symbols.insert(self.symbol(s).escaped_name().to_owned(), s.clone());
            }
        }
    }

    pub(super) fn is_static_private_identifier_property(&self, s: Id<Symbol>) -> bool {
        matches!(
            self.symbol(s).maybe_value_declaration().as_ref(),
            Some(value_declaration) if is_private_identifier_class_element_declaration(value_declaration) && is_static(value_declaration)
        )
    }

    pub(super) fn resolve_declared_members(
        &self,
        type_: Id<Type>, /*InterfaceType*/
    ) -> io::Result<Id<Type>> {
        if self
            .type_(type_)
            .as_interface_type()
            .maybe_declared_properties()
            .is_none()
        {
            let symbol = self.type_(type_).symbol();
            let members = self.get_members_of_symbol(symbol)?;
            let members = (*members).borrow();
            self.type_(type_)
                .as_interface_type()
                .set_declared_properties(self.get_named_members(&*members)?);
            self.type_(type_)
                .as_interface_type()
                .set_declared_call_signatures(vec![]);
            self.type_(type_)
                .as_interface_type()
                .set_declared_construct_signatures(vec![]);
            self.type_(type_)
                .as_interface_type()
                .set_declared_index_infos(vec![]);

            let signatures =
                self.get_signatures_of_symbol(members.get(InternalSymbolName::Call).cloned())?;
            self.type_(type_)
                .as_interface_type()
                .set_declared_call_signatures(signatures);
            let signatures =
                self.get_signatures_of_symbol(members.get(InternalSymbolName::New).cloned())?;
            self.type_(type_)
                .as_interface_type()
                .set_declared_construct_signatures(signatures);
            let index_infos = self.get_index_infos_of_symbol(symbol)?;
            self.type_(type_)
                .as_interface_type()
                .set_declared_index_infos(index_infos);
        }
        Ok(type_)
    }

    pub(super) fn is_type_usable_as_property_name(&self, type_: Id<Type>) -> bool {
        self.type_(type_)
            .flags()
            .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
    }

    pub(super) fn is_late_bindable_name(
        &self,
        node: &Node, /*DeclarationName*/
    ) -> io::Result<bool> {
        if !is_computed_property_name(node) && !is_element_access_expression(node) {
            return Ok(false);
        }
        let expr = if is_computed_property_name(node) {
            &node.as_computed_property_name().expression
        } else {
            &node.as_element_access_expression().argument_expression
        };
        Ok(is_entity_name_expression(expr)
            && self.is_type_usable_as_property_name(if is_computed_property_name(node) {
                self.check_computed_property_name(node)?
            } else {
                self.check_expression_cached(expr, None)?
            }))
    }

    pub(super) fn is_late_bound_name(&self, name: &str /*__String*/) -> bool {
        name.starts_with("__@")
    }
}
