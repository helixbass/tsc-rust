#![allow(non_upper_case_globals)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{MappedTypeModifiers, MembersOrExportsResolutionKind};
use crate::{
    concatenate, create_symbol_table, escape_leading_underscores, every,
    get_effective_constraint_of_type_parameter, get_effective_return_type_node,
    get_effective_type_annotation_node, get_effective_type_parameter_declarations,
    get_interface_base_type_nodes, has_dynamic_name, has_initializer, is_computed_property_name,
    is_element_access_expression, is_entity_name_expression, is_jsdoc_type_alias,
    is_named_declaration, is_private_identifier_class_element_declaration, is_static,
    is_string_literal_like, is_type_alias, node_is_missing, range_equals_rc, BaseInterfaceType,
    CharacterCodes, Debug_, Diagnostics, EnumKind, GenericableTypeInterface,
    InterfaceTypeInterface, InterfaceTypeWithDeclaredMembersInterface, InternalSymbolName,
    LiteralType, Node, NodeFlags, NodeInterface, Number, ObjectFlags, ObjectFlagsTypeInterface,
    Signature, SignatureFlags, Symbol, SymbolFlags, SymbolInterface, SymbolTable, SyntaxKind,
    TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    TypePredicate, TypeReferenceInterface, TypeSystemPropertyName, UnderscoreEscapedMap,
    UnionReduction, __String,
};

impl TypeChecker {
    pub(super) fn are_all_outer_type_parameters_applied(&self, type_: &Type) -> bool {
        let outer_type_parameters = type_
            .maybe_as_interface_type()
            .and_then(|type_| type_.maybe_outer_type_parameters());
        if let Some(outer_type_parameters) = outer_type_parameters {
            let last = outer_type_parameters.len() - 1;
            let type_arguments = self.get_type_arguments(type_);
            return !match (
                outer_type_parameters[last].maybe_symbol(),
                type_arguments[last].maybe_symbol(),
            ) {
                (None, None) => true,
                (Some(symbol_a), Some(symbol_b)) => Rc::ptr_eq(&symbol_a, &symbol_b),
                _ => false,
            };
        }
        true
    }

    pub(super) fn is_valid_base_type(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::TypeParameter) {
            if type_.flags().intersects(TypeFlags::TypeParameter) {
                let constraint = self.get_base_constraint_of_type(type_);
                if let Some(constraint) = constraint {
                    return self.is_valid_base_type(&constraint);
                }
            }
        }
        type_
            .flags()
            .intersects(TypeFlags::Object | TypeFlags::NonPrimitive | TypeFlags::Any)
            && !self.is_generic_mapped_type(type_)
            || type_.flags().intersects(TypeFlags::Intersection)
                && every(
                    type_.as_union_or_intersection_type_interface().types(),
                    |type_: &Rc<Type>, _| self.is_valid_base_type(type_),
                )
    }

    pub(super) fn resolve_base_types_of_interface(&self, type_: &Type /*InterfaceType*/) {
        let type_as_interface_type = type_.as_interface_type();
        if type_as_interface_type.maybe_resolved_base_types().is_none() {
            *type_as_interface_type.maybe_resolved_base_types() = Some(Rc::new(vec![]));
        }
        if let Some(type_symbol_declarations) = type_.symbol().maybe_declarations().as_deref() {
            for declaration in type_symbol_declarations {
                if declaration.kind() == SyntaxKind::InterfaceDeclaration
                    && get_interface_base_type_nodes(declaration).is_some()
                {
                    for node in get_interface_base_type_nodes(declaration)
                        .as_deref()
                        .unwrap()
                    {
                        let base_type = self.get_reduced_type(&self.get_type_from_type_node_(node));
                        if !self.is_error_type(&base_type) {
                            if self.is_valid_base_type(&base_type) {
                                if !ptr::eq(type_, &*base_type)
                                    && !self.has_base_type(&base_type, Some(type_))
                                {
                                    let mut resolved_base_types = Vec::clone(
                                        type_as_interface_type
                                            .maybe_resolved_base_types()
                                            .as_ref()
                                            .unwrap(),
                                    );
                                    resolved_base_types.push(base_type);
                                    *type_as_interface_type.maybe_resolved_base_types() =
                                        Some(Rc::new(resolved_base_types));
                                } else {
                                    self.report_circular_base_type(declaration, type_);
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
    }

    pub(super) fn is_thisless_interface(&self, symbol: &Symbol) -> bool {
        let symbol_declarations = symbol.maybe_declarations();
        if symbol_declarations.is_none() {
            return true;
        }
        let symbol_declarations = symbol_declarations.as_deref().unwrap();
        for declaration in symbol_declarations {
            if declaration.kind() == SyntaxKind::InterfaceDeclaration {
                if declaration.flags().intersects(NodeFlags::ContainsThis) {
                    return false;
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
                            );
                            if match base_symbol {
                                None => true,
                                Some(base_symbol) => {
                                    !base_symbol.flags().intersects(SymbolFlags::Interface)
                                        || self
                                            .get_declared_type_of_class_or_interface(&base_symbol)
                                            .as_interface_type()
                                            .maybe_this_type()
                                            .is_some()
                                }
                            } {
                                return false;
                            }
                        }
                    }
                }
            }
        }
        true
    }

    pub(super) fn get_declared_type_of_class_or_interface(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type /*InterfaceType*/> {
        let mut links = self.get_symbol_links(symbol);
        let original_links = links.clone();
        let mut symbol = symbol.symbol_wrapper();
        if (*links).borrow().declared_type.is_none() {
            let kind = if symbol.flags().intersects(SymbolFlags::Class) {
                ObjectFlags::Class
            } else {
                ObjectFlags::Interface
            };
            let merged = self.merge_js_symbols(
                &symbol,
                symbol
                    .maybe_value_declaration()
                    .as_ref()
                    .and_then(|value_declaration| {
                        self.get_assigned_class_symbol(value_declaration)
                    }),
            );
            if let Some(merged) = merged {
                symbol = merged.clone();
                links = merged.as_transient_symbol().symbol_links();
            }

            let type_ = self.create_object_type(kind, Some(&*symbol));
            let outer_type_parameters =
                self.get_outer_type_parameters_of_class_or_interface(&symbol);
            let local_type_parameters =
                self.get_local_type_parameters_of_class_or_interface_or_type_alias(&symbol);
            let mut need_to_set_constraint = false;
            let type_: Rc<Type> = if outer_type_parameters.is_some()
                || local_type_parameters.is_some()
                || kind == ObjectFlags::Class
                || !self.is_thisless_interface(&symbol)
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
                    Some(this_type.into()),
                )
            } else {
                BaseInterfaceType::new(type_, None, None, None, None)
            }
            .into();
            let type_as_interface_type = type_.as_interface_type();
            if need_to_set_constraint {
                *type_as_interface_type
                    .maybe_this_type_mut()
                    .as_ref()
                    .unwrap()
                    .as_type_parameter()
                    .constraint
                    .borrow_mut() = Some(Rc::downgrade(&type_));
            }
            let mut instantiations: HashMap<String, Rc<Type /*TypeReference*/>> = HashMap::new();
            instantiations.insert(
                self.get_type_list_id(type_as_interface_type.maybe_type_parameters()),
                type_.clone(),
            );
            type_as_interface_type.genericize(instantiations);
            type_as_interface_type.set_target(type_.clone());
            *type_as_interface_type.maybe_resolved_type_arguments() = type_as_interface_type
                .maybe_type_parameters()
                .map(ToOwned::to_owned);
            original_links.borrow_mut().declared_type = Some(type_.clone());
            links.borrow_mut().declared_type = Some(type_.clone());
        }
        let ret = (*links).borrow().declared_type.clone().unwrap();
        ret
    }

    pub(super) fn get_declared_type_of_type_alias(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().declared_type.is_none() {
            if !self.push_type_resolution(
                &symbol.symbol_wrapper().into(),
                TypeSystemPropertyName::DeclaredType,
            ) {
                return self.error_type();
            }

            let declaration = Debug_.check_defined(
                symbol
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
            let mut type_ = type_node.map_or_else(
                || self.error_type(),
                |type_node| self.get_type_from_type_node_(&type_node),
            );

            if self.pop_type_resolution() {
                let type_parameters =
                    self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol);
                if let Some(type_parameters) = type_parameters {
                    let mut links = links.borrow_mut();
                    let mut instantiations: HashMap<String, Rc<Type>> = HashMap::new();
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
                        )]),
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
                        )]),
                    );
                }
            }
            links.borrow_mut().declared_type = Some(type_);
        }
        let ret = (*links).borrow().declared_type.clone().unwrap();
        ret
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

    pub(super) fn is_literal_enum_member(&self, member: &Node /*EnumMember*/) -> bool {
        let member_as_enum_member = member.as_enum_member();
        let expr = member_as_enum_member.initializer.as_deref();
        if expr.is_none() {
            return !member.flags().intersects(NodeFlags::Ambient);
        }
        let expr = expr.unwrap();
        match expr.kind() {
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
                    || (*self.get_symbol_of_node(&member.parent()).unwrap().exports())
                        .borrow()
                        .get(&expr.as_identifier().escaped_text)
                        .is_some()
            }
            SyntaxKind::BinaryExpression => self.is_string_concat_expression(expr),
            _ => false,
        }
    }

    pub(super) fn get_enum_kind(&self, symbol: &Symbol) -> EnumKind {
        let links = self.get_symbol_links(symbol);
        if let Some(links_enum_kind) = (*links).borrow().enum_kind {
            return links_enum_kind;
        }
        let mut has_non_literal_member = false;
        if let Some(symbol_declarations) = symbol.maybe_declarations().as_deref() {
            for declaration in symbol_declarations {
                if declaration.kind() == SyntaxKind::EnumDeclaration {
                    for member in &declaration.as_enum_declaration().members {
                        if matches!(member.as_enum_member().initializer.as_ref(), Some(initializer) if is_string_literal_like(initializer))
                        {
                            let ret = EnumKind::Literal;
                            links.borrow_mut().enum_kind = Some(ret);
                            return ret;
                        }
                        if !self.is_literal_enum_member(member) {
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
        ret
    }

    pub(super) fn get_base_type_of_enum_literal_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::EnumLiteral)
            && !type_.flags().intersects(TypeFlags::Union)
        {
            self.get_declared_type_of_symbol(&self.get_parent_of_symbol(&type_.symbol()).unwrap())
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_declared_type_of_enum(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        if let Some(links_declared_type) = (*links).borrow().declared_type.clone() {
            return links_declared_type;
        }
        if self.get_enum_kind(symbol) == EnumKind::Literal {
            self.increment_enum_count();
            let mut member_type_list: Vec<Rc<Type>> = vec![];
            if let Some(symbol_declarations) = symbol.maybe_declarations().as_deref() {
                for declaration in symbol_declarations {
                    if declaration.kind() == SyntaxKind::EnumDeclaration {
                        for member in &declaration.as_enum_declaration().members {
                            let value = self.get_enum_member_value(member);
                            let member_type =
                                self.get_fresh_type_of_literal_type(&self.get_enum_literal_type(
                                    value.unwrap_or_else(|| Number::new(0.0).into()),
                                    self.enum_count(),
                                    &self.get_symbol_of_node(member).unwrap(),
                                ));
                            self.get_symbol_links(&self.get_symbol_of_node(member).unwrap())
                                .borrow_mut()
                                .declared_type = Some(member_type.clone());
                            member_type_list
                                .push(self.get_regular_type_of_literal_type(&member_type));
                        }
                    }
                }
            }
            if !member_type_list.is_empty() {
                let enum_type = self.get_union_type(
                    member_type_list,
                    Some(UnionReduction::Literal),
                    Some(symbol),
                    None,
                    Option::<&Type>::None,
                );
                if enum_type.flags().intersects(TypeFlags::Union) {
                    enum_type.set_flags(enum_type.flags() | TypeFlags::EnumLiteral);
                    enum_type.set_symbol(Some(symbol.symbol_wrapper()));
                }
                links.borrow_mut().declared_type = Some(enum_type.clone());
                return enum_type;
            }
        }
        let enum_type: Rc<Type> = self.create_type(TypeFlags::Enum).into();
        enum_type.set_symbol(Some(symbol.symbol_wrapper()));
        links.borrow_mut().declared_type = Some(enum_type.clone());
        enum_type
    }

    pub(super) fn get_declared_type_of_enum_member(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().declared_type.is_none() {
            let enum_type =
                self.get_declared_type_of_enum(&self.get_parent_of_symbol(symbol).unwrap());
            let mut links = links.borrow_mut();
            if links.declared_type.is_none() {
                links.declared_type = Some(enum_type);
            }
        }
        let ret = (*links).borrow().declared_type.clone().unwrap();
        ret
    }

    pub(super) fn get_declared_type_of_type_parameter(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type /*TypeParameter*/> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.declared_type.is_none() {
            links.declared_type = Some(self.create_type_parameter(Some(symbol)).into());
        }
        links.declared_type.clone().unwrap()
    }

    pub(super) fn get_declared_type_of_alias(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        if let Some(links_declared_type) = (*links).borrow().declared_type.clone() {
            return links_declared_type;
        }
        let declared_type = self.get_declared_type_of_symbol(&self.resolve_alias(symbol));
        links.borrow_mut().declared_type = Some(declared_type.clone());
        declared_type
    }

    pub(super) fn get_declared_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        self.try_get_declared_type_of_symbol(symbol)
            .unwrap_or_else(|| self.error_type())
    }

    pub(super) fn try_get_declared_type_of_symbol(&self, symbol: &Symbol) -> Option<Rc<Type>> {
        if symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return Some(self.get_declared_type_of_class_or_interface(symbol));
        }
        if symbol.flags().intersects(SymbolFlags::TypeAlias) {
            return Some(self.get_declared_type_of_type_alias(symbol));
        }
        if symbol.flags().intersects(SymbolFlags::TypeParameter) {
            return Some(self.get_declared_type_of_type_parameter(symbol));
        }
        if symbol.flags().intersects(SymbolFlags::Enum) {
            return Some(self.get_declared_type_of_enum(symbol));
        }
        if symbol.flags().intersects(SymbolFlags::EnumMember) {
            return Some(self.get_declared_type_of_enum_member(symbol));
        }
        if symbol.flags().intersects(SymbolFlags::Alias) {
            return Some(self.get_declared_type_of_alias(symbol));
        }
        None
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
                match node.as_type_reference_node().type_arguments.as_deref() {
                    None => true,
                    Some(type_arguments) => every(type_arguments, |type_argument: &Rc<Node>, _| {
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
                .as_function_like_declaration()
                .parameters()
                .iter()
                .all(|parameter: &Rc<Node>| self.is_thisless_variable_like_declaration(parameter))
            && type_parameters
                .iter()
                .all(|type_parameter: &Rc<Node>| self.is_thisless_type_parameter(type_parameter))
    }

    pub(super) fn is_thisless(&self, symbol: &Symbol) -> bool {
        if let Some(symbol_declarations) = symbol.maybe_declarations().as_deref() {
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
        symbols: &[Rc<Symbol>],
        mapper: &TypeMapper,
        mapping_this_only: bool,
    ) -> SymbolTable {
        let mut result = create_symbol_table(None);
        for symbol in symbols {
            result.insert(
                symbol.escaped_name().clone(),
                if mapping_this_only && self.is_thisless(symbol) {
                    symbol.clone()
                } else {
                    self.instantiate_symbol(&symbol, mapper)
                },
            );
        }
        result
    }

    pub(super) fn add_inherited_members(
        &self,
        symbols: &mut SymbolTable,
        base_symbols: &[Rc<Symbol>],
    ) {
        for s in base_symbols {
            if !symbols.contains_key(s.escaped_name())
                && !self.is_static_private_identifier_property(s)
            {
                symbols.insert(s.escaped_name().clone(), s.clone());
            }
        }
    }

    pub(super) fn is_static_private_identifier_property(&self, s: &Symbol) -> bool {
        matches!(
            s.maybe_value_declaration().as_ref(),
            Some(value_declaration) if is_private_identifier_class_element_declaration(value_declaration) && is_static(value_declaration)
        )
    }

    pub(super) fn resolve_declared_members(&self, type_: &Type /*InterfaceType*/) -> Rc<Type> {
        let type_as_interface_type = type_.as_interface_type();
        if type_as_interface_type.maybe_declared_properties().is_none() {
            let symbol = type_.symbol();
            let members = self.get_members_of_symbol(&symbol);
            let members = (*members).borrow();
            type_as_interface_type.set_declared_properties(self.get_named_members(&*members));
            type_as_interface_type.set_declared_call_signatures(vec![]);
            type_as_interface_type.set_declared_construct_signatures(vec![]);
            type_as_interface_type.set_declared_index_infos(vec![]);

            type_as_interface_type.set_declared_call_signatures(self.get_signatures_of_symbol(
                members.get(&InternalSymbolName::Call()).map(Clone::clone),
            ));
            type_as_interface_type.set_declared_construct_signatures(
                self.get_signatures_of_symbol(
                    members.get(&InternalSymbolName::New()).map(Clone::clone),
                ),
            );
            type_as_interface_type
                .set_declared_index_infos(self.get_index_infos_of_symbol(&symbol));
        }
        type_.type_wrapper()
    }

    pub(super) fn is_type_usable_as_property_name(&self, type_: &Type) -> bool {
        type_
            .flags()
            .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
    }

    pub(super) fn is_late_bindable_name(&self, node: &Node /*DeclarationName*/) -> bool {
        if !is_computed_property_name(node) && !is_element_access_expression(node) {
            return false;
        }
        let expr = if is_computed_property_name(node) {
            &node.as_computed_property_name().expression
        } else {
            &node.as_element_access_expression().argument_expression
        };
        is_entity_name_expression(expr)
            && self.is_type_usable_as_property_name(&*if is_computed_property_name(node) {
                self.check_computed_property_name(node)
            } else {
                self.check_expression_cached(expr, None)
            })
    }

    pub(super) fn is_late_bound_name(&self, name: &__String) -> bool {
        let name = &**name;
        let mut name_chars = name.chars();
        matches!(name_chars.next(), Some(ch) if ch == CharacterCodes::underscore)
            && matches!(name_chars.next(), Some(ch) if ch == CharacterCodes::underscore)
            && matches!(name_chars.next(), Some(ch) if ch == CharacterCodes::at)
    }

    pub(super) fn has_bindable_name(&self, node: &Node /*Declaration*/) -> bool {
        !has_dynamic_name(node) || unimplemented!()
    }

    pub(super) fn get_property_name_from_type(
        &self,
        type_: &Type, /*StringLiteralType | NumberLiteralType | UniqueESSymbolType*/
    ) -> __String {
        if type_
            .flags()
            .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
        {
            return match type_ {
                Type::LiteralType(LiteralType::NumberLiteralType(number_literal_type)) => {
                    escape_leading_underscores(&number_literal_type.value.to_string())
                }
                Type::LiteralType(LiteralType::StringLiteralType(string_literal_type)) => {
                    escape_leading_underscores(&string_literal_type.value)
                }
                _ => panic!("Expected NumberLiteralType or StringLiteralType"),
            };
        }
        Debug_.fail(None)
    }

    pub(super) fn get_resolved_members_or_exports_of_symbol(
        &self,
        symbol: &Symbol,
        resolution_kind: MembersOrExportsResolutionKind,
    ) -> Rc<RefCell<UnderscoreEscapedMap<Rc<Symbol>>>> {
        unimplemented!()
    }

    pub(super) fn get_members_of_symbol(&self, symbol: &Symbol) -> Rc<RefCell<SymbolTable>> {
        if false {
            unimplemented!()
        } else {
            symbol
                .maybe_members()
                .clone()
                .unwrap_or_else(|| unimplemented!())
        }
    }

    pub(super) fn get_late_bound_symbol(&self, symbol: &Symbol) -> Rc<Symbol> {
        symbol.symbol_wrapper()
    }

    pub(super) fn resolve_object_type_members(
        &self,
        type_: &Type,  /*ObjectType*/
        source: &Type, /*InterfaceTypeWithDeclaredMembers*/
        type_parameters: Vec<Rc<Type /*TypeParameter*/>>,
        type_arguments: Vec<Rc<Type>>,
    ) {
        let mut mapper: Option<TypeMapper> = None;
        let members: Rc<RefCell<SymbolTable>>;
        let call_signatures: Vec<Rc<Signature>>;
        let construct_signatures: Vec<Rc<Signature>>;
        let source_as_interface_type_with_declared_members =
            source.as_interface_type_with_declared_members();
        if range_equals_rc(&type_parameters, &type_arguments, 0, type_parameters.len()) {
            members = if let Some(source_symbol) = source.maybe_symbol() {
                self.get_members_of_symbol(&source_symbol)
            } else {
                unimplemented!()
            };
            call_signatures = source_as_interface_type_with_declared_members
                .declared_call_signatures()
                .clone();
            construct_signatures = source_as_interface_type_with_declared_members
                .declared_construct_signatures()
                .clone();
        } else {
            let type_parameters_len_is_1 = type_parameters.len() == 1;
            mapper = Some(self.create_type_mapper(type_parameters, Some(type_arguments)));
            members = Rc::new(RefCell::new(
                self.create_instantiated_symbol_table(
                    source
                        .as_base_interface_type()
                        .maybe_declared_properties()
                        .as_ref()
                        .unwrap(),
                    mapper.as_ref().unwrap(),
                    type_parameters_len_is_1,
                ),
            ));
            call_signatures = self.instantiate_signatures(
                &*source_as_interface_type_with_declared_members.declared_call_signatures(),
                mapper.as_ref().unwrap(),
            );
            construct_signatures = self.instantiate_signatures(
                &*source_as_interface_type_with_declared_members.declared_construct_signatures(),
                mapper.as_ref().unwrap(),
            );
        }
        self.set_structured_type_members(
            type_.as_object_type(),
            members,
            call_signatures,
            construct_signatures,
            vec![], // TODO: this is wrong
        );
    }

    pub(super) fn resolve_class_or_interface_members(&self, type_: &Type /*InterfaceType*/) {
        self.resolve_object_type_members(
            type_,
            &self.resolve_declared_members(type_),
            vec![],
            vec![],
        );
    }

    pub(super) fn resolve_type_reference_members(&self, type_: &Type /*TypeReference*/) {
        let type_as_type_reference = type_.as_type_reference();
        let source = self.resolve_declared_members(&type_as_type_reference.target);
        let source_as_interface_type = source.as_interface_type();
        let type_parameters = concatenate(
            source_as_interface_type
                .maybe_type_parameters()
                .map(|type_parameters| type_parameters.to_owned())
                .unwrap(),
            vec![source_as_interface_type.maybe_this_type().unwrap()],
        );
        let type_arguments = self.get_type_arguments(type_);
        let padded_type_arguments = if type_arguments.len() == type_parameters.len() {
            type_arguments
        } else {
            concatenate(type_arguments, vec![type_.type_wrapper()])
        };
        self.resolve_object_type_members(type_, &source, type_parameters, padded_type_arguments);
    }

    pub(super) fn create_signature(
        &self,
        declaration: Option<Rc<Node>>,
        type_parameters: Option<Vec<Rc<Type>>>,
        this_parameter: Option<Rc<Symbol>>,
        parameters: Vec<Rc<Symbol>>,
        resolved_return_type: Option<Rc<Type>>,
        resolved_type_predicate: Option<TypePredicate>,
        min_argument_count: usize,
        flags: SignatureFlags,
    ) -> Signature {
        let mut sig = (self.Signature)(flags);
        sig.declaration = declaration;
        sig.type_parameters = type_parameters;
        sig.set_parameters(parameters);
        sig.this_parameter = this_parameter;
        *sig.resolved_return_type.borrow_mut() = resolved_return_type;
        sig.resolved_type_predicate = resolved_type_predicate;
        sig.set_min_argument_count(min_argument_count);
        sig
    }

    pub(super) fn create_union_signature(
        &self,
        signature: Rc<Signature>,
        union_signatures: &[Rc<Signature>],
    ) -> Signature {
        unimplemented!()
    }

    pub(super) fn get_type_of_mapped_symbol(
        &self,
        symbol: &Symbol, /*MappedSymbol*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_parameter_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_constraint_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_name_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_template_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_mapped_type_with_keyof_constraint_declaration(
        &self,
        type_: &Type, /*MappedType*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_modifiers_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_mapped_type_modifiers(
        &self,
        type_: &Type, /*MappedType*/
    ) -> MappedTypeModifiers {
        unimplemented!()
    }

    pub(super) fn is_generic_mapped_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn resolve_structured_type_members(
        &self,
        type_: &Type, /*StructuredType*/
    ) -> Rc<Type /*ResolvedType*/> {
        if !type_.as_resolvable_type().is_resolved() {
            if let Type::ObjectType(object_type) = &*type_
            /*type_.flags().intersects(TypeFlags::Object)*/
            {
                if object_type
                    .object_flags()
                    .intersects(ObjectFlags::Reference)
                {
                    self.resolve_type_reference_members(type_);
                } else if object_type
                    .object_flags()
                    .intersects(ObjectFlags::ClassOrInterface)
                {
                    self.resolve_class_or_interface_members(type_);
                } else {
                    unimplemented!()
                }
            } else {
                unimplemented!()
            }
        }
        type_.type_wrapper()
    }

    pub(super) fn get_properties_of_object_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        if type_.flags().intersects(TypeFlags::Object) {
            return self
                .resolve_structured_type_members(type_)
                .as_resolved_type()
                .properties()
                .iter()
                .map(Clone::clone)
                .collect();
        }
        unimplemented!()
    }

    pub(super) fn get_property_of_object_type(
        &self,
        type_: &Type,
        name: &__String,
    ) -> Option<Rc<Symbol>> {
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_);
            let symbol = (*resolved.as_resolved_type().members())
                .borrow()
                .get(name)
                .map(Clone::clone);
            if let Some(symbol) = symbol {
                if self.symbol_is_value(&symbol) {
                    return Some(symbol);
                }
            }
        }
        None
    }

    pub(super) fn get_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            unimplemented!()
        } else {
            self.get_properties_of_object_type(&type_)
        }
    }

    pub(super) fn for_each_property_of_type<TAction: FnMut(&Symbol, &__String)>(
        &self,
        type_: &Type,
        action: TAction,
    ) {
        unimplemented!()
    }

    pub(super) fn get_constraint_of_type_parameter(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> Option<Rc<Type>> {
        if self.has_non_circular_base_constraint(type_parameter) {
            self.get_constraint_from_type_parameter(type_parameter)
        } else {
            None
        }
    }

    pub(super) fn get_base_constraint_of_type(&self, type_: &Type) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_base_constraint_or_type(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn has_non_circular_base_constraint(
        &self,
        type_: &Type, /*InstantiableType*/
    ) -> bool {
        !Rc::ptr_eq(
            &self.get_resolved_base_constraint(type_),
            &self.circular_constraint_type(),
        )
    }

    pub(super) fn get_resolved_base_constraint(
        &self,
        type_: &Type, /*InstantiableType | UnionOrIntersectionType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_default_from_type_parameter_(
        &self,
        type_: &Type, /*TypeParameter*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }
}
