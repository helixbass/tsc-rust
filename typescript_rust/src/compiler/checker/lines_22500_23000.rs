use std::{borrow::Borrow, collections::HashMap, convert::TryInto, io, ptr};

use gc::Gc;
use id_arena::Id;

use super::{get_next_flow_id, increment_next_flow_id, IterationUse, TypeFacts};
use crate::{
    are_option_gcs_equal, count_where, find, CheckFlags, FlowNodeBase, TransientSymbolInterface,
    UnionOrIntersectionTypeInterface, __String, escape_leading_underscores, find_ancestor,
    get_check_flags, get_node_id, get_object_flags, get_symbol_id, is_access_expression,
    is_assignment_expression, is_binary_expression, is_binding_element, is_identifier,
    is_optional_chain, is_string_or_numeric_literal_like, is_this_in_type_query,
    is_variable_declaration, is_write_only_access, node_is_missing, return_ok_default_if_none,
    try_for_each, try_reduce_left, FindAncestorCallbackReturn, FlowNode, HasInitializerInterface,
    HasTypeInterface, Node, NodeInterface, Number, ObjectFlags, OptionTry, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeId, TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_resolved_symbol(
        &self,
        node: &Node, /*Identifier*/
    ) -> io::Result<Id<Symbol>> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_symbol.is_none() {
            links.borrow_mut().resolved_symbol = Some(if !node_is_missing(Some(node)) {
                self.resolve_name_(
                    Some(node),
                    &node.as_identifier().escaped_text,
                    SymbolFlags::Value | SymbolFlags::ExportValue,
                    Some(self.get_cannot_find_name_diagnostic_for_name(node)),
                    Some(node.node_wrapper()),
                    !is_write_only_access(node),
                    Some(false),
                )?
                .unwrap_or_else(|| self.unknown_symbol())
            } else {
                self.unknown_symbol()
            });
        }
        let ret = (*links).borrow().resolved_symbol.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn is_in_type_query(&self, node: &Node) -> bool {
        find_ancestor(Some(node), |n: &Node| {
            if n.kind() == SyntaxKind::TypeQuery {
                true.into()
            } else if matches!(n.kind(), SyntaxKind::Identifier | SyntaxKind::QualifiedName) {
                false.into()
            } else {
                FindAncestorCallbackReturn::Quit
            }
        })
        .is_some()
    }

    pub(super) fn get_flow_cache_key(
        &self,
        node: &Node,
        declared_type: Id<Type>,
        initial_type: Id<Type>,
        flow_container: Option<impl Borrow<Node>>,
    ) -> io::Result<Option<String>> {
        let flow_container =
            flow_container.map(|flow_container| flow_container.borrow().node_wrapper());
        match node.kind() {
            SyntaxKind::Identifier => {
                if !is_this_in_type_query(node) {
                    let symbol = self.get_resolved_symbol(node)?;
                    return Ok(if symbol != self.unknown_symbol() {
                        Some(format!(
                            "{}|{}|{}|{}",
                            if let Some(flow_container) = flow_container.as_ref() {
                                get_node_id(flow_container).to_string()
                            } else {
                                "-1".to_owned()
                            },
                            self.get_type_id(declared_type),
                            self.get_type_id(initial_type),
                            get_symbol_id(&self.symbol(symbol))
                        ))
                    } else {
                        None
                    });
                }
                return Ok(Some(format!(
                    "0|{}|{}|{}",
                    if let Some(flow_container) = flow_container.as_ref() {
                        get_node_id(flow_container).to_string()
                    } else {
                        "-1".to_owned()
                    },
                    self.get_type_id(declared_type),
                    self.get_type_id(initial_type),
                )));
            }
            SyntaxKind::ThisKeyword => {
                return Ok(Some(format!(
                    "0|{}|{}|{}",
                    if let Some(flow_container) = flow_container.as_ref() {
                        get_node_id(flow_container).to_string()
                    } else {
                        "-1".to_owned()
                    },
                    self.get_type_id(declared_type),
                    self.get_type_id(initial_type),
                )));
            }
            SyntaxKind::NonNullExpression | SyntaxKind::ParenthesizedExpression => {
                return self.get_flow_cache_key(
                    &node.as_has_expression().expression(),
                    declared_type,
                    initial_type,
                    flow_container,
                );
            }
            SyntaxKind::QualifiedName => {
                let node_as_qualified_name = node.as_qualified_name();
                let left = self.get_flow_cache_key(
                    &node_as_qualified_name.left,
                    declared_type,
                    initial_type,
                    flow_container.as_deref(),
                )?;
                return Ok(left.map(|left| {
                    format!(
                        "{}.{}",
                        left,
                        &*node_as_qualified_name.right.as_identifier().escaped_text,
                    )
                }));
            }
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                let prop_name = self.get_accessed_property_name(node)?;
                if let Some(prop_name) = prop_name.as_ref() {
                    let key = self.get_flow_cache_key(
                        &node.as_has_expression().expression(),
                        declared_type,
                        initial_type,
                        flow_container.as_deref(),
                    )?;
                    return Ok(key.map(|key| format!("{}.{}", key, &**prop_name)));
                }
            }
            _ => (),
        }
        Ok(None)
    }

    pub(super) fn is_matching_reference(&self, source: &Node, target: &Node) -> io::Result<bool> {
        match target.kind() {
            SyntaxKind::ParenthesizedExpression | SyntaxKind::NonNullExpression => {
                return self
                    .is_matching_reference(source, &target.as_has_expression().expression());
            }
            SyntaxKind::BinaryExpression => {
                let target_as_binary_expression = target.as_binary_expression();
                return Ok(is_assignment_expression(target, None)
                    && self.is_matching_reference(source, &target_as_binary_expression.left)?
                    || is_binary_expression(target)
                        && target_as_binary_expression.operator_token.kind()
                            == SyntaxKind::CommaToken
                        && self
                            .is_matching_reference(source, &target_as_binary_expression.right)?);
            }
            _ => (),
        }
        match source.kind() {
            SyntaxKind::MetaProperty => {
                return Ok(target.kind() == SyntaxKind::MetaProperty && {
                    let source_as_meta_property = source.as_meta_property();
                    let target_as_meta_property = target.as_meta_property();
                    source_as_meta_property.keyword_token == target_as_meta_property.keyword_token
                        && source_as_meta_property.name.as_identifier().escaped_text
                            == target_as_meta_property.name.as_identifier().escaped_text
                });
            }
            SyntaxKind::Identifier | SyntaxKind::PrivateIdentifier => {
                return Ok(if is_this_in_type_query(source) {
                    target.kind() == SyntaxKind::ThisKeyword
                } else {
                    target.kind() == SyntaxKind::Identifier
                        && self.get_resolved_symbol(source)? == self.get_resolved_symbol(target)?
                        || matches!(
                            target.kind(),
                            SyntaxKind::VariableDeclaration | SyntaxKind::BindingElement
                        ) && self.get_export_symbol_of_value_symbol_if_exported(Some(
                            self.get_resolved_symbol(source)?,
                        )) == self.get_symbol_of_node(target)?
                });
            }
            SyntaxKind::ThisKeyword => {
                return Ok(target.kind() == SyntaxKind::ThisKeyword);
            }
            SyntaxKind::SuperKeyword => {
                return Ok(target.kind() == SyntaxKind::SuperKeyword);
            }
            SyntaxKind::NonNullExpression | SyntaxKind::ParenthesizedExpression => {
                return self
                    .is_matching_reference(&source.as_has_expression().expression(), target);
            }
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                return Ok(is_access_expression(target)
                    && self.get_accessed_property_name(source)?
                        == self.get_accessed_property_name(target)?
                    && self.is_matching_reference(
                        &source.as_has_expression().expression(),
                        &target.as_has_expression().expression(),
                    )?);
            }
            SyntaxKind::QualifiedName => {
                let source_as_qualified_name = source.as_qualified_name();
                return Ok(is_access_expression(target)
                    && matches!(
                        self.get_accessed_property_name(target)?.as_ref(),
                        Some(accessed_property_name) if &source_as_qualified_name.right.as_identifier().escaped_text == accessed_property_name
                    )
                    && self.is_matching_reference(
                        &source_as_qualified_name.left,
                        &target.as_has_expression().expression(),
                    )?);
            }
            SyntaxKind::BinaryExpression => {
                return Ok(is_binary_expression(source) && {
                    let source_as_binary_expression = source.as_binary_expression();
                    source_as_binary_expression.operator_token.kind() == SyntaxKind::CommaToken
                        && self.is_matching_reference(&source_as_binary_expression.right, target)?
                });
            }
            _ => (),
        }
        Ok(false)
    }

    pub(super) fn get_property_access(
        &self,
        expr: &Node, /*Expression*/
    ) -> io::Result<Option<Gc<Node>>> {
        if is_access_expression(expr) {
            return Ok(Some(expr.node_wrapper()));
        }
        if is_identifier(expr) {
            let symbol = self.get_resolved_symbol(expr)?;
            if self.is_const_variable(symbol) {
                let declaration = self.symbol(symbol).maybe_value_declaration().unwrap();
                if is_variable_declaration(&declaration) {
                    let declaration_as_variable_declaration = declaration.as_variable_declaration();
                    if declaration_as_variable_declaration.maybe_type().is_none() {
                        if let Some(declaration_initializer) = declaration_as_variable_declaration
                            .maybe_initializer()
                            .filter(|declaration_initializer| {
                                is_access_expression(declaration_initializer)
                            })
                        {
                            return Ok(Some(declaration_initializer));
                        }
                    }
                }
                if is_binding_element(&declaration)
                    && declaration
                        .as_has_initializer()
                        .maybe_initializer()
                        .is_none()
                {
                    let parent = declaration.parent().parent();
                    if is_variable_declaration(&parent) {
                        let parent_as_variable_declaration = parent.as_variable_declaration();
                        if parent_as_variable_declaration.maybe_type().is_none()
                            && matches!(
                                parent_as_variable_declaration.maybe_initializer().as_ref(),
                                Some(parent_initializer) if is_identifier(parent_initializer) || is_access_expression(parent_initializer)
                            )
                        {
                            return Ok(Some(declaration));
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    pub(super) fn get_accessed_property_name(
        &self,
        access: &Node, /*AccessExpression | BindingElement */
    ) -> io::Result<Option<__String>> {
        let mut property_name: Option<String> = None;
        Ok(if access.kind() == SyntaxKind::PropertyAccessExpression {
            Some(
                access
                    .as_property_access_expression()
                    .name
                    .as_member_name()
                    .escaped_text()
                    .to_owned(),
            )
        } else if access.kind() == SyntaxKind::ElementAccessExpression
            && is_string_or_numeric_literal_like(
                &access.as_element_access_expression().argument_expression,
            )
        {
            Some(
                escape_leading_underscores(
                    &access
                        .as_element_access_expression()
                        .argument_expression
                        .as_literal_like_node()
                        .text(),
                )
                .into_owned(),
            )
        } else if access.kind() == SyntaxKind::BindingElement && {
            property_name = self.get_destructuring_property_name(access)?;
            property_name.is_some()
        } {
            Some(escape_leading_underscores(property_name.as_ref().unwrap()).into_owned())
        } else {
            None
        })
    }

    pub(super) fn contains_matching_reference(
        &self,
        source: &Node,
        target: &Node,
    ) -> io::Result<bool> {
        let mut source = source.node_wrapper();
        while is_access_expression(&source) {
            source = source.as_has_expression().expression();
            if self.is_matching_reference(&source, target)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn optional_chain_contains_reference(
        &self,
        source: &Node,
        target: &Node,
    ) -> io::Result<bool> {
        let mut source = source.node_wrapper();
        while is_optional_chain(&source) {
            source = source.as_has_expression().expression();
            if self.is_matching_reference(&source, target)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn is_discriminant_property(
        &self,
        type_: Option<Id<Type>>,
        name: &str, /*__String*/
    ) -> io::Result<bool> {
        if let Some(type_) = type_ {
            if self.type_(type_).flags().intersects(TypeFlags::Union) {
                let prop = self.get_union_or_intersection_property(type_, name, None)?;
                if let Some(prop) = prop.filter(|&prop| {
                    get_check_flags(&self.symbol(prop)).intersects(CheckFlags::SyntheticProperty)
                }) {
                    let prop_as_transient_symbol = self.symbol(prop).as_transient_symbol();
                    let prop_symbol_links = prop_as_transient_symbol.symbol_links();
                    if (*prop_symbol_links)
                        .borrow()
                        .is_discriminant_property
                        .is_none()
                    {
                        prop_symbol_links.borrow_mut().is_discriminant_property = Some(
                            prop_as_transient_symbol.check_flags() & CheckFlags::Discriminant
                                == CheckFlags::Discriminant
                                && !self.is_generic_type(self.get_type_of_symbol(prop)?)?,
                        );
                    }
                    return Ok((*prop_symbol_links)
                        .borrow()
                        .is_discriminant_property
                        .unwrap());
                }
            }
        }
        Ok(false)
    }

    pub(super) fn find_discriminant_properties(
        &self,
        source_properties: impl IntoIterator<Item = impl Borrow<Id<Symbol>>>,
        target: Id<Type>,
    ) -> io::Result<Option<Vec<Id<Symbol>>>> {
        let mut result: Option<Vec<Id<Symbol>>> = None;
        for source_property in source_properties {
            if self.is_discriminant_property(
                Some(target),
                self.symbol(source_property).escaped_name(),
            )? {
                if result.is_some() {
                    result.as_mut().unwrap().push(source_property.clone());
                    continue;
                }
                result = Some(vec![source_property.clone()]);
            }
        }
        Ok(result)
    }

    pub(super) fn map_types_by_key_property(
        &self,
        types: &[Id<Type>],
        name: &str, /*__String*/
    ) -> io::Result<Option<HashMap<TypeId, Id<Type>>>> {
        let mut map: HashMap<TypeId, Id<Type>> = HashMap::new();
        let mut count = 0;
        for &type_ in types {
            if self.type_(type_).flags().intersects(
                TypeFlags::Object | TypeFlags::Intersection | TypeFlags::InstantiableNonPrimitive,
            ) {
                let discriminant = self.get_type_of_property_of_type_(type_, name)?;
                if let Some(discriminant) = discriminant {
                    if !self.is_literal_type(discriminant) {
                        return Ok(None);
                    }
                    let mut duplicate = false;
                    self.for_each_type(discriminant, |t: Id<Type>| -> Option<()> {
                        let id = self.get_type_id(self.get_regular_type_of_literal_type(t));
                        let existing = map.get(&id).copied();
                        match existing {
                            None => {
                                map.insert(id, type_.clone());
                            }
                            Some(existing) => {
                                if existing != self.unknown_type() {
                                    map.insert(id, self.unknown_type());
                                    duplicate = true;
                                }
                            }
                        }
                        None
                    });
                    if !duplicate {
                        count += 1;
                    }
                }
            }
        }
        Ok(if count >= 10 && count * 2 >= types.len() {
            Some(map)
        } else {
            None
        })
    }

    pub(super) fn get_key_property_name(
        &self,
        union_type: Id<Type>, /*UnionType*/
    ) -> io::Result<Option<__String>> {
        let union_type_ref = self.type_(union_type);
        let types = union_type_ref.as_union_type().types();
        if types.len() < 10
            || get_object_flags(&self.type_(union_type)).intersects(ObjectFlags::PrimitiveUnion)
            || count_where(Some(types), |&t: &Id<Type>, _| {
                self.type_(t)
                    .flags()
                    .intersects(TypeFlags::Object | TypeFlags::InstantiableNonPrimitive)
            }) < 10
        {
            return Ok(None);
        }
        if self
            .type_(union_type)
            .as_union_type()
            .maybe_key_property_name()
            .is_none()
        {
            let key_property_name = try_for_each(types, |&t: &Id<Type>, _| -> io::Result<_> {
                Ok(
                    if self
                        .type_(t)
                        .flags()
                        .intersects(TypeFlags::Object | TypeFlags::InstantiableNonPrimitive)
                    {
                        try_for_each(
                            self.get_properties_of_type(t)?,
                            |p: Id<Symbol>, _| -> io::Result<_> {
                                Ok(if self.is_unit_type(self.get_type_of_symbol(p)?) {
                                    Some(self.symbol(p).escaped_name().to_owned())
                                } else {
                                    None
                                })
                            },
                        )?
                    } else {
                        None
                    },
                )
            })?;
            let map_by_key_property =
                key_property_name
                    .as_ref()
                    .try_and_then(|key_property_name| {
                        self.map_types_by_key_property(types, key_property_name)
                    })?;
            *self
                .type_(union_type)
                .as_union_type()
                .maybe_key_property_name() = if map_by_key_property.is_some() {
                key_property_name
            } else {
                Some("".to_owned())
            };
            *self
                .type_(union_type)
                .as_union_type()
                .maybe_constituent_map() = map_by_key_property;
        }
        let union_type_key_property_name = self
            .type_(union_type)
            .as_union_type()
            .maybe_key_property_name()
            .clone()
            .unwrap();
        Ok(if !union_type_key_property_name.is_empty() {
            Some(union_type_key_property_name)
        } else {
            None
        })
    }

    pub(super) fn get_constituent_type_for_key_type(
        &self,
        union_type: Id<Type>, /*UnionType*/
        key_type: Id<Type>,
    ) -> Option<Id<Type>> {
        let result = self
            .type_(union_type)
            .as_union_type()
            .maybe_constituent_map()
            .as_ref()
            .and_then(|union_type_constituent_map| {
                union_type_constituent_map
                    .get(&self.get_type_id(self.get_regular_type_of_literal_type(key_type)))
                    .map(Clone::clone)
            });
        result.filter(|&result| result != self.unknown_type())
    }

    pub(super) fn get_matching_union_constituent_for_type(
        &self,
        union_type: Id<Type>, /*UnionType*/
        type_: Id<Type>,
    ) -> io::Result<Option<Id<Type>>> {
        let key_property_name = self.get_key_property_name(union_type)?;
        let prop_type = key_property_name
            .as_ref()
            .try_and_then(|key_property_name| {
                self.get_type_of_property_of_type(type_, key_property_name)
            })?;
        Ok(prop_type
            .and_then(|prop_type| self.get_constituent_type_for_key_type(union_type, prop_type)))
    }

    pub(super) fn get_matching_union_constituent_for_object_literal(
        &self,
        union_type: Id<Type>, /*UnionType*/
        node: &Node,          /*ObjectLiteralExpression*/
    ) -> io::Result<Option<Id<Type>>> {
        let key_property_name = self.get_key_property_name(union_type)?;
        let prop_node = key_property_name.as_ref().and_then(|key_property_name| {
            find(
                &node.as_object_literal_expression().properties,
                |p: &Gc<Node>, _| {
                    let p_symbol = p.maybe_symbol();
                    if p_symbol.is_none() {
                        return false;
                    }
                    let p_symbol = p_symbol.unwrap();
                    if p.kind() != SyntaxKind::PropertyAssignment {
                        return false;
                    }
                    self.symbol(p_symbol).escaped_name() == key_property_name
                        && self.is_possibly_discriminant_value(
                            &p.as_has_initializer().maybe_initializer().unwrap(),
                        )
                },
            )
            .cloned()
        });
        let prop_type = prop_node.try_map(|prop_node| {
            self.get_context_free_type_of_expression(
                &prop_node.as_has_initializer().maybe_initializer().unwrap(),
            )
        })?;
        Ok(prop_type
            .and_then(|prop_type| self.get_constituent_type_for_key_type(union_type, prop_type)))
    }

    pub(super) fn is_or_contains_matching_reference(
        &self,
        source: &Node,
        target: &Node,
    ) -> io::Result<bool> {
        Ok(self.is_matching_reference(source, target)?
            || self.contains_matching_reference(source, target)?)
    }

    pub(super) fn has_matching_argument(
        &self,
        expression: &Node, /*CallExpression | NewExpression*/
        reference: &Node,
    ) -> io::Result<bool> {
        if let Some(expression_arguments) = expression.as_has_arguments().maybe_arguments() {
            for argument in &expression_arguments {
                if self.is_or_contains_matching_reference(reference, argument)? {
                    return Ok(true);
                }
            }
        }
        if {
            let expression_expression = expression.as_has_expression().expression();
            expression_expression.kind() == SyntaxKind::PropertyAccessExpression
                && self.is_or_contains_matching_reference(
                    reference,
                    &expression_expression.as_has_expression().expression(),
                )?
        } {
            return Ok(true);
        }
        Ok(false)
    }

    pub(super) fn get_flow_node_id(&self, flow: &FlowNode) -> usize {
        if match flow.maybe_id() {
            None => true,
            Some(flow_id) => flow_id < 0,
        } {
            flow.set_id(Some(get_next_flow_id().try_into().unwrap()));
            increment_next_flow_id();
        }
        return flow.maybe_id().unwrap().try_into().unwrap();
    }

    pub(super) fn type_maybe_assignable_to(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<bool> {
        if !self.type_(source).flags().intersects(TypeFlags::Union) {
            return self.is_type_assignable_to(source, target);
        }
        for &t in self
            .type_(source)
            .as_union_or_intersection_type_interface()
            .types()
        {
            if self.is_type_assignable_to(t, target)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn get_assignment_reduced_type(
        &self,
        declared_type: Id<Type>, /*UnionType*/
        assigned_type: Id<Type>,
    ) -> io::Result<Id<Type>> {
        if declared_type != assigned_type {
            if self
                .type_(assigned_type)
                .flags()
                .intersects(TypeFlags::Never)
            {
                return Ok(assigned_type);
            }
            let mut reduced_type = self.try_filter_type(declared_type, |t: Id<Type>| {
                self.type_maybe_assignable_to(assigned_type, t)
            })?;
            if self
                .type_(assigned_type)
                .flags()
                .intersects(TypeFlags::BooleanLiteral)
                && self.is_fresh_literal_type(assigned_type)
            {
                reduced_type = self
                    .map_type(
                        reduced_type,
                        &mut |type_: Id<Type>| Some(self.get_fresh_type_of_literal_type(type_)),
                        None,
                    )
                    .unwrap();
            }
            if self.is_type_assignable_to(assigned_type, reduced_type)? {
                return Ok(reduced_type);
            }
        }
        Ok(declared_type)
    }

    pub(super) fn is_function_object_type(
        &self,
        type_: Id<Type>, /*ObjectType*/
    ) -> io::Result<bool> {
        let resolved = self.resolve_structured_type_members(type_)?;
        let ret = !self
            .type_(resolved)
            .as_resolved_type()
            .call_signatures()
            .is_empty()
            || !self
                .type_(resolved)
                .as_resolved_type()
                .construct_signatures()
                .is_empty()
            || (*self.type_(resolved).as_resolved_type().members())
                .borrow()
                .contains_key("bind")
                && self.is_type_subtype_of(type_, self.global_function_type())?;
        Ok(ret)
    }

    pub(super) fn get_type_facts(
        &self,
        type_: Id<Type>,
        ignore_objects: Option<bool>,
    ) -> io::Result<TypeFacts> {
        let mut ignore_objects = ignore_objects.unwrap_or(false);
        let flags = self.type_(type_).flags();
        if flags.intersects(TypeFlags::String) {
            return Ok(if self.strict_null_checks {
                TypeFacts::StringStrictFacts
            } else {
                TypeFacts::StringFacts
            });
        }
        if flags.intersects(TypeFlags::StringLiteral) {
            let is_empty = self.type_(type_).as_string_literal_type().value == "";
            return Ok(if self.strict_null_checks {
                if is_empty {
                    TypeFacts::EmptyStringStrictFacts
                } else {
                    TypeFacts::NonEmptyStringStrictFacts
                }
            } else {
                if is_empty {
                    TypeFacts::EmptyStringFacts
                } else {
                    TypeFacts::NonEmptyStringFacts
                }
            });
        }
        if flags.intersects(TypeFlags::Number | TypeFlags::Enum) {
            return Ok(if self.strict_null_checks {
                TypeFacts::NumberStrictFacts
            } else {
                TypeFacts::NumberFacts
            });
        }
        if flags.intersects(TypeFlags::NumberLiteral) {
            let is_zero = self.type_(type_).as_number_literal_type().value == Number::new(0.0);
            return Ok(if self.strict_null_checks {
                if is_zero {
                    TypeFacts::ZeroNumberStrictFacts
                } else {
                    TypeFacts::NonZeroNumberStrictFacts
                }
            } else {
                if is_zero {
                    TypeFacts::ZeroNumberFacts
                } else {
                    TypeFacts::NonZeroNumberFacts
                }
            });
        }
        if flags.intersects(TypeFlags::BigInt) {
            return Ok(if self.strict_null_checks {
                TypeFacts::BigIntStrictFacts
            } else {
                TypeFacts::BigIntFacts
            });
        }
        if flags.intersects(TypeFlags::BigIntLiteral) {
            let is_zero = self.is_zero_big_int(type_);
            return Ok(if self.strict_null_checks {
                if is_zero {
                    TypeFacts::ZeroBigIntStrictFacts
                } else {
                    TypeFacts::NonZeroBigIntStrictFacts
                }
            } else {
                if is_zero {
                    TypeFacts::ZeroBigIntFacts
                } else {
                    TypeFacts::NonZeroBigIntFacts
                }
            });
        }
        if flags.intersects(TypeFlags::Boolean) {
            return Ok(if self.strict_null_checks {
                TypeFacts::BooleanStrictFacts
            } else {
                TypeFacts::BooleanFacts
            });
        }
        if flags.intersects(TypeFlags::BooleanLike) {
            return Ok(if self.strict_null_checks {
                if type_ == self.false_type() || type_ == self.regular_false_type() {
                    TypeFacts::FalseStrictFacts
                } else {
                    TypeFacts::TrueStrictFacts
                }
            } else {
                if type_ == self.false_type() || type_ == self.regular_false_type() {
                    TypeFacts::FalseFacts
                } else {
                    TypeFacts::TrueFacts
                }
            });
        }
        if flags.intersects(TypeFlags::Object) && !ignore_objects {
            return Ok(
                if get_object_flags(&self.type_(type_)).intersects(ObjectFlags::Anonymous)
                    && self.is_empty_object_type(type_)?
                {
                    if self.strict_null_checks {
                        TypeFacts::EmptyObjectStrictFacts
                    } else {
                        TypeFacts::EmptyObjectFacts
                    }
                } else if self.is_function_object_type(type_)? {
                    if self.strict_null_checks {
                        TypeFacts::FunctionStrictFacts
                    } else {
                        TypeFacts::FunctionFacts
                    }
                } else {
                    if self.strict_null_checks {
                        TypeFacts::ObjectStrictFacts
                    } else {
                        TypeFacts::ObjectFacts
                    }
                },
            );
        }
        if flags.intersects(TypeFlags::Void | TypeFlags::Undefined) {
            return Ok(TypeFacts::UndefinedFacts);
        }
        if flags.intersects(TypeFlags::Null) {
            return Ok(TypeFacts::NullFacts);
        }
        if flags.intersects(TypeFlags::ESSymbolLike) {
            return Ok(if self.strict_null_checks {
                TypeFacts::SymbolStrictFacts
            } else {
                TypeFacts::SymbolFacts
            });
        }
        if flags.intersects(TypeFlags::NonPrimitive) {
            return Ok(if self.strict_null_checks {
                TypeFacts::ObjectStrictFacts
            } else {
                TypeFacts::ObjectFacts
            });
        }
        if flags.intersects(TypeFlags::Never) {
            return Ok(TypeFacts::None);
        }
        if flags.intersects(TypeFlags::Instantiable) {
            return Ok(if !self.is_pattern_literal_type(type_) {
                self.get_type_facts(
                    self.get_base_constraint_of_type(type_)?
                        .unwrap_or_else(|| self.unknown_type()),
                    Some(ignore_objects),
                )?
            } else if self.strict_null_checks {
                TypeFacts::NonEmptyStringStrictFacts
            } else {
                TypeFacts::NonEmptyStringFacts
            });
        }
        if flags.intersects(TypeFlags::Union) {
            return try_reduce_left(
                self.type_(type_)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |facts, &t: &Id<Type>, _| -> io::Result<_> {
                    Ok(facts | self.get_type_facts(t, Some(ignore_objects))?)
                },
                TypeFacts::None,
                None,
                None,
            );
        }
        if flags.intersects(TypeFlags::Intersection) {
            ignore_objects = ignore_objects || self.maybe_type_of_kind(type_, TypeFlags::Primitive);
            return try_reduce_left(
                self.type_(type_)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |facts, &t: &Id<Type>, _| -> io::Result<_> {
                    Ok(facts & self.get_type_facts(t, Some(ignore_objects))?)
                },
                TypeFacts::All,
                None,
                None,
            );
        }
        Ok(TypeFacts::All)
    }

    pub(super) fn get_type_with_facts(
        &self,
        type_: Id<Type>,
        include: TypeFacts,
    ) -> io::Result<Id<Type>> {
        self.try_filter_type(type_, |t: Id<Type>| {
            Ok(self.get_type_facts(t, None)? & include != TypeFacts::None)
        })
    }

    pub(super) fn get_type_with_default(
        &self,
        type_: Id<Type>,
        default_expression: &Node, /*Expression*/
    ) -> io::Result<Id<Type>> {
        /*defaultExpression ? */
        self.get_union_type(
            &[
                self.get_non_undefined_type(type_)?,
                self.get_type_of_expression(default_expression)?,
            ],
            None,
            Option::<Id<Symbol>>::None,
            None,
            None,
        )
        /*: type*/
    }

    pub(super) fn get_type_of_destructured_property(
        &self,
        type_: Id<Type>,
        name: &Node, /*PropertyName*/
    ) -> io::Result<Id<Type>> {
        let name_type = self.get_literal_type_from_property_name(name)?;
        if !self.is_type_usable_as_property_name(name_type) {
            return Ok(self.error_type());
        }
        let text = self.get_property_name_from_type(name_type);
        Ok(self
            .get_type_of_property_of_type_(type_, &text)?
            .try_or_else(|| {
                self.include_undefined_in_index_signature(
                    self.get_applicable_index_info_for_name(type_, &text)?
                        .map(|applicable_index_info| applicable_index_info.type_.clone()),
                )
            })?
            .unwrap_or_else(|| self.error_type()))
    }

    pub(super) fn get_type_of_destructured_array_element(
        &self,
        type_: Id<Type>,
        index: usize,
    ) -> io::Result<Id<Type>> {
        Ok(
            if self.try_every_type(type_, |type_: Id<Type>| self.is_tuple_like_type(type_))? {
                self.get_tuple_element_type(type_, index)?
            } else {
                None
            }
            .try_or_else(|| {
                self.include_undefined_in_index_signature(Some(
                    self.check_iterated_type_or_element_type(
                        IterationUse::Destructuring,
                        type_,
                        self.undefined_type(),
                        Option::<&Node>::None,
                    )?,
                ))
            })?
            .unwrap_or_else(|| self.error_type()),
        )
    }

    pub(super) fn include_undefined_in_index_signature(
        &self,
        type_: Option<Id<Type>>,
    ) -> io::Result<Option<Id<Type>>> {
        let type_ = return_ok_default_if_none!(type_);
        Ok(Some(
            if self.compiler_options.no_unchecked_indexed_access == Some(true) {
                self.get_union_type(
                    &[type_, self.undefined_type()],
                    None,
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?
            } else {
                type_
            },
        ))
    }

    pub(super) fn get_type_of_destructured_spread_expression(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Id<Type>> {
        Ok(self.create_array_type(
            self.check_iterated_type_or_element_type(
                IterationUse::Destructuring,
                type_,
                self.undefined_type(),
                Option::<&Node>::None,
            )?, /* || errorType */
            None,
        ))
    }

    pub(super) fn get_assigned_type_of_binary_expression(
        &self,
        node: &Node, /*BinaryExpression*/
    ) -> io::Result<Id<Type>> {
        let is_destructuring_default_assignment = node.parent().kind()
            == SyntaxKind::ArrayLiteralExpression
            && self.is_destructuring_assignment_target(&node.parent())
            || node.parent().kind() == SyntaxKind::PropertyAssignment
                && self.is_destructuring_assignment_target(&node.parent().parent());
        Ok(if is_destructuring_default_assignment {
            self.get_type_with_default(
                self.get_assigned_type(node)?,
                &node.as_binary_expression().right,
            )?
        } else {
            self.get_type_of_expression(&node.as_binary_expression().right)?
        })
    }

    pub(super) fn is_destructuring_assignment_target(&self, parent: &Node) -> bool {
        parent.parent().kind() == SyntaxKind::BinaryExpression
            && ptr::eq(&*parent.parent().as_binary_expression().left, parent)
            || parent.parent().kind() == SyntaxKind::ForOfStatement
                && ptr::eq(&*parent.parent().as_for_of_statement().initializer, parent)
    }

    pub(super) fn get_assigned_type_of_array_literal_element(
        &self,
        node: &Node,    /*ArrayLiteralExpression*/
        element: &Node, /*Expression*/
    ) -> io::Result<Id<Type>> {
        self.get_type_of_destructured_array_element(
            self.get_assigned_type(node)?,
            node.as_array_literal_expression()
                .elements
                .iter()
                .position(|el| ptr::eq(&**el, element))
                .unwrap(),
        )
    }

    pub(super) fn get_assigned_type_of_spread_expression(
        &self,
        node: &Node, /*SpreadElement*/
    ) -> io::Result<Id<Type>> {
        self.get_type_of_destructured_spread_expression(self.get_assigned_type(&node.parent())?)
    }

    pub(super) fn get_assigned_type_of_property_assignment(
        &self,
        node: &Node, /*PropertyAssignment | ShorthandPropertyAssignment*/
    ) -> io::Result<Id<Type>> {
        self.get_type_of_destructured_property(
            self.get_assigned_type(&node.parent())?,
            &node.as_named_declaration().name(),
        )
    }

    pub(super) fn get_assigned_type_of_shorthand_property_assignment(
        &self,
        node: &Node, /*ShorthandPropertyAssignment*/
    ) -> io::Result<Id<Type>> {
        self.get_type_with_default(
            self.get_assigned_type_of_property_assignment(node)?,
            node.as_shorthand_property_assignment()
                .object_assignment_initializer
                .as_ref()
                .unwrap(),
        )
    }

    pub(super) fn get_assigned_type(
        &self,
        node: &Node, /*Expression*/
    ) -> io::Result<Id<Type>> {
        let parent = node.parent();
        Ok(match parent.kind() {
            SyntaxKind::ForInStatement => self.string_type(),
            SyntaxKind::ForOfStatement => self.check_right_hand_side_of_for_of(&parent)?, /*|| errorType*/
            SyntaxKind::BinaryExpression => self.get_assigned_type_of_binary_expression(&parent)?,
            SyntaxKind::DeleteExpression => self.undefined_type(),
            SyntaxKind::ArrayLiteralExpression => {
                self.get_assigned_type_of_array_literal_element(&parent, node)?
            }
            SyntaxKind::SpreadElement => self.get_assigned_type_of_spread_expression(&parent)?,
            SyntaxKind::PropertyAssignment => {
                self.get_assigned_type_of_property_assignment(&parent)?
            }
            SyntaxKind::ShorthandPropertyAssignment => {
                self.get_assigned_type_of_shorthand_property_assignment(&parent)?
            }
            _ => self.error_type(),
        })
    }
}
