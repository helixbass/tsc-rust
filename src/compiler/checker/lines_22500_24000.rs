#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use super::TypeFacts;
use crate::{
    CheckFlags, TransientSymbolInterface, __String, are_option_rcs_equal,
    escape_leading_underscores, find_ancestor, get_check_flags, get_node_id, get_symbol_id,
    is_access_expression, is_assignment_expression, is_binary_expression, is_binding_element,
    is_identifier, is_optional_chain, is_string_or_numeric_literal_like, is_this_in_type_query,
    is_variable_declaration, is_write_only_access, node_is_missing, FindAncestorCallbackReturn,
    HasInitializerInterface, HasTypeInterface, Node, NodeInterface, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeId, TypeInterface,
    UnionReduction,
};

impl TypeChecker {
    pub(super) fn get_resolved_symbol(&self, node: &Node /*Identifier*/) -> Rc<Symbol> {
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
                )
                .unwrap_or_else(|| self.unknown_symbol())
            } else {
                self.unknown_symbol()
            });
        }
        let ret = (*links).borrow().resolved_symbol.clone().unwrap();
        ret
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

    pub(super) fn get_flow_cache_key<TFlowContainer: Borrow<Node>>(
        &self,
        node: &Node,
        declared_type: &Type,
        initial_type: &Type,
        flow_container: Option<TFlowContainer>,
    ) -> Option<String> {
        let flow_container =
            flow_container.map(|flow_container| flow_container.borrow().node_wrapper());
        match node.kind() {
            SyntaxKind::Identifier => {
                if !is_this_in_type_query(node) {
                    let symbol = self.get_resolved_symbol(node);
                    return if !Rc::ptr_eq(&symbol, &self.unknown_symbol()) {
                        Some(format!(
                            "{}|{}|{}|{}",
                            if let Some(flow_container) = flow_container.as_ref() {
                                get_node_id(flow_container).to_string()
                            } else {
                                "-1".to_owned()
                            },
                            self.get_type_id(declared_type),
                            self.get_type_id(initial_type),
                            get_symbol_id(&symbol)
                        ))
                    } else {
                        None
                    };
                }
                return Some(format!(
                    "0|{}|{}|{}",
                    if let Some(flow_container) = flow_container.as_ref() {
                        get_node_id(flow_container).to_string()
                    } else {
                        "-1".to_owned()
                    },
                    self.get_type_id(declared_type),
                    self.get_type_id(initial_type),
                ));
            }
            SyntaxKind::ThisKeyword => {
                return Some(format!(
                    "0|{}|{}|{}",
                    if let Some(flow_container) = flow_container.as_ref() {
                        get_node_id(flow_container).to_string()
                    } else {
                        "-1".to_owned()
                    },
                    self.get_type_id(declared_type),
                    self.get_type_id(initial_type),
                ));
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
                );
                return left.map(|left| {
                    format!(
                        "{}.{}",
                        left,
                        &*node_as_qualified_name.right.as_identifier().escaped_text,
                    )
                });
            }
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                let prop_name = self.get_accessed_property_name(node);
                if let Some(prop_name) = prop_name.as_ref() {
                    let key = self.get_flow_cache_key(
                        &node.as_has_expression().expression(),
                        declared_type,
                        initial_type,
                        flow_container.as_deref(),
                    );
                    return key.map(|key| format!("{}.{}", key, &**prop_name));
                }
            }
            _ => (),
        }
        None
    }

    pub(super) fn is_matching_reference(&self, source: &Node, target: &Node) -> bool {
        match target.kind() {
            SyntaxKind::ParenthesizedExpression | SyntaxKind::NonNullExpression => {
                return self
                    .is_matching_reference(source, &target.as_has_expression().expression());
            }
            SyntaxKind::BinaryExpression => {
                let target_as_binary_expression = target.as_binary_expression();
                return is_assignment_expression(target, None)
                    && self.is_matching_reference(source, &target_as_binary_expression.left)
                    || is_binary_expression(target)
                        && target_as_binary_expression.operator_token.kind()
                            == SyntaxKind::CommaToken
                        && self.is_matching_reference(source, &target_as_binary_expression.right);
            }
            _ => (),
        }
        match source.kind() {
            SyntaxKind::MetaProperty => {
                return target.kind() == SyntaxKind::MetaProperty && {
                    let source_as_meta_property = source.as_meta_property();
                    let target_as_meta_property = target.as_meta_property();
                    source_as_meta_property.keyword_token == target_as_meta_property.keyword_token
                        && source_as_meta_property.name.as_identifier().escaped_text
                            == target_as_meta_property.name.as_identifier().escaped_text
                };
            }
            SyntaxKind::Identifier | SyntaxKind::PrivateIdentifier => {
                return if is_this_in_type_query(source) {
                    target.kind() == SyntaxKind::ThisKeyword
                } else {
                    target.kind() == SyntaxKind::Identifier
                        && Rc::ptr_eq(
                            &self.get_resolved_symbol(source),
                            &self.get_resolved_symbol(target),
                        )
                        || matches!(
                            target.kind(),
                            SyntaxKind::VariableDeclaration | SyntaxKind::BindingElement
                        ) && are_option_rcs_equal(
                            self.get_export_symbol_of_value_symbol_if_exported(Some(
                                self.get_resolved_symbol(source),
                            ))
                            .as_ref(),
                            self.get_symbol_of_node(target).as_ref(),
                        )
                };
            }
            SyntaxKind::ThisKeyword => {
                return target.kind() == SyntaxKind::ThisKeyword;
            }
            SyntaxKind::SuperKeyword => {
                return target.kind() == SyntaxKind::SuperKeyword;
            }
            SyntaxKind::NonNullExpression | SyntaxKind::ParenthesizedExpression => {
                return self
                    .is_matching_reference(&source.as_has_expression().expression(), target);
            }
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                return is_access_expression(target)
                    && self.get_accessed_property_name(source)
                        == self.get_accessed_property_name(target)
                    && self.is_matching_reference(
                        &source.as_has_expression().expression(),
                        &target.as_has_expression().expression(),
                    );
            }
            SyntaxKind::QualifiedName => {
                let source_as_qualified_name = source.as_qualified_name();
                return is_access_expression(target)
                    && matches!(
                        self.get_accessed_property_name(target).as_ref(),
                        Some(accessed_property_name) if &source_as_qualified_name.right.as_identifier().escaped_text == accessed_property_name
                    )
                    && self.is_matching_reference(
                        &source_as_qualified_name.left,
                        &target.as_has_expression().expression(),
                    );
            }
            SyntaxKind::BinaryExpression => {
                return is_binary_expression(source) && {
                    let source_as_binary_expression = source.as_binary_expression();
                    source_as_binary_expression.operator_token.kind() == SyntaxKind::CommaToken
                        && self.is_matching_reference(&source_as_binary_expression.right, target)
                };
            }
            _ => (),
        }
        false
    }

    pub(super) fn get_property_access(&self, expr: &Node /*Expression*/) -> Option<Rc<Node>> {
        if is_access_expression(expr) {
            return Some(expr.node_wrapper());
        }
        if is_identifier(expr) {
            let symbol = self.get_resolved_symbol(expr);
            if self.is_const_variable(&symbol) {
                let declaration = symbol.maybe_value_declaration().unwrap();
                if is_variable_declaration(&declaration) {
                    let declaration_as_variable_declaration = declaration.as_variable_declaration();
                    if declaration_as_variable_declaration.maybe_type().is_none() {
                        if let Some(declaration_initializer) = declaration_as_variable_declaration
                            .maybe_initializer()
                            .filter(|declaration_initializer| {
                                is_access_expression(declaration_initializer)
                            })
                        {
                            return Some(declaration_initializer);
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
                            return Some(declaration);
                        }
                    }
                }
            }
        }
        None
    }

    pub(super) fn get_accessed_property_name(
        &self,
        access: &Node, /*AccessExpression | BindingElement */
    ) -> Option<__String> {
        let mut property_name: Option<String> = None;
        if access.kind() == SyntaxKind::PropertyAccessExpression {
            Some(
                access
                    .as_property_access_expression()
                    .name
                    .as_identifier()
                    .escaped_text
                    .clone(),
            )
        } else if access.kind() == SyntaxKind::ElementAccessExpression
            && is_string_or_numeric_literal_like(
                &access.as_element_access_expression().argument_expression,
            )
        {
            Some(escape_leading_underscores(
                &access
                    .as_element_access_expression()
                    .argument_expression
                    .as_literal_like_node()
                    .text(),
            ))
        } else if access.kind() == SyntaxKind::BindingElement && {
            property_name = self.get_destructuring_property_name(access);
            property_name.is_some()
        } {
            Some(escape_leading_underscores(property_name.as_ref().unwrap()))
        } else {
            None
        }
    }

    pub(super) fn contains_matching_reference(&self, source: &Node, target: &Node) -> bool {
        let mut source = source.node_wrapper();
        while is_access_expression(&source) {
            source = source.as_has_expression().expression();
            if self.is_matching_reference(&source, target) {
                return true;
            }
        }
        false
    }

    pub(super) fn optional_chain_contains_reference(&self, source: &Node, target: &Node) -> bool {
        let mut source = source.node_wrapper();
        while is_optional_chain(&source) {
            source = source.as_has_expression().expression();
            if self.is_matching_reference(&source, target) {
                return true;
            }
        }
        false
    }

    pub(super) fn is_discriminant_property<TType: Borrow<Type>>(
        &self,
        type_: Option<TType>,
        name: &__String,
    ) -> bool {
        if let Some(type_) = type_ {
            let type_ = type_.borrow();
            if type_.flags().intersects(TypeFlags::Union) {
                let prop = self.get_union_or_intersection_property(type_, name, None);
                if let Some(prop) = prop
                    .as_ref()
                    .filter(|prop| get_check_flags(prop).intersects(CheckFlags::SyntheticProperty))
                {
                    let prop_as_transient_symbol = prop.as_transient_symbol();
                    let prop_symbol_links = prop_as_transient_symbol.symbol_links();
                    if (*prop_symbol_links)
                        .borrow()
                        .is_discriminant_property
                        .is_none()
                    {
                        prop_symbol_links.borrow_mut().is_discriminant_property = Some(
                            prop_as_transient_symbol.check_flags() & CheckFlags::Discriminant
                                == CheckFlags::Discriminant
                                && !self.is_generic_type(&self.get_type_of_symbol(&prop)),
                        );
                    }
                    return (*prop_symbol_links)
                        .borrow()
                        .is_discriminant_property
                        .unwrap();
                }
            }
        }
        false
    }

    pub(super) fn find_discriminant_properties(
        &self,
        source_properties: &[Rc<Symbol>],
        target: &Type,
    ) -> Option<Vec<Rc<Symbol>>> {
        let mut result: Option<Vec<Rc<Symbol>>> = None;
        for source_property in source_properties {
            if self.is_discriminant_property(Some(target), source_property.escaped_name()) {
                if result.is_some() {
                    result.as_mut().unwrap().push(source_property.clone());
                    continue;
                }
                result = Some(vec![source_property.clone()]);
            }
        }
        result
    }

    pub(super) fn map_types_by_key_property(
        &self,
        types: &[Rc<Type>],
        name: &__String,
    ) -> Option<HashMap<TypeId, Rc<Type>>> {
        let mut map: HashMap<TypeId, Rc<Type>> = HashMap::new();
        let mut count = 0;
        for type_ in types {
            if type_.flags().intersects(
                TypeFlags::Object | TypeFlags::Intersection | TypeFlags::InstantiableNonPrimitive,
            ) {
                let discriminant = self.get_type_of_property_of_type_(type_, name);
                if let Some(discriminant) = discriminant.as_ref() {
                    if !self.is_literal_type(discriminant) {
                        return None;
                    }
                    let mut duplicate = false;
                    self.for_each_type(discriminant, |t: &Type| -> Option<()> {
                        let id = self.get_type_id(&self.get_regular_type_of_literal_type(t));
                        let existing = map.get(&id);
                        match existing {
                            None => {
                                map.insert(id, type_.clone());
                            }
                            Some(existing) => {
                                if !Rc::ptr_eq(existing, &self.unknown_type()) {
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
        if count >= 10 && count * 2 >= types.len() {
            Some(map)
        } else {
            None
        }
    }

    pub(super) fn get_matching_union_constituent_for_type(
        &self,
        union_type: &Type, /*UnionType*/
        type_: &Type,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn has_matching_argument(
        &self,
        expression: &Node, /*CallExpression | NewExpression*/
        reference: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_function_object_type(&self, type_: &Type /*ObjectType*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_type_facts(&self, type_: &Type, ignore_objects: Option<bool>) -> TypeFacts {
        let ignore_objects = ignore_objects.unwrap_or(false);
        unimplemented!()
    }

    pub(super) fn get_type_with_facts(&self, type_: &Type, include: TypeFacts) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_of_initializer(&self, node: &Node /*Expression*/) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_type_subset_of(&self, source: &Type, target: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn for_each_type<TReturn, TCallback: FnMut(&Type) -> Option<TReturn>>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> Option<TReturn> {
        unimplemented!()
    }

    pub(super) fn some_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn every_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn filter_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            unimplemented!()
        }
        if type_.flags().intersects(TypeFlags::Never) || f(type_) {
            type_.type_wrapper()
        } else {
            self.never_type()
        }
    }

    pub(super) fn remove_type(&self, type_: &Type, target_type: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn count_types(&self, type_: &Type) -> usize {
        unimplemented!()
    }

    pub(super) fn map_type<TMapper: FnMut(&Type) -> Option<Rc<Type>>>(
        &self,
        type_: &Type,
        mapper: &mut TMapper,
        no_reductions: Option<bool>,
    ) -> Option<Rc<Type>> {
        let no_reductions = no_reductions.unwrap_or(false);
        if type_.flags().intersects(TypeFlags::Never) {
            return Some(type_.type_wrapper());
        }
        if !type_.flags().intersects(TypeFlags::Union) {
            return mapper(type_);
        }
        let types = type_.as_union_or_intersection_type_interface().types();
        let mut mapped_types: Vec<Rc<Type>> = vec![];
        let mut changed = false;
        for t in types {
            let mapped = if t.flags().intersects(TypeFlags::Union) {
                self.map_type(&t, mapper, Some(no_reductions))
            } else {
                mapper(&t)
            };
            changed = changed
                || match mapped.as_ref() {
                    None => true,
                    Some(mapped) => !Rc::ptr_eq(t, mapped),
                };
            if let Some(mapped) = mapped {
                mapped_types.push(mapped);
            }
        }
        if changed {
            if !mapped_types.is_empty() {
                Some(self.get_union_type(
                    mapped_types,
                    Some(if no_reductions {
                        UnionReduction::None
                    } else {
                        UnionReduction::Literal
                    }),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                ))
            } else {
                None
            }
        } else {
            Some(type_.type_wrapper())
        }
    }

    pub(super) fn map_type_with_alias<
        TMapper: FnMut(&Type) -> Rc<Type>,
        TAliasSymbol: Borrow<Symbol>,
    >(
        &self,
        type_: &Type,
        mapper: &mut TMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_constituent_count(&self, type_: &Type) -> usize {
        if type_.flags().intersects(TypeFlags::Union) {
            type_
                .as_union_or_intersection_type_interface()
                .types()
                .len()
        } else {
            1
        }
    }

    pub(super) fn extract_types_of_kind(&self, type_: &Type, kind: TypeFlags) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_flow_type_of_reference<
        TInitialType: Borrow<Type>,
        TFlowContainer: Borrow<Node>,
    >(
        &self,
        reference: &Node,
        declared_type: &Type,
        initial_type: Option<TInitialType>,
        flow_container: Option<TFlowContainer>,
    ) -> Rc<Type> {
        let initial_type = initial_type.map_or_else(
            || declared_type.type_wrapper(),
            |initial_type| initial_type.borrow().type_wrapper(),
        );
        unimplemented!()
    }
}
