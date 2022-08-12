#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use crate::{
    contains_rc, every, filter, for_each, for_each_bool, get_object_flags, is_string_literal_like,
    map, some, EvolvingArrayType, FlowType, HasInitializerInterface, IncompleteType,
    NamedDeclarationInterface, Node, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, Symbol,
    SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, UnionOrIntersectionTypeInterface,
    UnionReduction,
};

impl TypeChecker {
    pub(super) fn get_initial_type_of_binding_element(
        &self,
        node: &Node, /*BindingElement*/
    ) -> Rc<Type> {
        let pattern = node.parent();
        let parent_type = self.get_initial_type(&pattern.parent());
        let node_as_binding_element = node.as_binding_element();
        let type_ = if pattern.kind() == SyntaxKind::ObjectBindingPattern {
            self.get_type_of_destructured_property(
                &parent_type,
                &node_as_binding_element
                    .property_name
                    .clone()
                    .unwrap_or_else(|| node_as_binding_element.name()),
            )
        } else if node_as_binding_element.dot_dot_dot_token.is_none() {
            self.get_type_of_destructured_array_element(
                &parent_type,
                pattern
                    .as_array_binding_pattern()
                    .elements
                    .iter()
                    .position(|element| ptr::eq(&**element, node))
                    .unwrap(),
            )
        } else {
            self.get_type_of_destructured_spread_expression(&parent_type)
        };
        self.get_type_with_default(
            &type_,
            &node_as_binding_element.maybe_initializer().unwrap(),
        )
    }

    pub(super) fn get_type_of_initializer(&self, node: &Node /*Expression*/) -> Rc<Type> {
        let links = self.get_node_links(node);
        let ret = (*links)
            .borrow()
            .resolved_type
            .clone()
            .unwrap_or_else(|| self.get_type_of_expression(node));
        ret
    }

    pub(super) fn get_initial_type_of_variable_declaration(
        &self,
        node: &Node, /*VariableDeclaration*/
    ) -> Rc<Type> {
        let node_as_variable_declaration = node.as_variable_declaration();
        if let Some(node_initializer) = node_as_variable_declaration.maybe_initializer() {
            return self.get_type_of_initializer(&node_initializer);
        }
        if node.parent().parent().kind() == SyntaxKind::ForInStatement {
            return self.string_type();
        }
        if node.parent().parent().kind() == SyntaxKind::ForOfStatement {
            return self.check_right_hand_side_of_for_of(&node.parent().parent());
            /*|| errorType*/
        }
        self.error_type()
    }

    pub(super) fn get_initial_type(
        &self,
        node: &Node, /*VariableDeclaration | BindingElement*/
    ) -> Rc<Type> {
        if node.kind() == SyntaxKind::VariableDeclaration {
            self.get_initial_type_of_variable_declaration(node)
        } else {
            self.get_initial_type_of_binding_element(node)
        }
    }

    pub(super) fn is_empty_array_assignment(
        &self,
        node: &Node, /*VariableDeclaration | BindingElement | Expression*/
    ) -> bool {
        node.kind() == SyntaxKind::VariableDeclaration
            && matches!(
                node.as_variable_declaration().maybe_initializer().as_ref(),
                Some(node_initializer) if self.is_empty_array_literal(node_initializer)
            )
            || node.kind() != SyntaxKind::BindingElement
                && node.parent().kind() == SyntaxKind::BinaryExpression
                && self.is_empty_array_literal(&node.parent().as_binary_expression().right)
    }

    pub(super) fn get_reference_candidate(&self, node: &Node /*Expression*/) -> Rc<Node> {
        match node.kind() {
            SyntaxKind::ParenthesizedExpression => {
                return self
                    .get_reference_candidate(&node.as_parenthesized_expression().expression);
            }
            SyntaxKind::BinaryExpression => {
                let node_as_binary_expression = node.as_binary_expression();
                match node_as_binary_expression.operator_token.kind() {
                    SyntaxKind::EqualsToken
                    | SyntaxKind::BarBarEqualsToken
                    | SyntaxKind::AmpersandAmpersandEqualsToken
                    | SyntaxKind::QuestionQuestionEqualsToken => {
                        return self.get_reference_candidate(&node_as_binary_expression.left);
                    }
                    SyntaxKind::CommaToken => {
                        return self.get_reference_candidate(&node_as_binary_expression.right);
                    }
                    _ => (),
                }
            }
            _ => (),
        }
        node.node_wrapper()
    }

    pub(super) fn get_reference_root(&self, node: &Node) -> Rc<Node> {
        let parent = node.parent();
        if parent.kind() == SyntaxKind::ParenthesizedExpression
            || parent.kind() == SyntaxKind::BinaryExpression && {
                let parent_as_binary_expression = parent.as_binary_expression();
                parent_as_binary_expression.operator_token.kind() == SyntaxKind::EqualsToken
                    && ptr::eq(&*parent_as_binary_expression.left, node)
            }
            || parent.kind() == SyntaxKind::BinaryExpression && {
                let parent_as_binary_expression = parent.as_binary_expression();
                parent_as_binary_expression.operator_token.kind() == SyntaxKind::CommaToken
                    && ptr::eq(&*parent_as_binary_expression.right, node)
            }
        {
            self.get_reference_root(&parent)
        } else {
            node.node_wrapper()
        }
    }

    pub(super) fn get_type_of_switch_clause(
        &self,
        clause: &Node, /*CaseClause | DefaultClause*/
    ) -> Rc<Type> {
        if clause.kind() == SyntaxKind::CaseClause {
            return self.get_regular_type_of_literal_type(
                &self.get_type_of_expression(&clause.as_has_expression().expression()),
            );
        }
        self.never_type()
    }

    pub(super) fn get_switch_clause_types(
        &self,
        switch_statement: &Node, /*SwitchStatement*/
    ) -> Vec<Rc<Type>> {
        let links = self.get_node_links(switch_statement);
        if (*links).borrow().switch_types.is_none() {
            let mut switch_types = vec![];
            for clause in &switch_statement
                .as_switch_statement()
                .case_block
                .as_case_block()
                .clauses
            {
                switch_types.push(self.get_type_of_switch_clause(clause));
            }
            links.borrow_mut().switch_types = Some(switch_types);
        }
        let ret = (*links).borrow().switch_types.clone().unwrap();
        ret
    }

    pub(super) fn get_switch_clause_type_of_witnesses(
        &self,
        switch_statement: &Node, /*SwitchStatement*/
        retain_default: bool,
    ) -> Vec<Option<String>> {
        let mut witnesses: Vec<Option<String>> = vec![];
        for clause in &switch_statement
            .as_switch_statement()
            .case_block
            .as_case_block()
            .clauses
        {
            if clause.kind() == SyntaxKind::CaseClause {
                let clause_as_case_clause = clause.as_case_clause();
                if is_string_literal_like(&clause_as_case_clause.expression) {
                    witnesses.push(Some(
                        clause_as_case_clause
                            .expression
                            .as_literal_like_node()
                            .text()
                            .clone(),
                    ));
                    continue;
                }
                return vec![];
            }
            if retain_default {
                witnesses.push(None);
            }
        }
        witnesses
    }

    pub(super) fn each_type_contained_in(&self, source: &Type, types: &[Rc<Type>]) -> bool {
        if source.flags().intersects(TypeFlags::Union) {
            !for_each_bool(
                source.as_union_or_intersection_type_interface().types(),
                |t: &Rc<Type>, _| !contains_rc(Some(types), t),
            )
        } else {
            contains_rc(Some(types), &source.type_wrapper())
        }
    }

    pub(super) fn is_type_subset_of(&self, source: &Type, target: &Type) -> bool {
        ptr::eq(source, target)
            || target.flags().intersects(TypeFlags::Union)
                && self.is_type_subset_of_union(source, target)
    }

    pub(super) fn is_type_subset_of_union(
        &self,
        source: &Type,
        target: &Type, /*UnionType*/
    ) -> bool {
        let target_as_union_type = target.as_union_type();
        if source.flags().intersects(TypeFlags::Union) {
            for t in source.as_union_or_intersection_type_interface().types() {
                if !self.contains_type(target_as_union_type.types(), t) {
                    return false;
                }
            }
            return true;
        }
        if source.flags().intersects(TypeFlags::EnumLiteral)
            && ptr::eq(&*self.get_base_type_of_enum_literal_type(source), target)
        {
            return true;
        }
        self.contains_type(target_as_union_type.types(), source)
    }

    pub(super) fn for_each_type<TReturn, TCallback: FnMut(&Type) -> Option<TReturn>>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> Option<TReturn> {
        if type_.flags().intersects(TypeFlags::Union) {
            for_each(
                type_.as_union_or_intersection_type_interface().types(),
                |type_: &Rc<Type>, _| f(type_),
            )
        } else {
            f(type_)
        }
    }

    pub(super) fn some_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> bool {
        if type_.flags().intersects(TypeFlags::Union) {
            some(
                Some(type_.as_union_or_intersection_type_interface().types()),
                Some(|type_: &Rc<Type>| f(type_)),
            )
        } else {
            f(type_)
        }
    }

    pub(super) fn every_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> bool {
        if type_.flags().intersects(TypeFlags::Union) {
            every(
                type_.as_union_or_intersection_type_interface().types(),
                |type_: &Rc<Type>, _| f(type_),
            )
        } else {
            f(type_)
        }
    }

    pub(super) fn every_contained_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> bool {
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            every(
                type_.as_union_or_intersection_type_interface().types(),
                |type_: &Rc<Type>, _| f(type_),
            )
        } else {
            f(type_)
        }
    }

    pub(super) fn filter_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            let type_as_union_type = type_.as_union_type();
            let types = type_as_union_type.types();
            let filtered = filter(types, |type_: &Rc<Type>| f(type_));
            if filtered.len() == types.len() {
                return type_.type_wrapper();
            }
            let origin = type_as_union_type.origin.as_ref();
            let mut new_origin: Option<Rc<Type>> = None;
            if let Some(origin) = origin
                .as_ref()
                .filter(|origin| origin.flags().intersects(TypeFlags::Union))
            {
                let origin_types = origin.as_union_type().types();
                let origin_filtered = filter(origin_types, |t: &Rc<Type>| {
                    t.flags().intersects(TypeFlags::Union) || f(t)
                });
                if origin_types.len() - origin_filtered.len() == types.len() - filtered.len() {
                    if origin_filtered.len() == 1 {
                        return origin_filtered[0].clone();
                    }
                    new_origin = Some(self.create_origin_union_or_intersection_type(
                        TypeFlags::Union,
                        origin_filtered,
                    ));
                }
            }
            return self.get_union_type_from_sorted_list(
                filtered,
                type_as_union_type.object_flags(),
                Option::<&Symbol>::None,
                None,
                new_origin.as_deref(),
            );
        }
        if type_.flags().intersects(TypeFlags::Never) || f(type_) {
            type_.type_wrapper()
        } else {
            self.never_type()
        }
    }

    pub(super) fn remove_type(&self, type_: &Type, target_type: &Type) -> Rc<Type> {
        self.filter_type(type_, |t: &Type| !ptr::eq(t, target_type))
    }

    pub(super) fn count_types(&self, type_: &Type) -> usize {
        if type_.flags().intersects(TypeFlags::Union) {
            type_
                .as_union_or_intersection_type_interface()
                .types()
                .len()
        } else {
            1
        }
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
        let type_as_union_type = type_.as_union_type();
        let origin = type_as_union_type.origin.as_ref();
        let types = if let Some(origin) =
            origin.filter(|origin| origin.flags().intersects(TypeFlags::Union))
        {
            origin
                .as_union_or_intersection_type_interface()
                .types()
                .to_owned()
        } else {
            type_as_union_type.types().to_owned()
        };
        let mut mapped_types: Vec<Rc<Type>> = vec![];
        let mut changed = false;
        for t in &types {
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
        if type_.flags().intersects(TypeFlags::Union) && alias_symbol.is_some() {
            self.get_union_type(
                map(type_.as_union_type().types(), |type_: &Rc<Type>, _| {
                    mapper(type_)
                }),
                Some(UnionReduction::Literal),
                alias_symbol,
                alias_type_arguments,
                Option::<&Type>::None,
            )
        } else {
            self.map_type(type_, &mut |type_: &Type| Some(mapper(type_)), None)
                .unwrap()
        }
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

    pub(super) fn replace_primitives_with_literals(
        &self,
        type_with_primitives: &Type,
        type_with_literals: &Type,
    ) -> Rc<Type> {
        if self.maybe_type_of_kind(
            type_with_primitives,
            TypeFlags::String | TypeFlags::TemplateLiteral | TypeFlags::Number | TypeFlags::BigInt,
        ) && self.maybe_type_of_kind(
            type_with_literals,
            TypeFlags::StringLiteral
                | TypeFlags::TemplateLiteral
                | TypeFlags::StringMapping
                | TypeFlags::NumberLiteral
                | TypeFlags::BigIntLiteral,
        ) {
            return self
                .map_type(
                    type_with_primitives,
                    &mut |t: &Type| {
                        Some(if t.flags().intersects(TypeFlags::String) {
                            self.extract_types_of_kind(
                                type_with_literals,
                                TypeFlags::String
                                    | TypeFlags::StringLiteral
                                    | TypeFlags::TemplateLiteral
                                    | TypeFlags::StringMapping,
                            )
                        } else if self.is_pattern_literal_type(t)
                            && !self.maybe_type_of_kind(
                                type_with_literals,
                                TypeFlags::String
                                    | TypeFlags::TemplateLiteral
                                    | TypeFlags::StringMapping,
                            )
                        {
                            self.extract_types_of_kind(type_with_literals, TypeFlags::StringLiteral)
                        } else if t.flags().intersects(TypeFlags::Number) {
                            self.extract_types_of_kind(
                                type_with_literals,
                                TypeFlags::Number | TypeFlags::NumberLiteral,
                            )
                        } else if t.flags().intersects(TypeFlags::BigInt) {
                            self.extract_types_of_kind(
                                type_with_literals,
                                TypeFlags::BigInt | TypeFlags::BigIntLiteral,
                            )
                        } else {
                            t.type_wrapper()
                        })
                    },
                    None,
                )
                .unwrap();
        }
        type_with_primitives.type_wrapper()
    }

    pub(super) fn is_incomplete(&self, flow_type: &FlowType) -> bool {
        flow_type.flags() == TypeFlags::None
    }

    pub(super) fn get_type_from_flow_type(&self, flow_type: &FlowType) -> Rc<Type> {
        if flow_type.flags() == TypeFlags::None {
            flow_type.as_incomplete_type().type_.clone()
        } else {
            flow_type.as_type().clone()
        }
    }

    pub(super) fn create_flow_type(&self, type_: &Type, incomplete: bool) -> FlowType {
        if incomplete {
            FlowType::IncompleteType(IncompleteType::new(
                TypeFlags::None,
                if type_.flags().intersects(TypeFlags::Never) {
                    self.silent_never_type()
                } else {
                    type_.type_wrapper()
                },
            ))
        } else {
            FlowType::Type(type_.type_wrapper())
        }
    }

    pub(super) fn create_evolving_array_type(
        &self,
        element_type: &Type,
    ) -> Rc<Type /*EvolvingArrayType*/> {
        let result = self.create_object_type(ObjectFlags::EvolvingArray, Option::<&Symbol>::None);
        EvolvingArrayType::new(result, element_type.type_wrapper()).into()
    }

    pub(super) fn get_evolving_array_type(
        &self,
        element_type: &Type,
    ) -> Rc<Type /*EvolvingArrayType*/> {
        self.evolving_array_types()
            .entry(element_type.id())
            .or_insert_with(|| self.create_evolving_array_type(element_type))
            .clone()
    }

    pub(super) fn add_evolving_array_element_type(
        &self,
        evolving_array_type: &Type, /*EvolvingArrayType*/
        node: &Node,                /*Expression*/
    ) -> Rc<Type /*EvolvingArrayType*/> {
        let element_type = self.get_regular_type_of_object_literal(
            &self.get_base_type_of_literal_type(&self.get_context_free_type_of_expression(node)),
        );
        let evolving_array_type_as_evolving_array_type =
            evolving_array_type.as_evolving_array_type();
        if self.is_type_subset_of(
            &element_type,
            &evolving_array_type_as_evolving_array_type.element_type,
        ) {
            evolving_array_type.type_wrapper()
        } else {
            self.get_evolving_array_type(&self.get_union_type(
                vec![
                        evolving_array_type_as_evolving_array_type.element_type.clone(),
                        element_type,
                    ],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            ))
        }
    }

    pub(super) fn create_final_array_type(&self, element_type: &Type) -> Rc<Type> {
        if element_type.flags().intersects(TypeFlags::Never) {
            self.auto_array_type()
        } else {
            self.create_array_type(
                &*if element_type.flags().intersects(TypeFlags::Union) {
                    self.get_union_type(
                        element_type.as_union_type().types().to_owned(),
                        Some(UnionReduction::Subtype),
                        Option::<&Symbol>::None,
                        None,
                        Option::<&Type>::None,
                    )
                } else {
                    element_type.type_wrapper()
                },
                None,
            )
        }
    }

    pub(super) fn get_final_array_type(
        &self,
        evolving_array_type: &Type, /*EvolvingArrayType*/
    ) -> Rc<Type> {
        let evolving_array_type_as_evolving_array_type =
            evolving_array_type.as_evolving_array_type();
        let mut final_array_type =
            evolving_array_type_as_evolving_array_type.maybe_final_array_type();
        if final_array_type.is_none() {
            *final_array_type =
                Some(self.create_final_array_type(
                    &evolving_array_type_as_evolving_array_type.element_type,
                ));
        }
        final_array_type.clone().unwrap()
    }

    pub(super) fn finalize_evolving_array_type(&self, type_: &Type) -> Rc<Type> {
        if get_object_flags(type_).intersects(ObjectFlags::EvolvingArray) {
            self.get_final_array_type(type_)
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_element_type_of_evolving_array_type(&self, type_: &Type) -> Rc<Type> {
        if get_object_flags(type_).intersects(ObjectFlags::EvolvingArray) {
            type_.as_evolving_array_type().element_type.clone()
        } else {
            self.never_type()
        }
    }

    pub(super) fn is_evolving_array_type_list(&self, types: &[Rc<Type>]) -> bool {
        let mut has_evolving_array_type = false;
        for t in types {
            if !t.flags().intersects(TypeFlags::Never) {
                if !get_object_flags(t).intersects(ObjectFlags::EvolvingArray) {
                    return false;
                }
                has_evolving_array_type = true;
            }
        }
        has_evolving_array_type
    }

    pub(super) fn extract_types_of_kind(&self, type_: &Type, kind: TypeFlags) -> Rc<Type> {
        self.filter_type(type_, |t: &Type| t.flags().intersects(kind))
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
