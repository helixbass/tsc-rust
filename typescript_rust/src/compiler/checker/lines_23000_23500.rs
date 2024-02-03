use std::{convert::TryInto, io};

use id_arena::Id;

use super::IterationUse;
use crate::{
    add_related_info, contains, create_diagnostic_for_node, create_file_diagnostic, every,
    find_ancestor, for_each_bool, get_check_flags, get_effective_return_type_node,
    get_effective_type_annotation_node, get_object_flags, get_source_file_of_node,
    get_span_of_token_at_position, get_symbol_name_for_private_identifier, has_initializer,
    is_access_expression, is_assignment_target, is_function_expression_or_arrow_function,
    is_function_or_module_block, is_identifier, is_in_js_file, is_optional_chain, is_parameter,
    is_private_identifier, is_property_access_expression, is_property_declaration,
    is_property_signature, is_push_or_unshift_identifier, is_string_literal_like,
    is_variable_declaration, skip_parentheses, try_every, try_filter, try_for_each, try_map,
    try_some, CheckFlags, Diagnostic, Diagnostics, EvolvingArrayType, FlowFlags, FlowNode,
    FlowNodeBase, FlowType, HasArena, HasInitializerInterface, InArena, IncompleteType,
    NamedDeclarationInterface, Node, NodeFlags, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, OptionTry, ReadonlyTextRange, Signature, SignatureKind, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, TransientSymbolInterface, Type, TypeChecker,
    TypeFlags, TypeInterface, TypePredicate, TypePredicateKind, UnionOrIntersectionTypeInterface,
    UnionReduction,
};

impl TypeChecker {
    pub(super) fn get_initial_type_of_binding_element(
        &self,
        node: Id<Node>, /*BindingElement*/
    ) -> io::Result<Id<Type>> {
        let pattern = node.ref_(self).parent();
        let parent_type = self.get_initial_type(pattern.ref_(self).parent())?;
        let node_ref = node.ref_(self);
        let node_as_binding_element = node_ref.as_binding_element();
        let type_ = if pattern.ref_(self).kind() == SyntaxKind::ObjectBindingPattern {
            self.get_type_of_destructured_property(
                parent_type,
                node_as_binding_element
                    .property_name
                    .unwrap_or_else(|| node_as_binding_element.name()),
            )?
        } else if node_as_binding_element.dot_dot_dot_token.is_none() {
            self.get_type_of_destructured_array_element(
                parent_type,
                pattern
                    .ref_(self)
                    .as_array_binding_pattern()
                    .elements
                    .ref_(self)
                    .iter()
                    .position(|&element| element == node)
                    .unwrap(),
            )?
        } else {
            self.get_type_of_destructured_spread_expression(parent_type)?
        };
        self.get_type_with_default(type_, node_as_binding_element.maybe_initializer().unwrap())
    }

    pub(super) fn get_type_of_initializer(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        let ret = links
            .ref_(self)
            .resolved_type
            .try_unwrap_or_else(|| self.get_type_of_expression(node))?;
        Ok(ret)
    }

    pub(super) fn get_initial_type_of_variable_declaration(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_variable_declaration = node_ref.as_variable_declaration();
        if let Some(node_initializer) = node_as_variable_declaration.maybe_initializer() {
            return self.get_type_of_initializer(node_initializer);
        }
        if node
            .ref_(self)
            .parent()
            .ref_(self)
            .parent()
            .ref_(self)
            .kind()
            == SyntaxKind::ForInStatement
        {
            return Ok(self.string_type());
        }
        if node
            .ref_(self)
            .parent()
            .ref_(self)
            .parent()
            .ref_(self)
            .kind()
            == SyntaxKind::ForOfStatement
        {
            return self
                .check_right_hand_side_of_for_of(node.ref_(self).parent().ref_(self).parent());
            /*|| errorType*/
        }
        Ok(self.error_type())
    }

    pub(super) fn get_initial_type(
        &self,
        node: Id<Node>, /*VariableDeclaration | BindingElement*/
    ) -> io::Result<Id<Type>> {
        Ok(
            if node.ref_(self).kind() == SyntaxKind::VariableDeclaration {
                self.get_initial_type_of_variable_declaration(node)?
            } else {
                self.get_initial_type_of_binding_element(node)?
            },
        )
    }

    pub(super) fn is_empty_array_assignment(
        &self,
        node: Id<Node>, /*VariableDeclaration | BindingElement | Expression*/
    ) -> bool {
        node.ref_(self).kind() == SyntaxKind::VariableDeclaration
            && matches!(
                node.ref_(self).as_variable_declaration().maybe_initializer(),
                Some(node_initializer) if self.is_empty_array_literal(node_initializer)
            )
            || node.ref_(self).kind() != SyntaxKind::BindingElement
                && node.ref_(self).parent().ref_(self).kind() == SyntaxKind::BinaryExpression
                && self.is_empty_array_literal(
                    node.ref_(self)
                        .parent()
                        .ref_(self)
                        .as_binary_expression()
                        .right,
                )
    }

    pub(super) fn get_reference_candidate(&self, node: Id<Node> /*Expression*/) -> Id<Node> {
        match node.ref_(self).kind() {
            SyntaxKind::ParenthesizedExpression => {
                return self.get_reference_candidate(
                    node.ref_(self).as_parenthesized_expression().expression,
                );
            }
            SyntaxKind::BinaryExpression => {
                let node_ref = node.ref_(self);
                let node_as_binary_expression = node_ref.as_binary_expression();
                match node_as_binary_expression.operator_token.ref_(self).kind() {
                    SyntaxKind::EqualsToken
                    | SyntaxKind::BarBarEqualsToken
                    | SyntaxKind::AmpersandAmpersandEqualsToken
                    | SyntaxKind::QuestionQuestionEqualsToken => {
                        return self.get_reference_candidate(node_as_binary_expression.left);
                    }
                    SyntaxKind::CommaToken => {
                        return self.get_reference_candidate(node_as_binary_expression.right);
                    }
                    _ => (),
                }
            }
            _ => (),
        }
        node
    }

    pub(super) fn get_reference_root(&self, node: Id<Node>) -> Id<Node> {
        let parent = node.ref_(self).parent();
        if parent.ref_(self).kind() == SyntaxKind::ParenthesizedExpression
            || parent.ref_(self).kind() == SyntaxKind::BinaryExpression && {
                let parent_ref = parent.ref_(self);
                let parent_as_binary_expression = parent_ref.as_binary_expression();
                parent_as_binary_expression.operator_token.ref_(self).kind()
                    == SyntaxKind::EqualsToken
                    && parent_as_binary_expression.left == node
            }
            || parent.ref_(self).kind() == SyntaxKind::BinaryExpression && {
                let parent_ref = parent.ref_(self);
                let parent_as_binary_expression = parent_ref.as_binary_expression();
                parent_as_binary_expression.operator_token.ref_(self).kind()
                    == SyntaxKind::CommaToken
                    && parent_as_binary_expression.right == node
            }
        {
            self.get_reference_root(parent)
        } else {
            node
        }
    }

    pub(super) fn get_type_of_switch_clause(
        &self,
        clause: Id<Node>, /*CaseClause | DefaultClause*/
    ) -> io::Result<Id<Type>> {
        if clause.ref_(self).kind() == SyntaxKind::CaseClause {
            return Ok(self.get_regular_type_of_literal_type(
                self.get_type_of_expression(clause.ref_(self).as_has_expression().expression())?,
            ));
        }
        Ok(self.never_type())
    }

    pub(super) fn get_switch_clause_types(
        &self,
        switch_statement: Id<Node>, /*SwitchStatement*/
    ) -> io::Result<Vec<Id<Type>>> {
        let links = self.get_node_links(switch_statement);
        if links.ref_(self).switch_types.is_none() {
            let mut switch_types = vec![];
            for &clause in &*switch_statement
                .ref_(self)
                .as_switch_statement()
                .case_block
                .ref_(self)
                .as_case_block()
                .clauses
                .ref_(self)
            {
                switch_types.push(self.get_type_of_switch_clause(clause)?);
            }
            links.ref_mut(self).switch_types = Some(switch_types);
        }
        let ret = links.ref_(self).switch_types.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_switch_clause_type_of_witnesses(
        &self,
        switch_statement: Id<Node>, /*SwitchStatement*/
        retain_default: bool,
    ) -> Vec<Option<String>> {
        let mut witnesses: Vec<Option<String>> = vec![];
        for clause in &*switch_statement
            .ref_(self)
            .as_switch_statement()
            .case_block
            .ref_(self)
            .as_case_block()
            .clauses
            .ref_(self)
        {
            if clause.ref_(self).kind() == SyntaxKind::CaseClause {
                let clause_ref = clause.ref_(self);
                let clause_as_case_clause = clause_ref.as_case_clause();
                if is_string_literal_like(&clause_as_case_clause.expression.ref_(self)) {
                    witnesses.push(Some(
                        clause_as_case_clause
                            .expression
                            .ref_(self)
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

    pub(super) fn each_type_contained_in(&self, source: Id<Type>, types: &[Id<Type>]) -> bool {
        if source.ref_(self).flags().intersects(TypeFlags::Union) {
            !for_each_bool(
                source
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |t: &Id<Type>, _| !contains(Some(types), t),
            )
        } else {
            contains(Some(types), &source)
        }
    }

    pub(super) fn is_type_subset_of(&self, source: Id<Type>, target: Id<Type>) -> io::Result<bool> {
        Ok(source == target
            || target.ref_(self).flags().intersects(TypeFlags::Union)
                && self.is_type_subset_of_union(source, target)?)
    }

    pub(super) fn is_type_subset_of_union(
        &self,
        source: Id<Type>,
        target: Id<Type>, /*UnionType*/
    ) -> io::Result<bool> {
        if source.ref_(self).flags().intersects(TypeFlags::Union) {
            for &t in source
                .ref_(self)
                .as_union_or_intersection_type_interface()
                .types()
            {
                if !self.contains_type(target.ref_(self).as_union_type().types(), t) {
                    return Ok(false);
                }
            }
            return Ok(true);
        }
        if source.ref_(self).flags().intersects(TypeFlags::EnumLiteral)
            && self.get_base_type_of_enum_literal_type(source)? == target
        {
            return Ok(true);
        }
        Ok(self.contains_type(target.ref_(self).as_union_type().types(), source))
    }

    pub(super) fn for_each_type<TReturn>(
        &self,
        type_: Id<Type>,
        mut f: impl FnMut(Id<Type>) -> Option<TReturn>,
    ) -> Option<TReturn> {
        self.try_for_each_type(type_, |type_: Id<Type>| Ok(f(type_)))
            .unwrap()
    }

    pub(super) fn try_for_each_type<TReturn>(
        &self,
        type_: Id<Type>,
        mut f: impl FnMut(Id<Type>) -> io::Result<Option<TReturn>>,
    ) -> io::Result<Option<TReturn>> {
        if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            try_for_each(
                type_
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |&type_: &Id<Type>, _| f(type_),
            )
        } else {
            f(type_)
        }
    }

    pub(super) fn some_type(&self, type_: Id<Type>, mut f: impl FnMut(Id<Type>) -> bool) -> bool {
        self.try_some_type(type_, |type_: Id<Type>| Ok(f(type_)))
            .unwrap()
    }

    pub(super) fn try_some_type(
        &self,
        type_: Id<Type>,
        mut f: impl FnMut(Id<Type>) -> io::Result<bool>,
    ) -> io::Result<bool> {
        Ok(if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            try_some(
                Some(&{
                    let types = type_
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned();
                    types
                }),
                Some(|&type_: &Id<Type>| f(type_)),
            )?
        } else {
            f(type_)?
        })
    }

    pub(super) fn every_type(&self, type_: Id<Type>, mut f: impl FnMut(Id<Type>) -> bool) -> bool {
        self.try_every_type(type_, |type_: Id<Type>| Ok(f(type_)))
            .unwrap()
    }

    pub(super) fn try_every_type(
        &self,
        type_: Id<Type>,
        mut f: impl FnMut(Id<Type>) -> io::Result<bool>,
    ) -> io::Result<bool> {
        Ok(if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            try_every(
                type_
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |&type_: &Id<Type>, _| f(type_),
            )?
        } else {
            f(type_)?
        })
    }

    pub(super) fn every_contained_type(
        &self,
        type_: Id<Type>,
        mut f: impl FnMut(Id<Type>) -> bool,
    ) -> bool {
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
        {
            every(
                type_
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |&type_: &Id<Type>, _| f(type_),
            )
        } else {
            f(type_)
        }
    }

    pub(super) fn filter_type(
        &self,
        type_: Id<Type>,
        mut f: impl FnMut(Id<Type>) -> bool,
    ) -> Id<Type> {
        self.try_filter_type(type_, |type_: Id<Type>| Ok(f(type_)))
            .unwrap()
    }

    pub(super) fn try_filter_type(
        &self,
        type_: Id<Type>,
        mut f: impl FnMut(Id<Type>) -> io::Result<bool>,
    ) -> io::Result<Id<Type>> {
        if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            let types = &type_.ref_(self).as_union_type().types().to_owned();
            let filtered = try_filter(types, |&type_: &Id<Type>| f(type_))?;
            if filtered.len() == types.len() {
                return Ok(type_);
            }
            let origin = type_.ref_(self).as_union_type().origin;
            let mut new_origin: Option<Id<Type>> = None;
            if let Some(origin) =
                origin.filter(|&origin| origin.ref_(self).flags().intersects(TypeFlags::Union))
            {
                let origin_ref = origin.ref_(self);
                let origin_types = origin_ref.as_union_type().types();
                let origin_filtered = try_filter(origin_types, |&t: &Id<Type>| -> io::Result<_> {
                    Ok(t.ref_(self).flags().intersects(TypeFlags::Union) || f(t)?)
                })?;
                if origin_types.len() - origin_filtered.len() == types.len() - filtered.len() {
                    if origin_filtered.len() == 1 {
                        return Ok(origin_filtered[0].clone());
                    }
                    new_origin = Some(self.create_origin_union_or_intersection_type(
                        TypeFlags::Union,
                        origin_filtered,
                    ));
                }
            }
            return Ok(self.get_union_type_from_sorted_list(
                filtered,
                {
                    let object_flags = type_.ref_(self).as_union_type().object_flags();
                    object_flags
                },
                Option::<Id<Symbol>>::None,
                None,
                new_origin,
            ));
        }
        Ok(
            if type_.ref_(self).flags().intersects(TypeFlags::Never) || f(type_)? {
                type_
            } else {
                self.never_type()
            },
        )
    }

    pub(super) fn remove_type(&self, type_: Id<Type>, target_type: Id<Type>) -> Id<Type> {
        self.filter_type(type_, |t: Id<Type>| t != target_type)
    }

    pub(super) fn count_types(&self, type_: Id<Type>) -> usize {
        if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            type_
                .ref_(self)
                .as_union_or_intersection_type_interface()
                .types()
                .len()
        } else {
            1
        }
    }

    pub(super) fn try_map_type(
        &self,
        type_: Id<Type>,
        mapper: &mut impl FnMut(Id<Type>) -> io::Result<Option<Id<Type>>>,
        no_reductions: Option<bool>,
    ) -> io::Result<Option<Id<Type>>> {
        let no_reductions = no_reductions.unwrap_or(false);
        if type_.ref_(self).flags().intersects(TypeFlags::Never) {
            return Ok(Some(type_));
        }
        if !type_.ref_(self).flags().intersects(TypeFlags::Union) {
            return mapper(type_);
        }
        let origin = type_.ref_(self).as_union_type().origin;
        let types = if let Some(origin) =
            origin.filter(|&origin| origin.ref_(self).flags().intersects(TypeFlags::Union))
        {
            origin
                .ref_(self)
                .as_union_or_intersection_type_interface()
                .types()
                .to_owned()
        } else {
            type_.ref_(self).as_union_type().types().to_owned()
        };
        let mut mapped_types: Vec<Id<Type>> = vec![];
        let mut changed = false;
        for &t in &types {
            let mapped = if t.ref_(self).flags().intersects(TypeFlags::Union) {
                self.try_map_type(t, mapper, Some(no_reductions))?
            } else {
                mapper(t)?
            };
            changed = changed
                || match mapped {
                    None => true,
                    Some(mapped) => t != mapped,
                };
            if let Some(mapped) = mapped {
                mapped_types.push(mapped);
            }
        }
        Ok(if changed {
            if !mapped_types.is_empty() {
                Some(self.get_union_type(
                    &mapped_types,
                    Some(if no_reductions {
                        UnionReduction::None
                    } else {
                        UnionReduction::Literal
                    }),
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?)
            } else {
                None
            }
        } else {
            Some(type_)
        })
    }

    pub(super) fn map_type(
        &self,
        type_: Id<Type>,
        mapper: &mut impl FnMut(Id<Type>) -> Option<Id<Type>>,
        no_reductions: Option<bool>,
    ) -> Option<Id<Type>> {
        self.try_map_type(
            type_,
            &mut |type_: Id<Type>| Ok(mapper(type_)),
            no_reductions,
        )
        .unwrap()
    }

    #[allow(dead_code)]
    pub(super) fn map_type_with_alias(
        &self,
        type_: Id<Type>,
        mapper: &mut impl FnMut(Id<Type>) -> Id<Type>,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> Id<Type> {
        self.try_map_type_with_alias(
            type_,
            &mut |type_: Id<Type>| Ok(mapper(type_)),
            alias_symbol,
            alias_type_arguments,
        )
        .unwrap()
    }

    pub(super) fn try_map_type_with_alias(
        &self,
        type_: Id<Type>,
        mapper: &mut impl FnMut(Id<Type>) -> io::Result<Id<Type>>,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type>> {
        Ok(
            if type_.ref_(self).flags().intersects(TypeFlags::Union) && alias_symbol.is_some() {
                self.get_union_type(
                    &try_map(
                        &{
                            let types = type_.ref_(self).as_union_type().types().to_owned();
                            types
                        },
                        |&type_: &Id<Type>, _| mapper(type_),
                    )?,
                    Some(UnionReduction::Literal),
                    alias_symbol,
                    alias_type_arguments,
                    None,
                )?
            } else {
                self.try_map_type(type_, &mut |type_: Id<Type>| Ok(Some(mapper(type_)?)), None)?
                    .unwrap()
            },
        )
    }

    pub(super) fn get_constituent_count(&self, type_: Id<Type>) -> usize {
        if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            type_
                .ref_(self)
                .as_union_or_intersection_type_interface()
                .types()
                .len()
        } else {
            1
        }
    }

    pub(super) fn extract_types_of_kind(&self, type_: Id<Type>, kind: TypeFlags) -> Id<Type> {
        self.filter_type(type_, |t: Id<Type>| t.ref_(self).flags().intersects(kind))
    }

    pub(super) fn replace_primitives_with_literals(
        &self,
        type_with_primitives: Id<Type>,
        type_with_literals: Id<Type>,
    ) -> Id<Type> {
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
                    &mut |t: Id<Type>| {
                        Some(if t.ref_(self).flags().intersects(TypeFlags::String) {
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
                        } else if t.ref_(self).flags().intersects(TypeFlags::Number) {
                            self.extract_types_of_kind(
                                type_with_literals,
                                TypeFlags::Number | TypeFlags::NumberLiteral,
                            )
                        } else if t.ref_(self).flags().intersects(TypeFlags::BigInt) {
                            self.extract_types_of_kind(
                                type_with_literals,
                                TypeFlags::BigInt | TypeFlags::BigIntLiteral,
                            )
                        } else {
                            t
                        })
                    },
                    None,
                )
                .unwrap();
        }
        type_with_primitives
    }

    pub(super) fn is_incomplete(&self, flow_type: &FlowType) -> bool {
        flow_type.flags(self.arena()) == TypeFlags::None
    }

    pub(super) fn get_type_from_flow_type(&self, flow_type: &FlowType) -> Id<Type> {
        if flow_type.flags(self.arena()) == TypeFlags::None {
            flow_type.as_incomplete_type().type_.clone()
        } else {
            flow_type.as_type().clone()
        }
    }

    pub(super) fn create_flow_type(&self, type_: Id<Type>, incomplete: bool) -> FlowType {
        if incomplete {
            FlowType::IncompleteType(IncompleteType::new(
                TypeFlags::None,
                if type_.ref_(self).flags().intersects(TypeFlags::Never) {
                    self.silent_never_type()
                } else {
                    type_
                },
            ))
        } else {
            FlowType::Type(type_)
        }
    }

    pub(super) fn create_evolving_array_type(
        &self,
        element_type: Id<Type>,
    ) -> Id<Type /*EvolvingArrayType*/> {
        let result =
            self.create_object_type(ObjectFlags::EvolvingArray, Option::<Id<Symbol>>::None);
        self.alloc_type(EvolvingArrayType::new(result, element_type).into())
    }

    pub(super) fn get_evolving_array_type(
        &self,
        element_type: Id<Type>,
    ) -> Id<Type /*EvolvingArrayType*/> {
        self.evolving_array_types()
            .entry(element_type.ref_(self).id())
            .or_insert_with(|| self.create_evolving_array_type(element_type))
            .clone()
    }

    pub(super) fn add_evolving_array_element_type(
        &self,
        evolving_array_type: Id<Type>, /*EvolvingArrayType*/
        node: Id<Node>,                /*Expression*/
    ) -> io::Result<Id<Type /*EvolvingArrayType*/>> {
        let element_type = self.get_regular_type_of_object_literal(
            self.get_base_type_of_literal_type(self.get_context_free_type_of_expression(node)?)?,
        )?;
        Ok(
            if self.is_type_subset_of(
                element_type,
                evolving_array_type
                    .ref_(self)
                    .as_evolving_array_type()
                    .element_type,
            )? {
                evolving_array_type
            } else {
                self.get_evolving_array_type(
                    self.get_union_type(
                        &[
                            evolving_array_type
                                .ref_(self)
                                .as_evolving_array_type()
                                .element_type
                                .clone(),
                            element_type,
                        ],
                        None,
                        Option::<Id<Symbol>>::None,
                        None,
                        None,
                    )?,
                )
            },
        )
    }

    pub(super) fn create_final_array_type(&self, element_type: Id<Type>) -> io::Result<Id<Type>> {
        Ok(
            if element_type.ref_(self).flags().intersects(TypeFlags::Never) {
                self.auto_array_type()
            } else {
                self.create_array_type(
                    if element_type.ref_(self).flags().intersects(TypeFlags::Union) {
                        self.get_union_type(
                            element_type.ref_(self).as_union_type().types(),
                            Some(UnionReduction::Subtype),
                            Option::<Id<Symbol>>::None,
                            None,
                            None,
                        )?
                    } else {
                        element_type
                    },
                    None,
                )
            },
        )
    }

    pub(super) fn get_final_array_type(
        &self,
        evolving_array_type: Id<Type>, /*EvolvingArrayType*/
    ) -> io::Result<Id<Type>> {
        let evolving_array_type_ref = evolving_array_type.ref_(self);
        if evolving_array_type_ref
            .as_evolving_array_type()
            .maybe_final_array_type()
            .is_none()
        {
            evolving_array_type_ref
                .as_evolving_array_type()
                .set_final_array_type(Some(
                    self.create_final_array_type(
                        evolving_array_type
                            .ref_(self)
                            .as_evolving_array_type()
                            .element_type,
                    )?,
                ));
        }
        Ok(evolving_array_type_ref
            .as_evolving_array_type()
            .maybe_final_array_type()
            .unwrap())
    }

    pub(super) fn finalize_evolving_array_type(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        Ok(
            if get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::EvolvingArray) {
                self.get_final_array_type(type_)?
            } else {
                type_
            },
        )
    }

    pub(super) fn get_element_type_of_evolving_array_type(&self, type_: Id<Type>) -> Id<Type> {
        if get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::EvolvingArray) {
            type_
                .ref_(self)
                .as_evolving_array_type()
                .element_type
                .clone()
        } else {
            self.never_type()
        }
    }

    pub(super) fn is_evolving_array_type_list(&self, types: &[Id<Type>]) -> bool {
        let mut has_evolving_array_type = false;
        for &t in types {
            if !t.ref_(self).flags().intersects(TypeFlags::Never) {
                if !get_object_flags(&t.ref_(self)).intersects(ObjectFlags::EvolvingArray) {
                    return false;
                }
                has_evolving_array_type = true;
            }
        }
        has_evolving_array_type
    }

    pub(super) fn is_evolving_array_operation_target(&self, node: Id<Node>) -> io::Result<bool> {
        let root = self.get_reference_root(node);
        let parent = root.ref_(self).parent();
        let is_length_push_or_unshift = is_property_access_expression(&parent.ref_(self)) && {
            let parent_ref = parent.ref_(self);
            let parent_as_property_access_expression = parent_ref.as_property_access_expression();
            parent_as_property_access_expression
                .name
                .ref_(self)
                .as_member_name()
                .escaped_text()
                == "length"
                || parent.ref_(self).parent().ref_(self).kind() == SyntaxKind::CallExpression
                    && is_identifier(&parent_as_property_access_expression.name.ref_(self))
                    && is_push_or_unshift_identifier(
                        &parent_as_property_access_expression.name.ref_(self),
                    )
        };
        let is_element_assignment = parent.ref_(self).kind() == SyntaxKind::ElementAccessExpression
            && {
                let parent_ref = parent.ref_(self);
                let parent_as_element_access_expression = parent_ref.as_element_access_expression();
                let parent_parent = parent.ref_(self).parent();
                parent_as_element_access_expression.expression == root
                    && parent_parent.ref_(self).kind() == SyntaxKind::BinaryExpression
                    && {
                        let parent_parent_ref = parent_parent.ref_(self);
                        let parent_parent_as_binary_expression =
                            parent_parent_ref.as_binary_expression();
                        parent_parent_as_binary_expression
                            .operator_token
                            .ref_(self)
                            .kind()
                            == SyntaxKind::EqualsToken
                            && parent_parent_as_binary_expression.left == parent
                            && !is_assignment_target(parent_parent, self)
                            && self.is_type_assignable_to_kind(
                                self.get_type_of_expression(
                                    parent_as_element_access_expression.argument_expression,
                                )?,
                                TypeFlags::NumberLike,
                                None,
                            )?
                    }
            };
        Ok(is_length_push_or_unshift || is_element_assignment)
    }

    pub(super) fn is_declaration_with_explicit_type_annotation(
        &self,
        node: Id<Node>, /*Declaration*/
    ) -> bool {
        (is_variable_declaration(&node.ref_(self))
            || is_property_declaration(&node.ref_(self))
            || is_property_signature(&node.ref_(self))
            || is_parameter(&node.ref_(self)))
            && (get_effective_type_annotation_node(node, self).is_some()
                || is_in_js_file(Some(&node.ref_(self)))
                    && has_initializer(&node.ref_(self))
                    && matches!(
                        node.ref_(self).as_has_initializer().maybe_initializer(),
                        Some(node_initializer) if is_function_expression_or_arrow_function(&node_initializer.ref_(self))
                          && get_effective_return_type_node(node_initializer, self).is_some()
                    ))
    }

    pub(super) fn get_explicit_type_of_symbol(
        &self,
        symbol: Id<Symbol>,
        diagnostic: Option<&Diagnostic>,
    ) -> io::Result<Option<Id<Type>>> {
        if symbol.ref_(self).flags().intersects(
            SymbolFlags::Function
                | SymbolFlags::Method
                | SymbolFlags::Class
                | SymbolFlags::ValueModule,
        ) {
            return Ok(Some(self.get_type_of_symbol(symbol)?));
        }
        if symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::Variable | SymbolFlags::Property)
        {
            if get_check_flags(&symbol.ref_(self)).intersects(CheckFlags::Mapped) {
                let origin = symbol
                    .ref_(self)
                    .as_mapped_symbol()
                    .symbol_links()
                    .ref_(self)
                    .synthetic_origin;
                if matches!(
                    origin,
                    Some(origin) if self.get_explicit_type_of_symbol(origin, None)?.is_some()
                ) {
                    return Ok(Some(self.get_type_of_symbol(symbol)?));
                }
            }
            let declaration = symbol.ref_(self).maybe_value_declaration();
            if let Some(declaration) = declaration {
                if self.is_declaration_with_explicit_type_annotation(declaration) {
                    return Ok(Some(self.get_type_of_symbol(symbol)?));
                }
                if is_variable_declaration(&declaration.ref_(self))
                    && declaration
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .kind()
                        == SyntaxKind::ForOfStatement
                {
                    let statement = declaration.ref_(self).parent().ref_(self).parent();
                    let statement_ref = statement.ref_(self);
                    let statement_as_for_of_statement = statement_ref.as_for_of_statement();
                    let expression_type = self
                        .get_type_of_dotted_name(statement_as_for_of_statement.expression, None)?;
                    if let Some(expression_type) = expression_type {
                        let use_ = if statement_as_for_of_statement.await_modifier.is_some() {
                            IterationUse::ForAwaitOf
                        } else {
                            IterationUse::ForOf
                        };
                        return Ok(Some(self.check_iterated_type_or_element_type(
                            use_,
                            expression_type,
                            self.undefined_type(),
                            Option::<Id<Node>>::None,
                        )?));
                    }
                }
                if let Some(diagnostic) = diagnostic {
                    add_related_info(
                        diagnostic,
                        vec![self.alloc_diagnostic_related_information(
                            create_diagnostic_for_node(
                                declaration,
                                &Diagnostics::_0_needs_an_explicit_type_annotation,
                                Some(vec![self.symbol_to_string_(
                                    symbol,
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                    None,
                                )?]),
                                self,
                            )
                            .into(),
                        )],
                    );
                }
            }
        }
        Ok(None)
    }

    pub(super) fn get_type_of_dotted_name(
        &self,
        node: Id<Node>, /*Expression*/
        diagnostic: Option<&Diagnostic>,
    ) -> io::Result<Option<Id<Type>>> {
        if !node
            .ref_(self)
            .flags()
            .intersects(NodeFlags::InWithStatement)
        {
            match node.ref_(self).kind() {
                SyntaxKind::Identifier => {
                    let symbol = self
                        .get_export_symbol_of_value_symbol_if_exported(Some(
                            self.get_resolved_symbol(node)?,
                        ))
                        .unwrap();
                    return Ok(self.get_explicit_type_of_symbol(
                        if symbol.ref_(self).flags().intersects(SymbolFlags::Alias) {
                            self.resolve_alias(symbol)?
                        } else {
                            symbol
                        },
                        diagnostic,
                    )?);
                }
                SyntaxKind::ThisKeyword => {
                    return Ok(self.get_explicit_this_type(node)?);
                }
                SyntaxKind::SuperKeyword => {
                    return Ok(Some(self.check_super_expression(node)?));
                }
                SyntaxKind::PropertyAccessExpression => {
                    let node_ref = node.ref_(self);
                    let node_as_property_access_expression =
                        node_ref.as_property_access_expression();
                    let type_ = self.get_type_of_dotted_name(
                        node_as_property_access_expression.expression,
                        diagnostic,
                    )?;
                    if let Some(type_) = type_ {
                        let name = &node_as_property_access_expression.name;
                        let prop: Option<Id<Symbol>>;
                        if is_private_identifier(&name.ref_(self)) {
                            if type_.ref_(self).maybe_symbol().is_none() {
                                return Ok(None);
                            }
                            prop = self.get_property_of_type_(
                                type_,
                                &get_symbol_name_for_private_identifier(
                                    &type_.ref_(self).symbol().ref_(self),
                                    &name.ref_(self).as_private_identifier().escaped_text,
                                ),
                                None,
                            )?;
                        } else {
                            prop = self.get_property_of_type_(
                                type_,
                                &name.ref_(self).as_identifier().escaped_text,
                                None,
                            )?;
                        }
                        return prop.try_and_then(|prop| {
                            self.get_explicit_type_of_symbol(prop, diagnostic)
                        });
                    }
                    return Ok(None);
                }
                SyntaxKind::ParenthesizedExpression => {
                    return Ok(self.get_type_of_dotted_name(
                        node.ref_(self).as_parenthesized_expression().expression,
                        diagnostic,
                    )?);
                }
                _ => (),
            }
        }
        Ok(None)
    }

    pub(super) fn get_effects_signature(
        &self,
        node: Id<Node>, /*CallExpression*/
    ) -> io::Result<Option<Id<Signature>>> {
        let links = self.get_node_links(node);
        let mut signature = links.ref_(self).effects_signature.clone();
        if signature.is_none() {
            let mut func_type: Option<Id<Type>> = None;
            let node_ref = node.ref_(self);
            let node_as_call_expression = node_ref.as_call_expression();
            if node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ExpressionStatement {
                func_type =
                    self.get_type_of_dotted_name(node_as_call_expression.expression, None)?;
            } else if node_as_call_expression.expression.ref_(self).kind()
                != SyntaxKind::SuperKeyword
            {
                if is_optional_chain(&node.ref_(self)) {
                    func_type = Some(self.check_non_null_type(
                        self.get_optional_expression_type(
                            self.check_expression(node_as_call_expression.expression, None, None)?,
                            node_as_call_expression.expression,
                        )?,
                        node_as_call_expression.expression,
                    )?);
                } else {
                    func_type =
                        Some(self.check_non_null_expression(node_as_call_expression.expression)?);
                }
            }
            let signatures = self.get_signatures_of_type(
                func_type
                    .try_map(|func_type| self.get_apparent_type(func_type))?
                    .unwrap_or_else(|| self.unknown_type()),
                SignatureKind::Call,
            )?;
            let candidate = if signatures.len() == 1
                && signatures[0].ref_(self).maybe_type_parameters().is_none()
            {
                Some(signatures[0].clone())
            } else if try_some(
                Some(&signatures),
                Some(|&signature: &Id<Signature>| {
                    self.has_type_predicate_or_never_return_type(signature)
                }),
            )? {
                Some(self.get_resolved_signature_(node, None, None)?)
            } else {
                None
            };
            signature = Some(
                if let Some(candidate) = candidate.try_filter(|&candidate| {
                    self.has_type_predicate_or_never_return_type(candidate)
                })? {
                    candidate
                } else {
                    self.unknown_signature()
                },
            );
            links.ref_mut(self).effects_signature = signature.clone();
        }
        let signature = signature.unwrap();
        Ok(if signature == self.unknown_signature() {
            None
        } else {
            Some(signature)
        })
    }

    pub(super) fn has_type_predicate_or_never_return_type(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<bool> {
        Ok(self.get_type_predicate_of_signature(signature)?.is_some()
            || matches!(
                signature.ref_(self).declaration,
                Some(signature_declaration) if self.get_return_type_from_annotation(
                    signature_declaration
                )?.unwrap_or_else(|| self.unknown_type()).ref_(self).flags().intersects(TypeFlags::Never)
            ))
    }

    pub(super) fn get_type_predicate_argument(
        &self,
        predicate: Id<TypePredicate>,
        call_expression: Id<Node>, /*CallExpression*/
    ) -> Option<Id<Node>> {
        let call_expression_ref = call_expression.ref_(self);
        let call_expression_as_call_expression = call_expression_ref.as_call_expression();
        if matches!(
            predicate.ref_(self).kind,
            TypePredicateKind::Identifier | TypePredicateKind::AssertsIdentifier
        ) {
            return Some(
                call_expression_as_call_expression.arguments.ref_(self)
                    [predicate.ref_(self).parameter_index.unwrap()]
                .clone(),
            );
        }
        let invoked_expression =
            skip_parentheses(call_expression_as_call_expression.expression, None, self);
        if is_access_expression(&invoked_expression.ref_(self)) {
            Some(skip_parentheses(
                invoked_expression
                    .ref_(self)
                    .as_has_expression()
                    .expression(),
                None,
                self,
            ))
        } else {
            None
        }
    }

    pub(super) fn report_flow_control_error(&self, node: Id<Node>) {
        let block = find_ancestor(
            Some(node),
            |ancestor: Id<Node>| is_function_or_module_block(ancestor, self),
            self,
        )
        .unwrap();
        let source_file = get_source_file_of_node(node, self);
        let span = get_span_of_token_at_position(
            &source_file.ref_(self),
            block
                .ref_(self)
                .as_has_statements()
                .statements()
                .ref_(self)
                .pos()
                .try_into()
                .unwrap(),
        );
        self.diagnostics().add(
            self.alloc_diagnostic(
                create_file_diagnostic(
                    source_file,
                    span.start,
                    span.length,
                    &Diagnostics::The_containing_function_or_module_body_is_too_large_for_control_flow_analysis,
                    None,
                ).into()
            )
        );
    }

    pub(super) fn is_reachable_flow_node(&self, flow: Id<FlowNode>) -> io::Result<bool> {
        let result = self.is_reachable_flow_node_worker(flow.clone(), false)?;
        self.set_last_flow_node(Some(flow));
        self.set_last_flow_node_reachable(result);
        Ok(result)
    }

    pub(super) fn is_false_expression(&self, expr: Id<Node> /*Expression*/) -> bool {
        let node = skip_parentheses(expr, Some(true), self);
        node.ref_(self).kind() == SyntaxKind::FalseKeyword
            || node.ref_(self).kind() == SyntaxKind::BinaryExpression && {
                let node_ref = node.ref_(self);
                let node_as_binary_expression = node_ref.as_binary_expression();
                node_as_binary_expression.operator_token.ref_(self).kind()
                    == SyntaxKind::AmpersandAmpersandToken
                    && (self.is_false_expression(node_as_binary_expression.left)
                        || self.is_false_expression(node_as_binary_expression.right))
                    || node_as_binary_expression.operator_token.ref_(self).kind()
                        == SyntaxKind::BarBarToken
                        && (self.is_false_expression(node_as_binary_expression.left)
                            && self.is_false_expression(node_as_binary_expression.right))
            }
    }

    pub(super) fn is_reachable_flow_node_worker(
        &self,
        mut flow: Id<FlowNode>,
        mut no_cache_check: bool,
    ) -> io::Result<bool> {
        loop {
            if self.maybe_last_flow_node() == Some(flow) {
                return Ok(self.last_flow_node_reachable());
            }
            let flags = flow.ref_(self).flags();
            if flags.intersects(FlowFlags::Shared) {
                if !no_cache_check {
                    let id = self.get_flow_node_id(&flow.ref_(self));
                    let reachable = self.flow_node_reachable().get(&id).copied();
                    return Ok(if let Some(reachable) = reachable {
                        reachable
                    } else {
                        let ret = self.is_reachable_flow_node_worker(flow.clone(), true)?;
                        self.flow_node_reachable().insert(id, ret);
                        ret
                    });
                }
                no_cache_check = false;
            }
            if flags
                .intersects(FlowFlags::Assignment | FlowFlags::Condition | FlowFlags::ArrayMutation)
            {
                flow = flow.ref_(self).as_has_antecedent().antecedent();
            } else if flags.intersects(FlowFlags::Call) {
                let flow_ref = flow.ref_(self);
                let flow_as_flow_call = flow_ref.as_flow_call();
                let signature = self.get_effects_signature(flow_as_flow_call.node)?;
                if let Some(signature) = signature {
                    let predicate = self.get_type_predicate_of_signature(signature)?;
                    if let Some(predicate) = predicate.as_ref().filter(|predicate| {
                        predicate.ref_(self).kind == TypePredicateKind::AssertsIdentifier
                            && predicate.ref_(self).type_.is_none()
                    }) {
                        let predicate_argument = flow_as_flow_call
                            .node
                            .ref_(self)
                            .as_call_expression()
                            .arguments
                            .ref_(self)[predicate.ref_(self).parameter_index.unwrap()];
                        if
                        /*predicateArgument &&*/
                        self.is_false_expression(predicate_argument) {
                            return Ok(false);
                        }
                    }
                    if self
                        .get_return_type_of_signature(signature.clone())?
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::Never)
                    {
                        return Ok(false);
                    }
                }
                flow = flow_as_flow_call.antecedent.clone();
            } else if flags.intersects(FlowFlags::BranchLabel) {
                return try_some(
                    flow.ref_(self)
                        .as_flow_label()
                        .maybe_antecedents()
                        .as_deref(),
                    Some(|f: &Id<FlowNode>| self.is_reachable_flow_node_worker(f.clone(), false)),
                );
            } else if flags.intersects(FlowFlags::LoopLabel) {
                let antecedents = flow.ref_(self).as_flow_label().maybe_antecedents().clone();
                if antecedents.is_none() {
                    return Ok(false);
                }
                let antecedents = antecedents.unwrap();
                if antecedents.is_empty() {
                    return Ok(false);
                }
                flow = antecedents[0].clone();
            } else if flags.intersects(FlowFlags::SwitchClause) {
                let flow_ref = flow.ref_(self);
                let flow_as_flow_switch_clause = flow_ref.as_flow_switch_clause();
                if flow_as_flow_switch_clause.clause_start == flow_as_flow_switch_clause.clause_end
                    && self.is_exhaustive_switch_statement(
                        flow_as_flow_switch_clause.switch_statement,
                    )?
                {
                    return Ok(false);
                }
                flow = flow_as_flow_switch_clause.antecedent.clone();
            } else if flags.intersects(FlowFlags::ReduceLabel) {
                self.set_last_flow_node(None);
                let flow_ref = flow.ref_(self);
                let flow_as_flow_reduce_label = flow_ref.as_flow_reduce_label();
                let target = &flow_as_flow_reduce_label.target;
                let target_ref = target.ref_(self);
                let target_as_flow_label = target_ref.as_flow_label();
                let save_antecedents = target_as_flow_label.maybe_antecedents().clone();
                *target_as_flow_label.maybe_antecedents_mut() =
                    Some(flow_as_flow_reduce_label.antecedents.clone());
                let result = self.is_reachable_flow_node_worker(
                    flow_as_flow_reduce_label.antecedent.clone(),
                    false,
                )?;
                *target_as_flow_label.maybe_antecedents_mut() = save_antecedents;
                return Ok(result);
            } else {
                return Ok(!flags.intersects(FlowFlags::Unreachable));
            }
        }
    }
}
