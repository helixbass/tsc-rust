use std::{io, ptr};

use gc::Gc;
use id_arena::Id;

use super::{CheckMode, GetFlowTypeOfReference, TypeFacts};
use crate::{
    are_option_gcs_equal, escape_leading_underscores, find_ancestor, get_assignment_target_kind,
    get_containing_class, get_containing_function, get_immediately_invoked_function_expression,
    get_name_of_declaration, get_object_flags, get_root_declaration, get_this_container,
    has_syntactic_modifier, is_access_expression, is_assignment_target, is_binary_expression,
    is_binding_element, is_call_chain, is_catch_clause, is_class_static_block_declaration,
    is_declaration_name, is_element_access_expression, is_entity_name_expression,
    is_export_assignment, is_export_specifier, is_expression_node,
    is_expression_of_optional_chain_root, is_function_like, is_identifier, is_in_js_file,
    is_jsx_opening_element, is_jsx_self_closing_element,
    is_object_literal_or_class_expression_method_or_accessor,
    is_parameter_or_catch_clause_variable, is_property_access_expression, is_property_declaration,
    is_right_side_of_qualified_name_or_property_access, is_set_accessor, is_spread_assignment,
    is_static, is_string_literal_like, is_variable_declaration, is_write_access, node_is_decorated,
    should_preserve_const_enums, try_find, try_for_each_child, try_map, AssignmentKind,
    ContextFlags, Diagnostics, FindAncestorCallbackReturn, HasInitializerInterface,
    HasTypeInterface, ModifierFlags, Node, NodeArray, NodeCheckFlags, NodeFlags, NodeInterface,
    ObjectFlags, OptionTry, ScriptTarget, Signature, SignatureKind, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, TypePredicate,
    TypePredicateKind, TypeSystemPropertyName, UnionOrIntersectionTypeInterface, HasArena, InArena,
};

impl GetFlowTypeOfReference {
    pub(super) fn narrow_type_by_constructor(
        &self,
        type_: Id<Type>,
        operator: SyntaxKind,
        identifier: &Node, /*Expression*/
        assume_true: bool,
    ) -> io::Result<Id<Type>> {
        if if assume_true {
            !matches!(
                operator,
                SyntaxKind::EqualsEqualsToken | SyntaxKind::EqualsEqualsEqualsToken
            )
        } else {
            !matches!(
                operator,
                SyntaxKind::ExclamationEqualsToken | SyntaxKind::ExclamationEqualsEqualsToken
            )
        } {
            return Ok(type_);
        }

        let identifier_type = self.type_checker.get_type_of_expression(identifier)?;
        if !self.type_checker.is_function_type(identifier_type)?
            && !self.type_checker.is_constructor_type(identifier_type)?
        {
            return Ok(type_);
        }

        let prototype_property =
            self.type_checker
                .get_property_of_type_(identifier_type, "prototype", None)?;
        if prototype_property.is_none() {
            return Ok(type_);
        }
        let prototype_property = prototype_property.unwrap();

        let prototype_type = self.type_checker.get_type_of_symbol(prototype_property)?;
        let candidate = if !self.type_checker.is_type_any(Some(prototype_type)) {
            Some(prototype_type)
        } else {
            None
        };
        if candidate.is_none() {
            return Ok(type_);
        }
        let candidate = candidate.unwrap();
        if candidate == self.type_checker.global_object_type()
            || candidate == self.type_checker.global_function_type()
        {
            return Ok(type_);
        }

        if self.type_checker.is_type_any(Some(type_)) {
            return Ok(candidate);
        }

        let is_constructed_by = |source: Id<Type>, target: Id<Type>| {
            if self
                .type_checker
                .type_(source)
                .flags()
                .intersects(TypeFlags::Object)
                && get_object_flags(&self.type_checker.type_(source)).intersects(ObjectFlags::Class)
                || self
                    .type_checker
                    .type_(target)
                    .flags()
                    .intersects(TypeFlags::Object)
                    && get_object_flags(&self.type_checker.type_(target))
                        .intersects(ObjectFlags::Class)
            {
                return Ok(self.type_checker.type_(source).maybe_symbol()
                    == self.type_checker.type_(target).maybe_symbol());
            }

            self.type_checker.is_type_subtype_of(source, target)
        };

        self.type_checker
            .try_filter_type(type_, |t: Id<Type>| is_constructed_by(t, candidate))
    }

    pub(super) fn narrow_type_by_instanceof(
        &self,
        type_: Id<Type>,
        expr: &Node, /*BinaryExpression*/
        assume_true: bool,
    ) -> io::Result<Id<Type>> {
        let expr_as_binary_expression = expr.as_binary_expression();
        let left = self
            .type_checker
            .get_reference_candidate(&expr_as_binary_expression.left);
        if !self
            .type_checker
            .is_matching_reference(&self.reference, &left)?
        {
            if assume_true
                && self.type_checker.strict_null_checks
                && self
                    .type_checker
                    .optional_chain_contains_reference(&left, &self.reference)?
            {
                return self
                    .type_checker
                    .get_type_with_facts(type_, TypeFacts::NEUndefinedOrNull);
            }
            return Ok(type_);
        }

        let right_type = self
            .type_checker
            .get_type_of_expression(&expr_as_binary_expression.right)?;
        if !self
            .type_checker
            .is_type_derived_from(right_type, self.type_checker.global_function_type())?
        {
            return Ok(type_);
        }

        let mut target_type: Option<Id<Type>> = None;
        let prototype_property =
            self.type_checker
                .get_property_of_type_(right_type, "prototype", None)?;
        if let Some(prototype_property) = prototype_property {
            let prototype_property_type =
                self.type_checker.get_type_of_symbol(prototype_property)?;
            if !self.type_checker.is_type_any(Some(prototype_property_type)) {
                target_type = Some(prototype_property_type);
            }
        }

        if self.type_checker.is_type_any(Some(type_))
            && matches!(
                target_type,
                Some(target_type) if
                    target_type == self.type_checker.global_object_type()
                        || target_type == self.type_checker.global_function_type()
            )
        {
            return Ok(type_);
        }

        if target_type.is_none() {
            let construct_signatures = self
                .type_checker
                .get_signatures_of_type(right_type, SignatureKind::Construct)?;
            target_type = Some(if !construct_signatures.is_empty() {
                self.type_checker.get_union_type(
                    &try_map(&construct_signatures, |signature: &Gc<Signature>, _| {
                        self.type_checker.get_return_type_of_signature(
                            self.type_checker.get_erased_signature(signature.clone())?,
                        )
                    })?,
                    None,
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?
            } else {
                self.type_checker.empty_object_type()
            });
        }
        let target_type = target_type.unwrap();

        if !assume_true
            && self
                .type_checker
                .type_(right_type)
                .flags()
                .intersects(TypeFlags::Union)
        {
            let non_constructor_type_in_union = try_find(
                &self.type_checker.type_(right_type).as_union_type().types(),
                |t: &Id<Type>, _| -> io::Result<_> {
                    Ok(!self.type_checker.is_constructor_type(*t)?)
                },
            )?
            .cloned();
            if non_constructor_type_in_union.is_none() {
                return Ok(type_);
            }
        }

        self.get_narrowed_type(
            type_,
            target_type,
            assume_true,
            |source: Id<Type>, target: Id<Type>| {
                self.type_checker.is_type_derived_from(source, target)
            },
        )
    }

    pub(super) fn get_narrowed_type(
        &self,
        type_: Id<Type>,
        candidate: Id<Type>,
        assume_true: bool,
        mut is_related: impl FnMut(Id<Type>, Id<Type>) -> io::Result<bool>,
    ) -> io::Result<Id<Type>> {
        if !assume_true {
            return self
                .type_checker
                .try_filter_type(type_, |t: Id<Type>| Ok(!is_related(t, candidate)?));
        }
        if self
            .type_checker
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Union)
        {
            let assignable_type = self
                .type_checker
                .try_filter_type(type_, |t: Id<Type>| is_related(t, candidate))?;
            if !self
                .type_checker
                .type_(assignable_type)
                .flags()
                .intersects(TypeFlags::Never)
            {
                return Ok(assignable_type);
            }
        }

        Ok(if self.type_checker.is_type_subtype_of(candidate, type_)? {
            candidate
        } else if self.type_checker.is_type_assignable_to(type_, candidate)? {
            type_
        } else if self.type_checker.is_type_assignable_to(candidate, type_)? {
            candidate
        } else {
            self.type_checker.get_intersection_type(
                &vec![type_, candidate],
                Option::<Id<Symbol>>::None,
                None,
            )?
        })
    }

    pub(super) fn narrow_type_by_call_expression(
        &self,
        type_: Id<Type>,
        call_expression: &Node, /*CallExpression*/
        assume_true: bool,
    ) -> io::Result<Id<Type>> {
        if self
            .type_checker
            .has_matching_argument(call_expression, &self.reference)?
        {
            let signature = if assume_true || !is_call_chain(call_expression) {
                self.type_checker.get_effects_signature(call_expression)?
            } else {
                None
            };
            let predicate = signature.as_ref().try_and_then(|signature| {
                self.type_checker.get_type_predicate_of_signature(signature)
            })?;
            if let Some(predicate) = predicate.as_ref().filter(|predicate| {
                matches!(
                    predicate.kind,
                    TypePredicateKind::This | TypePredicateKind::Identifier
                )
            }) {
                return self.narrow_type_by_type_predicate(
                    type_,
                    predicate,
                    call_expression,
                    assume_true,
                );
            }
        }
        let call_expression_as_call_expression = call_expression.as_call_expression();
        if self.type_checker.contains_missing_type(type_)
            && is_access_expression(&self.reference)
            && is_property_access_expression(&call_expression_as_call_expression.expression)
        {
            let call_access = &call_expression_as_call_expression.expression;
            let call_access_as_property_access_expression =
                call_access.as_property_access_expression();
            if self.type_checker.is_matching_reference(
                &self.reference.as_has_expression().expression(),
                &self
                    .type_checker
                    .get_reference_candidate(&call_access_as_property_access_expression.expression),
            )? && is_identifier(&call_access_as_property_access_expression.name)
                && call_access_as_property_access_expression
                    .name
                    .as_identifier()
                    .escaped_text
                    == "hasOwnProperty"
                && call_expression_as_call_expression.arguments.len() == 1
            {
                let argument = &call_expression_as_call_expression.arguments[0];
                if is_string_literal_like(argument)
                    && matches!(
                        self.type_checker.get_accessed_property_name(&self.reference)?.as_ref(),
                        Some(accessed_property_name) if accessed_property_name == &escape_leading_underscores(&argument.as_literal_like_node().text())
                    )
                {
                    return self.type_checker.get_type_with_facts(
                        type_,
                        if assume_true {
                            TypeFacts::NEUndefined
                        } else {
                            TypeFacts::EQUndefined
                        },
                    );
                }
            }
        }
        Ok(type_)
    }

    pub(super) fn narrow_type_by_type_predicate(
        &self,
        mut type_: Id<Type>,
        predicate: &TypePredicate,
        call_expression: &Node, /*CallExpression*/
        assume_true: bool,
    ) -> io::Result<Id<Type>> {
        if let Some(predicate_type) = predicate.type_.filter(|&predicate_type| {
            !(self.type_checker.is_type_any(Some(type_))
                && (predicate_type == self.type_checker.global_object_type()
                    || predicate_type == self.type_checker.global_function_type()))
        }) {
            let predicate_argument = self
                .type_checker
                .get_type_predicate_argument(predicate, call_expression);
            if let Some(predicate_argument) = predicate_argument.as_ref() {
                if self
                    .type_checker
                    .is_matching_reference(&self.reference, predicate_argument)?
                {
                    return self.get_narrowed_type(
                        type_,
                        predicate_type,
                        assume_true,
                        |type1: Id<Type>, type2: Id<Type>| {
                            self.type_checker.is_type_subtype_of(type1, type2)
                        },
                    );
                }
                if self.type_checker.strict_null_checks
                    && assume_true
                    && self
                        .type_checker
                        .optional_chain_contains_reference(predicate_argument, &self.reference)?
                    && !self
                        .type_checker
                        .get_type_facts(predicate_type, None)?
                        .intersects(TypeFacts::EQUndefined)
                {
                    type_ = self
                        .type_checker
                        .get_type_with_facts(type_, TypeFacts::NEUndefinedOrNull)?;
                }
                let access = self.get_discriminant_property_access(predicate_argument, type_)?;
                if let Some(access) = access.as_ref() {
                    return self.try_narrow_type_by_discriminant(type_, access, |t: Id<Type>| {
                        self.get_narrowed_type(
                            t,
                            predicate_type,
                            assume_true,
                            |type1: Id<Type>, type2: Id<Type>| {
                                self.type_checker.is_type_subtype_of(type1, type2)
                            },
                        )
                    });
                }
            }
        }
        Ok(type_)
    }

    pub(super) fn narrow_type(
        &self,
        type_: Id<Type>,
        expr: &Node, /*Expression*/
        assume_true: bool,
    ) -> io::Result<Id<Type>> {
        if is_expression_of_optional_chain_root(expr) || {
            let expr_parent = expr.parent();
            is_binary_expression(&expr_parent) && {
                let expr_parent_as_binary_expression = expr_parent.as_binary_expression();
                expr_parent_as_binary_expression.operator_token.kind()
                    == SyntaxKind::QuestionQuestionToken
                    && ptr::eq(&*expr_parent_as_binary_expression.left, expr)
            }
        } {
            return self.narrow_type_by_optionality(type_, expr, assume_true);
        }
        match expr.kind() {
            SyntaxKind::Identifier => {
                if !self
                    .type_checker
                    .is_matching_reference(&self.reference, expr)?
                    && self.type_checker.inline_level() < 5
                {
                    let symbol = self.type_checker.get_resolved_symbol(expr)?;
                    if self.type_checker.is_const_variable(symbol) {
                        let declaration =
                            self.type_checker.symbol(symbol).maybe_value_declaration();
                        if let Some(declaration) =
                            declaration
                                .as_ref()
                                .try_filter(|declaration| -> io::Result<_> {
                                    Ok(is_variable_declaration(declaration) && {
                                        let declaration_as_variable_declaration =
                                            declaration.as_variable_declaration();
                                        declaration_as_variable_declaration.maybe_type().is_none()
                                            && declaration_as_variable_declaration
                                                .maybe_initializer()
                                                .is_some()
                                            && self
                                                .type_checker
                                                .is_constant_reference(&self.reference)?
                                    })
                                })?
                        {
                            self.type_checker
                                .set_inline_level(self.type_checker.inline_level() + 1);
                            let result = self.narrow_type(
                                type_,
                                &declaration
                                    .as_has_initializer()
                                    .maybe_initializer()
                                    .unwrap(),
                                assume_true,
                            )?;
                            self.type_checker
                                .set_inline_level(self.type_checker.inline_level() - 1);
                            return Ok(result);
                        }
                    }
                }
                return self.narrow_type_by_truthiness(type_, expr, assume_true);
            }
            SyntaxKind::ThisKeyword
            | SyntaxKind::SuperKeyword
            | SyntaxKind::PropertyAccessExpression
            | SyntaxKind::ElementAccessExpression => {
                return self.narrow_type_by_truthiness(type_, expr, assume_true);
            }
            SyntaxKind::CallExpression => {
                return self.narrow_type_by_call_expression(type_, expr, assume_true);
            }
            SyntaxKind::ParenthesizedExpression | SyntaxKind::NonNullExpression => {
                return self.narrow_type(
                    type_,
                    &expr.as_has_expression().expression(),
                    assume_true,
                );
            }
            SyntaxKind::BinaryExpression => {
                return self.narrow_type_by_binary_expression(type_, expr, assume_true);
            }
            SyntaxKind::PrefixUnaryExpression => {
                let expr_as_prefix_unary_expression = expr.as_prefix_unary_expression();
                if expr_as_prefix_unary_expression.operator == SyntaxKind::ExclamationToken {
                    return self.narrow_type(
                        type_,
                        &expr_as_prefix_unary_expression.operand,
                        !assume_true,
                    );
                }
            }
            _ => (),
        }
        Ok(type_)
    }

    pub(super) fn narrow_type_by_optionality(
        &self,
        type_: Id<Type>,
        expr: &Node, /*Expression*/
        assume_present: bool,
    ) -> io::Result<Id<Type>> {
        if self
            .type_checker
            .is_matching_reference(&self.reference, expr)?
        {
            return self.type_checker.get_type_with_facts(
                type_,
                if assume_present {
                    TypeFacts::NEUndefinedOrNull
                } else {
                    TypeFacts::EQUndefinedOrNull
                },
            );
        }
        let access = self.get_discriminant_property_access(expr, type_)?;
        if let Some(access) = access.as_ref() {
            return self.try_narrow_type_by_discriminant(type_, access, |t: Id<Type>| {
                self.type_checker.get_type_with_facts(
                    t,
                    if assume_present {
                        TypeFacts::NEUndefinedOrNull
                    } else {
                        TypeFacts::EQUndefinedOrNull
                    },
                )
            });
        }
        Ok(type_)
    }
}

impl TypeChecker {
    pub(super) fn get_type_of_symbol_at_location_(
        &self,
        symbol: Id<Symbol>,
        location: &Node,
    ) -> io::Result<Id<Type>> {
        let symbol = self
            .symbol(symbol)
            .maybe_export_symbol()
            .unwrap_or_else(|| symbol);

        let mut location = location.node_wrapper();
        if matches!(
            location.kind(),
            SyntaxKind::Identifier | SyntaxKind::PrivateIdentifier
        ) {
            if is_right_side_of_qualified_name_or_property_access(&location) {
                location = location.parent();
            }
            if is_expression_node(&location)
                && (!is_assignment_target(&location) || is_write_access(&location))
            {
                let type_ = self.get_type_of_expression(&location)?;
                if matches!(
                    self.get_export_symbol_of_value_symbol_if_exported(
                        (*self.get_node_links(&location)).borrow().resolved_symbol.clone()
                    ),
                    Some(export_symbol) if export_symbol == symbol
                ) {
                    return Ok(type_);
                }
            }
        }
        if is_declaration_name(&location)
            && is_set_accessor(&location.parent())
            && self
                .get_annotated_accessor_type_node(location.maybe_parent())
                .is_some()
        {
            return Ok(self
                .resolve_type_of_accessors(location.parent().symbol(), Some(true))?
                .unwrap());
        }
        self.get_non_missing_type_of_symbol(symbol)
    }

    pub(super) fn maybe_get_control_flow_container(&self, node: &Node) -> Option<Gc<Node>> {
        find_ancestor(node.maybe_parent(), |node: &Node| {
            is_function_like(Some(node))
                && get_immediately_invoked_function_expression(node).is_none()
                || matches!(
                    node.kind(),
                    SyntaxKind::ModuleBlock
                        | SyntaxKind::SourceFile
                        | SyntaxKind::PropertyDeclaration
                )
        })
    }

    pub(super) fn get_control_flow_container(&self, node: &Node) -> Gc<Node> {
        self.maybe_get_control_flow_container(node).unwrap()
    }

    pub(super) fn is_symbol_assigned(&self, symbol: Id<Symbol>) -> io::Result<bool> {
        let symbol_value_declaration = self.symbol(symbol).maybe_value_declaration();
        if symbol_value_declaration.is_none() {
            return Ok(false);
        }
        let symbol_value_declaration = symbol_value_declaration.unwrap();
        let parent = get_root_declaration(&symbol_value_declaration).parent();
        let links = self.get_node_links(&parent);
        if !(*links)
            .borrow()
            .flags
            .intersects(NodeCheckFlags::AssignmentsMarked)
        {
            {
                let mut links = links.borrow_mut();
                links.flags = links.flags | NodeCheckFlags::AssignmentsMarked;
            }
            if !self.has_parent_with_assignments_marked(&parent) {
                self.mark_node_assignments(&parent)?;
            }
        }
        Ok(self.symbol(symbol).maybe_is_assigned().unwrap_or(false))
    }

    pub(super) fn has_parent_with_assignments_marked(&self, node: &Node) -> bool {
        find_ancestor(node.maybe_parent(), |node: &Node| {
            (is_function_like(Some(node)) || is_catch_clause(node))
                && (*self.get_node_links(node))
                    .borrow()
                    .flags
                    .intersects(NodeCheckFlags::AssignmentsMarked)
        })
        .is_some()
    }

    pub(super) fn mark_node_assignments(&self, node: &Node) -> io::Result<()> {
        if node.kind() == SyntaxKind::Identifier {
            if is_assignment_target(node) {
                let symbol = self.get_resolved_symbol(node)?;
                if is_parameter_or_catch_clause_variable(&self.symbol(symbol)) {
                    self.symbol(symbol).set_is_assigned(Some(true));
                }
            }
        } else {
            try_for_each_child(
                node,
                |node: &Node| self.mark_node_assignments(node),
                Option::<fn(&NodeArray) -> io::Result<()>>::None,
            )?;
        }

        Ok(())
    }

    pub(super) fn is_const_variable(&self, symbol: Id<Symbol>) -> bool {
        self.symbol(symbol)
            .flags()
            .intersects(SymbolFlags::Variable)
            && self
                .get_declaration_node_flags_from_symbol(symbol)
                .intersects(NodeFlags::Const)
    }

    pub(super) fn remove_optionality_from_declared_type(
        &self,
        declared_type: Id<Type>,
        declaration: &Node, /*VariableLikeDeclaration (actually also includes BindingElement)*/
    ) -> io::Result<Id<Type>> {
        let declaration_as_has_initializer = declaration.as_has_initializer();
        Ok(
            if self.push_type_resolution(
                &declaration.symbol().into(),
                TypeSystemPropertyName::DeclaredType,
            ) {
                let annotation_includes_undefined = self.strict_null_checks
                    && declaration.kind() == SyntaxKind::Parameter
                    && matches!(
                        declaration_as_has_initializer.maybe_initializer().as_ref(),
                        Some(declaration_initializer) if self.get_falsy_flags(declared_type).intersects(TypeFlags::Undefined) &&
                            !self.get_falsy_flags(self.check_expression(declaration_initializer, None, None)?).intersects(TypeFlags::Undefined)
                    );
                self.pop_type_resolution();

                if annotation_includes_undefined {
                    self.get_type_with_facts(declared_type, TypeFacts::NEUndefined)?
                } else {
                    declared_type
                }
            } else {
                self.report_circularity_error(declaration.symbol())?;
                declared_type
            },
        )
    }

    pub(super) fn is_constraint_position(&self, type_: Id<Type>, node: &Node) -> io::Result<bool> {
        let parent = node.parent();
        Ok(parent.kind() == SyntaxKind::PropertyAccessExpression
            || parent.kind() == SyntaxKind::CallExpression
                && ptr::eq(&*parent.as_call_expression().expression, node)
            || parent.kind() == SyntaxKind::ElementAccessExpression
                && ptr::eq(&*parent.as_element_access_expression().expression, node)
                && !(self.try_some_type(type_, |type_: Id<Type>| {
                    self.is_generic_type_without_nullable_constraint(type_)
                })? && self.is_generic_index_type(self.get_type_of_expression(
                    &parent.as_element_access_expression().argument_expression,
                )?)?))
    }

    pub(super) fn is_generic_type_with_union_constraint(
        &self,
        type_: Id<Type>,
    ) -> io::Result<bool> {
        Ok(self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Instantiable)
            && self
                .type_(self.get_base_constraint_or_type(type_)?)
                .flags()
                .intersects(TypeFlags::Nullable | TypeFlags::Union))
    }

    pub(super) fn is_generic_type_without_nullable_constraint(
        &self,
        type_: Id<Type>,
    ) -> io::Result<bool> {
        Ok(self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Instantiable)
            && !self.maybe_type_of_kind(
                self.get_base_constraint_or_type(type_)?,
                TypeFlags::Nullable,
            ))
    }

    pub(super) fn has_non_binding_pattern_contextual_type_with_no_generic_types(
        &self,
        node: &Node,
    ) -> io::Result<bool> {
        let contextual_type = if (is_identifier(node)
            || is_property_access_expression(node)
            || is_element_access_expression(node))
            && !((is_jsx_opening_element(&node.parent())
                || is_jsx_self_closing_element(&node.parent()))
                && ptr::eq(
                    &*node.parent().as_jsx_opening_like_element().tag_name(),
                    node,
                )) {
            self.get_contextual_type_(node, Some(ContextFlags::SkipBindingPatterns))?
        } else {
            None
        };
        Ok(matches!(
            contextual_type,
            Some(contextual_type) if !self.is_generic_type(contextual_type)?
        ))
    }

    pub(super) fn get_narrowable_type_for_reference(
        &self,
        type_: Id<Type>,
        reference: &Node,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let substitute_constraints = !matches!(
            check_mode,
            Some(check_mode) if check_mode.intersects(CheckMode::Inferential)
        ) && self.try_some_type(type_, |type_: Id<Type>| {
            self.is_generic_type_with_union_constraint(type_)
        })? && (self.is_constraint_position(type_, reference)?
            || self.has_non_binding_pattern_contextual_type_with_no_generic_types(reference)?);
        Ok(if substitute_constraints {
            self.try_map_type(
                type_,
                &mut |t: Id<Type>| {
                    Ok(Some(
                        if t.ref_(self).flags().intersects(TypeFlags::Instantiable) {
                            self.get_base_constraint_or_type(t)?
                        } else {
                            t
                        },
                    ))
                },
                None,
            )?
            .unwrap()
        } else {
            type_
        })
    }

    pub(super) fn is_export_or_export_expression(&self, location: &Node) -> bool {
        find_ancestor(Some(location), |n: &Node| {
            let parent = n.maybe_parent();
            if parent.is_none() {
                return FindAncestorCallbackReturn::Quit;
            }
            let parent = parent.unwrap();
            if is_export_assignment(&parent) {
                return (ptr::eq(&*parent.as_export_assignment().expression, n)
                    && is_entity_name_expression(n))
                .into();
            }
            if is_export_specifier(&parent) {
                let parent_as_export_specifier = parent.as_export_specifier();
                return (ptr::eq(&*parent_as_export_specifier.name, n)
                    || matches!(
                        parent_as_export_specifier.property_name.as_deref(),
                        Some(parent_property_name) if ptr::eq(
                            parent_property_name,
                            n
                        )
                    ))
                .into();
            }
            false.into()
        })
        .is_some()
    }

    pub(super) fn mark_alias_referenced(
        &self,
        symbol: Id<Symbol>,
        location: &Node,
    ) -> io::Result<()> {
        if self.is_non_local_alias(Some(symbol), Some(SymbolFlags::Value))
            && !self.is_in_type_query(location)
            && self.get_type_only_alias_declaration(symbol).is_none()
        {
            let target = self.resolve_alias(symbol)?;
            if self.symbol(target).flags().intersects(SymbolFlags::Value) {
                if self.compiler_options.isolated_modules == Some(true)
                    || should_preserve_const_enums(&self.compiler_options)
                        && self.is_export_or_export_expression(location)
                    || !self.is_const_enum_or_const_enum_only_module(target)
                {
                    self.mark_alias_symbol_as_referenced(symbol)?;
                } else {
                    self.mark_const_enum_alias_as_referenced(symbol);
                }
            }
        }

        Ok(())
    }

    pub(super) fn check_identifier(
        &self,
        node: &Node, /*Identifier*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let symbol = self.get_resolved_symbol(node)?;
        if symbol == self.unknown_symbol() {
            return Ok(self.error_type());
        }

        if symbol == self.arguments_symbol() {
            if self.is_in_property_initializer_or_class_static_block(node) {
                self.error(
                    Some(node),
                    &Diagnostics::arguments_cannot_be_referenced_in_property_initializers,
                    None,
                );
                return Ok(self.error_type());
            }

            let container = get_containing_function(node).unwrap();
            if self.language_version < ScriptTarget::ES2015 {
                if container.kind() == SyntaxKind::ArrowFunction {
                    self.error(
                        Some(node),
                        &Diagnostics::The_arguments_object_cannot_be_referenced_in_an_arrow_function_in_ES3_and_ES5_Consider_using_a_standard_function_expression,
                        None,
                    );
                } else if has_syntactic_modifier(&container, ModifierFlags::Async) {
                    self.error(
                        Some(node),
                        &Diagnostics::The_arguments_object_cannot_be_referenced_in_an_async_function_or_method_in_ES3_and_ES5_Consider_using_a_standard_function_or_method,
                        None,
                    );
                }
            }

            self.get_node_links(&container).borrow_mut().flags |= NodeCheckFlags::CaptureArguments;
            return self.get_type_of_symbol(symbol);
        }

        if !matches!(
            node.maybe_parent().as_ref(),
            Some(node_parent) if is_property_access_expression(node_parent) && ptr::eq(
                &*node_parent.as_property_access_expression().expression,
                node
            )
        ) {
            self.mark_alias_referenced(symbol, node)?;
        }

        let local_or_export_symbol = self
            .get_export_symbol_of_value_symbol_if_exported(Some(symbol))
            .unwrap();
        let source_symbol = if self
            .symbol(local_or_export_symbol)
            .flags()
            .intersects(SymbolFlags::Alias)
        {
            self.resolve_alias(local_or_export_symbol)?
        } else {
            local_or_export_symbol.clone()
        };
        if let Some(source_symbol_declarations) =
            self.symbol(source_symbol).maybe_declarations().as_ref()
        {
            if self
                .get_declaration_node_flags_from_symbol(source_symbol)
                .intersects(NodeFlags::Deprecated)
                && self.is_uncalled_function_reference(node, source_symbol)?
            {
                self.add_deprecated_suggestion(
                    node,
                    source_symbol_declarations,
                    &*node.as_identifier().escaped_text,
                );
            }
        }

        let mut declaration = self
            .symbol(local_or_export_symbol)
            .maybe_value_declaration();
        if let Some(declaration) = declaration.as_ref() {
            if self
                .symbol(local_or_export_symbol)
                .flags()
                .intersects(SymbolFlags::Class)
            {
                if declaration.kind() == SyntaxKind::ClassDeclaration
                    && node_is_decorated(declaration, Option::<&Node>::None, Option::<&Node>::None)
                {
                    let mut container = get_containing_class(node);
                    while let Some(container_present) = container.as_ref() {
                        if Gc::ptr_eq(container_present, declaration)
                            && !matches!(
                                container_present.as_class_like_declaration().maybe_name().as_deref(),
                                Some(container_name) if ptr::eq(
                                    container_name,
                                    node
                                )
                            )
                        {
                            self.get_node_links(declaration).borrow_mut().flags |=
                                NodeCheckFlags::ClassWithConstructorReference;
                            self.get_node_links(node).borrow_mut().flags |=
                                NodeCheckFlags::ConstructorReferenceInClass;
                            break;
                        }

                        container = get_containing_class(container_present);
                    }
                } else if declaration.kind() == SyntaxKind::ClassExpression {
                    let mut container = get_this_container(node, false);
                    while container.kind() != SyntaxKind::SourceFile {
                        if Gc::ptr_eq(&container.parent(), declaration) {
                            if is_property_declaration(&container) && is_static(&container)
                                || is_class_static_block_declaration(&container)
                            {
                                self.get_node_links(declaration).borrow_mut().flags |=
                                    NodeCheckFlags::ClassWithConstructorReference;
                                self.get_node_links(node).borrow_mut().flags |=
                                    NodeCheckFlags::ConstructorReferenceInClass;
                            }
                            break;
                        }

                        container = get_this_container(&container, false);
                    }
                }
            }
        }

        self.check_nested_block_scoped_binding(node, symbol);

        let mut type_ = self.get_type_of_symbol(local_or_export_symbol)?;
        let assignment_kind = get_assignment_target_kind(node);

        if assignment_kind != AssignmentKind::None {
            if !self
                .symbol(local_or_export_symbol)
                .flags()
                .intersects(SymbolFlags::Variable)
                && !(is_in_js_file(Some(node))
                    && self
                        .symbol(local_or_export_symbol)
                        .flags()
                        .intersects(SymbolFlags::ValueModule))
            {
                let assignment_error = if self
                    .symbol(local_or_export_symbol)
                    .flags()
                    .intersects(SymbolFlags::Enum)
                {
                    &*Diagnostics::Cannot_assign_to_0_because_it_is_an_enum
                } else if self
                    .symbol(local_or_export_symbol)
                    .flags()
                    .intersects(SymbolFlags::Class)
                {
                    &*Diagnostics::Cannot_assign_to_0_because_it_is_a_class
                } else if self
                    .symbol(local_or_export_symbol)
                    .flags()
                    .intersects(SymbolFlags::Module)
                {
                    &*Diagnostics::Cannot_assign_to_0_because_it_is_a_namespace
                } else if self
                    .symbol(local_or_export_symbol)
                    .flags()
                    .intersects(SymbolFlags::Function)
                {
                    &*Diagnostics::Cannot_assign_to_0_because_it_is_a_function
                } else if self
                    .symbol(local_or_export_symbol)
                    .flags()
                    .intersects(SymbolFlags::Alias)
                {
                    &*Diagnostics::Cannot_assign_to_0_because_it_is_an_import
                } else {
                    &*Diagnostics::Cannot_assign_to_0_because_it_is_not_a_variable
                };

                self.error(
                    Some(node),
                    assignment_error,
                    Some(vec![self.symbol_to_string_(
                        symbol,
                        Option::<&Node>::None,
                        None,
                        None,
                        None,
                    )?]),
                );
                return Ok(self.error_type());
            }
            if self.is_readonly_symbol(local_or_export_symbol)? {
                if self
                    .symbol(local_or_export_symbol)
                    .flags()
                    .intersects(SymbolFlags::Variable)
                {
                    self.error(
                        Some(node),
                        &Diagnostics::Cannot_assign_to_0_because_it_is_a_constant,
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
                        Some(node),
                        &Diagnostics::Cannot_assign_to_0_because_it_is_a_read_only_property,
                        Some(vec![self.symbol_to_string_(
                            symbol,
                            Option::<&Node>::None,
                            None,
                            None,
                            None,
                        )?]),
                    );
                }
                return Ok(self.error_type());
            }
        }

        let is_alias = self
            .symbol(local_or_export_symbol)
            .flags()
            .intersects(SymbolFlags::Alias);

        if self
            .symbol(local_or_export_symbol)
            .flags()
            .intersects(SymbolFlags::Variable)
        {
            if assignment_kind == AssignmentKind::Definite {
                return Ok(type_);
            }
        } else if is_alias {
            declaration = self.get_declaration_of_alias_symbol(symbol)?;
        } else {
            return Ok(type_);
        }

        if declaration.is_none() {
            return Ok(type_);
        }
        let declaration = declaration.unwrap();

        type_ = self.get_narrowable_type_for_reference(type_, node, check_mode)?;

        let is_parameter = get_root_declaration(&declaration).kind() == SyntaxKind::Parameter;
        let declaration_container = self.maybe_get_control_flow_container(&declaration);
        let mut flow_container = self.get_control_flow_container(node);
        let is_outer_variable = !matches!(
            declaration_container.as_ref(),
            Some(declaration_container) if Gc::ptr_eq(&flow_container, declaration_container)
        );
        let is_spread_destructuring_assignment_target = matches!(
            node.maybe_parent().as_ref(),
            Some(node_parent) if matches!(
                node_parent.maybe_parent().as_ref(),
                Some(node_parent_parent) if is_spread_assignment(node_parent) && self.is_destructuring_assignment_target(node_parent_parent)
            )
        );
        let is_module_exports = self
            .symbol(symbol)
            .flags()
            .intersects(SymbolFlags::ModuleExports);
        while !matches!(
            declaration_container.as_ref(),
            Some(declaration_container) if Gc::ptr_eq(&flow_container, declaration_container)
        ) && (matches!(
            flow_container.kind(),
            SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
        ) || is_object_literal_or_class_expression_method_or_accessor(&flow_container))
            && (self.is_const_variable(local_or_export_symbol) && type_ != self.auto_array_type()
                || is_parameter && !self.is_symbol_assigned(local_or_export_symbol)?)
        {
            flow_container = self.get_control_flow_container(&flow_container);
        }
        let assume_initialized = is_parameter
            || is_alias
            || is_outer_variable
            || is_spread_destructuring_assignment_target
            || is_module_exports
            || is_binding_element(&declaration)
            || type_ != self.auto_type()
                && type_ != self.auto_array_type()
                && (!self.strict_null_checks
                    || self
                        .type_(type_)
                        .flags()
                        .intersects(TypeFlags::AnyOrUnknown | TypeFlags::Void)
                    || self.is_in_type_query(node)
                    || node.parent().kind() == SyntaxKind::ExportSpecifier)
            || node.parent().kind() == SyntaxKind::NonNullExpression
            || declaration.kind() == SyntaxKind::VariableDeclaration
                && declaration
                    .as_variable_declaration()
                    .exclamation_token
                    .is_some()
            || declaration.flags().intersects(NodeFlags::Ambient);
        let initial_type = if assume_initialized {
            if is_parameter {
                self.remove_optionality_from_declared_type(type_, &declaration)?
            } else {
                type_.clone()
            }
        } else if type_ == self.auto_type() || type_ == self.auto_array_type() {
            self.undefined_type()
        } else {
            self.get_optional_type_(type_, None)?
        };
        let flow_type = self.get_flow_type_of_reference(
            node,
            type_,
            Some(initial_type),
            Some(&*flow_container),
        )?;
        if !self.is_evolving_array_operation_target(node)?
            && (type_ == self.auto_type() || type_ == self.auto_array_type())
        {
            if flow_type == self.auto_type() || flow_type == self.auto_array_type() {
                if self.no_implicit_any {
                    self.error(
                        get_name_of_declaration(Some(&*declaration)),
                        &Diagnostics::Variable_0_implicitly_has_type_1_in_some_locations_where_its_type_cannot_be_determined,
                        Some(vec![
                            self.symbol_to_string_(
                                symbol,
                                Option::<&Node>::None,
                                None, None, None,
                            )?,
                            self.type_to_string_(
                                flow_type,
                                Option::<&Node>::None,
                                None, None,
                            )?
                        ])
                    );
                    self.error(
                        Some(node),
                        &Diagnostics::Variable_0_implicitly_has_an_1_type,
                        Some(vec![
                            self.symbol_to_string_(
                                symbol,
                                Option::<&Node>::None,
                                None,
                                None,
                                None,
                            )?,
                            self.type_to_string_(flow_type, Option::<&Node>::None, None, None)?,
                        ]),
                    );
                }
                return Ok(self.convert_auto_to_any(flow_type));
            }
        } else if !assume_initialized
            && !self.get_falsy_flags(type_).intersects(TypeFlags::Undefined)
            && self
                .get_falsy_flags(flow_type)
                .intersects(TypeFlags::Undefined)
        {
            self.error(
                Some(node),
                &Diagnostics::Variable_0_is_used_before_being_assigned,
                Some(vec![self.symbol_to_string_(
                    symbol,
                    Option::<&Node>::None,
                    None,
                    None,
                    None,
                )?]),
            );
            return Ok(type_);
        }
        Ok(if assignment_kind != AssignmentKind::None {
            self.get_base_type_of_literal_type(flow_type)?
        } else {
            flow_type
        })
    }
}
