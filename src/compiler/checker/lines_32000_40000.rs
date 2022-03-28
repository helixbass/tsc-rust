#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, IterationUse, UnusedKind};
use crate::{
    for_each, get_combined_node_flags, get_containing_function_or_class_static_block,
    get_effective_initializer, get_function_flags, is_binding_element, is_function_or_module_block,
    is_private_identifier, map, maybe_for_each, parse_pseudo_big_int, ArrayTypeNode, Block,
    Diagnostic, DiagnosticMessage, Diagnostics, ExpressionStatement, FunctionDeclaration,
    FunctionFlags, HasTypeParametersInterface, IfStatement, InterfaceDeclaration, IterationTypes,
    LiteralLikeNodeInterface, NamedDeclarationInterface, Node, NodeArray, NodeFlags, NodeInterface,
    PrefixUnaryExpression, PropertyAssignment, PropertySignature, PseudoBigInt, ReturnStatement,
    SymbolInterface, SyntaxKind, TemplateExpression, Type, TypeAliasDeclaration, TypeChecker,
    TypeFlags, TypeInterface, TypeParameterDeclaration, TypeReferenceNode,
    UnionOrIntersectionTypeInterface, VariableDeclaration, VariableLikeDeclarationInterface,
    VariableStatement,
};

impl TypeChecker {
    pub(super) fn check_arithmetic_operand_type(
        &self,
        operand: &Node, /*Expression*/
        type_: &Type,
        diagnostic: &DiagnosticMessage,
    ) -> bool {
        if !self.is_type_assignable_to(type_, &self.number_or_big_int_type()) {
            self.error_and_maybe_suggest_await(
                operand, false, // TODO: this is wrong
                diagnostic, None,
            );
            return false;
        }
        true
    }

    pub(super) fn check_prefix_unary_expression(
        &self,
        node: &Node, /*PrefixUnaryExpression*/
    ) -> Rc<Type> {
        let node_as_prefix_unary_expression = node.as_prefix_unary_expression();
        let operand = &node_as_prefix_unary_expression.operand;
        let operand_type = self.check_expression(operand, None, None);
        match node_as_prefix_unary_expression.operator {
            SyntaxKind::PlusPlusToken => {
                self.check_arithmetic_operand_type(operand, &operand_type, &Diagnostics::An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type);
                return self.get_unary_result_type(&operand_type);
            }
            _ => {
                unimplemented!();
            }
        }
    }

    pub(super) fn get_unary_result_type(&self, operand_type: &Type) -> Rc<Type> {
        self.number_type()
    }

    pub(super) fn maybe_type_of_kind(&self, type_: &Type, kind: TypeFlags) -> bool {
        if type_.flags().intersects(kind) {
            return true;
        }
        if let Type::UnionOrIntersectionType(type_) = type_ {
            for t in type_.types() {
                if self.maybe_type_of_kind(&**t, kind) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn all_types_assignable_to_kind(
        &self,
        source: &Type,
        kind: TypeFlags,
        strict: Option<bool>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_template_expression(
        &self,
        node: &Node, /*TemplateExpression*/
    ) -> Rc<Type> {
        let node_as_template_expression = node.as_template_expression();
        let mut texts = vec![node_as_template_expression
            .head
            .as_literal_like_node()
            .text()];
        let mut types = vec![];
        for span in node_as_template_expression.template_spans.iter() {
            let span = span.as_template_span();
            let type_ = self.check_expression(&span.expression, None, None);
            texts.push(span.literal.as_literal_like_node().text());
            types.push(
                if self.is_type_assignable_to(&type_, &self.template_constraint_type()) {
                    type_
                } else {
                    self.string_type()
                },
            );
        }
        if false {
            unimplemented!()
        } else {
            self.string_type()
        }
    }

    pub(super) fn check_expression_cached(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let links = self.get_node_links(node);
        let links_resolved_type_is_none = (*links).borrow().resolved_type.is_none();
        if links_resolved_type_is_none {
            if let Some(check_mode) = check_mode {
                if check_mode != CheckMode::Normal {
                    return self.check_expression(node, Some(check_mode), None);
                }
            }
            let resolved_type = self.check_expression(node, check_mode, None);
            links.borrow_mut().resolved_type = Some(resolved_type);
        }
        let resolved_type = (*links).borrow().resolved_type.clone().unwrap();
        resolved_type
    }

    pub(super) fn check_declaration_initializer<TType: Borrow<Type>>(
        &self,
        declaration: &Node, /*HasExpressionInitializer*/
        contextual_type: Option<TType>,
    ) -> Rc<Type> {
        let initializer = get_effective_initializer(declaration).unwrap();
        let type_ = self
            .get_quick_type_of_expression(&initializer)
            .unwrap_or_else(|| {
                if let Some(contextual_type) = contextual_type {
                    unimplemented!()
                } else {
                    self.check_expression_cached(&initializer, None)
                }
            });
        if false {
            unimplemented!()
        } else {
            type_
        }
    }

    pub(super) fn widen_type_inferred_from_initializer(
        &self,
        declaration: &Node, /*HasExpressionInitializer*/
        type_: &Type,
    ) -> Rc<Type> {
        let widened = if get_combined_node_flags(declaration).intersects(NodeFlags::Const) {
            type_.type_wrapper()
        } else {
            self.get_widened_literal_type(type_)
        };
        widened
    }

    pub(super) fn is_literal_of_contextual_type<TTypeRef: Borrow<Type>>(
        &self,
        candidate_type: &Type,
        contextual_type: Option<TTypeRef>,
    ) -> bool {
        if let Some(contextual_type) = contextual_type {
            let contextual_type = contextual_type.borrow();
            if let Type::UnionOrIntersectionType(union_or_intersection_type) = contextual_type {
                let types = union_or_intersection_type.types();
                // return some(
                //     types,
                //     Some(Box::new(|t| {
                //         self.is_literal_of_contextual_type(candidate_type, Some(t.clone()))
                //     })),
                // );
                return types
                    .iter()
                    .any(|t| self.is_literal_of_contextual_type(candidate_type, Some(&**t)));
            }
            return contextual_type.flags().intersects(
                TypeFlags::StringLiteral
                    | TypeFlags::Index
                    | TypeFlags::TemplateLiteral
                    | TypeFlags::StringMapping,
            ) && self.maybe_type_of_kind(&*candidate_type, TypeFlags::StringLiteral)
                || contextual_type.flags().intersects(TypeFlags::NumberLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::NumberLiteral)
                || contextual_type.flags().intersects(TypeFlags::BigIntLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::BigIntLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::BooleanLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::BooleanLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::UniqueESSymbol)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::UniqueESSymbol);
        }
        false
    }

    pub(super) fn check_expression_for_mutable_location<TContextualType: Borrow<Type>>(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
        contextual_type: Option<TContextualType>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        let type_ = self.check_expression(node, check_mode, force_tuple);
        if false {
            unimplemented!()
        } else {
            self.get_widened_literal_like_type_for_contextual_type(
                &type_,
                self.instantiate_contextual_type(
                    if contextual_type.is_none() {
                        self.get_contextual_type_(node, None)
                    } else {
                        Some(contextual_type.unwrap().borrow().type_wrapper())
                    },
                    node,
                    None,
                ),
            )
        }
    }

    pub(super) fn check_property_assignment(
        &self,
        node: &Node, /*PropertyAssignment*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_expression_for_mutable_location(
            &node.as_property_assignment().initializer,
            check_mode,
            Option::<&Type>::None,
            None,
        )
    }

    pub(super) fn get_quick_type_of_expression(
        &self,
        node: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        let expr = node;
        if false {
            unimplemented!()
        } else if matches!(
            node.kind(),
            SyntaxKind::NumericLiteral
                | SyntaxKind::StringLiteral
                | SyntaxKind::TrueKeyword
                | SyntaxKind::FalseKeyword
        ) {
            return Some(self.check_expression(node, None, None));
        }
        None
    }

    pub(super) fn check_expression(
        &self,
        node: &Node, /*Expression | QualifiedName*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        self.check_expression_worker(node, check_mode)
    }

    pub(super) fn check_expression_worker(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        match node {
            Node::Identifier(_) => self.check_identifier(node, check_mode),
            Node::BaseNode(node_as_base_node) => match node_as_base_node.kind() {
                SyntaxKind::TrueKeyword => self.true_type(),
                _ => unimplemented!(),
            },
            Node::ObjectLiteralExpression(_) => self.check_object_literal(node),
            Node::PrefixUnaryExpression(_) => self.check_prefix_unary_expression(node),
            // Node::Expression(Expression::BinaryExpression(_)) => {
            //     return self.check_binary_expression(node);
            // }
            Node::TemplateLiteralLikeNode(template_literal_like_node) => {
                let type_: Rc<Type> =
                    self.get_string_literal_type(&*template_literal_like_node.text());
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::StringLiteral(string_literal) => {
                let type_: Rc<Type> = self.get_string_literal_type(&*string_literal.text());
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::NumericLiteral(numeric_literal) => {
                self.check_grammar_numeric_literal(node);
                let type_: Rc<Type> =
                    self.get_number_literal_type(numeric_literal.text().as_str().into());
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::BigIntLiteral(big_int_literal) => {
                let type_: Rc<Type> = self
                    .get_big_int_literal_type(PseudoBigInt::new(
                        false,
                        parse_pseudo_big_int(&*big_int_literal.text()),
                    ))
                    .into();
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::TemplateExpression(_) => self.check_template_expression(node),
            _ => unimplemented!("{:#?}", node),
        }
    }

    pub(super) fn check_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        // TODO
    }

    pub(super) fn check_property_declaration(&self, node: &Node /*PropertySignature*/) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_property_signature(&self, node: &Node /*PropertySignature*/) {
        if is_private_identifier(&*node.as_property_signature().name()) {
            self.error(
                Some(node.node_wrapper()),
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                None,
            );
        }
        self.check_property_declaration(node)
    }

    pub(super) fn get_effective_type_arguments(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
        type_parameters: &[Rc<Type /*TypeParameter*/>],
    ) -> Vec<Rc<Type>> {
        self.fill_missing_type_arguments(
            map(
                Some(node.as_has_type_arguments().maybe_type_arguments().unwrap()),
                |type_argument, _| self.get_type_from_type_node_(type_argument),
            ),
            Some(type_parameters),
        )
        .unwrap()
    }

    pub(super) fn check_type_reference_node(&self, node: &Node /*TypeReferenceNode*/) {
        maybe_for_each(
            node.as_type_reference_node().type_arguments.as_ref(),
            |type_argument, _| {
                self.check_source_element(Some(&**type_argument));
                Option::<()>::None
            },
        );
        let type_ = self.get_type_from_type_reference(node);
    }

    pub(super) fn get_type_argument_constraint_(
        &self,
        node: &Node, /*TypeNode*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn check_array_type(&self, node: &Node /*ArrayTypeNode*/) {
        self.check_source_element(Some(&*node.as_array_type_node().element_type));
    }

    pub(super) fn check_union_or_intersection_type(
        &self,
        node: &Node, /*UnionOrIntersectionTypeNode*/
    ) {
        for_each(
            node.as_union_or_intersection_type_node().types(),
            |type_, _| {
                self.check_source_element(Some(&**type_));
                Option::<()>::None
            },
        );
        self.get_type_from_type_node_(node);
    }

    pub(super) fn check_awaited_type(
        &self,
        type_: &Type,
        with_alias: bool,
        error_node: &Node,
        diagnostic_message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_awaited_type_instantiation(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Conditional) {
            unimplemented!()
        }
        false
    }

    pub(super) fn unwrap_awaited_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.unwrap_awaited_type(type_)),
                None,
            )
            .unwrap()
        } else if self.is_awaited_type_instantiation(type_) {
            unimplemented!()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_awaited_type_<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_awaited_type_no_alias<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        if self.is_type_any(Some(type_)) {
            return Some(type_.type_wrapper());
        }

        if self.is_awaited_type_instantiation(type_) {
            return Some(type_.type_wrapper());
        }

        unimplemented!()
    }

    pub(super) fn check_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if self.produce_diagnostics {
            self.check_function_or_method_declaration(node);
        }
    }

    pub(super) fn check_function_or_method_declaration(
        &self,
        node: &Node, /*FunctionDeclaration | MethodDeclaration | MethodSignature*/
    ) {
        // self.check_decorators(node);
        // self.check_signature_declaration(node);
    }

    pub(super) fn check_unused_identifiers<
        TAddDiagnostic: FnMut(&Node, UnusedKind, Rc<Diagnostic>),
    >(
        &self,
        potentially_unused_identifiers: &[Rc<Node /*PotentiallyUnusedIdentifier*/>],
        add_diagnostic: TAddDiagnostic, /*AddUnusedDiagnostic*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_block(&self, node: &Node /*Block*/) {
        let node_as_block = node.as_block();
        if is_function_or_module_block(node) {
            for_each(&node_as_block.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        } else {
            for_each(&node_as_block.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        }
    }

    pub(super) fn convert_auto_to_any(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn check_variable_like_declaration(&self, node: &Node) {
        let node_as_variable_like_declaration = node.as_variable_like_declaration();
        if !is_binding_element(node) {
            self.check_source_element(node_as_variable_like_declaration.maybe_type());
        }

        let symbol = self.get_symbol_of_node(node).unwrap();

        let type_ = self.convert_auto_to_any(&self.get_type_of_symbol(&*symbol));
        let value_declaration = symbol.maybe_value_declaration();
        if value_declaration.is_some() && ptr::eq(node, &*value_declaration.unwrap()) {
            let initializer = get_effective_initializer(node);
            if let Some(initializer) = initializer {
                if true {
                    let initializer_type = self.check_expression_cached(&initializer, None);
                    self.check_type_assignable_to_and_optionally_elaborate(
                        &initializer_type,
                        &type_,
                        Some(node),
                        Some(&*initializer),
                        None,
                    );
                }
            }
        } else {
            unimplemented!()
        }
    }

    pub(super) fn error_next_variable_or_property_declaration_must_have_same_type<
        TFirstDeclaration: Borrow<Node>,
    >(
        &self,
        first_declaration: Option<TFirstDeclaration /*Declaration*/>,
        first_type: &Type,
        next_declaration: &Node, /*Declaration*/
        next_type: &Type,
    ) {
        unimplemented!()
    }

    pub(super) fn check_variable_declaration(&self, node: &Node /*VariableDeclaration*/) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_variable_statement(&self, node: &Node /*VariableStatement*/) {
        for_each(
            &node
                .as_variable_statement()
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
            |declaration, _| Some(self.check_source_element(Some(&**declaration))),
        );
    }

    pub(super) fn check_expression_statement(&self, node: &Node /*ExpressionStatement*/) {
        let expression = &node.as_expression_statement().expression;
        self.check_expression(expression, None, None);
    }

    pub(super) fn check_if_statement(&self, node: &Node /*IfStatement*/) {
        let node_as_if_statement = node.as_if_statement();
        let type_ = self.check_truthiness_expression(&node_as_if_statement.expression, None);
        self.check_source_element(Some(&*node_as_if_statement.then_statement));

        if node_as_if_statement.then_statement.kind() == SyntaxKind::EmptyStatement {
            self.error(
                Some(&*node_as_if_statement.then_statement),
                &Diagnostics::The_body_of_an_if_statement_cannot_be_the_empty_statement,
                None,
            );
        }

        self.check_source_element(node_as_if_statement.else_statement.clone());
    }

    pub(super) fn check_truthiness_of_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Void) {
            self.error(
                Some(node),
                &Diagnostics::An_expression_of_type_void_cannot_be_tested_for_truthiness,
                None,
            );
        }

        type_.type_wrapper()
    }

    pub(super) fn check_truthiness_expression(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_truthiness_of_type(&self.check_expression(node, check_mode, None), node)
    }

    pub(super) fn check_right_hand_side_of_for_of(
        &self,
        statement: &Node, /*ForOfStatement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_iteration_types(
        &self,
        yield_type: Option<Rc<Type>>,
        return_type: Option<Rc<Type>>,
        next_type: Option<Rc<Type>>,
    ) -> IterationTypes {
        let yield_type = yield_type.unwrap_or_else(|| self.never_type());
        let return_type = return_type.unwrap_or_else(|| self.never_type());
        let next_type = next_type.unwrap_or_else(|| self.unknown_type());
        IterationTypes::new(yield_type, return_type, next_type)
    }

    pub(super) fn unwrap_return_type(
        &self,
        return_type: &Type,
        function_flags: FunctionFlags,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_return_statement(&self, node: &Node /*ReturnStatement*/) {
        let container = get_containing_function_or_class_static_block(node);

        if container.is_none() {
            unimplemented!()
        }
        let container = container.unwrap();

        let signature = self.get_signature_from_declaration_(&container);
        let return_type = self.get_return_type_of_signature(&signature);
        let function_flags = get_function_flags(Some(&*container));
        let node_as_return_statement = node.as_return_statement();
        if self.strict_null_checks
            || node_as_return_statement.expression.is_some()
            || return_type.flags().intersects(TypeFlags::Never)
        {
            let expr_type = match node_as_return_statement.expression.as_ref() {
                Some(expression) => self.check_expression_cached(&expression, None),
                None => self.undefined_type(),
            };
            if false {
                unimplemented!()
            } else if self.get_return_type_from_annotation(&container).is_some() {
                let unwrapped_return_type = self
                    .unwrap_return_type(&return_type, function_flags)/*.unwrap_or(return_type)*/;
                let unwrapped_expr_type = if function_flags.intersects(FunctionFlags::Async) {
                    self.check_awaited_type(&expr_type, false, node, &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member, None)
                } else {
                    expr_type
                };
                // if unwrappedReturnType {
                self.check_type_assignable_to_and_optionally_elaborate(
                    &unwrapped_expr_type,
                    &unwrapped_return_type,
                    Some(node),
                    node_as_return_statement.expression.clone(),
                    None,
                );
                // }
            }
        }
    }

    pub(super) fn check_type_parameters(
        &self,
        type_parameter_declarations: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
    ) {
        if let Some(type_parameter_declarations) = type_parameter_declarations {
            for node in type_parameter_declarations {
                self.check_type_parameter(&node);
            }
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
        unimplemented!()
    }

    pub(super) fn check_interface_declaration(&self, node: &Node /*InterfaceDeclaration*/) {
        let node_as_interface_declaration = node.as_interface_declaration();
        self.check_type_parameters(
            node_as_interface_declaration
                .maybe_type_parameters()
                .as_ref(),
        );
        for_each(&node_as_interface_declaration.members, |member, _| {
            self.check_source_element(Some(&**member));
            Option::<()>::None
        });
    }

    pub(super) fn check_type_alias_declaration(&self, node: &Node /*TypeAliasDeclaration*/) {
        let node_as_type_alias_declaration = node.as_type_alias_declaration();
        self.check_type_parameters(
            node_as_type_alias_declaration
                .maybe_type_parameters()
                .as_ref(),
        );
        if false {
            unimplemented!()
        } else {
            self.check_source_element(Some(&*node_as_type_alias_declaration.type_));
        }
    }
}
