use std::borrow::Borrow;

use gc::{Gc, GcCell};
use id_arena::Id;

use super::{init_flow_node, BinderType, ContainerFlags};
use crate::{
    contains_gc, create_symbol_table, for_each, for_each_child, get_combined_modifier_flags,
    get_immediately_invoked_function_expression, get_name_of_declaration, has_syntactic_modifier,
    is_ambient_module, is_assignment_expression, is_binary_expression, is_declaration,
    is_destructuring_assignment, is_dotted_name, is_element_access_expression,
    is_expression_of_optional_chain_root, is_in_js_file, is_jsdoc_enum_tag, is_jsdoc_type_alias,
    is_module_declaration, is_non_null_expression, is_nullish_coalesce, is_optional_chain,
    is_parenthesized_expression, is_property_access_entity_name_expression,
    is_property_access_expression, is_string_literal_like, is_string_or_numeric_literal_like,
    is_type_of_expression, node_is_present, Debug_, FlowCondition, FlowFlags, FlowLabel, FlowNode,
    FlowNodeBase, FlowReduceLabel, FlowStart, HasArena, HasStatementsInterface, InArena,
    ModifierFlags, Node, NodeArray, NodeFlags, NodeInterface, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, OptionInArena,
    contains,
};

impl BinderType {
    pub(super) fn declare_module_member(
        &self,
        node: Id<Node>, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Id<Symbol> {
        let has_export_modifier = get_combined_modifier_flags(node, self)
            .intersects(ModifierFlags::Export)
            || self.jsdoc_treat_as_exported(node);
        if symbol_flags.intersects(SymbolFlags::Alias) {
            if node.ref_(self).kind() == SyntaxKind::ExportSpecifier
                || node.ref_(self).kind() == SyntaxKind::ImportEqualsDeclaration
                    && has_export_modifier
            {
                self.declare_symbol(
                    &mut self.container().ref_(self).symbol().ref_(self).exports().ref_mut(self),
                    Some(self.container().ref_(self).symbol()),
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )
            } else {
                self.declare_symbol(
                    &mut self.container().ref_(self).locals().ref_mut(self),
                    None,
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )
            }
        } else {
            if is_jsdoc_type_alias(&node.ref_(self)) {
                Debug_.assert(is_in_js_file(Some(&node.ref_(self))), None);
            }
            if !is_ambient_module(node, self)
                && (has_export_modifier
                    || self
                        .container()
                        .ref_(self).flags()
                        .intersects(NodeFlags::ExportContext))
            {
                if self.container().ref_(self).maybe_locals().is_none()
                    || has_syntactic_modifier(node, ModifierFlags::Default, self)
                        && self.get_declaration_name(node).is_none()
                {
                    return self.declare_symbol(
                        &mut self.container().ref_(self).symbol().ref_(self).exports().ref_mut(self),
                        Some(self.container().ref_(self).symbol()),
                        node,
                        symbol_flags,
                        symbol_excludes,
                        None,
                        None,
                    );
                }
                let export_kind = if symbol_flags.intersects(SymbolFlags::Value) {
                    SymbolFlags::ExportValue
                } else {
                    SymbolFlags::None
                };
                let local = self.declare_symbol(
                    &mut self.container().ref_(self).locals().ref_mut(self),
                    Option::<Id<Symbol>>::None,
                    node,
                    export_kind,
                    symbol_excludes,
                    None,
                    None,
                );
                local.ref_(self).set_export_symbol(Some(self.declare_symbol(
                    &mut self.container().ref_(self).symbol().ref_(self).exports().ref_mut(self),
                    Some(self.container().ref_(self).symbol()),
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )));
                node.ref_(self).set_local_symbol(Some(local));
                local
            } else {
                self.declare_symbol(
                    &mut self.container().ref_(self).locals().ref_mut(self),
                    Option::<Id<Symbol>>::None,
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )
            }
        }
    }

    pub(super) fn jsdoc_treat_as_exported(&self, mut node: Id<Node>) -> bool {
        if node.ref_(self).maybe_parent().is_some() && is_module_declaration(&node.ref_(self)) {
            node = node.ref_(self).parent();
        }
        if !is_jsdoc_type_alias(&node.ref_(self)) {
            return false;
        }
        if !is_jsdoc_enum_tag(&node.ref_(self))
            && node
                .ref_(self).as_jsdoc_typedef_or_callback_tag()
                .maybe_full_name()
                .is_some()
        {
            return true;
        }
        let Some(decl_name) = get_name_of_declaration(Some(node), self) else {
            return false;
        };
        let decl_name_parent = decl_name.ref_(self).parent();
        if is_property_access_entity_name_expression(decl_name_parent, self)
            && self.is_top_level_namespace_assignment(decl_name_parent)
        {
            return true;
        }
        if is_declaration(decl_name_parent, self)
            && get_combined_modifier_flags(decl_name_parent, self).intersects(ModifierFlags::Export)
        {
            return true;
        }
        false
    }

    pub(super) fn bind_container(&self, node: Id<Node>, container_flags: ContainerFlags) {
        let save_container = self.maybe_container();
        let save_this_parent_container = self.maybe_this_parent_container();
        let saved_block_scope_container = self.maybe_block_scope_container();

        if container_flags.intersects(ContainerFlags::IsContainer) {
            if node.ref_(self).kind() != SyntaxKind::ArrowFunction {
                self.set_this_parent_container(self.maybe_container());
            }
            self.set_container(Some(node));
            self.set_block_scope_container(Some(node));
            if container_flags.intersects(ContainerFlags::HasLocals) {
                self.container()
                    .ref_(self).set_locals(Some(self.alloc_symbol_table(create_symbol_table(
                        self.arena(),
                        Option::<&[Id<Symbol>]>::None,
                    ))));
            }
            self.add_to_container_chain(self.container());
        } else if container_flags.intersects(ContainerFlags::IsBlockScopedContainer) {
            self.set_block_scope_container(Some(node));
            self.block_scope_container().ref_(self).set_locals(None);
        }
        if container_flags.intersects(ContainerFlags::IsControlFlowContainer) {
            let save_current_flow = self.maybe_current_flow();
            let save_break_target = self.maybe_current_break_target();
            let save_continue_target = self.maybe_current_continue_target();
            let save_return_target = self.maybe_current_return_target();
            let save_exception_target = self.maybe_current_exception_target();
            let save_active_label_list = self.maybe_active_label_list();
            let save_has_explicit_return = self.maybe_has_explicit_return();
            let is_iife = container_flags.intersects(ContainerFlags::IsFunctionExpression)
                && !has_syntactic_modifier(node, ModifierFlags::Async, self)
                && node
                    .ref_(self).as_function_like_declaration()
                    .maybe_asterisk_token()
                    .is_none()
                && get_immediately_invoked_function_expression(node, self).is_some();
            if !is_iife {
                self.set_current_flow(Some(self.alloc_flow_node(init_flow_node(
                    FlowStart::new(FlowFlags::Start, None).into(),
                ))));
                if container_flags.intersects(
                    ContainerFlags::IsFunctionExpression
                        | ContainerFlags::IsObjectLiteralOrClassExpressionMethodOrAccessor,
                ) {
                    self.current_flow()
                        .ref_(self).as_flow_start()
                        .set_node(Some(node));
                }
            }
            self.set_current_return_target(
                if is_iife
                    || matches!(
                        node.ref_(self).kind(),
                        SyntaxKind::Constructor | SyntaxKind::ClassStaticBlockDeclaration
                    )
                    || (is_in_js_file(Some(&node.ref_(self)))
                        && matches!(
                            node.ref_(self).kind(),
                            SyntaxKind::FunctionDeclaration | SyntaxKind::FunctionExpression
                        ))
                {
                    Some(self.create_branch_label())
                } else {
                    None
                },
            );
            self.set_current_exception_target(None);
            self.set_current_break_target(None);
            self.set_current_continue_target(None);
            self.set_active_label_list(None);
            self.set_has_explicit_return(Some(false));
            self.bind_children(node);
            node.ref_(self).set_flags(node.ref_(self).flags() & !NodeFlags::ReachabilityAndEmitFlags);
            if !self
                .current_flow()
                .ref_(self).flags()
                .intersects(FlowFlags::Unreachable)
                && container_flags.intersects(ContainerFlags::IsFunctionLike)
                && node_is_present(match node.ref_(self).kind() {
                    SyntaxKind::ClassStaticBlockDeclaration => {
                        Some(node.ref_(self).as_class_static_block_declaration().body)
                    }
                    _ => node
                        .ref_(self).maybe_as_function_like_declaration()
                        .and_then(|node| node.maybe_body()),
                }.refed(self).as_deref())
            {
                node.ref_(self).set_flags(node.ref_(self).flags() | NodeFlags::HasImplicitReturn);
                if matches!(self.maybe_has_explicit_return(), Some(true)) {
                    node.ref_(self).set_flags(node.ref_(self).flags() | NodeFlags::HasExplicitReturn);
                }
                match node.ref_(self).kind() {
                    SyntaxKind::ClassStaticBlockDeclaration => node
                        .ref_(self).as_class_static_block_declaration()
                        .set_end_flow_node(self.maybe_current_flow()),
                    _ => node
                        .ref_(self).as_function_like_declaration()
                        .set_end_flow_node(self.maybe_current_flow()),
                }
            }
            if node.ref_(self).kind() == SyntaxKind::SourceFile {
                node.ref_(self).set_flags(node.ref_(self).flags() | self.emit_flags());
                node.ref_(self).as_source_file()
                    .set_end_flow_node(self.maybe_current_flow());
            }

            if let Some(current_return_target) = self.maybe_current_return_target() {
                self.add_antecedent(&current_return_target.ref_(self), self.current_flow());
                self.set_current_flow(Some(self.finish_flow_label(current_return_target)));
                if matches!(
                    node.ref_(self).kind(),
                    SyntaxKind::Constructor | SyntaxKind::ClassStaticBlockDeclaration
                ) || is_in_js_file(Some(&node.ref_(self)))
                    && matches!(
                        node.ref_(self).kind(),
                        SyntaxKind::FunctionDeclaration | SyntaxKind::FunctionExpression
                    )
                {
                    match node.ref_(self).kind() {
                        SyntaxKind::ClassStaticBlockDeclaration => node
                            .ref_(self).as_class_static_block_declaration()
                            .set_return_flow_node(self.maybe_current_flow()),
                        _ => node
                            .ref_(self).as_function_like_declaration()
                            .set_return_flow_node(self.maybe_current_flow()),
                    }
                }
            }
            if !is_iife {
                self.set_current_flow(save_current_flow);
            }
            self.set_current_break_target(save_break_target);
            self.set_current_continue_target(save_continue_target);
            self.set_current_return_target(save_return_target);
            self.set_current_exception_target(save_exception_target);
            self.set_active_label_list(save_active_label_list);
            self.set_has_explicit_return(save_has_explicit_return);
        } else if container_flags.intersects(ContainerFlags::IsInterface) {
            self.set_seen_this_keyword(Some(false));
            self.bind_children(node);
            node.ref_(self).set_flags(if self.maybe_seen_this_keyword() == Some(true) {
                node.ref_(self).flags() | NodeFlags::ContainsThis
            } else {
                node.ref_(self).flags() & !NodeFlags::ContainsThis
            });
        } else {
            self.bind_children(node);
        }

        self.set_container(save_container);
        self.set_this_parent_container(save_this_parent_container);
        self.set_block_scope_container(saved_block_scope_container);
    }

    pub(super) fn bind_each_functions_first(&self, nodes: Option<&[Id<Node>] /*NodeArray*/>) {
        self.bind_each_callback(nodes, |n| {
            if n.ref_(self).kind() == SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
        self.bind_each_callback(nodes, |n| {
            if n.ref_(self).kind() != SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
    }

    pub(super) fn bind_each(&self, nodes: Option<&[Id<Node>] /*NodeArray*/>) {
        if nodes.is_none() {
            return;
        }
        let nodes = nodes.unwrap();

        for_each(nodes, |&node, _| {
            self.bind(Some(node));
            Option::<()>::None
        });
    }

    pub(super) fn bind_each_callback(
        &self,
        nodes: Option<&[Id<Node>] /*NodeArray*/>,
        mut bind_function: impl FnMut(Id<Node>),
    ) {
        if nodes.is_none() {
            return;
        }
        let nodes = nodes.unwrap();

        for_each(nodes, |&node, _| {
            bind_function(node);
            Option::<()>::None
        });
    }

    pub(super) fn bind_each_child(&self, node: Id<Node>) {
        for_each_child(
            node,
            |node| self.bind(Some(node)),
            Some(|nodes: Id<NodeArray>| self.bind_each(Some(&nodes.ref_(self)))),
            self,
        );
    }

    pub(super) fn bind_children(&self, node: Id<Node>) {
        let save_in_assignment_pattern = self.in_assignment_pattern();
        self.set_in_assignment_pattern(false);
        if self.check_unreachable(node) {
            self.bind_each_child(node);
            self.bind_jsdoc(node);
            self.set_in_assignment_pattern(save_in_assignment_pattern);
            return;
        }
        if node.ref_(self).kind() >= SyntaxKind::FirstStatement
            && node.ref_(self).kind() <= SyntaxKind::LastStatement
            && self.options().ref_(self).allow_unreachable_code != Some(true)
        {
            node.ref_(self).set_flow_node(self.maybe_current_flow());
        }
        match node.ref_(self).kind() {
            SyntaxKind::WhileStatement => self.bind_while_statement(node),
            SyntaxKind::DoStatement => self.bind_do_statement(node),
            SyntaxKind::ForStatement => self.bind_for_statement(node),
            SyntaxKind::ForInStatement | SyntaxKind::ForOfStatement => {
                self.bind_for_in_or_for_of_statement(node)
            }
            SyntaxKind::IfStatement => self.bind_if_statement(node),
            SyntaxKind::ReturnStatement | SyntaxKind::ThrowStatement => {
                self.bind_return_or_throw(node)
            }
            SyntaxKind::BreakStatement | SyntaxKind::ContinueStatement => {
                self.bind_break_or_continue_statement(node)
            }
            SyntaxKind::TryStatement => self.bind_try_statement(node),
            SyntaxKind::SwitchStatement => self.bind_switch_statement(node),
            SyntaxKind::CaseBlock => self.bind_case_block(node),
            SyntaxKind::CaseClause => self.bind_case_clause(node),
            SyntaxKind::ExpressionStatement => self.bind_expression_statement(node),
            SyntaxKind::LabeledStatement => self.bind_labeled_statement(node),
            SyntaxKind::PrefixUnaryExpression => self.bind_prefix_unary_expression_flow(node),
            SyntaxKind::PostfixUnaryExpression => self.bind_postfix_unary_expression_flow(node),
            SyntaxKind::BinaryExpression => {
                if is_destructuring_assignment(node, self) {
                    self.set_in_assignment_pattern(save_in_assignment_pattern);
                    self.bind_destructuring_assignment_flow(node);
                    return;
                }
                self.bind_binary_expression_flow().call(node)
            }
            SyntaxKind::DeleteExpression => self.bind_delete_expression_flow(node),
            SyntaxKind::ConditionalExpression => self.bind_conditional_expression_flow(node),
            SyntaxKind::VariableDeclaration => self.bind_variable_declaration_flow(node),
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                self.bind_access_expression_flow(node)
            }
            SyntaxKind::CallExpression => self.bind_call_expression_flow(node),
            SyntaxKind::NonNullExpression => self.bind_non_null_expression_flow(node),
            SyntaxKind::JSDocTypedefTag
            | SyntaxKind::JSDocCallbackTag
            | SyntaxKind::JSDocEnumTag => self.bind_jsdoc_type_alias(node),
            SyntaxKind::SourceFile => {
                let node_ref = node.ref_(self);
                let node_as_source_file = node_ref.as_source_file();
                self.bind_each_functions_first(Some(&node_as_source_file.statements().ref_(self)));
                self.bind(Some(node_as_source_file.end_of_file_token()));
            }
            SyntaxKind::Block | SyntaxKind::ModuleBlock => {
                self.bind_each_functions_first(Some(&node.ref_(self).as_has_statements().statements().ref_(self)));
            }
            SyntaxKind::BindingElement => self.bind_binding_element_flow(node),
            SyntaxKind::ObjectLiteralExpression
            | SyntaxKind::ArrayLiteralExpression
            | SyntaxKind::PropertyAssignment
            | SyntaxKind::SpreadElement => {
                self.set_in_assignment_pattern(save_in_assignment_pattern);
                self.bind_each_child(node);
            }
            _ => {
                self.bind_each_child(node);
            }
        };
        self.bind_jsdoc(node);
        self.set_in_assignment_pattern(save_in_assignment_pattern);
    }

    pub(super) fn is_narrowing_expression(&self, expr: Id<Node> /*Expression*/) -> bool {
        match expr.ref_(self).kind() {
            SyntaxKind::Identifier
            | SyntaxKind::PrivateIdentifier
            | SyntaxKind::ThisKeyword
            | SyntaxKind::PropertyAccessExpression
            | SyntaxKind::ElementAccessExpression => self.contains_narrowable_reference(expr),
            SyntaxKind::CallExpression => self.has_narrowable_argument(expr),
            SyntaxKind::ParenthesizedExpression | SyntaxKind::NonNullExpression => {
                self.is_narrowing_expression(expr.ref_(self).as_has_expression().expression())
            }
            SyntaxKind::BinaryExpression => self.is_narrowing_binary_expression(expr),
            SyntaxKind::PrefixUnaryExpression => {
                let expr_ref = expr.ref_(self);
                let expr_as_prefix_unary_expression = expr_ref.as_prefix_unary_expression();
                expr_as_prefix_unary_expression.operator == SyntaxKind::ExclamationToken
                    && self.is_narrowing_expression(expr_as_prefix_unary_expression.operand)
            }
            SyntaxKind::TypeOfExpression => {
                self.is_narrowing_expression(expr.ref_(self).as_type_of_expression().expression)
            }
            _ => false,
        }
    }

    pub(super) fn is_narrowable_reference(&self, expr: Id<Node> /*Expression*/) -> bool {
        is_dotted_name(expr, self)
            || (is_property_access_expression(&expr.ref_(self))
                || is_non_null_expression(&expr.ref_(self))
                || is_parenthesized_expression(&expr.ref_(self)))
                && self.is_narrowable_reference(expr.ref_(self).as_has_expression().expression())
            || is_binary_expression(&expr.ref_(self)) && {
                let expr_ref = expr.ref_(self);
                let expr_as_binary_expression = expr_ref.as_binary_expression();
                expr_as_binary_expression.operator_token.ref_(self).kind() == SyntaxKind::CommaToken
                    && self.is_narrowable_reference(expr_as_binary_expression.right)
            }
            || is_element_access_expression(&expr.ref_(self)) && {
                let expr_ref = expr.ref_(self);
                let expr_as_element_access_expression = expr_ref.as_element_access_expression();
                is_string_or_numeric_literal_like(
                    &expr_as_element_access_expression.argument_expression.ref_(self),
                ) && self.is_narrowable_reference(expr_as_element_access_expression.expression)
            }
            || is_assignment_expression(expr, None, self)
                && self.is_narrowable_reference(expr.ref_(self).as_binary_expression().left)
    }

    pub(super) fn contains_narrowable_reference(&self, expr: Id<Node> /*Expression*/) -> bool {
        self.is_narrowable_reference(expr)
            || is_optional_chain(&expr.ref_(self))
                && self.contains_narrowable_reference(expr.ref_(self).as_has_expression().expression())
    }

    pub(super) fn has_narrowable_argument(&self, expr: Id<Node> /*CallExpression*/) -> bool {
        let expr_ref = expr.ref_(self);
        let expr_as_call_expression = expr_ref.as_call_expression();
        // if (expr.arguments) {
        for &argument in &*expr_as_call_expression.arguments.ref_(self) {
            if self.contains_narrowable_reference(argument) {
                return true;
            }
        }
        // }
        if expr_as_call_expression.expression.ref_(self).kind() == SyntaxKind::PropertyAccessExpression
            && self.contains_narrowable_reference(
                expr_as_call_expression
                    .expression
                    .ref_(self).as_property_access_expression()
                    .expression,
            )
        {
            return true;
        }
        false
    }

    pub(super) fn is_narrowing_typeof_operands(
        &self,
        expr1: Id<Node>, /*Expression*/
        expr2: Id<Node>, /*Expression*/
    ) -> bool {
        is_type_of_expression(&expr1.ref_(self))
            && self.is_narrowable_operand(expr1.ref_(self).as_type_of_expression().expression)
            && is_string_literal_like(&expr2.ref_(self))
    }

    pub(super) fn is_narrowing_binary_expression(
        &self,
        expr: Id<Node>, /*BinaryExpression*/
    ) -> bool {
        let expr_ref = expr.ref_(self);
        let expr_as_binary_expression = expr_ref.as_binary_expression();
        match expr_as_binary_expression.operator_token.ref_(self).kind() {
            SyntaxKind::EqualsToken
            | SyntaxKind::BarBarEqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken => {
                self.contains_narrowable_reference(expr_as_binary_expression.left)
            }
            SyntaxKind::EqualsEqualsToken
            | SyntaxKind::ExclamationEqualsToken
            | SyntaxKind::EqualsEqualsEqualsToken
            | SyntaxKind::ExclamationEqualsEqualsToken => {
                self.is_narrowable_operand(expr_as_binary_expression.left)
                    || self.is_narrowable_operand(expr_as_binary_expression.right)
                    || self.is_narrowing_typeof_operands(
                        expr_as_binary_expression.right,
                        expr_as_binary_expression.left,
                    )
                    || self.is_narrowing_typeof_operands(
                        expr_as_binary_expression.left,
                        expr_as_binary_expression.right,
                    )
            }
            SyntaxKind::InstanceOfKeyword => {
                self.is_narrowable_operand(expr_as_binary_expression.left)
            }
            SyntaxKind::InKeyword => self.is_narrowing_expression(expr_as_binary_expression.right),
            SyntaxKind::CommaToken => {
                self.is_narrowing_expression(expr_as_binary_expression.right)
            }
            _ => false,
        }
    }

    pub(super) fn is_narrowable_operand(&self, expr: Id<Node> /*Expression*/) -> bool {
        match expr.ref_(self).kind() {
            SyntaxKind::ParenthesizedExpression => {
                return self.is_narrowable_operand(expr.ref_(self).as_parenthesized_expression().expression);
            }
            SyntaxKind::BinaryExpression => {
                let expr_ref = expr.ref_(self);
                let expr_as_binary_expression = expr_ref.as_binary_expression();
                match expr_as_binary_expression.operator_token.ref_(self).kind() {
                    SyntaxKind::EqualsToken => {
                        return self.is_narrowable_operand(expr_as_binary_expression.left);
                    }
                    SyntaxKind::CommaToken => {
                        return self.is_narrowable_operand(expr_as_binary_expression.right);
                    }
                    _ => (),
                }
            }
            _ => (),
        }
        self.contains_narrowable_reference(expr)
    }

    pub(super) fn create_branch_label(&self) -> Id<FlowNode /*FlowLabel*/> {
        self.alloc_flow_node(init_flow_node(
            FlowLabel::new(FlowFlags::BranchLabel, None).into(),
        ))
    }

    pub(super) fn create_loop_label(&self) -> Id<FlowNode /*FlowLabel*/> {
        self.alloc_flow_node(init_flow_node(
            FlowLabel::new(FlowFlags::LoopLabel, None).into(),
        ))
    }

    pub(super) fn create_reduce_label(
        &self,
        target: Id<FlowNode /*FlowLabel*/>,
        antecedents: Vec<Id<FlowNode>>,
        antecedent: Id<FlowNode>,
    ) -> Id<FlowNode /*FlowReduceLabel*/> {
        self.alloc_flow_node(init_flow_node(
            FlowReduceLabel::new(FlowFlags::ReduceLabel, target, antecedents, antecedent).into(),
        ))
    }

    pub(super) fn set_flow_node_referenced(&self, flow: &FlowNode) {
        flow.set_flags(
            flow.flags()
                | if flow.flags().intersects(FlowFlags::Referenced) {
                    FlowFlags::Shared
                } else {
                    FlowFlags::Referenced
                },
        );
    }

    pub(super) fn add_antecedent(
        &self,
        label: &FlowNode, /*FlowLabel*/
        antecedent: Id<FlowNode>,
    ) {
        let label_as_flow_label = label.as_flow_label();
        if !antecedent.ref_(self).flags().intersects(FlowFlags::Unreachable)
            && !contains(
                label_as_flow_label.maybe_antecedents().as_deref(),
                &antecedent,
            )
        {
            let mut label_antecedents = label_as_flow_label.maybe_antecedents_mut();
            if label_antecedents.is_none() {
                *label_antecedents = Some(vec![]);
            }
            label_antecedents.as_mut().unwrap().push(antecedent.clone());
            self.set_flow_node_referenced(&antecedent.ref_(self));
        }
    }

    pub(super) fn create_flow_condition(
        &self,
        flags: FlowFlags,
        antecedent: Id<FlowNode>,
        expression: Option<Id<Node>>,
    ) -> Id<FlowNode> {
        if antecedent.ref_(self).flags().intersects(FlowFlags::Unreachable) {
            return antecedent;
        }
        let Some(expression) = expression else {
            return if flags.intersects(FlowFlags::TrueCondition) {
                antecedent
            } else {
                self.unreachable_flow()
            };
        };
        if (expression.ref_(self).kind() == SyntaxKind::TrueKeyword
            && flags.intersects(FlowFlags::FalseCondition)
            || expression.ref_(self).kind() == SyntaxKind::FalseKeyword
                && flags.intersects(FlowFlags::TrueCondition))
            && !is_expression_of_optional_chain_root(expression, self)
            && !is_nullish_coalesce(expression.ref_(self).parent(), self)
        {
            return self.unreachable_flow();
        }
        if !self.is_narrowing_expression(expression) {
            return antecedent;
        }
        self.set_flow_node_referenced(&antecedent.ref_(self));
        self.alloc_flow_node(init_flow_node(
            FlowCondition::new(flags, antecedent, expression).into(),
        ))
    }
}
