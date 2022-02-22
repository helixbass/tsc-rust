#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{init_flow_node, BinderType, ContainerFlags};
use crate::{
    is_binary_expression, is_logical_or_coalescing_assignment_operator, is_optional_chain,
    is_outermost_optional_chain, is_parenthesized_expression, is_prefix_unary_expression,
    skip_parentheses, FlowAssignment, FlowCall, FlowFlags, FlowNode, FlowSwitchClause, Symbol,
    SyntaxKind, __String, create_symbol_table, is_binding_pattern, is_block_or_catch_scoped,
    is_class_static_block_declaration, is_function_like, set_parent, InternalSymbolName,
    NamedDeclarationInterface, Node, NodeInterface, SymbolFlags, SymbolInterface,
};

impl BinderType {
    pub(super) fn create_flow_switch_clause(
        &self,
        antecedent: Rc<FlowNode>,
        switch_statement: &Node, /*SwitchStatement*/
        clause_start: usize,
        clause_end: usize,
    ) -> Rc<FlowNode> {
        self.set_flow_node_referenced(&antecedent);
        Rc::new(init_flow_node(
            FlowSwitchClause::new(
                FlowFlags::SwitchClause,
                antecedent,
                switch_statement.node_wrapper(),
                clause_start,
                clause_end,
            )
            .into(),
        ))
    }

    pub(super) fn create_flow_mutation(
        &self,
        flags: FlowFlags,
        antecedent: Rc<FlowNode>,
        node: &Node, /*Expression | VariableDeclaration | ArrayBindingElement*/
    ) -> Rc<FlowNode> {
        self.set_flow_node_referenced(&antecedent);
        let result: Rc<FlowNode> = Rc::new(init_flow_node(
            FlowAssignment::new(flags, antecedent, node.node_wrapper()).into(),
        ));
        if let Some(current_exception_target) = self.maybe_current_exception_target() {
            self.add_antecedent(&current_exception_target, result.clone());
        }
        result
    }

    pub(super) fn create_flow_call(
        &self,
        antecedent: Rc<FlowNode>,
        node: &Node, /*CallExpression*/
    ) -> Rc<FlowNode> {
        self.set_flow_node_referenced(&antecedent);
        Rc::new(init_flow_node(
            FlowCall::new(FlowFlags::Call, antecedent, node.node_wrapper()).into(),
        ))
    }

    pub(super) fn finish_flow_label(&self, flow: Rc<FlowNode /*FlowLabel*/>) -> Rc<FlowNode> {
        let antecedents = flow.as_flow_label().maybe_antecedents();
        let antecedents = antecedents.as_ref();
        if antecedents.is_none() {
            return self.unreachable_flow();
        }
        let antecedents = antecedents.unwrap();
        if antecedents.len() == 1 {
            return antecedents[0].clone();
        }
        flow.clone()
    }

    pub(super) fn is_statement_condition(&self, node: &Node) -> bool {
        let parent = node.parent();
        match parent.kind() {
            SyntaxKind::IfStatement | SyntaxKind::WhileStatement | SyntaxKind::DoStatement => {
                ptr::eq(&*parent.as_has_expression().expression(), node)
            }
            SyntaxKind::ForStatement | SyntaxKind::ConditionalExpression => {
                matches!(parent.as_has_condition().maybe_condition(), Some(condition) if ptr::eq(&*condition, node))
            }
            _ => false,
        }
    }

    pub(super) fn is_logical_expression(&self, node: &Node) -> bool {
        let mut node = node.node_wrapper();
        loop {
            if node.kind() == SyntaxKind::ParenthesizedExpression {
                node = node.as_parenthesized_expression().expression.clone();
            } else if node.kind() == SyntaxKind::PrefixUnaryExpression
                && node.as_prefix_unary_expression().operator == SyntaxKind::ExclamationToken
            {
                node = node.as_prefix_unary_expression().operand.clone();
            } else {
                return node.kind() == SyntaxKind::BinaryExpression
                    && matches!(
                        node.as_binary_expression().operator_token.kind(),
                        SyntaxKind::AmpersandAmpersandToken
                            | SyntaxKind::BarBarToken
                            | SyntaxKind::QuestionQuestionToken
                    );
            }
        }
    }

    pub(super) fn is_logical_assignment_expression(&self, node: &Node) -> bool {
        let node = skip_parentheses(node, None);
        is_binary_expression(&node)
            && is_logical_or_coalescing_assignment_operator(
                node.as_binary_expression().operator_token.kind(),
            )
    }

    pub(super) fn is_top_level_logical_expression(&self, node: &Node) -> bool {
        let mut node = node.node_wrapper();
        while is_parenthesized_expression(&node.parent())
            || is_prefix_unary_expression(&node.parent())
                && node.parent().as_prefix_unary_expression().operator
                    == SyntaxKind::ExclamationToken
        {
            node = node.parent();
        }
        !self.is_statement_condition(&node)
            && !self.is_logical_assignment_expression(&node.parent())
            && !self.is_logical_expression(&node.parent())
            && !(is_optional_chain(&node.parent())
                && Rc::ptr_eq(&node.parent().as_has_expression().expression(), &node))
    }

    pub(super) fn do_with_conditional_branches<TArgument, TAction: FnMut(TArgument)>(
        &self,
        mut action: TAction,
        value: TArgument,
        true_target: Rc<FlowNode /*FlowLabel*/>,
        false_target: Rc<FlowNode /*FlowLabel*/>,
    ) {
        let saved_true_target = self.maybe_current_true_target();
        let saved_false_target = self.maybe_current_false_target();
        self.set_current_true_target(Some(true_target));
        self.set_current_false_target(Some(false_target));
        action(value);
        self.set_current_true_target(saved_true_target);
        self.set_current_false_target(saved_false_target);
    }

    pub(super) fn bind_condition<TNode: Borrow<Node> + Clone>(
        &self,
        node: Option<TNode>,
        true_target: Rc<FlowNode /*FlowLabel*/>,
        false_target: Rc<FlowNode /*FlowLabel*/>,
    ) {
        self.do_with_conditional_branches(
            |node| self.bind(node),
            node.clone(),
            true_target.clone(),
            false_target.clone(),
        );
        if match node.as_ref() {
            None => true,
            Some(node) => {
                let node = node.borrow();
                !self.is_logical_assignment_expression(node)
                    && !self.is_logical_expression(node)
                    && !(is_optional_chain(node) && is_outermost_optional_chain(node))
            }
        } {
            self.add_antecedent(
                &true_target,
                self.create_flow_condition(
                    FlowFlags::TrueCondition,
                    self.current_flow(),
                    node.as_ref().map(|node| node.borrow().node_wrapper()),
                ),
            );
            self.add_antecedent(
                &false_target,
                self.create_flow_condition(
                    FlowFlags::FalseCondition,
                    self.current_flow(),
                    node.as_ref().map(|node| node.borrow().node_wrapper()),
                ),
            );
        }
    }

    pub(super) fn bind_iterative_statement(
        &self,
        node: &Node, /*Statement*/
        break_target: Rc<FlowNode /*FlowLabel*/>,
        continue_target: Rc<FlowNode /*FlowLabel*/>,
    ) {
        let save_break_target = self.maybe_current_break_target();
        let save_continue_target = self.maybe_current_continue_target();
        self.set_current_break_target(Some(break_target));
        self.set_current_continue_target(Some(continue_target));
        self.bind(Some(node));
        self.set_current_break_target(save_break_target);
        self.set_current_continue_target(save_continue_target);
    }

    pub(super) fn set_continue_target(
        &self,
        node: &Node,
        target: Rc<FlowNode /*FlowLabel*/>,
    ) -> Rc<FlowNode> {
        let mut node = node.node_wrapper();
        let mut label = self.maybe_active_label_list();
        while label.is_some() && node.parent().kind() == SyntaxKind::LabeledStatement {
            let label_present = label.unwrap();
            label_present.set_continue_target(Some(target.clone()));
            label = label_present.next.clone();
            node = node.parent();
        }
        target
    }

    pub(super) fn bind_while_statement(&self, node: &Node /*WhileStatement*/) {
        unimplemented!()
    }

    pub(super) fn bind_do_statement(&self, node: &Node /*DoStatement*/) {
        unimplemented!()
    }

    pub(super) fn bind_for_statement(&self, node: &Node /*ForStatement*/) {
        unimplemented!()
    }

    pub(super) fn bind_for_in_or_for_of_statement(&self, node: &Node /*ForInOrOfStatement*/) {
        unimplemented!()
    }

    pub(super) fn bind_if_statement(&self, node: &Node /*IfStatement*/) {
        let node_as_if_statement = node.as_if_statement();
        let then_label = Rc::new(self.create_branch_label());
        let else_label = Rc::new(self.create_branch_label());
        let post_if_label = Rc::new(self.create_branch_label());
        self.bind_condition(
            Some(&*node_as_if_statement.expression),
            then_label.clone(),
            else_label.clone(),
        );
        self.set_current_flow(Some(self.finish_flow_label(then_label)));
        self.bind(Some(&*node_as_if_statement.then_statement));
        self.add_antecedent(&post_if_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(else_label)));
        self.bind(node_as_if_statement.else_statement.clone());
        self.add_antecedent(&post_if_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(post_if_label)));
    }

    pub(super) fn bind_return_or_throw(&self, node: &Node) {
        self.bind(match node {
            Node::ReturnStatement(return_statement) => return_statement.expression.clone(),
            _ => panic!("Expected return or throw"),
        });
    }

    pub(super) fn bind_break_or_continue_statement(
        &self,
        node: &Node, /*BreakOrContinueStatement*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_try_statement(&self, node: &Node /*TryStatement*/) {
        unimplemented!()
    }

    pub(super) fn bind_switch_statement(&self, node: &Node /*SwitchStatement*/) {
        unimplemented!()
    }

    pub(super) fn bind_case_block(&self, node: &Node /*CaseBlock*/) {
        unimplemented!()
    }

    pub(super) fn bind_case_clause(&self, node: &Node /*CaseClause*/) {
        unimplemented!()
    }

    pub(super) fn bind_expression_statement(&self, node: &Node /*ExpressionStatement*/) {
        self.bind(Some(&*node.as_expression_statement().expression));
    }

    pub(super) fn bind_labeled_statement(&self, node: &Node /*LabeledStatement*/) {
        unimplemented!()
    }

    pub(super) fn bind_prefix_unary_expression_flow(
        &self,
        node: &Node, /*PrefixUnaryExpression*/
    ) {
        if false {
        } else {
            self.bind_each_child(node);
        }
    }

    pub(super) fn bind_postfix_unary_expression_flow(
        &self,
        node: &Node, /*PostfixUnaryExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_destructuring_assignment_flow(
        &self,
        node: &Node, /*DestructuringAssignment*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_binary_expression_flow(&self, node: &Node /*BinaryExpression*/) {
        unimplemented!()
    }

    pub(super) fn bind_delete_expression_flow(&self, node: &Node /*DeleteExpression*/) {
        unimplemented!()
    }

    pub(super) fn bind_conditional_expression_flow(
        &self,
        node: &Node, /*ConditionalExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_variable_declaration_flow(&self, node: &Node /*VariableDeclaration*/) {
        self.bind_each_child(node);
    }

    pub(super) fn bind_binding_element_flow(&self, node: &Node /*BindingElement*/) {
        unimplemented!()
    }

    pub(super) fn bind_jsdoc_type_alias(
        &self,
        node: &Node, /*JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_non_null_expression_flow(
        &self,
        node: &Node, /*NonNullExpression | NonNullChain*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_access_expression_flow(
        &self,
        node: &Node, /*AccessExpression | PropertyAccessChain | ElementAccessChain*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_call_expression_flow(
        &self,
        node: &Node, /*CallExpression | CallChain*/
    ) {
        unimplemented!()
    }

    pub(super) fn get_container_flags(&self, node: &Node) -> ContainerFlags {
        match node.kind() {
            SyntaxKind::InterfaceDeclaration => {
                return ContainerFlags::IsContainer | ContainerFlags::IsInterface;
            }
            SyntaxKind::TypeAliasDeclaration => {
                return ContainerFlags::IsContainer | ContainerFlags::HasLocals;
            }
            SyntaxKind::SourceFile => {
                return ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals;
            }
            SyntaxKind::FunctionDeclaration => {
                return ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike;
            }
            SyntaxKind::Block => {
                return if is_function_like(node.maybe_parent())
                    || is_class_static_block_declaration(&*node.parent())
                {
                    ContainerFlags::None
                } else {
                    ContainerFlags::IsBlockScopedContainer
                };
            }
            _ => (),
        }

        ContainerFlags::None
    }

    pub(super) fn add_to_container_chain(&self, next: &Node) {
        // unimplemented!()
    }

    pub(super) fn declare_symbol_and_add_to_symbol_table(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        match self.container().kind() {
            SyntaxKind::SourceFile => {
                Some(self.declare_source_file_member(node, symbol_flags, symbol_excludes))
            }
            SyntaxKind::InterfaceDeclaration => Some(self.declare_symbol(
                &mut *self.container().symbol().members().borrow_mut(),
                Some(self.container().symbol()),
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )),
            SyntaxKind::FunctionDeclaration | SyntaxKind::TypeAliasDeclaration => {
                Some(self.declare_symbol(
                    &mut *self.container().locals(),
                    Option::<&Symbol>::None,
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                ))
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn declare_source_file_member(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Rc<Symbol> {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol(
                &mut *self.file().locals(),
                Option::<&Symbol>::None,
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )
        }
    }

    pub(super) fn bind_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) {
        self.bind_anonymous_declaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::Object(),
        );
    }

    pub(super) fn bind_anonymous_declaration(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        name: __String,
    ) -> Rc<Symbol> {
        let symbol = self.create_symbol(symbol_flags, name).wrap();
        self.file()
            .as_source_file()
            .keep_strong_reference_to_symbol(symbol.clone());
        self.add_declaration_to_symbol(&symbol, node, symbol_flags);
        symbol
    }

    pub(super) fn bind_block_scoped_declaration(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        let block_scope_container = self.block_scope_container();
        {
            let mut block_scope_container_locals = block_scope_container.maybe_locals();
            if block_scope_container_locals.is_none() {
                *block_scope_container_locals = Some(create_symbol_table(None));
            }
        }
        self.declare_symbol(
            &mut *block_scope_container.locals(),
            Option::<&Symbol>::None,
            node,
            symbol_flags,
            symbol_excludes,
            None,
            None,
        );
    }

    pub(super) fn delayed_bind_jsdoc_typedef_tag(&self) {
        // unimplemented!()
    }

    pub(super) fn bind<TNode: Borrow<Node>>(&self, node: Option<TNode>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let node = node.borrow();
        set_parent(node, self.maybe_parent());

        self.bind_worker(node);

        if node.kind() > SyntaxKind::LastToken {
            let save_parent = self.maybe_parent();
            self.set_parent(Some(node.node_wrapper()));
            let container_flags = self.get_container_flags(node);
            if container_flags == ContainerFlags::None {
                self.bind_children(node);
            } else {
                self.bind_container(node, container_flags);
            }
            self.set_parent(save_parent);
        }
    }

    pub(super) fn bind_jsdoc(&self, node: &Node) {
        // unimplemented!()
    }

    pub(super) fn bind_worker(&self, node: &Node) {
        match node {
            Node::TypeParameterDeclaration(_) => self.bind_type_parameter(node),
            Node::ParameterDeclaration(_) => self.bind_parameter(node),
            Node::VariableDeclaration(_) => self.bind_variable_declaration_or_binding_element(node),
            Node::PropertySignature(_) => self.bind_property_worker(node),
            Node::PropertyAssignment(_) => self.bind_property_or_method_or_accessor(
                node,
                SymbolFlags::Property,
                SymbolFlags::PropertyExcludes,
            ),
            Node::FunctionDeclaration(_) => self.bind_function_declaration(node),
            Node::ObjectLiteralExpression(_) => self.bind_object_literal_expression(node),
            Node::InterfaceDeclaration(_) => self.bind_block_scoped_declaration(
                node,
                SymbolFlags::Interface,
                SymbolFlags::InterfaceExcludes,
            ),
            Node::TypeAliasDeclaration(_) => self.bind_block_scoped_declaration(
                node,
                SymbolFlags::TypeAlias,
                SymbolFlags::TypeAliasExcludes,
            ),
            _ => (),
        }
    }

    pub(super) fn bind_property_worker(&self, node: &Node /*PropertySignature*/) {
        self.bind_property_or_method_or_accessor(
            node,
            SymbolFlags::Property
                | if false {
                    unimplemented!()
                } else {
                    SymbolFlags::None
                },
            SymbolFlags::PropertyExcludes,
        )
    }

    pub(super) fn is_top_level_namespace_assignment(
        &self,
        property_access: &Node, /*BindableAccessExpression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn bind_variable_declaration_or_binding_element(
        &self,
        node: &Node, /*VariableDeclaration*/
    ) {
        let node_as_variable_declaration = node.as_variable_declaration();
        if !is_binding_pattern(Some(node_as_variable_declaration.name())) {
            if false {
                unimplemented!()
            } else if is_block_or_catch_scoped(node) {
                self.bind_block_scoped_declaration(
                    node,
                    SymbolFlags::BlockScopedVariable,
                    SymbolFlags::BlockScopedVariableExcludes,
                );
            } else {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::FunctionScopedVariable,
                    SymbolFlags::FunctionScopedVariableExcludes,
                );
            }
        }
    }

    pub(super) fn bind_parameter(&self, node: &Node /*ParameterDeclaration*/) {
        if is_binding_pattern(Some(node.as_parameter_declaration().name())) {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::FunctionScopedVariable,
                SymbolFlags::ParameterExcludes,
            );
        }
    }

    pub(super) fn bind_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::Function,
                SymbolFlags::FunctionExcludes,
            );
        }
    }

    pub(super) fn bind_property_or_method_or_accessor(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(node, symbol_flags, symbol_excludes);
        }
    }

    pub(super) fn bind_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::TypeParameter,
                SymbolFlags::TypeParameterExcludes,
            );
        }
    }

    pub(super) fn check_unreachable(&self, node: &Node) -> bool {
        false
        // unimplemented!()
    }
}
