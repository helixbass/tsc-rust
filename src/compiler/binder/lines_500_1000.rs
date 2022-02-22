#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use super::{init_flow_node, BinderType, ContainerFlags};
use crate::{
    contains_rc, get_combined_modifier_flags, get_immediately_invoked_function_expression,
    get_name_of_declaration, has_syntactic_modifier, is_ambient_module, is_declaration,
    is_destructuring_assignment, is_in_js_file, is_jsdoc_enum_tag, is_jsdoc_type_alias,
    is_module_declaration, is_property_access_entity_name_expression, node_is_present, Debug_,
    FlowFlags, FlowLabel, FlowNode, FlowNodeBase, FlowStart, ModifierFlags, NodeFlags, Symbol,
    SyntaxKind, __String, create_symbol_table, for_each, for_each_child, is_binding_pattern,
    is_block_or_catch_scoped, is_class_static_block_declaration, is_function_like, set_parent,
    InternalSymbolName, NamedDeclarationInterface, Node, NodeArray, NodeInterface, SymbolFlags,
    SymbolInterface,
};

impl BinderType {
    pub(super) fn declare_module_member(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Rc<Symbol> {
        let has_export_modifier = get_combined_modifier_flags(node)
            .intersects(ModifierFlags::Export)
            || self.jsdoc_treat_as_exported(node);
        if symbol_flags.intersects(SymbolFlags::Alias) {
            if node.kind() == SyntaxKind::ExportSpecifier
                || node.kind() == SyntaxKind::ImportEqualsDeclaration && has_export_modifier
            {
                self.declare_symbol(
                    &mut self.container().symbol().exports().borrow_mut(),
                    Some(self.container().symbol()),
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )
            } else {
                self.declare_symbol(
                    &mut self.container().locals(),
                    Option::<&Symbol>::None,
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )
            }
        } else {
            if is_jsdoc_type_alias(node) {
                Debug_.assert(is_in_js_file(Some(node)), None);
            }
            if !is_ambient_module(node)
                && (has_export_modifier
                    || self
                        .container()
                        .flags()
                        .intersects(NodeFlags::ExportContext))
            {
                if self.container().maybe_locals().is_none()
                    || has_syntactic_modifier(node, ModifierFlags::Default)
                        && self.get_declaration_name(node).is_none()
                {
                    return self.declare_symbol(
                        &mut self.container().symbol().exports().borrow_mut(),
                        Some(self.container().symbol()),
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
                    &mut self.container().locals(),
                    Option::<&Symbol>::None,
                    node,
                    export_kind,
                    symbol_excludes,
                    None,
                    None,
                );
                local.set_export_symbol(Some(self.declare_symbol(
                    &mut self.container().symbol().exports().borrow_mut(),
                    Some(self.container().symbol()),
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )));
                node.set_local_symbol(Some(local.clone()));
                local
            } else {
                self.declare_symbol(
                    &mut self.container().locals(),
                    Option::<&Symbol>::None,
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )
            }
        }
    }

    pub(super) fn jsdoc_treat_as_exported(&self, node: &Node) -> bool {
        let mut node = node.node_wrapper();
        if node.maybe_parent().is_some() && is_module_declaration(&node) {
            node = node.parent();
        }
        if !is_jsdoc_type_alias(&node) {
            return false;
        }
        if !is_jsdoc_enum_tag(&node)
            && node
                .as_jsdoc_typedef_or_callback_tag()
                .maybe_full_name()
                .is_some()
        {
            return true;
        }
        let decl_name = get_name_of_declaration(Some(&*node));
        if decl_name.is_none() {
            return false;
        }
        let decl_name = decl_name.unwrap();
        let decl_name_parent = decl_name.parent();
        if is_property_access_entity_name_expression(&decl_name_parent)
            && self.is_top_level_namespace_assignment(&decl_name_parent)
        {
            return true;
        }
        if is_declaration(&decl_name_parent)
            && get_combined_modifier_flags(&decl_name_parent).intersects(ModifierFlags::Export)
        {
            return true;
        }
        false
    }

    pub(super) fn bind_container(&self, node: &Node, container_flags: ContainerFlags) {
        let save_container = self.maybe_container();
        let save_this_parent_container = self.maybe_this_parent_container();
        let saved_block_scope_container = self.maybe_block_scope_container();

        if container_flags.intersects(ContainerFlags::IsContainer) {
            if node.kind() != SyntaxKind::ArrowFunction {
                self.set_this_parent_container(self.maybe_container());
            }
            self.set_container(Some(node.node_wrapper()));
            self.set_block_scope_container(Some(node.node_wrapper()));
            if container_flags.intersects(ContainerFlags::HasLocals) {
                self.container().set_locals(Some(create_symbol_table(None)));
            }
            self.add_to_container_chain(&self.container());
        } else if container_flags.intersects(ContainerFlags::IsBlockScopedContainer) {
            self.set_block_scope_container(Some(node.node_wrapper()));
            self.block_scope_container().set_locals(None);
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
                && !has_syntactic_modifier(node, ModifierFlags::Async)
                && node
                    .as_function_like_declaration()
                    .maybe_asterisk_token()
                    .is_none()
                && get_immediately_invoked_function_expression(node).is_some();
            if !is_iife {
                self.set_current_flow(Some(Rc::new(init_flow_node(
                    FlowStart::new(FlowFlags::Start, None, None).into(),
                ))));
                if container_flags.intersects(
                    ContainerFlags::IsFunctionExpression
                        | ContainerFlags::IsObjectLiteralOrClassExpressionMethodOrAccessor,
                ) {
                    self.current_flow()
                        .as_flow_start()
                        .set_node(Some(node.node_wrapper()));
                }
            }
            self.set_current_return_target(
                if is_iife
                    || matches!(
                        node.kind(),
                        SyntaxKind::Constructor | SyntaxKind::ClassStaticBlockDeclaration
                    )
                    || (is_in_js_file(Some(node))
                        && matches!(
                            node.kind(),
                            SyntaxKind::FunctionDeclaration | SyntaxKind::FunctionExpression
                        ))
                {
                    Some(Rc::new(self.create_branch_label().into()))
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
            node.set_flags(node.flags() & !NodeFlags::ReachabilityAndEmitFlags);
            if !self
                .current_flow()
                .flags()
                .intersects(FlowFlags::Unreachable)
                && container_flags.intersects(ContainerFlags::IsFunctionLike)
                && node_is_present(match node.kind() {
                    SyntaxKind::ClassStaticBlockDeclaration => {
                        Some(node.as_class_static_block_declaration().body.clone())
                    }
                    _ => node.as_function_like_declaration().maybe_body(),
                })
            {
                node.set_flags(node.flags() | NodeFlags::HasImplicitReturn);
                if matches!(self.maybe_has_explicit_return(), Some(true)) {
                    node.set_flags(node.flags() | NodeFlags::HasExplicitReturn);
                }
                match node.kind() {
                    SyntaxKind::ClassStaticBlockDeclaration => node
                        .as_class_static_block_declaration()
                        .set_end_flow_node(self.maybe_current_flow()),
                    _ => node
                        .as_function_like_declaration()
                        .set_end_flow_node(self.maybe_current_flow()),
                }
            }
            if node.kind() == SyntaxKind::SourceFile {
                node.set_flags(node.flags() | self.emit_flags());
                node.as_source_file()
                    .set_end_flow_node(self.maybe_current_flow());
            }

            if let Some(current_return_target) = self.maybe_current_return_target() {
                self.add_antecedent(&current_return_target, self.current_flow());
                self.set_current_flow(Some(self.finish_flow_label(current_return_target)));
                if matches!(
                    node.kind(),
                    SyntaxKind::Constructor | SyntaxKind::ClassStaticBlockDeclaration
                ) || is_in_js_file(Some(node))
                    && matches!(
                        node.kind(),
                        SyntaxKind::FunctionDeclaration | SyntaxKind::FunctionExpression
                    )
                {
                    match node.kind() {
                        SyntaxKind::ClassStaticBlockDeclaration => node
                            .as_class_static_block_declaration()
                            .set_return_flow_node(self.maybe_current_flow()),
                        _ => node
                            .as_function_like_declaration()
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
            node.set_flags(if matches!(self.maybe_seen_this_keyword(), Some(true)) {
                node.flags() | NodeFlags::ContainsThis
            } else {
                node.flags() & !NodeFlags::ContainsThis
            });
        } else {
            self.bind_children(node);
        }

        self.set_container(save_container);
        self.set_this_parent_container(save_this_parent_container);
        self.set_block_scope_container(saved_block_scope_container);
    }

    pub(super) fn bind_each_functions_first(&self, nodes: Option<&[Rc<Node>] /*NodeArray*/>) {
        self.bind_each_callback(nodes, |n| {
            if n.kind() == SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
        self.bind_each_callback(nodes, |n| {
            if n.kind() != SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
    }

    pub(super) fn bind_each(&self, nodes: Option<&[Rc<Node>] /*NodeArray*/>) {
        if nodes.is_none() {
            return;
        }
        let nodes = nodes.unwrap();

        for_each(nodes, |node, _| {
            self.bind(Some(&**node));
            Option::<()>::None
        });
    }

    pub(super) fn bind_each_callback<TNodeCallback: FnMut(&Node)>(
        &self,
        nodes: Option<&[Rc<Node>] /*NodeArray*/>,
        mut bind_function: TNodeCallback,
    ) {
        if nodes.is_none() {
            return;
        }
        let nodes = nodes.unwrap();

        for_each(nodes, |node, _| {
            bind_function(&*node);
            Option::<()>::None
        });
    }

    pub(super) fn bind_each_child(&self, node: &Node) {
        for_each_child(
            node,
            |node| self.bind(Some(node)),
            Some(|nodes: &NodeArray| self.bind_each(Some(nodes))),
        );
    }

    pub(super) fn bind_children(&self, node: &Node) {
        let save_in_assignment_pattern = self.in_assignment_pattern();
        self.set_in_assignment_pattern(false);
        if self.check_unreachable(node) {
            self.bind_each_child(node);
            self.bind_jsdoc(node);
            self.set_in_assignment_pattern(save_in_assignment_pattern);
            return;
        }
        if node.kind() >= SyntaxKind::FirstStatement
            && node.kind() <= SyntaxKind::LastStatement
            && !matches!(self.options().allow_unreachable_code, Some(true))
        {
            node.set_flow_node(self.maybe_current_flow());
        }
        match node.kind() {
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
                if is_destructuring_assignment(node) {
                    self.set_in_assignment_pattern(save_in_assignment_pattern);
                    self.bind_destructuring_assignment_flow(node);
                    return;
                }
                self.bind_binary_expression_flow(node)
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
                let node_as_source_file = node.as_source_file();
                self.bind_each_functions_first(Some(&node_as_source_file.statements));
                self.bind(Some(&*node_as_source_file.end_of_file_token));
            }
            SyntaxKind::Block | SyntaxKind::ModuleBlock => {
                self.bind_each_functions_first(Some(node.as_has_statements().statements()));
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

    pub(super) fn create_branch_label(&self) -> FlowLabel {
        unimplemented!()
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
        antecedent: Rc<FlowNode>,
    ) {
        let label_as_flow_label = label.as_flow_label();
        if !antecedent.flags().intersects(FlowFlags::Unreachable)
            && !contains_rc(
                label_as_flow_label.maybe_antecedents().as_deref(),
                &antecedent,
            )
        {
            let mut label_antecedents = label_as_flow_label.maybe_antecedents();
            if label_antecedents.is_none() {
                *label_antecedents = Some(vec![]);
            }
            label_antecedents.as_mut().unwrap().push(antecedent.clone());
            self.set_flow_node_referenced(&antecedent);
        }
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

    pub(super) fn do_with_conditional_branches<TArgument>(
        &self,
        action: fn(&BinderType, TArgument),
        value: TArgument,
    ) {
        action(self, value);
    }

    pub(super) fn bind_condition<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef>) {
        self.do_with_conditional_branches(BinderType::bind, node);
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
        self.bind_condition(Some(node_as_if_statement.expression.clone()));
        self.bind(Some(&*node_as_if_statement.then_statement));
        self.bind(node_as_if_statement.else_statement.clone());
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
