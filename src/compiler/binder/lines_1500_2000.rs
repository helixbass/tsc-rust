use std::rc::Rc;

use super::{BinderType, ContainerFlags, ModuleInstanceState};
use crate::{
    append, get_host_signature_from_jsdoc, has_syntactic_modifier, is_ambient_module,
    is_binding_pattern, is_class_static_block_declaration, is_export_assignment,
    is_export_declaration, is_external_module, is_for_in_or_of_statement, is_function_like,
    is_identifier, is_module_augmentation_external, is_module_block,
    is_object_literal_or_class_expression_method_or_accessor, is_omitted_expression,
    is_optional_chain, is_optional_chain_root, is_outermost_optional_chain,
    is_push_or_unshift_identifier, is_source_file, is_static, set_parent, set_parent_recursive,
    skip_parentheses, try_cast, try_parse_pattern, Diagnostics, FlowFlags, FlowNode,
    HasInitializerInterface, ModifierFlags, NamedDeclarationInterface, Node, NodeFlags,
    NodeInterface, PatternAmbientModule, StringOrNodeArray, StringOrPattern, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind,
};

impl BinderType {
    pub(super) fn bind_binary_expression_flow(&self, node: &Node /*BinaryExpression*/) {
        unimplemented!()
    }

    pub(super) fn bind_delete_expression_flow(&self, node: &Node /*DeleteExpression*/) {
        self.bind_each_child(node);
        let node_as_delete_expression = node.as_delete_expression();
        if node_as_delete_expression.expression.kind() == SyntaxKind::PropertyAccessExpression {
            self.bind_assignment_target_flow(&node_as_delete_expression.expression);
        }
    }

    pub(super) fn bind_conditional_expression_flow(
        &self,
        node: &Node, /*ConditionalExpression*/
    ) {
        let true_label = self.create_branch_label();
        let false_label = self.create_branch_label();
        let post_expression_label = self.create_branch_label();
        let node_as_conditional_expression = node.as_conditional_expression();
        self.bind_condition(
            Some(&*node_as_conditional_expression.condition),
            true_label.clone(),
            false_label.clone(),
        );
        self.set_current_flow(Some(self.finish_flow_label(true_label)));
        self.bind(Some(&*node_as_conditional_expression.question_token));
        self.bind(Some(&*node_as_conditional_expression.when_true));
        self.add_antecedent(&post_expression_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(false_label)));
        self.bind(Some(&*node_as_conditional_expression.colon_token));
        self.bind(Some(&*node_as_conditional_expression.when_false));
        self.add_antecedent(&post_expression_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(post_expression_label)));
    }

    pub(super) fn bind_initialized_variable_flow(
        &self,
        node: &Node, /*VariableDeclaration | ArrayBindingElement*/
    ) {
        let name: Option<Rc<Node>> = if !is_omitted_expression(node) {
            node.as_named_declaration().maybe_name()
        } else {
            None
        };
        if is_binding_pattern(Some(node)) {
            for child in node.as_has_elements().elements() {
                self.bind_initialized_variable_flow(child);
            }
        } else {
            self.set_current_flow(Some(self.create_flow_mutation(
                FlowFlags::Assignment,
                self.current_flow(),
                node,
            )));
        }
    }

    pub(super) fn bind_variable_declaration_flow(&self, node: &Node /*VariableDeclaration*/) {
        self.bind_each_child(node);
        if node.as_variable_declaration().maybe_initializer().is_some()
            || is_for_in_or_of_statement(&node.parent().parent())
        {
            self.bind_initialized_variable_flow(node);
        }
    }

    pub(super) fn bind_binding_element_flow(&self, node: &Node /*BindingElement*/) {
        let node_as_binding_element = node.as_binding_element();
        if is_binding_pattern(node_as_binding_element.maybe_name()) {
            self.bind_each(node_as_binding_element.maybe_decorators().as_deref());
            self.bind_each(node_as_binding_element.maybe_modifiers().as_deref());
            self.bind(node_as_binding_element.dot_dot_dot_token.clone());
            self.bind(node_as_binding_element.property_name.clone());
            self.bind(node_as_binding_element.maybe_initializer());
            self.bind(node_as_binding_element.maybe_name());
        } else {
            self.bind_each_child(node);
        }
    }

    pub(super) fn bind_jsdoc_type_alias(
        &self,
        node: &Node, /*JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag*/
    ) {
        let node_as_jsdoc_tag = node.as_jsdoc_tag();
        self.bind(Some(node_as_jsdoc_tag.tag_name()));
        if node.kind() != SyntaxKind::JSDocEnumTag {
            let node_as_jsdoc_typedef_or_callback_tag = node.as_jsdoc_typedef_or_callback_tag();
            if let Some(node_full_name) = node_as_jsdoc_typedef_or_callback_tag.maybe_full_name() {
                set_parent(&node_full_name, Some(node.node_wrapper()));
                set_parent_recursive(Some(node_full_name), false);
            }
        }
        if let Some(StringOrNodeArray::NodeArray(node_comment)) = node_as_jsdoc_tag.maybe_comment()
        {
            self.bind_each(Some(node_comment));
        }
    }

    pub(super) fn bind_jsdoc_class_tag(&self, node: &Node /*JSDocClassTag*/) {
        self.bind_each_child(node);
        let host = get_host_signature_from_jsdoc(node);
        if let Some(host) = host.filter(|host| host.kind() != SyntaxKind::MethodDeclaration) {
            self.add_declaration_to_symbol(&host.symbol(), &host, SymbolFlags::Class);
        }
    }

    pub(super) fn bind_optional_expression(
        &self,
        node: &Node, /*expression*/
        true_target: Rc<FlowNode /*FlowLabel*/>,
        false_target: Rc<FlowNode /*FlowLabel*/>,
    ) {
        self.do_with_conditional_branches(
            |node| self.bind(Some(node)),
            node,
            true_target.clone(),
            false_target.clone(),
        );
        if !is_optional_chain(node) || is_outermost_optional_chain(node) {
            self.add_antecedent(
                &true_target,
                self.create_flow_condition(
                    FlowFlags::TrueCondition,
                    self.current_flow(),
                    Some(node),
                ),
            );
            self.add_antecedent(
                &false_target,
                self.create_flow_condition(
                    FlowFlags::FalseCondition,
                    self.current_flow(),
                    Some(node),
                ),
            );
        }
    }

    pub(super) fn bind_optional_chain_rest(&self, node: &Node /*OptionalChain*/) {
        match node.kind() {
            SyntaxKind::PropertyAccessExpression => {
                let node_as_property_access_expression = node.as_property_access_expression();
                self.bind(
                    node_as_property_access_expression
                        .question_dot_token
                        .clone(),
                );
                self.bind(Some(&*node_as_property_access_expression.name));
            }
            SyntaxKind::ElementAccessExpression => {
                let node_as_element_access_expression = node.as_element_access_expression();
                self.bind(node_as_element_access_expression.question_dot_token.clone());
                self.bind(Some(
                    &*node_as_element_access_expression.argument_expression,
                ));
            }
            SyntaxKind::CallExpression => {
                let node_as_call_expression = node.as_call_expression();
                self.bind(node_as_call_expression.question_dot_token.clone());
                self.bind_each(node_as_call_expression.type_arguments.as_deref());
                self.bind_each(Some(&node_as_call_expression.arguments));
            }
            _ => (),
        }
    }

    pub(super) fn bind_optional_chain(
        &self,
        node: &Node, /*OptionalChain*/
        true_target: Rc<FlowNode /*FlowLabel*/>,
        false_target: Rc<FlowNode /*FlowLabel*/>,
    ) {
        let pre_chain_label = if is_optional_chain_root(node) {
            Some(self.create_branch_label())
        } else {
            None
        };
        self.bind_optional_expression(
            &node.as_has_expression().expression(),
            pre_chain_label
                .clone()
                .unwrap_or_else(|| true_target.clone()),
            false_target.clone(),
        );
        if let Some(pre_chain_label) = pre_chain_label {
            self.set_current_flow(Some(self.finish_flow_label(pre_chain_label)));
        }
        self.do_with_conditional_branches(
            |node| self.bind_optional_chain_rest(node),
            node,
            true_target.clone(),
            false_target.clone(),
        );
        if is_outermost_optional_chain(node) {
            self.add_antecedent(
                &true_target,
                self.create_flow_condition(
                    FlowFlags::TrueCondition,
                    self.current_flow(),
                    Some(node),
                ),
            );
            self.add_antecedent(
                &false_target,
                self.create_flow_condition(
                    FlowFlags::FalseCondition,
                    self.current_flow(),
                    Some(node),
                ),
            );
        }
    }

    pub(super) fn bind_optional_chain_flow(&self, node: &Node /*OptionalChain*/) {
        if self.is_top_level_logical_expression(node) {
            let post_expression_label = self.create_branch_label();
            self.bind_optional_chain(
                node,
                post_expression_label.clone(),
                post_expression_label.clone(),
            );
            self.set_current_flow(Some(self.finish_flow_label(post_expression_label)));
        } else {
            self.bind_optional_chain(
                node,
                self.current_true_target(),
                self.current_false_target(),
            );
        }
    }

    pub(super) fn bind_non_null_expression_flow(
        &self,
        node: &Node, /*NonNullExpression | NonNullChain*/
    ) {
        if is_optional_chain(node) {
            self.bind_optional_chain_flow(node);
        } else {
            self.bind_each_child(node);
        }
    }

    pub(super) fn bind_access_expression_flow(
        &self,
        node: &Node, /*AccessExpression | PropertyAccessChain | ElementAccessChain*/
    ) {
        if is_optional_chain(node) {
            self.bind_optional_chain_flow(node);
        } else {
            self.bind_each_child(node);
        }
    }

    pub(super) fn bind_call_expression_flow(
        &self,
        node: &Node, /*CallExpression | CallChain*/
    ) {
        let node_as_call_expression = node.as_call_expression();
        if is_optional_chain(node) {
            self.bind_optional_chain_flow(node);
        } else {
            let expr = skip_parentheses(&node_as_call_expression.expression, None);
            if matches!(
                expr.kind(),
                SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
            ) {
                self.bind_each(node_as_call_expression.type_arguments.as_deref());
                self.bind_each(Some(&*node_as_call_expression.arguments));
                self.bind(Some(&*node_as_call_expression.expression));
            } else {
                self.bind_each_child(node);
                if node_as_call_expression.expression.kind() == SyntaxKind::SuperKeyword {
                    self.set_current_flow(Some(self.create_flow_call(self.current_flow(), node)));
                }
            }
        }
        if node_as_call_expression.expression.kind() == SyntaxKind::PropertyAccessExpression {
            let property_access = node_as_call_expression
                .expression
                .as_property_access_expression();
            if is_identifier(&property_access.name)
                && self.is_narrowable_operand(&property_access.expression)
                && is_push_or_unshift_identifier(&property_access.name)
            {
                self.set_current_flow(Some(self.create_flow_mutation(
                    FlowFlags::ArrayMutation,
                    self.current_flow(),
                    node,
                )));
            }
        }
    }

    pub(super) fn get_container_flags(&self, node: &Node) -> ContainerFlags {
        match node.kind() {
            SyntaxKind::ClassExpression
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::ObjectLiteralExpression
            | SyntaxKind::TypeLiteral
            | SyntaxKind::JSDocTypeLiteral
            | SyntaxKind::JsxAttributes => ContainerFlags::IsContainer,

            SyntaxKind::InterfaceDeclaration => {
                ContainerFlags::IsContainer | ContainerFlags::IsInterface
            }

            SyntaxKind::ModuleDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::MappedType => ContainerFlags::IsContainer | ContainerFlags::HasLocals,

            SyntaxKind::SourceFile => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
            }

            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor | SyntaxKind::MethodDeclaration => {
                if is_object_literal_or_class_expression_method_or_accessor(node) {
                    return ContainerFlags::IsContainer
                        | ContainerFlags::IsControlFlowContainer
                        | ContainerFlags::HasLocals
                        | ContainerFlags::IsFunctionLike
                        | ContainerFlags::IsObjectLiteralOrClassExpressionMethodOrAccessor;
                }
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
            }

            SyntaxKind::Constructor
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::CallSignature
            | SyntaxKind::JSDocSignature
            | SyntaxKind::JSDocFunctionType
            | SyntaxKind::FunctionType
            | SyntaxKind::ConstructSignature
            | SyntaxKind::IndexSignature
            | SyntaxKind::ConstructorType
            | SyntaxKind::ClassStaticBlockDeclaration => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
            }

            SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
                    | ContainerFlags::IsFunctionExpression
            }

            SyntaxKind::ModuleBlock => ContainerFlags::IsControlFlowContainer,
            SyntaxKind::PropertyDeclaration => {
                if node.as_property_declaration().maybe_initializer().is_some() {
                    ContainerFlags::IsControlFlowContainer
                } else {
                    ContainerFlags::None
                }
            }

            SyntaxKind::CatchClause
            | SyntaxKind::ForStatement
            | SyntaxKind::ForInStatement
            | SyntaxKind::ForOfStatement
            | SyntaxKind::CaseBlock => ContainerFlags::IsBlockScopedContainer,

            SyntaxKind::Block => {
                if is_function_like(node.maybe_parent())
                    || is_class_static_block_declaration(&node.parent())
                {
                    ContainerFlags::None
                } else {
                    ContainerFlags::IsBlockScopedContainer
                }
            }
            _ => ContainerFlags::None,
        }
    }

    pub(super) fn add_to_container_chain(&self, next: &Node) {
        if let Some(last_container) = self.maybe_last_container() {
            last_container.set_next_container(Some(next.node_wrapper()));
        }

        self.set_last_container(Some(next.node_wrapper()));
    }

    pub(super) fn declare_symbol_and_add_to_symbol_table(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        match self.container().kind() {
            SyntaxKind::ModuleDeclaration => {
                Some(self.declare_module_member(node, symbol_flags, symbol_excludes))
            }

            SyntaxKind::SourceFile => {
                Some(self.declare_source_file_member(node, symbol_flags, symbol_excludes))
            }

            SyntaxKind::ClassExpression | SyntaxKind::ClassDeclaration => {
                Some(self.declare_class_member(node, symbol_flags, symbol_excludes))
            }

            SyntaxKind::EnumDeclaration => Some(self.declare_symbol(
                &mut *self.container().symbol().exports().borrow_mut(),
                Some(self.container().symbol()),
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )),

            SyntaxKind::TypeLiteral
            | SyntaxKind::JSDocTypeLiteral
            | SyntaxKind::ObjectLiteralExpression
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::JsxAttributes => Some(self.declare_symbol(
                &mut *self.container().symbol().members().borrow_mut(),
                Some(self.container().symbol()),
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )),

            SyntaxKind::FunctionType
            | SyntaxKind::ConstructorType
            | SyntaxKind::CallSignature
            | SyntaxKind::ConstructSignature
            | SyntaxKind::JSDocSignature
            | SyntaxKind::IndexSignature
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::JSDocFunctionType
            | SyntaxKind::JSDocTypedefTag
            | SyntaxKind::JSDocCallbackTag
            | SyntaxKind::ClassStaticBlockDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::MappedType => Some(self.declare_symbol(
                &mut self.container().locals().borrow_mut(),
                Option::<&Symbol>::None,
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )),
            _ => None,
        }
    }

    pub(super) fn declare_class_member(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Rc<Symbol> {
        if is_static(node) {
            self.declare_symbol(
                &mut *self.container().symbol().exports().borrow_mut(),
                Some(self.container().symbol()),
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )
        } else {
            self.declare_symbol(
                &mut *self.container().symbol().members().borrow_mut(),
                Some(self.container().symbol()),
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )
        }
    }

    pub(super) fn declare_source_file_member(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Rc<Symbol> {
        if is_external_module(&self.file()) {
            self.declare_module_member(node, symbol_flags, symbol_excludes)
        } else {
            self.declare_symbol(
                &mut *self.file().locals().borrow_mut(),
                Option::<&Symbol>::None,
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )
        }
    }

    pub(super) fn has_export_declarations(
        &self,
        node: &Node, /*ModuleDeclaration | SourceFile*/
    ) -> bool {
        let body: Option<Rc<Node>> = if is_source_file(node) {
            Some(node.node_wrapper())
        } else {
            node.as_module_declaration()
                .body
                .as_ref()
                .and_then(|body| try_cast(body, |body| is_module_block(body)))
                .map(Clone::clone)
        };
        match body {
            None => false,
            Some(body) => body
                .as_has_statements()
                .statements()
                .iter()
                .any(|s| is_export_declaration(s) || is_export_assignment(s)),
        }
    }

    pub(super) fn set_export_context_flag(
        &self,
        node: &Node, /*ModuleDeclaration | SourceFile*/
    ) {
        if node.flags().intersects(NodeFlags::Ambient) && !self.has_export_declarations(node) {
            node.set_flags(node.flags() | NodeFlags::ExportContext);
        } else {
            node.set_flags(node.flags() & !NodeFlags::ExportContext);
        }
    }

    pub(super) fn bind_module_declaration(&self, node: &Node /*ModuleDeclaration*/) {
        self.set_export_context_flag(node);
        if is_ambient_module(node) {
            if has_syntactic_modifier(node, ModifierFlags::Export) {
                self.error_on_first_token(node, &Diagnostics::export_modifier_cannot_be_applied_to_ambient_modules_and_module_augmentations_since_they_are_always_visible, None);
            }
            if is_module_augmentation_external(node) {
                self.declare_module_symbol(node);
            } else {
                let mut pattern: Option<StringOrPattern> = None;
                let node_as_module_declaration = node.as_module_declaration();
                if node_as_module_declaration.name.kind() == SyntaxKind::StringLiteral {
                    let text = node_as_module_declaration
                        .name
                        .as_literal_like_node()
                        .text();
                    pattern = try_parse_pattern(&text);
                    if pattern.is_none() {
                        self.error_on_first_token(
                            &node_as_module_declaration.name,
                            &Diagnostics::Pattern_0_can_have_at_most_one_Asterisk_character,
                            Some(vec![text.to_owned()]),
                        );
                    }
                }

                let symbol = self
                    .declare_symbol_and_add_to_symbol_table(
                        node,
                        SymbolFlags::ValueModule,
                        SymbolFlags::ValueModuleExcludes,
                    )
                    .unwrap();
                let file = self.file();
                let mut pattern_ambient_modules =
                    file.as_source_file().maybe_pattern_ambient_modules();
                if pattern_ambient_modules.is_none() {
                    *pattern_ambient_modules = Some(vec![]);
                }
                append(
                    pattern_ambient_modules.as_mut().unwrap(),
                    match pattern {
                        Some(StringOrPattern::Pattern(pattern)) => {
                            Some(Rc::new(PatternAmbientModule::new(pattern, symbol)))
                        }
                        _ => None,
                    },
                );
            }
        } else {
            let state = self.declare_module_symbol(node);
            if state != ModuleInstanceState::NonInstantiated {
                let symbol = node.symbol();
                symbol.set_const_enum_only_module(Some(
                    !symbol.flags().intersects(
                        SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::RegularEnum,
                    ) && state == ModuleInstanceState::ConstEnumOnly
                        && !matches!(symbol.maybe_const_enum_only_module(), Some(false)),
                ));
            }
        }
    }
}
