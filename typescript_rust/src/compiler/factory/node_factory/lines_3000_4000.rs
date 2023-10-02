use gc::{Finalize, Gc, Trace};
use id_arena::Id;
use local_macros::generate_node_factory_method_wrapper;

use super::{propagate_child_flags, propagate_children_flags};
use crate::{
    are_option_gcs_equal, has_node_array_changed, has_option_node_array_changed,
    is_external_module_reference, is_non_null_chain, modifiers_to_flags, AsDoubleDeref,
    AsExpression, BaseNodeFactory, Block, BreakStatement, CaseBlock, ClassDeclaration,
    ClassLikeDeclarationInterface, ContinueStatement, Debug_, DebuggerStatement, DoStatement,
    EmptyStatement, EnumDeclaration, ExpressionStatement, ExpressionWithTypeArguments,
    ForInStatement, ForOfStatement, ForStatement, FunctionDeclaration,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasMembersInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, IfStatement,
    ImportClause, ImportDeclaration, ImportEqualsDeclaration, InterfaceDeclaration,
    InterfaceOrClassLikeDeclarationInterface, LabeledStatement, MetaProperty, ModifierFlags,
    ModuleBlock, ModuleDeclaration, NamedDeclarationInterface, NamespaceExportDeclaration, Node,
    NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags, NodeInterface, NonNullExpression,
    OmittedExpression, RcNodeOrNodeArrayOrVec, ReturnStatement, SemicolonClassElement,
    SignatureDeclarationInterface, StrOrRcNode, SwitchStatement, SyntaxKind, TemplateSpan,
    ThrowStatement, TransformFlags, TryStatement, TypeAliasDeclaration, VariableDeclaration,
    VariableDeclarationList, VariableStatement, WhileStatement, WithStatement,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize> NodeFactory<TBaseNodeFactory> {
    #[generate_node_factory_method_wrapper]
    pub fn create_omitted_expression_raw(&self) -> OmittedExpression {
        let node = self.create_base_expression(SyntaxKind::OmittedExpression);
        OmittedExpression::new(node)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_expression_with_type_arguments_raw(
        &self,
        expression: Gc<Node>, /*Expression*/
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
    ) -> ExpressionWithTypeArguments {
        let node = self.create_base_node(SyntaxKind::ExpressionWithTypeArguments);
        let node = ExpressionWithTypeArguments::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
            type_arguments.and_then(|type_arguments| {
                self.parenthesizer_rules()
                    .parenthesize_type_arguments(Some(type_arguments.into()))
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_children_flags(node.maybe_type_arguments().as_deref())
                | TransformFlags::ContainsES2015,
        );
        node
    }

    pub fn update_expression_with_type_arguments(
        &self,
        node: &Node,          /*ExpressionWithTypeArguments*/
        expression: Gc<Node>, /*Expression*/
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
    ) -> Gc<Node> {
        let node_as_expression_with_type_arguments = node.as_expression_with_type_arguments();
        let type_arguments = type_arguments.map(Into::into);
        if !Gc::ptr_eq(
            &node_as_expression_with_type_arguments.expression,
            &expression,
        ) || has_option_node_array_changed(
            node_as_expression_with_type_arguments
                .maybe_type_arguments()
                .as_deref(),
            type_arguments.as_ref(),
        ) {
            self.update(
                self.create_expression_with_type_arguments(expression, type_arguments),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_as_expression_raw(
        &self,
        expression: Gc<Node>, /*Expression*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> AsExpression {
        let node = self.create_base_expression(SyntaxKind::AsExpression);
        let node = AsExpression::new(node, expression, type_);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.type_))
                | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_as_expression(
        &self,
        node: &Node,          /*AsExpression*/
        expression: Gc<Node>, /*Expression*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_as_expression = node.as_as_expression();
        if !Gc::ptr_eq(&node_as_as_expression.expression, &expression)
            || !Gc::ptr_eq(&node_as_as_expression.type_, &type_)
        {
            self.update(self.create_as_expression(expression, type_), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_non_null_expression_raw(
        &self,
        expression: Gc<Node>, /*Expression*/
    ) -> NonNullExpression {
        let node = self.create_base_expression(SyntaxKind::NonNullExpression);
        let node = NonNullExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression)) | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_non_null_expression(
        &self,
        node: &Node,          /*NonNullExpression*/
        expression: Gc<Node>, /*Expression*/
    ) -> Gc<Node> {
        let node_as_non_null_expression = node.as_non_null_expression();
        if is_non_null_chain(node) {
            return self.update_non_null_chain(node, expression);
        }
        if !Gc::ptr_eq(&node_as_non_null_expression.expression, &expression) {
            self.update(self.create_non_null_expression(expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_non_null_chain_raw(
        &self,
        expression: Gc<Node>, /*Expression*/
    ) -> NonNullExpression {
        let node = self.create_base_expression(SyntaxKind::NonNullExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let node = NonNullExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression)) | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_non_null_chain(
        &self,
        node: &Node,          /*NonNullChain*/
        expression: Gc<Node>, /*Expression*/
    ) -> Gc<Node> {
        let node_as_non_null_expression = node.as_non_null_expression();
        Debug_.assert(
            node.flags().intersects(NodeFlags::OptionalChain),
            Some("Cannot update a NonNullExpression using updateNonNullChain. Use updateNonNullExpression instead.")
        );
        if !Gc::ptr_eq(&node_as_non_null_expression.expression, &expression) {
            self.update(self.create_non_null_chain(expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_meta_property_raw(
        &self,
        keyword_token: SyntaxKind, /*MetaProperty["keywordToken"]*/
        name: Gc<Node>,            /*Identifier*/
    ) -> MetaProperty {
        let node = self.create_base_expression(SyntaxKind::MetaProperty);
        let node = MetaProperty::new(node, keyword_token, name);
        node.add_transform_flags(propagate_child_flags(Some(&*node.name)));
        match keyword_token {
            SyntaxKind::NewKeyword => {
                node.add_transform_flags(TransformFlags::ContainsES2015);
            }
            SyntaxKind::ImportKeyword => {
                node.add_transform_flags(TransformFlags::ContainsESNext);
            }
            _ => {
                Debug_.assert_never(keyword_token, None);
            }
        }
        node
    }

    pub fn update_meta_property(
        &self,
        node: &Node,    /*MetaProperty*/
        name: Gc<Node>, /*Identifier*/
    ) -> Gc<Node> {
        let node_as_meta_property = node.as_meta_property();
        if !Gc::ptr_eq(&node_as_meta_property.name, &name) {
            self.update(
                self.create_meta_property(node_as_meta_property.keyword_token, name),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_template_span_raw(
        &self,
        expression: Gc<Node /*Expression*/>,
        literal: Gc<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> TemplateSpan {
        let node = self.create_base_node(SyntaxKind::TemplateSpan);
        let node = TemplateSpan::new(node, expression, literal);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.literal))
                | TransformFlags::ContainsES2015,
        );
        node
    }

    pub fn update_template_span(
        &self,
        node: &Node, /*TemplateSpan*/
        expression: Gc<Node /*Expression*/>,
        literal: Gc<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> Gc<Node> {
        let node_as_template_span = node.as_template_span();
        if !Gc::ptr_eq(&node_as_template_span.expression, &expression)
            || !Gc::ptr_eq(&node_as_template_span.literal, &literal)
        {
            self.update(self.create_template_span(expression, literal), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_semicolon_class_element_raw(&self) -> SemicolonClassElement {
        let node = self.create_base_node(SyntaxKind::SemicolonClassElement);
        let node = SemicolonClassElement::new(node);
        node.add_transform_flags(TransformFlags::ContainsES2015);
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_block_raw(
        &self,
        statements: impl Into<NodeArrayOrVec>, /*Statement*/
        multi_line: Option<bool>,
    ) -> Block {
        let node = self.create_base_node(SyntaxKind::Block);
        let node = Block::new(
            node,
            self.create_node_array(Some(statements), None),
            multi_line,
        );
        node.add_transform_flags(propagate_children_flags(Some(&node.statements)));
        node
    }

    pub fn update_block(
        &self,
        node: &Node,                           /*Block*/
        statements: impl Into<NodeArrayOrVec>, /*Statement*/
    ) -> Gc<Node> {
        let node_as_block = node.as_block();
        let statements = statements.into();
        if has_node_array_changed(&node_as_block.statements, &statements) {
            self.update(
                self.create_block(statements, node_as_block.multi_line),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_variable_statement_raw(
        &self,
        id: Id<Node>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        declaration_list: impl Into<RcNodeOrNodeArrayOrVec>,
    ) -> VariableStatement {
        let node = self.create_base_declaration(
            id,
            SyntaxKind::VariableStatement,
            Option::<Id<NodeArray>>::None,
            modifiers,
        );
        let node = VariableStatement::new(
            node,
            match declaration_list.into() {
                RcNodeOrNodeArrayOrVec::RcNode(declaration_list) => declaration_list,
                RcNodeOrNodeArrayOrVec::NodeArray(declaration_list) => {
                    self.create_variable_declaration_list(declaration_list, None)
                }
                RcNodeOrNodeArrayOrVec::Vec(declaration_list) => {
                    self.create_variable_declaration_list(declaration_list, None)
                }
            },
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.declaration_list)));
        if modifiers_to_flags(node.maybe_modifiers().as_double_deref())
            .intersects(ModifierFlags::Ambient)
        {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_variable_statement(
        &self,
        node: &Node, /*VariableStatement*/
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        declaration_list: Gc<Node /*VariableDeclarationList*/>,
    ) -> Gc<Node> {
        let node_as_variable_statement = node.as_variable_statement();
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !Gc::ptr_eq(
                &node_as_variable_statement.declaration_list,
                &declaration_list,
            )
        {
            self.update(
                self.create_variable_statement(modifiers, declaration_list),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_empty_statement_raw(&self) -> EmptyStatement {
        let node = self.create_base_node(SyntaxKind::EmptyStatement);
        EmptyStatement::new(node)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_expression_statement_raw(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> ExpressionStatement {
        let node = self.create_base_node(SyntaxKind::ExpressionStatement);
        let node = ExpressionStatement::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_of_expression_statement(&expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn update_expression_statement(
        &self,
        node: &Node, /*ExpressionStatement*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_expression_statement = node.as_expression_statement();
        if !Gc::ptr_eq(&node_as_expression_statement.expression, &expression) {
            self.update(self.create_expression_statement(expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_if_statement_raw(
        &self,
        expression: Gc<Node /*Expression*/>,
        then_statement: Gc<Node /*Statement*/>,
        else_statement: Option<Gc<Node /*Statement*/>>,
    ) -> IfStatement {
        let node = self.create_base_node(SyntaxKind::IfStatement);
        let node = IfStatement::new(
            node,
            expression,
            self.as_embedded_statement(Some(then_statement)).unwrap(),
            self.as_embedded_statement(else_statement),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.then_statement))
                | propagate_child_flags(node.else_statement.clone()),
        );
        node
    }

    pub fn update_if_statement(
        &self,
        node: &Node, /*IfStatement*/
        expression: Gc<Node /*Expression*/>,
        then_statement: Gc<Node /*Statement*/>,
        else_statement: Option<Gc<Node /*Statement*/>>,
    ) -> Gc<Node> {
        let node_as_if_statement = node.as_if_statement();
        if !Gc::ptr_eq(&node_as_if_statement.expression, &expression)
            || !Gc::ptr_eq(&node_as_if_statement.then_statement, &then_statement)
            || !are_option_gcs_equal(
                node_as_if_statement.else_statement.as_ref(),
                else_statement.as_ref(),
            )
        {
            self.update(
                self.create_if_statement(expression, then_statement, else_statement),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_do_statement_raw(
        &self,
        statement: Gc<Node /*Statement*/>,
        expression: Gc<Node /*Expression*/>,
    ) -> DoStatement {
        let node = self.create_base_node(SyntaxKind::DoStatement);
        let node = DoStatement::new(
            node,
            self.as_embedded_statement(Some(statement)).unwrap(),
            expression,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.statement))
                | propagate_child_flags(Some(&*node.expression)),
        );
        node
    }

    pub fn update_do_statement(
        &self,
        node: &Node, /*DoStatement*/
        statement: Gc<Node /*Statement*/>,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_do_statement = node.as_do_statement();
        if !Gc::ptr_eq(&node_as_do_statement.statement, &statement)
            || !Gc::ptr_eq(&node_as_do_statement.expression, &expression)
        {
            self.update(self.create_do_statement(statement, expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_while_statement_raw(
        &self,
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> WhileStatement {
        let node = self.create_base_node(SyntaxKind::WhileStatement);
        let node = WhileStatement::new(
            node,
            expression,
            self.as_embedded_statement(Some(statement)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.statement)),
        );
        node
    }

    pub fn update_while_statement(
        &self,
        node: &Node, /*WhileStatement*/
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> Gc<Node> {
        let node_as_while_statement = node.as_while_statement();
        if !Gc::ptr_eq(&node_as_while_statement.expression, &expression)
            || !Gc::ptr_eq(&node_as_while_statement.statement, &statement)
        {
            self.update(self.create_while_statement(expression, statement), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_for_statement_raw(
        &self,
        initializer: Option<Gc<Node /*ForInitializer*/>>,
        condition: Option<Gc<Node /*Expression*/>>,
        incrementor: Option<Gc<Node /*Expression*/>>,
        statement: Gc<Node /*Statement*/>,
    ) -> ForStatement {
        let node = self.create_base_node(SyntaxKind::ForStatement);
        let node = ForStatement::new(
            node,
            initializer,
            condition,
            incrementor,
            self.as_embedded_statement(Some(statement)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(node.initializer.clone())
                | propagate_child_flags(node.condition.clone())
                | propagate_child_flags(node.incrementor.clone())
                | propagate_child_flags(Some(&*node.statement)),
        );
        node
    }

    pub fn update_for_statement(
        &self,
        node: &Node, /*ForStatement*/
        initializer: Option<Gc<Node /*ForInitializer*/>>,
        condition: Option<Gc<Node /*Expression*/>>,
        incrementor: Option<Gc<Node /*Expression*/>>,
        statement: Gc<Node /*Statement*/>,
    ) -> Gc<Node> {
        let node_as_for_statement = node.as_for_statement();
        if !are_option_gcs_equal(
            node_as_for_statement.initializer.as_ref(),
            initializer.as_ref(),
        ) || !are_option_gcs_equal(node_as_for_statement.condition.as_ref(), condition.as_ref())
            || !are_option_gcs_equal(
                node_as_for_statement.incrementor.as_ref(),
                incrementor.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_for_statement.statement, &statement)
        {
            self.update(
                self.create_for_statement(initializer, condition, incrementor, statement),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_for_in_statement_raw(
        &self,
        initializer: Gc<Node /*ForInitializer*/>,
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> ForInStatement {
        let node = self.create_base_node(SyntaxKind::ForInStatement);
        let node = ForInStatement::new(
            node,
            initializer,
            expression,
            self.as_embedded_statement(Some(statement)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.initializer))
                | propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.statement)),
        );
        node
    }

    pub fn update_for_in_statement(
        &self,
        node: &Node, /*ForInStatement*/
        initializer: Gc<Node /*ForInitializer*/>,
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> Gc<Node> {
        let node_as_for_in_statement = node.as_for_in_statement();
        if !Gc::ptr_eq(&node_as_for_in_statement.initializer, &initializer)
            || !Gc::ptr_eq(&node_as_for_in_statement.expression, &expression)
            || !Gc::ptr_eq(&node_as_for_in_statement.statement, &statement)
        {
            self.update(
                self.create_for_in_statement(initializer, expression, statement),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_for_of_statement_raw(
        &self,
        await_modifier: Option<Gc<Node /*AwaitKeyword*/>>,
        initializer: Gc<Node /*ForInitializer*/>,
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> ForOfStatement {
        let node = self.create_base_node(SyntaxKind::ForOfStatement);
        let await_modifier_is_some = await_modifier.is_some();
        let node = ForOfStatement::new(
            node,
            await_modifier,
            initializer,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(&expression),
            self.as_embedded_statement(Some(statement)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(node.await_modifier.clone())
                | propagate_child_flags(Some(&*node.initializer))
                | propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.statement))
                | TransformFlags::ContainsES2015,
        );
        if await_modifier_is_some {
            node.add_transform_flags(TransformFlags::ContainsES2018);
        }
        node
    }

    pub fn update_for_of_statement(
        &self,
        node: &Node, /*ForOfStatement*/
        await_modifier: Option<Gc<Node /*AwaitKeyword*/>>,
        initializer: Gc<Node /*ForInitializer*/>,
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> Gc<Node> {
        let node_as_for_of_statement = node.as_for_of_statement();
        if !are_option_gcs_equal(
            node_as_for_of_statement.await_modifier.as_ref(),
            await_modifier.as_ref(),
        ) || !Gc::ptr_eq(&node_as_for_of_statement.initializer, &initializer)
            || !Gc::ptr_eq(&node_as_for_of_statement.expression, &expression)
            || !Gc::ptr_eq(&node_as_for_of_statement.statement, &statement)
        {
            self.update(
                self.create_for_of_statement(await_modifier, initializer, expression, statement),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_continue_statement_raw<'label>(
        &self,
        label: Option<impl Into<StrOrRcNode<'label>>>,
    ) -> ContinueStatement {
        let node = self.create_base_node(SyntaxKind::ContinueStatement);
        let node = ContinueStatement::new(node, self.as_name(label));
        node.add_transform_flags(
            propagate_child_flags(node.label.clone())
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        node
    }

    pub fn update_continue_statement(
        &self,
        node: &Node, /*ContinueStatement*/
        label: Option<Gc<Node /*Identifier*/>>,
    ) -> Gc<Node> {
        let node_as_continue_statement = node.as_continue_statement();
        if !are_option_gcs_equal(node_as_continue_statement.label.as_ref(), label.as_ref()) {
            self.update(self.create_continue_statement(label), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_break_statement_raw<'label>(
        &self,
        label: Option<impl Into<StrOrRcNode<'label>>>,
    ) -> BreakStatement {
        let node = self.create_base_node(SyntaxKind::BreakStatement);
        let node = BreakStatement::new(node, self.as_name(label));
        node.add_transform_flags(
            propagate_child_flags(node.label.clone())
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        node
    }

    pub fn update_break_statement(
        &self,
        node: &Node, /*BreakStatement*/
        label: Option<Gc<Node /*Identifier*/>>,
    ) -> Gc<Node> {
        let node_as_break_statement = node.as_break_statement();
        if !are_option_gcs_equal(node_as_break_statement.label.as_ref(), label.as_ref()) {
            self.update(self.create_break_statement(label), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_return_statement_raw(
        &self,
        expression: Option<Gc<Node /*Expression*/>>,
    ) -> ReturnStatement {
        let node = self.create_base_node(SyntaxKind::ReturnStatement);
        let node = ReturnStatement::new(node, expression);
        node.add_transform_flags(
            propagate_child_flags(node.expression.clone())
                | TransformFlags::ContainsES2018
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        node
    }

    pub fn update_return_statement(
        &self,
        node: &Node, /*ReturnStatement*/
        expression: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        let node_as_return_statement = node.as_return_statement();
        if !are_option_gcs_equal(
            node_as_return_statement.expression.as_ref(),
            expression.as_ref(),
        ) {
            self.update(self.create_return_statement(expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_with_statement_raw(
        &self,
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> WithStatement {
        let node = self.create_base_node(SyntaxKind::WithStatement);
        let node = WithStatement::new(
            node,
            expression,
            self.as_embedded_statement(Some(statement)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.statement)),
        );
        node
    }

    pub fn update_with_statement(
        &self,
        node: &Node, /*WithStatement*/
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> Gc<Node> {
        let node_as_with_statement = node.as_with_statement();
        if !Gc::ptr_eq(&node_as_with_statement.expression, &expression)
            || !Gc::ptr_eq(&node_as_with_statement.statement, &statement)
        {
            self.update(self.create_with_statement(expression, statement), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_switch_statement_raw(
        &self,
        expression: Gc<Node /*Expression*/>,
        case_block: Gc<Node /*CaseBlock*/>,
    ) -> SwitchStatement {
        let node = self.create_base_node(SyntaxKind::SwitchStatement);
        let node = SwitchStatement::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(&expression),
            case_block,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.case_block)),
        );
        node
    }

    pub fn update_switch_statement(
        &self,
        node: &Node, /*SwitchStatement*/
        expression: Gc<Node /*Expression*/>,
        case_block: Gc<Node /*CaseBlock*/>,
    ) -> Gc<Node> {
        let node_as_switch_statement = node.as_switch_statement();
        if !Gc::ptr_eq(&node_as_switch_statement.expression, &expression)
            || !Gc::ptr_eq(&node_as_switch_statement.case_block, &case_block)
        {
            self.update(self.create_switch_statement(expression, case_block), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_labeled_statement_raw<'label>(
        &self,
        label: impl Into<StrOrRcNode<'label>>,
        statement: Gc<Node /*Statement*/>,
    ) -> LabeledStatement {
        let node = self.create_base_node(SyntaxKind::LabeledStatement);
        let node = LabeledStatement::new(
            node,
            self.as_name(Some(label)).unwrap(),
            self.as_embedded_statement(Some(statement)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.label))
                | propagate_child_flags(Some(&*node.statement)),
        );
        node
    }

    pub fn update_labeled_statement(
        &self,
        node: &Node, /*LabeledStatement*/
        label: Gc<Node /*Identifier*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> Gc<Node> {
        let node_as_labeled_statement = node.as_labeled_statement();
        if !Gc::ptr_eq(&node_as_labeled_statement.label, &label)
            || !Gc::ptr_eq(&node_as_labeled_statement.statement, &statement)
        {
            self.update(self.create_labeled_statement(label, statement), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_throw_statement_raw(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> ThrowStatement {
        let node = self.create_base_node(SyntaxKind::ThrowStatement);
        let node = ThrowStatement::new(node, expression);
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn update_throw_statement(
        &self,
        node: &Node, /*ThrowStatement*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_throw_statement = node.as_throw_statement();
        if !Gc::ptr_eq(&node_as_throw_statement.expression, &expression) {
            self.update(self.create_throw_statement(expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_try_statement_raw(
        &self,
        try_block: Gc<Node /*Block*/>,
        catch_clause: Option<Gc<Node /*CatchClause*/>>,
        finally_block: Option<Gc<Node /*Block*/>>,
    ) -> TryStatement {
        let node = self.create_base_node(SyntaxKind::TryStatement);
        let node = TryStatement::new(node, try_block, catch_clause, finally_block);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.try_block))
                | propagate_child_flags(node.catch_clause.clone())
                | propagate_child_flags(node.finally_block.clone()),
        );
        node
    }

    pub fn update_try_statement(
        &self,
        node: &Node, /*TryStatement*/
        try_block: Gc<Node /*Block*/>,
        catch_clause: Option<Gc<Node /*CatchClause*/>>,
        finally_block: Option<Gc<Node /*Block*/>>,
    ) -> Gc<Node> {
        let node_as_try_statement = node.as_try_statement();
        if !Gc::ptr_eq(&node_as_try_statement.try_block, &try_block)
            || !are_option_gcs_equal(
                node_as_try_statement.catch_clause.as_ref(),
                catch_clause.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_try_statement.finally_block.as_ref(),
                finally_block.as_ref(),
            )
        {
            self.update(
                self.create_try_statement(try_block, catch_clause, finally_block),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_debugger_statement_raw(&self) -> DebuggerStatement {
        let node = self.create_base_node(SyntaxKind::DebuggerStatement);
        DebuggerStatement::new(node)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_variable_declaration_raw<'name>(
        &self,
        name: Option<impl Into<StrOrRcNode<'name> /*BindingName*/>>,
        exclamation_token: Option<Gc<Node /*ExclamationToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> VariableDeclaration {
        let node = self.create_base_variable_like_declaration(
            SyntaxKind::VariableDeclaration,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            name,
            type_,
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(&initializer)
            }),
        );
        let exclamation_token_is_some = exclamation_token.is_some();
        let node = VariableDeclaration::new(node, exclamation_token);
        node.add_transform_flags(propagate_child_flags(node.exclamation_token.clone()));
        if exclamation_token_is_some {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_variable_declaration(
        &self,
        node: &Node, /*VariableDeclaration*/
        name: Option<Gc<Node /*BindingName*/>>,
        exclamation_token: Option<Gc<Node /*ExclamationToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        let node_as_variable_declaration = node.as_variable_declaration();
        if !are_option_gcs_equal(
            node_as_variable_declaration.maybe_name().as_ref(),
            name.as_ref(),
        ) || !are_option_gcs_equal(
            node_as_variable_declaration.maybe_type().as_ref(),
            type_.as_ref(),
        ) || !are_option_gcs_equal(
            node_as_variable_declaration.exclamation_token.as_ref(),
            exclamation_token.as_ref(),
        ) || !are_option_gcs_equal(
            node_as_variable_declaration.maybe_initializer().as_ref(),
            initializer.as_ref(),
        ) {
            self.update(
                self.create_variable_declaration(name, exclamation_token, type_, initializer),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_variable_declaration_list_raw(
        &self,
        declarations: impl Into<NodeArrayOrVec>,
        flags: Option<NodeFlags>,
    ) -> VariableDeclarationList {
        let flags = flags.unwrap_or(NodeFlags::None);
        let node = self.create_base_node(SyntaxKind::VariableDeclarationList);
        node.set_flags(node.flags() | (flags & NodeFlags::BlockScoped));
        let node =
            VariableDeclarationList::new(node, self.create_node_array(Some(declarations), None));
        node.add_transform_flags(
            propagate_children_flags(Some(&node.declarations))
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        if flags.intersects(NodeFlags::BlockScoped) {
            node.add_transform_flags(
                TransformFlags::ContainsES2015 | TransformFlags::ContainsBlockScopedBinding,
            );
        }
        node
    }

    pub fn update_variable_declaration_list(
        &self,
        node: &Node, /*VariableDeclarationList*/
        declarations: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_variable_declaration_list = node.as_variable_declaration_list();
        let declarations = declarations.into();
        if has_node_array_changed(
            &node_as_variable_declaration_list.declarations,
            &declarations,
        ) {
            self.update(
                self.create_variable_declaration_list(declarations, Some(node.flags())),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_function_declaration_raw<'name>(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Gc<Node /*AsteriskToken*/>>,
        name: Option<
            impl Into<StrOrRcNode<'name>>,
            /*Identifier*/
        >,
        type_parameters: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeParameterDeclaration>*/
        >,
        parameters: impl Into<NodeArrayOrVec>, /*<ParameterDeclaration>*/
        type_: Option<Gc<Node /*TypeNode*/>>,
        body: Option<Gc<Node /*Block*/>>,
    ) -> FunctionDeclaration {
        let mut node = self.create_base_function_like_declaration(
            SyntaxKind::FunctionDeclaration,
            decorators,
            modifiers,
            name,
            type_parameters,
            Some(parameters),
            type_,
            body,
        );
        node.asterisk_token = asterisk_token;
        let node = FunctionDeclaration::new(node);
        if node.maybe_body().is_none()
            || modifiers_to_flags(node.maybe_modifiers().as_double_deref())
                .intersects(ModifierFlags::Ambient)
        {
            node.set_transform_flags(TransformFlags::ContainsTypeScript);
        } else {
            node.add_transform_flags(
                propagate_child_flags(node.maybe_asterisk_token())
                    | TransformFlags::ContainsHoistedDeclarationOrCompletion,
            );
            if modifiers_to_flags(node.maybe_modifiers().as_double_deref())
                .intersects(ModifierFlags::Async)
            {
                match node.maybe_asterisk_token() {
                    Some(_) => {
                        node.add_transform_flags(TransformFlags::ContainsES2018);
                    }
                    None => {
                        node.add_transform_flags(TransformFlags::ContainsES2017);
                    }
                }
            } else if node.maybe_asterisk_token().is_some() {
                node.add_transform_flags(TransformFlags::ContainsGenerator);
            }
        }
        node
    }

    pub fn update_function_declaration(
        &self,
        node: &Node, /*FunctionDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Gc<Node /*AsteriskToken*/>>,
        name: Option<Gc<Node /*Identifier*/>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        body: Option<Gc<Node /*Block*/>>,
    ) -> Gc<Node> {
        let node_as_function_declaration = node.as_function_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let parameters = parameters.into();
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !are_option_gcs_equal(
                node_as_function_declaration.maybe_asterisk_token().as_ref(),
                asterisk_token.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_function_declaration.maybe_name().as_ref(),
                name.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_function_declaration
                    .maybe_type_parameters()
                    .as_deref(),
                type_parameters.as_ref(),
            )
            || has_node_array_changed(&node_as_function_declaration.parameters(), &parameters)
            || !are_option_gcs_equal(
                node_as_function_declaration.maybe_type().as_ref(),
                type_.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_function_declaration.maybe_body().as_ref(),
                body.as_ref(),
            )
        {
            self.update_base_function_like_declaration(
                self.create_function_declaration(
                    decorators,
                    modifiers,
                    asterisk_token,
                    name,
                    type_parameters,
                    parameters,
                    type_,
                    body,
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_class_declaration_raw<'name>(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<
            impl Into<StrOrRcNode<'name>>,
            /*string | Identifier*/
        >,
        type_parameters: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeParameterDeclaration>*/
        >,
        heritage_clauses: Option<
            impl Into<NodeArrayOrVec>,
            /*<HeritageClause>*/
        >,
        members: impl Into<NodeArrayOrVec>, /*<ClassElement>*/
    ) -> ClassDeclaration {
        let node = self.create_base_class_like_declaration(
            SyntaxKind::ClassDeclaration,
            decorators,
            modifiers,
            name,
            type_parameters,
            heritage_clauses,
            members,
        );
        let node = ClassDeclaration::new(node);
        if modifiers_to_flags(node.maybe_modifiers().as_double_deref())
            .intersects(ModifierFlags::Ambient)
        {
            node.set_transform_flags(TransformFlags::ContainsTypeScript);
        } else {
            node.add_transform_flags(TransformFlags::ContainsES2015);
            if node
                .transform_flags()
                .intersects(TransformFlags::ContainsTypeScriptClassSyntax)
            {
                node.add_transform_flags(TransformFlags::ContainsTypeScript);
            }
        }
        node
    }

    pub fn update_class_declaration(
        &self,
        node: &Node, /*ClassDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<Gc<Node>>,
        type_parameters: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeParameterDeclaration>*/
        >,
        heritage_clauses: Option<
            impl Into<NodeArrayOrVec>,
            /*<HeritageClause>*/
        >,
        members: impl Into<NodeArrayOrVec>, /*<ClassElement>*/
    ) -> Gc<Node> {
        let node_as_class_declaration = node.as_class_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let heritage_clauses = heritage_clauses.map(Into::into);
        let members = members.into();
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !are_option_gcs_equal(
                node_as_class_declaration.maybe_name().as_ref(),
                name.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_class_declaration.maybe_type_parameters().as_deref(),
                type_parameters.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_class_declaration
                    .maybe_heritage_clauses()
                    .as_deref(),
                heritage_clauses.as_ref(),
            )
            || has_node_array_changed(&node_as_class_declaration.members(), &members)
        {
            self.update(
                self.create_class_declaration(
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    heritage_clauses,
                    members,
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_interface_declaration_raw<'name>(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        heritage_clauses: Option<impl Into<NodeArrayOrVec>>,
        members: impl Into<NodeArrayOrVec>,
    ) -> InterfaceDeclaration {
        let node = self.create_base_interface_or_class_like_declaration(
            SyntaxKind::InterfaceDeclaration,
            decorators,
            modifiers,
            Some(name),
            type_parameters,
            heritage_clauses,
        );
        let node = InterfaceDeclaration::new(node, self.create_node_array(Some(members), None));
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_interface_declaration<'name>(
        &self,
        node: &Node, /*InterfaceDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Gc<Node>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        heritage_clauses: Option<impl Into<NodeArrayOrVec>>,
        members: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_interface_declaration = node.as_interface_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let heritage_clauses = heritage_clauses.map(Into::into);
        let members = members.into();
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !Gc::ptr_eq(&node_as_interface_declaration.name(), &name)
            || has_option_node_array_changed(
                node_as_interface_declaration
                    .maybe_type_parameters()
                    .as_deref(),
                type_parameters.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_interface_declaration
                    .maybe_heritage_clauses()
                    .as_deref(),
                heritage_clauses.as_ref(),
            )
            || has_node_array_changed(&node_as_interface_declaration.members(), &members)
        {
            self.update(
                self.create_interface_declaration(
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    heritage_clauses,
                    members,
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_alias_declaration_raw<'name>(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        type_: Gc<Node /*TypeNode*/>,
    ) -> TypeAliasDeclaration {
        let node = self.create_base_generic_named_declaration(
            SyntaxKind::TypeAliasDeclaration,
            decorators,
            modifiers,
            Some(name),
            type_parameters,
        );
        let node = TypeAliasDeclaration::new(node, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_alias_declaration(
        &self,
        node: &Node, /*TypeAliasDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Gc<Node>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_type_alias_declaration = node.as_type_alias_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !Gc::ptr_eq(&node_as_type_alias_declaration.name(), &name)
            || has_option_node_array_changed(
                node_as_type_alias_declaration
                    .maybe_type_parameters()
                    .as_deref(),
                type_parameters.as_ref(),
            )
            || !Gc::ptr_eq(
                &node_as_type_alias_declaration.maybe_type().unwrap(),
                &type_,
            )
        {
            self.update(
                self.create_type_alias_declaration(
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    type_,
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_enum_declaration_raw<'name>(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        members: Option<impl Into<NodeArrayOrVec>>,
    ) -> EnumDeclaration {
        let node = self.create_base_named_declaration(
            SyntaxKind::EnumDeclaration,
            decorators,
            modifiers,
            Some(name),
        );
        let node = EnumDeclaration::new(node, self.create_node_array(members, None));
        node.add_transform_flags(
            propagate_children_flags(Some(&node.members)) | TransformFlags::ContainsTypeScript,
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_enum_declaration(
        &self,
        node: &Node, /*EnumDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Gc<Node>,
        members: Option<impl Into<NodeArrayOrVec>>,
    ) -> Gc<Node> {
        let node_as_enum_declaration = node.as_enum_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let members = members.map(Into::into);
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !Gc::ptr_eq(&node_as_enum_declaration.name(), &name)
            || match members.as_ref() {
                None => true,
                Some(members) => has_node_array_changed(&node_as_enum_declaration.members, members),
            }
        {
            self.update(
                self.create_enum_declaration(decorators, modifiers, name, members),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_module_declaration_raw(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Gc<Node /*ModuleName*/>,
        body: Option<Gc<Node /*ModuleBody*/>>,
        flags: Option<NodeFlags>,
    ) -> ModuleDeclaration {
        let flags = flags.unwrap_or(NodeFlags::None);
        let node =
            self.create_base_declaration(SyntaxKind::ModuleDeclaration, decorators, modifiers);
        node.set_flags(
            node.flags()
                | (flags
                    & (NodeFlags::Namespace
                        | NodeFlags::NestedNamespace
                        | NodeFlags::GlobalAugmentation)),
        );
        let node = ModuleDeclaration::new(node, name, body);
        if modifiers_to_flags(node.maybe_modifiers().as_double_deref())
            .intersects(ModifierFlags::Ambient)
        {
            node.set_transform_flags(TransformFlags::ContainsTypeScript);
        } else {
            node.add_transform_flags(
                propagate_child_flags(Some(&*node.name))
                    | propagate_child_flags(node.body.clone())
                    | TransformFlags::ContainsTypeScript,
            );
        }
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_module_declaration(
        &self,
        node: &Node, /*ModuleDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Gc<Node /*ModuleName*/>,
        body: Option<Gc<Node /*ModuleBody*/>>,
    ) -> Gc<Node> {
        let node_as_module_declaration = node.as_module_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !Gc::ptr_eq(&node_as_module_declaration.name, &name)
            || !are_option_gcs_equal(node_as_module_declaration.body.as_ref(), body.as_ref())
        {
            self.update(
                self.create_module_declaration(
                    decorators,
                    modifiers,
                    name,
                    body,
                    Some(node.flags()),
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_module_block_raw(
        &self,
        statements: Option<impl Into<NodeArrayOrVec>>,
    ) -> ModuleBlock {
        let node = self.create_base_node(SyntaxKind::ModuleBlock);
        let node = ModuleBlock::new(node, self.create_node_array(statements, None));
        node.add_transform_flags(propagate_children_flags(Some(&node.statements)));
        node
    }

    pub fn update_module_block(
        &self,
        node: &Node, /*ModuleBlock*/
        statements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_module_block = node.as_module_block();
        let statements = statements.into();
        if has_node_array_changed(&node_as_module_block.statements, &statements) {
            self.update(self.create_module_block(Some(statements)), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_case_block_raw(
        &self,
        clauses: impl Into<NodeArrayOrVec>, /*<CaseOrDefaultClause>*/
    ) -> CaseBlock {
        let node = self.create_base_node(SyntaxKind::CaseBlock);
        let node = CaseBlock::new(node, self.create_node_array(Some(clauses), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.clauses)));
        node
    }

    pub fn update_case_block(
        &self,
        node: &Node,                        /*CaseBlock*/
        clauses: impl Into<NodeArrayOrVec>, /*<CaseOrDefaultClause>*/
    ) -> Gc<Node> {
        let node_as_case_block = node.as_case_block();
        let clauses = clauses.into();
        if has_node_array_changed(&node_as_case_block.clauses, &clauses) {
            self.update(self.create_case_block(clauses), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_namespace_export_declaration_raw<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>,
    ) -> NamespaceExportDeclaration {
        let node = self.create_base_named_declaration(
            SyntaxKind::NamespaceExportDeclaration,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(name),
        );
        let node = NamespaceExportDeclaration::new(node);
        node.set_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_namespace_export_declaration(
        &self,
        _node: &Node, /*NamespaceExportDeclaration*/
        _name: Gc<Node /*Identifier*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_import_equals_declaration_raw<'name>(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_type_only: bool,
        name: impl Into<StrOrRcNode<'name>>,
        module_reference: Gc<Node /*ModuleReference*/>,
    ) -> ImportEqualsDeclaration {
        let node = self.create_base_named_declaration(
            SyntaxKind::ImportEqualsDeclaration,
            decorators,
            modifiers,
            Some(name),
        );
        let node = ImportEqualsDeclaration::new(node, is_type_only, module_reference);
        node.add_transform_flags(propagate_child_flags(Some(&*node.module_reference)));
        if !is_external_module_reference(&node.module_reference) {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_import_equals_declaration(
        &self,
        node: &Node, /*ImportEqualsDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_type_only: bool,
        name: Gc<Node /*Identifier*/>,
        module_reference: Gc<Node /*ModuleReference*/>,
    ) -> Gc<Node> {
        let node_as_import_equals_declaration = node.as_import_equals_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || node_as_import_equals_declaration.is_type_only != is_type_only
            || !Gc::ptr_eq(
                &node_as_import_equals_declaration.module_reference,
                &module_reference,
            )
        {
            self.update(
                self.create_import_equals_declaration(
                    decorators,
                    modifiers,
                    is_type_only,
                    name,
                    module_reference,
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_import_declaration_raw(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        import_clause: Option<Gc<Node /*ImportClause*/>>,
        module_specifier: Gc<Node /*Expression*/>,
        assert_clause: Option<Gc<Node /*AssertClause*/>>,
    ) -> ImportDeclaration {
        let node =
            self.create_base_declaration(SyntaxKind::ImportDeclaration, decorators, modifiers);
        let node = ImportDeclaration::new(node, import_clause, module_specifier, assert_clause);
        node.add_transform_flags(
            propagate_child_flags(node.import_clause.clone())
                | propagate_child_flags(Some(&*node.module_specifier)),
        );
        node
    }

    pub fn update_import_declaration(
        &self,
        node: &Node, /*ImportDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        import_clause: Option<Gc<Node /*ImportClause*/>>,
        module_specifier: Gc<Node /*Expression*/>,
        assert_clause: Option<Gc<Node /*AssertClause*/>>,
    ) -> Gc<Node> {
        let node_as_import_declaration = node.as_import_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !are_option_gcs_equal(
                node_as_import_declaration.import_clause.as_ref(),
                import_clause.as_ref(),
            )
            || !Gc::ptr_eq(
                &node_as_import_declaration.module_specifier,
                &module_specifier,
            )
            || !are_option_gcs_equal(
                node_as_import_declaration.assert_clause.as_ref(),
                assert_clause.as_ref(),
            )
        {
            self.update(
                self.create_import_declaration(
                    decorators,
                    modifiers,
                    import_clause,
                    module_specifier,
                    assert_clause,
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_import_clause_raw(
        &self,
        is_type_only: bool,
        name: Option<Gc<Node /*Identifier*/>>,
        named_bindings: Option<Gc<Node /*NamedImportBindings*/>>,
    ) -> ImportClause {
        let node = self.create_base_node(SyntaxKind::ImportClause);
        let node = ImportClause::new(node, is_type_only, name, named_bindings);
        node.add_transform_flags(
            propagate_child_flags(node.name.clone())
                | propagate_child_flags(node.named_bindings.clone()),
        );
        if is_type_only {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_import_clause(
        &self,
        node: &Node, /*ImportClause*/
        is_type_only: bool,
        name: Option<Gc<Node /*Identifier*/>>,
        named_bindings: Option<Gc<Node /*NamedImportBindings*/>>,
    ) -> Gc<Node> {
        let node_as_import_clause = node.as_import_clause();
        if node_as_import_clause.is_type_only != is_type_only
            || !are_option_gcs_equal(node_as_import_clause.name.as_ref(), name.as_ref())
            || !are_option_gcs_equal(
                node_as_import_clause.named_bindings.as_ref(),
                named_bindings.as_ref(),
            )
        {
            self.update(
                self.create_import_clause(is_type_only, name, named_bindings),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }
}
