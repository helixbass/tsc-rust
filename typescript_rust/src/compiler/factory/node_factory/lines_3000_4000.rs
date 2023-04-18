#![allow(non_upper_case_globals)]

use gc::Gc;

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

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_omitted_expression(&self, base_factory: &TBaseNodeFactory) -> OmittedExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::OmittedExpression);
        OmittedExpression::new(node)
    }

    pub fn create_expression_with_type_arguments(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node>, /*Expression*/
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
    ) -> ExpressionWithTypeArguments {
        let node = self.create_base_node(base_factory, SyntaxKind::ExpressionWithTypeArguments);
        let mut node = ExpressionWithTypeArguments::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
            type_arguments.and_then(|type_arguments| {
                self.parenthesizer_rules()
                    .parenthesize_type_arguments(base_factory, Some(type_arguments.into()))
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
        base_factory: &TBaseNodeFactory,
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
                self.create_expression_with_type_arguments(
                    base_factory,
                    expression,
                    type_arguments,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_as_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node>, /*Expression*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> AsExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::AsExpression);
        let mut node = AsExpression::new(node, expression, type_);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.type_))
                | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_as_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,          /*AsExpression*/
        expression: Gc<Node>, /*Expression*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_as_expression = node.as_as_expression();
        if !Gc::ptr_eq(&node_as_as_expression.expression, &expression)
            || !Gc::ptr_eq(&node_as_as_expression.type_, &type_)
        {
            self.update(
                self.create_as_expression(base_factory, expression, type_)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_non_null_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node>, /*Expression*/
    ) -> NonNullExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::NonNullExpression);
        let mut node = NonNullExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression)) | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_non_null_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,          /*NonNullExpression*/
        expression: Gc<Node>, /*Expression*/
    ) -> Gc<Node> {
        let node_as_non_null_expression = node.as_non_null_expression();
        if is_non_null_chain(node) {
            return self.update_non_null_chain(base_factory, node, expression);
        }
        if !Gc::ptr_eq(&node_as_non_null_expression.expression, &expression) {
            self.update(
                self.create_non_null_expression(base_factory, expression)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_non_null_chain(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node>, /*Expression*/
    ) -> NonNullExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::NonNullExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let mut node = NonNullExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression)) | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_non_null_chain(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,          /*NonNullChain*/
        expression: Gc<Node>, /*Expression*/
    ) -> Gc<Node> {
        let node_as_non_null_expression = node.as_non_null_expression();
        Debug_.assert(
            node.flags().intersects(NodeFlags::OptionalChain),
            Some("Cannot update a NonNullExpression using updateNonNullChain. Use updateNonNullExpression instead.")
        );
        if !Gc::ptr_eq(&node_as_non_null_expression.expression, &expression) {
            self.update(
                self.create_non_null_chain(base_factory, expression).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_meta_property(
        &self,
        base_factory: &TBaseNodeFactory,
        keyword_token: SyntaxKind, /*MetaProperty["keywordToken"]*/
        name: Gc<Node>,            /*Identifier*/
    ) -> MetaProperty {
        let node = self.create_base_expression(base_factory, SyntaxKind::MetaProperty);
        let mut node = MetaProperty::new(node, keyword_token, name);
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
        base_factory: &TBaseNodeFactory,
        node: &Node,    /*MetaProperty*/
        name: Gc<Node>, /*Identifier*/
    ) -> Gc<Node> {
        let node_as_meta_property = node.as_meta_property();
        if !Gc::ptr_eq(&node_as_meta_property.name, &name) {
            self.update(
                self.create_meta_property(base_factory, node_as_meta_property.keyword_token, name)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_template_span(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        literal: Gc<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> TemplateSpan {
        let node = self.create_base_node(base_factory, SyntaxKind::TemplateSpan);
        let mut node = TemplateSpan::new(node, expression, literal);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.literal))
                | TransformFlags::ContainsES2015,
        );
        node
    }

    pub fn update_template_span(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*TemplateSpan*/
        expression: Gc<Node /*Expression*/>,
        literal: Gc<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> Gc<Node> {
        let node_as_template_span = node.as_template_span();
        if !Gc::ptr_eq(&node_as_template_span.expression, &expression)
            || !Gc::ptr_eq(&node_as_template_span.literal, &literal)
        {
            self.update(
                self.create_template_span(base_factory, expression, literal)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_semicolon_class_element(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> SemicolonClassElement {
        let node = self.create_base_node(base_factory, SyntaxKind::SemicolonClassElement);
        let mut node = SemicolonClassElement::new(node);
        node.add_transform_flags(TransformFlags::ContainsES2015);
        node
    }

    pub fn create_block(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: impl Into<NodeArrayOrVec>, /*Statement*/
        multi_line: Option<bool>,
    ) -> Block {
        let node = self.create_base_node(base_factory, SyntaxKind::Block);
        let mut node = Block::new(
            node,
            self.create_node_array(Some(statements), None),
            multi_line,
        );
        node.add_transform_flags(propagate_children_flags(Some(&node.statements)));
        node
    }

    pub fn update_block(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,                           /*Block*/
        statements: impl Into<NodeArrayOrVec>, /*Statement*/
    ) -> Gc<Node> {
        let node_as_block = node.as_block();
        let statements = statements.into();
        if has_node_array_changed(&node_as_block.statements, &statements) {
            self.update(
                self.create_block(base_factory, statements, node_as_block.multi_line)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_variable_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        declaration_list: impl Into<RcNodeOrNodeArrayOrVec>,
    ) -> VariableStatement {
        let node = self.create_base_declaration(
            base_factory,
            SyntaxKind::VariableStatement,
            Option::<Gc<NodeArray>>::None,
            modifiers,
        );
        let mut node = VariableStatement::new(
            node,
            match declaration_list.into() {
                RcNodeOrNodeArrayOrVec::RcNode(declaration_list) => declaration_list,
                RcNodeOrNodeArrayOrVec::NodeArray(declaration_list) => self
                    .create_variable_declaration_list(base_factory, declaration_list, None)
                    .wrap(),
                RcNodeOrNodeArrayOrVec::Vec(declaration_list) => self
                    .create_variable_declaration_list(base_factory, declaration_list, None)
                    .wrap(),
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
        base_factory: &TBaseNodeFactory,
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
                self.create_variable_statement(base_factory, modifiers, declaration_list)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_empty_statement(&self, base_factory: &TBaseNodeFactory) -> EmptyStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::EmptyStatement);
        EmptyStatement::new(node)
    }

    pub fn create_expression_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
    ) -> ExpressionStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::ExpressionStatement);
        let mut node = ExpressionStatement::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_of_expression_statement(base_factory, &expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn update_expression_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ExpressionStatement*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_expression_statement = node.as_expression_statement();
        if !Gc::ptr_eq(&node_as_expression_statement.expression, &expression) {
            self.update(
                self.create_expression_statement(base_factory, expression)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_if_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        then_statement: Gc<Node /*Statement*/>,
        else_statement: Option<Gc<Node /*Statement*/>>,
    ) -> IfStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::IfStatement);
        let mut node = IfStatement::new(
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
        base_factory: &TBaseNodeFactory,
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
                self.create_if_statement(base_factory, expression, then_statement, else_statement)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_do_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        statement: Gc<Node /*Statement*/>,
        expression: Gc<Node /*Expression*/>,
    ) -> DoStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::DoStatement);
        let mut node = DoStatement::new(
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
        base_factory: &TBaseNodeFactory,
        node: &Node, /*DoStatement*/
        statement: Gc<Node /*Statement*/>,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_do_statement = node.as_do_statement();
        if !Gc::ptr_eq(&node_as_do_statement.statement, &statement)
            || !Gc::ptr_eq(&node_as_do_statement.expression, &expression)
        {
            self.update(
                self.create_do_statement(base_factory, statement, expression)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_while_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> WhileStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::WhileStatement);
        let mut node = WhileStatement::new(
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
        base_factory: &TBaseNodeFactory,
        node: &Node, /*WhileStatement*/
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> Gc<Node> {
        let node_as_while_statement = node.as_while_statement();
        if !Gc::ptr_eq(&node_as_while_statement.expression, &expression)
            || !Gc::ptr_eq(&node_as_while_statement.statement, &statement)
        {
            self.update(
                self.create_while_statement(base_factory, expression, statement)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_for_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        initializer: Option<Gc<Node /*ForInitializer*/>>,
        condition: Option<Gc<Node /*Expression*/>>,
        incrementor: Option<Gc<Node /*Expression*/>>,
        statement: Gc<Node /*Statement*/>,
    ) -> ForStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::ForStatement);
        let mut node = ForStatement::new(
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
        base_factory: &TBaseNodeFactory,
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
                self.create_for_statement(
                    base_factory,
                    initializer,
                    condition,
                    incrementor,
                    statement,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_for_in_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        initializer: Gc<Node /*ForInitializer*/>,
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> ForInStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::ForInStatement);
        let mut node = ForInStatement::new(
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
        base_factory: &TBaseNodeFactory,
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
                self.create_for_in_statement(base_factory, initializer, expression, statement)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_for_of_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        await_modifier: Option<Gc<Node /*AwaitKeyword*/>>,
        initializer: Gc<Node /*ForInitializer*/>,
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> ForOfStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::ForOfStatement);
        let await_modifier_is_some = await_modifier.is_some();
        let mut node = ForOfStatement::new(
            node,
            await_modifier,
            initializer,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(base_factory, &expression),
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
        base_factory: &TBaseNodeFactory,
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
                self.create_for_of_statement(
                    base_factory,
                    await_modifier,
                    initializer,
                    expression,
                    statement,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_continue_statement<'label>(
        &self,
        base_factory: &TBaseNodeFactory,
        label: Option<impl Into<StrOrRcNode<'label>>>,
    ) -> ContinueStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::ContinueStatement);
        let mut node = ContinueStatement::new(node, self.as_name(base_factory, label));
        node.add_transform_flags(
            propagate_child_flags(node.label.clone())
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        node
    }

    pub fn update_continue_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ContinueStatement*/
        label: Option<Gc<Node /*Identifier*/>>,
    ) -> Gc<Node> {
        let node_as_continue_statement = node.as_continue_statement();
        if !are_option_gcs_equal(node_as_continue_statement.label.as_ref(), label.as_ref()) {
            self.update(
                self.create_continue_statement(base_factory, label).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_break_statement<'label>(
        &self,
        base_factory: &TBaseNodeFactory,
        label: Option<impl Into<StrOrRcNode<'label>>>,
    ) -> BreakStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::BreakStatement);
        let mut node = BreakStatement::new(node, self.as_name(base_factory, label));
        node.add_transform_flags(
            propagate_child_flags(node.label.clone())
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        node
    }

    pub fn update_break_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*BreakStatement*/
        label: Option<Gc<Node /*Identifier*/>>,
    ) -> Gc<Node> {
        let node_as_break_statement = node.as_break_statement();
        if !are_option_gcs_equal(node_as_break_statement.label.as_ref(), label.as_ref()) {
            self.update(
                self.create_break_statement(base_factory, label).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_return_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Option<Gc<Node /*Expression*/>>,
    ) -> ReturnStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::ReturnStatement);
        let mut node = ReturnStatement::new(node, expression);
        node.add_transform_flags(
            propagate_child_flags(node.expression.clone())
                | TransformFlags::ContainsES2018
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        node
    }

    pub fn update_return_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ReturnStatement*/
        expression: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        let node_as_return_statement = node.as_return_statement();
        if !are_option_gcs_equal(
            node_as_return_statement.expression.as_ref(),
            expression.as_ref(),
        ) {
            self.update(
                self.create_return_statement(base_factory, expression)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_with_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> WithStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::WithStatement);
        let mut node = WithStatement::new(
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
        base_factory: &TBaseNodeFactory,
        node: &Node, /*WithStatement*/
        expression: Gc<Node /*Expression*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> Gc<Node> {
        let node_as_with_statement = node.as_with_statement();
        if !Gc::ptr_eq(&node_as_with_statement.expression, &expression)
            || !Gc::ptr_eq(&node_as_with_statement.statement, &statement)
        {
            self.update(
                self.create_with_statement(base_factory, expression, statement)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_switch_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        case_block: Gc<Node /*CaseBlock*/>,
    ) -> SwitchStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::SwitchStatement);
        let mut node = SwitchStatement::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(base_factory, &expression),
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
        base_factory: &TBaseNodeFactory,
        node: &Node, /*SwitchStatement*/
        expression: Gc<Node /*Expression*/>,
        case_block: Gc<Node /*CaseBlock*/>,
    ) -> Gc<Node> {
        let node_as_switch_statement = node.as_switch_statement();
        if !Gc::ptr_eq(&node_as_switch_statement.expression, &expression)
            || !Gc::ptr_eq(&node_as_switch_statement.case_block, &case_block)
        {
            self.update(
                self.create_switch_statement(base_factory, expression, case_block)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_labeled_statement<'label>(
        &self,
        base_factory: &TBaseNodeFactory,
        label: impl Into<StrOrRcNode<'label>>,
        statement: Gc<Node /*Statement*/>,
    ) -> LabeledStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::LabeledStatement);
        let mut node = LabeledStatement::new(
            node,
            self.as_name(base_factory, Some(label)).unwrap(),
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
        base_factory: &TBaseNodeFactory,
        node: &Node, /*LabeledStatement*/
        label: Gc<Node /*Identifier*/>,
        statement: Gc<Node /*Statement*/>,
    ) -> Gc<Node> {
        let node_as_labeled_statement = node.as_labeled_statement();
        if !Gc::ptr_eq(&node_as_labeled_statement.label, &label)
            || !Gc::ptr_eq(&node_as_labeled_statement.statement, &statement)
        {
            self.update(
                self.create_labeled_statement(base_factory, label, statement)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_throw_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
    ) -> ThrowStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::ThrowStatement);
        let mut node = ThrowStatement::new(node, expression);
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn update_throw_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ThrowStatement*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_throw_statement = node.as_throw_statement();
        if !Gc::ptr_eq(&node_as_throw_statement.expression, &expression) {
            self.update(
                self.create_throw_statement(base_factory, expression).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_try_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        try_block: Gc<Node /*Block*/>,
        catch_clause: Option<Gc<Node /*CatchClause*/>>,
        finally_block: Option<Gc<Node /*Block*/>>,
    ) -> TryStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::TryStatement);
        let mut node = TryStatement::new(node, try_block, catch_clause, finally_block);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.try_block))
                | propagate_child_flags(node.catch_clause.clone())
                | propagate_child_flags(node.finally_block.clone()),
        );
        node
    }

    pub fn update_try_statement(
        &self,
        base_factory: &TBaseNodeFactory,
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
                self.create_try_statement(base_factory, try_block, catch_clause, finally_block)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_debugger_statement(&self, base_factory: &TBaseNodeFactory) -> DebuggerStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::DebuggerStatement);
        DebuggerStatement::new(node)
    }

    pub fn create_variable_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Option<impl Into<StrOrRcNode<'name> /*BindingName*/>>,
        exclamation_token: Option<Gc<Node /*ExclamationToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> VariableDeclaration {
        let node = self.create_base_variable_like_declaration(
            base_factory,
            SyntaxKind::VariableDeclaration,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            name,
            type_,
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(base_factory, &initializer)
            }),
        );
        let exclamation_token_is_some = exclamation_token.is_some();
        let mut node = VariableDeclaration::new(node, exclamation_token);
        node.add_transform_flags(propagate_child_flags(node.exclamation_token.clone()));
        if exclamation_token_is_some {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_variable_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
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
                self.create_variable_declaration(
                    base_factory,
                    name,
                    exclamation_token,
                    type_,
                    initializer,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_variable_declaration_list(
        &self,
        base_factory: &TBaseNodeFactory,
        declarations: impl Into<NodeArrayOrVec>,
        flags: Option<NodeFlags>,
    ) -> VariableDeclarationList {
        let flags = flags.unwrap_or(NodeFlags::None);
        let node = self.create_base_node(base_factory, SyntaxKind::VariableDeclarationList);
        node.set_flags(node.flags() | (flags & NodeFlags::BlockScoped));
        let mut node =
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
        base_factory: &TBaseNodeFactory,
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
                self.create_variable_declaration_list(
                    base_factory,
                    declarations,
                    Some(node.flags()),
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_function_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
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
            base_factory,
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
        let mut node = FunctionDeclaration::new(node);
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
        base_factory: &TBaseNodeFactory,
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
                    base_factory,
                    decorators,
                    modifiers,
                    asterisk_token,
                    name,
                    type_parameters,
                    parameters,
                    type_,
                    body,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_class_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
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
            base_factory,
            SyntaxKind::ClassDeclaration,
            decorators,
            modifiers,
            name,
            type_parameters,
            heritage_clauses,
            members,
        );
        let mut node = ClassDeclaration::new(node);
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
        base_factory: &TBaseNodeFactory,
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
                    base_factory,
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    heritage_clauses,
                    members,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_interface_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        heritage_clauses: Option<impl Into<NodeArrayOrVec>>,
        members: impl Into<NodeArrayOrVec>,
    ) -> InterfaceDeclaration {
        let node = self.create_base_interface_or_class_like_declaration(
            base_factory,
            SyntaxKind::InterfaceDeclaration,
            decorators,
            modifiers,
            Some(name),
            type_parameters,
            heritage_clauses,
        );
        let mut node = InterfaceDeclaration::new(node, self.create_node_array(Some(members), None));
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_interface_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
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
                    base_factory,
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    heritage_clauses,
                    members,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_type_alias_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        type_: Gc<Node /*TypeNode*/>,
    ) -> TypeAliasDeclaration {
        let node = self.create_base_generic_named_declaration(
            base_factory,
            SyntaxKind::TypeAliasDeclaration,
            decorators,
            modifiers,
            Some(name),
            type_parameters,
        );
        let mut node = TypeAliasDeclaration::new(node, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_alias_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
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
                    base_factory,
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    type_,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_enum_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        members: Option<impl Into<NodeArrayOrVec>>,
    ) -> EnumDeclaration {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::EnumDeclaration,
            decorators,
            modifiers,
            Some(name),
        );
        let mut node = EnumDeclaration::new(node, self.create_node_array(members, None));
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
        base_factory: &TBaseNodeFactory,
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
                self.create_enum_declaration(base_factory, decorators, modifiers, name, members)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_module_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Gc<Node /*ModuleName*/>,
        body: Option<Gc<Node /*ModuleBody*/>>,
        flags: Option<NodeFlags>,
    ) -> ModuleDeclaration {
        let flags = flags.unwrap_or(NodeFlags::None);
        let node = self.create_base_declaration(
            base_factory,
            SyntaxKind::ModuleDeclaration,
            decorators,
            modifiers,
        );
        node.set_flags(
            node.flags()
                | (flags
                    & (NodeFlags::Namespace
                        | NodeFlags::NestedNamespace
                        | NodeFlags::GlobalAugmentation)),
        );
        let mut node = ModuleDeclaration::new(node, name, body);
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
        base_factory: &TBaseNodeFactory,
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
                    base_factory,
                    decorators,
                    modifiers,
                    name,
                    body,
                    Some(node.flags()),
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_module_block(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: Option<impl Into<NodeArrayOrVec>>,
    ) -> ModuleBlock {
        let node = self.create_base_node(base_factory, SyntaxKind::ModuleBlock);
        let mut node = ModuleBlock::new(node, self.create_node_array(statements, None));
        node.add_transform_flags(propagate_children_flags(Some(&node.statements)));
        node
    }

    pub fn update_module_block(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ModuleBlock*/
        statements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_module_block = node.as_module_block();
        let statements = statements.into();
        if has_node_array_changed(&node_as_module_block.statements, &statements) {
            self.update(
                self.create_module_block(base_factory, Some(statements))
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_case_block(
        &self,
        base_factory: &TBaseNodeFactory,
        clauses: impl Into<NodeArrayOrVec>, /*<CaseOrDefaultClause>*/
    ) -> CaseBlock {
        let node = self.create_base_node(base_factory, SyntaxKind::CaseBlock);
        let mut node = CaseBlock::new(node, self.create_node_array(Some(clauses), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.clauses)));
        node
    }

    pub fn update_case_block(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,                        /*CaseBlock*/
        clauses: impl Into<NodeArrayOrVec>, /*<CaseOrDefaultClause>*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_namespace_export_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: impl Into<StrOrRcNode<'name>>,
    ) -> NamespaceExportDeclaration {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::NamespaceExportDeclaration,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(name),
        );
        let mut node = NamespaceExportDeclaration::new(node);
        node.set_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_namespace_export_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*NamespaceExportDeclaration*/
        name: Gc<Node /*Identifier*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_import_equals_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_type_only: bool,
        name: impl Into<StrOrRcNode<'name>>,
        module_reference: Gc<Node /*ModuleReference*/>,
    ) -> ImportEqualsDeclaration {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::ImportEqualsDeclaration,
            decorators,
            modifiers,
            Some(name),
        );
        let mut node = ImportEqualsDeclaration::new(node, is_type_only, module_reference);
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
        base_factory: &TBaseNodeFactory,
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
                    base_factory,
                    decorators,
                    modifiers,
                    is_type_only,
                    name,
                    module_reference,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_import_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        import_clause: Option<Gc<Node /*ImportClause*/>>,
        module_specifier: Gc<Node /*Expression*/>,
        assert_clause: Option<Gc<Node /*AssertClause*/>>,
    ) -> ImportDeclaration {
        let node = self.create_base_declaration(
            base_factory,
            SyntaxKind::ImportDeclaration,
            decorators,
            modifiers,
        );
        let mut node = ImportDeclaration::new(node, import_clause, module_specifier, assert_clause);
        node.add_transform_flags(
            propagate_child_flags(node.import_clause.clone())
                | propagate_child_flags(Some(&*node.module_specifier)),
        );
        node
    }

    pub fn update_import_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
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
                    base_factory,
                    decorators,
                    modifiers,
                    import_clause,
                    module_specifier,
                    assert_clause,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_import_clause(
        &self,
        base_factory: &TBaseNodeFactory,
        is_type_only: bool,
        name: Option<Gc<Node /*Identifier*/>>,
        named_bindings: Option<Gc<Node /*NamedImportBindings*/>>,
    ) -> ImportClause {
        let node = self.create_base_node(base_factory, SyntaxKind::ImportClause);
        let mut node = ImportClause::new(node, is_type_only, name, named_bindings);
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
        base_factory: &TBaseNodeFactory,
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
                self.create_import_clause(base_factory, is_type_only, name, named_bindings)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }
}
