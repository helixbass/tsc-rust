use id_arena::Id;
use local_macros::generate_node_factory_method_wrapper;

use super::{propagate_child_flags, propagate_children_flags};
use crate::{
    has_node_array_changed, has_option_node_array_changed, is_external_module_reference,
    is_non_null_chain, modifiers_to_flags, released, AsDoubleDeref, AsExpression, Block,
    BreakStatement, CaseBlock, ClassDeclaration, ClassLikeDeclarationInterface, ContinueStatement,
    Debug_, DebuggerStatement, DoStatement, EmptyStatement, EnumDeclaration, ExpressionStatement,
    ExpressionWithTypeArguments, ForInStatement, ForOfStatement, ForStatement, FunctionDeclaration,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasMembersInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, IfStatement,
    ImportClause, ImportDeclaration, ImportEqualsDeclaration, InArena, InterfaceDeclaration,
    InterfaceOrClassLikeDeclarationInterface, LabeledStatement, MetaProperty, ModifierFlags,
    ModuleBlock, ModuleDeclaration, NamedDeclarationInterface, NamespaceExportDeclaration, Node,
    NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags, NodeInterface, NonNullExpression,
    OmittedExpression, OptionInArena, RcNodeOrNodeArrayOrVec, ReturnStatement,
    SemicolonClassElement, SignatureDeclarationInterface, StrOrRcNode, SwitchStatement, SyntaxKind,
    TemplateSpan, ThrowStatement, TransformFlags, TryStatement, TypeAliasDeclaration,
    VariableDeclaration, VariableDeclarationList, VariableStatement, WhileStatement, WithStatement,
};

impl NodeFactory {
    #[generate_node_factory_method_wrapper]
    pub fn create_omitted_expression_raw(&self) -> OmittedExpression {
        let node = self.create_base_expression(SyntaxKind::OmittedExpression);
        OmittedExpression::new(node)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_expression_with_type_arguments_raw(
        &self,
        expression: Id<Node>, /*Expression*/
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
    ) -> ExpressionWithTypeArguments {
        let node = self.create_base_node(SyntaxKind::ExpressionWithTypeArguments);
        let node = ExpressionWithTypeArguments::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(expression),
            type_arguments.and_then(|type_arguments| {
                self.parenthesizer_rules()
                    .ref_(self)
                    .parenthesize_type_arguments(Some(type_arguments.into()))
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_children_flags(node.maybe_type_arguments().refed(self).as_deref())
                | TransformFlags::ContainsES2015,
        );
        node
    }

    pub fn update_expression_with_type_arguments(
        &self,
        node: Id<Node>,       /*ExpressionWithTypeArguments*/
        expression: Id<Node>, /*Expression*/
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_expression_with_type_arguments = node_ref.as_expression_with_type_arguments();
        let type_arguments = type_arguments.map(Into::into);
        if node_as_expression_with_type_arguments.expression != expression
            || has_option_node_array_changed(
                node_as_expression_with_type_arguments.maybe_type_arguments(),
                type_arguments.as_ref(),
            )
        {
            self.update(
                self.create_expression_with_type_arguments(expression, type_arguments),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_as_expression_raw(
        &self,
        expression: Id<Node>, /*Expression*/
        type_: Id<Node /*TypeNode*/>,
    ) -> AsExpression {
        let node = self.create_base_expression(SyntaxKind::AsExpression);
        let node = AsExpression::new(node, expression, type_);
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(Some(node.type_), self)
                | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_as_expression(
        &self,
        node: Id<Node>,       /*AsExpression*/
        expression: Id<Node>, /*Expression*/
        type_: Id<Node /*TypeNode*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_as_expression = node_ref.as_as_expression();
        if node_as_as_expression.expression != expression || node_as_as_expression.type_ != type_ {
            self.update(self.create_as_expression(expression, type_), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_non_null_expression_raw(
        &self,
        expression: Id<Node>, /*Expression*/
    ) -> NonNullExpression {
        let node = self.create_base_expression(SyntaxKind::NonNullExpression);
        let node = NonNullExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self) | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_non_null_expression(
        &self,
        node: Id<Node>,       /*NonNullExpression*/
        expression: Id<Node>, /*Expression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_non_null_expression = node_ref.as_non_null_expression();
        if is_non_null_chain(&node.ref_(self)) {
            return self.update_non_null_chain(node, expression);
        }
        if node_as_non_null_expression.expression != expression {
            self.update(self.create_non_null_expression(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_non_null_chain_raw(
        &self,
        expression: Id<Node>, /*Expression*/
    ) -> NonNullExpression {
        let node = self.create_base_expression(SyntaxKind::NonNullExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let node = NonNullExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self) | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_non_null_chain(
        &self,
        node: Id<Node>,       /*NonNullChain*/
        expression: Id<Node>, /*Expression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_non_null_expression = node_ref.as_non_null_expression();
        Debug_.assert(
            node.ref_(self).flags().intersects(NodeFlags::OptionalChain),
            Some("Cannot update a NonNullExpression using updateNonNullChain. Use updateNonNullExpression instead.")
        );
        if node_as_non_null_expression.expression != expression {
            self.update(self.create_non_null_chain(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_meta_property_raw(
        &self,
        keyword_token: SyntaxKind, /*MetaProperty["keywordToken"]*/
        name: Id<Node>,            /*Identifier*/
    ) -> MetaProperty {
        let node = self.create_base_expression(SyntaxKind::MetaProperty);
        let node = MetaProperty::new(node, keyword_token, name);
        node.add_transform_flags(propagate_child_flags(Some(node.name), self));
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
        node: Id<Node>, /*MetaProperty*/
        name: Id<Node>, /*Identifier*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_meta_property = node_ref.as_meta_property();
        if node_as_meta_property.name != name {
            self.update(
                self.create_meta_property(node_as_meta_property.keyword_token, name),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_template_span_raw(
        &self,
        expression: Id<Node /*Expression*/>,
        literal: Id<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> TemplateSpan {
        let node = self.create_base_node(SyntaxKind::TemplateSpan);
        let node = TemplateSpan::new(node, expression, literal);
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(Some(node.literal), self)
                | TransformFlags::ContainsES2015,
        );
        node
    }

    pub fn update_template_span(
        &self,
        node: Id<Node>, /*TemplateSpan*/
        expression: Id<Node /*Expression*/>,
        literal: Id<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_template_span = node_ref.as_template_span();
        if node_as_template_span.expression != expression
            || node_as_template_span.literal != literal
        {
            self.update(self.create_template_span(expression, literal), node)
        } else {
            node
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
        node.add_transform_flags(propagate_children_flags(Some(&node.statements.ref_(self))));
        node
    }

    pub fn update_block(
        &self,
        node: Id<Node>,                        /*Block*/
        statements: impl Into<NodeArrayOrVec>, /*Statement*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_block = node_ref.as_block();
        let statements = statements.into();
        if has_node_array_changed(node_as_block.statements, &statements) {
            self.update(
                self.create_block(statements, node_as_block.multi_line),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_variable_statement_raw(
        &self,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        declaration_list: impl Into<RcNodeOrNodeArrayOrVec>,
    ) -> VariableStatement {
        let node = self.create_base_declaration(
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
        node.add_transform_flags(propagate_child_flags(Some(node.declaration_list), self));
        if modifiers_to_flags(node.maybe_modifiers().refed(self).as_double_deref(), self)
            .intersects(ModifierFlags::Ambient)
        {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_variable_statement(
        &self,
        node: Id<Node>, /*VariableStatement*/
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        declaration_list: Id<Node /*VariableDeclarationList*/>,
    ) -> Id<Node> {
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || node.ref_(self).as_variable_statement().declaration_list != declaration_list
        {
            self.update(
                self.create_variable_statement(modifiers, declaration_list),
                node,
            )
        } else {
            node
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
        expression: Id<Node /*Expression*/>,
    ) -> ExpressionStatement {
        let node = self.create_base_node(SyntaxKind::ExpressionStatement);
        let node = ExpressionStatement::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_expression_of_expression_statement(expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(node.expression), self));
        node
    }

    pub fn update_expression_statement(
        &self,
        node: Id<Node>, /*ExpressionStatement*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        if node.ref_(self).as_expression_statement().expression != expression {
            self.update(self.create_expression_statement(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_if_statement_raw(
        &self,
        expression: Id<Node /*Expression*/>,
        then_statement: Id<Node /*Statement*/>,
        else_statement: Option<Id<Node /*Statement*/>>,
    ) -> IfStatement {
        let node = self.create_base_node(SyntaxKind::IfStatement);
        let node = IfStatement::new(
            node,
            expression,
            self.as_embedded_statement(Some(then_statement)).unwrap(),
            self.as_embedded_statement(else_statement),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(Some(node.then_statement), self)
                | propagate_child_flags(node.else_statement.clone(), self),
        );
        node
    }

    pub fn update_if_statement(
        &self,
        node: Id<Node>, /*IfStatement*/
        expression: Id<Node /*Expression*/>,
        then_statement: Id<Node /*Statement*/>,
        else_statement: Option<Id<Node /*Statement*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_if_statement = node_ref.as_if_statement();
        if node_as_if_statement.expression != expression
            || node_as_if_statement.then_statement != then_statement
            || node_as_if_statement.else_statement != else_statement
        {
            self.update(
                self.create_if_statement(expression, then_statement, else_statement),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_do_statement_raw(
        &self,
        statement: Id<Node /*Statement*/>,
        expression: Id<Node /*Expression*/>,
    ) -> DoStatement {
        let node = self.create_base_node(SyntaxKind::DoStatement);
        let node = DoStatement::new(
            node,
            self.as_embedded_statement(Some(statement)).unwrap(),
            expression,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.statement), self)
                | propagate_child_flags(Some(node.expression), self),
        );
        node
    }

    pub fn update_do_statement(
        &self,
        node: Id<Node>, /*DoStatement*/
        statement: Id<Node /*Statement*/>,
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_do_statement = node_ref.as_do_statement();
        if node_as_do_statement.statement != statement
            || node_as_do_statement.expression != expression
        {
            self.update(self.create_do_statement(statement, expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_while_statement_raw(
        &self,
        expression: Id<Node /*Expression*/>,
        statement: Id<Node /*Statement*/>,
    ) -> WhileStatement {
        let node = self.create_base_node(SyntaxKind::WhileStatement);
        let node = WhileStatement::new(
            node,
            expression,
            self.as_embedded_statement(Some(statement)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(Some(node.statement), self),
        );
        node
    }

    pub fn update_while_statement(
        &self,
        node: Id<Node>, /*WhileStatement*/
        expression: Id<Node /*Expression*/>,
        statement: Id<Node /*Statement*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_while_statement = node_ref.as_while_statement();
        if node_as_while_statement.expression != expression
            || node_as_while_statement.statement != statement
        {
            self.update(self.create_while_statement(expression, statement), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_for_statement_raw(
        &self,
        initializer: Option<Id<Node /*ForInitializer*/>>,
        condition: Option<Id<Node /*Expression*/>>,
        incrementor: Option<Id<Node /*Expression*/>>,
        statement: Id<Node /*Statement*/>,
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
            propagate_child_flags(node.initializer.clone(), self)
                | propagate_child_flags(node.condition.clone(), self)
                | propagate_child_flags(node.incrementor.clone(), self)
                | propagate_child_flags(Some(node.statement), self),
        );
        node
    }

    pub fn update_for_statement(
        &self,
        node: Id<Node>, /*ForStatement*/
        initializer: Option<Id<Node /*ForInitializer*/>>,
        condition: Option<Id<Node /*Expression*/>>,
        incrementor: Option<Id<Node /*Expression*/>>,
        statement: Id<Node /*Statement*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_for_statement = node_ref.as_for_statement();
        if node_as_for_statement.initializer != initializer
            || node_as_for_statement.condition != condition
            || node_as_for_statement.incrementor != incrementor
            || node_as_for_statement.statement != statement
        {
            self.update(
                self.create_for_statement(initializer, condition, incrementor, statement),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_for_in_statement_raw(
        &self,
        initializer: Id<Node /*ForInitializer*/>,
        expression: Id<Node /*Expression*/>,
        statement: Id<Node /*Statement*/>,
    ) -> ForInStatement {
        let node = self.create_base_node(SyntaxKind::ForInStatement);
        let node = ForInStatement::new(
            node,
            initializer,
            expression,
            self.as_embedded_statement(Some(statement)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.initializer), self)
                | propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(Some(node.statement), self),
        );
        node
    }

    pub fn update_for_in_statement(
        &self,
        node: Id<Node>, /*ForInStatement*/
        initializer: Id<Node /*ForInitializer*/>,
        expression: Id<Node /*Expression*/>,
        statement: Id<Node /*Statement*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_for_in_statement = node_ref.as_for_in_statement();
        if node_as_for_in_statement.initializer != initializer
            || node_as_for_in_statement.expression != expression
            || node_as_for_in_statement.statement != statement
        {
            self.update(
                self.create_for_in_statement(initializer, expression, statement),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_for_of_statement_raw(
        &self,
        await_modifier: Option<Id<Node /*AwaitKeyword*/>>,
        initializer: Id<Node /*ForInitializer*/>,
        expression: Id<Node /*Expression*/>,
        statement: Id<Node /*Statement*/>,
    ) -> ForOfStatement {
        let node = self.create_base_node(SyntaxKind::ForOfStatement);
        let await_modifier_is_some = await_modifier.is_some();
        let node = ForOfStatement::new(
            node,
            await_modifier,
            initializer,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_expression_for_disallowed_comma(expression),
            self.as_embedded_statement(Some(statement)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(node.await_modifier.clone(), self)
                | propagate_child_flags(Some(node.initializer), self)
                | propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(Some(node.statement), self)
                | TransformFlags::ContainsES2015,
        );
        if await_modifier_is_some {
            node.add_transform_flags(TransformFlags::ContainsES2018);
        }
        node
    }

    pub fn update_for_of_statement(
        &self,
        node: Id<Node>, /*ForOfStatement*/
        await_modifier: Option<Id<Node /*AwaitKeyword*/>>,
        initializer: Id<Node /*ForInitializer*/>,
        expression: Id<Node /*Expression*/>,
        statement: Id<Node /*Statement*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_for_of_statement = node_ref.as_for_of_statement();
        if node_as_for_of_statement.await_modifier != await_modifier
            || node_as_for_of_statement.initializer != initializer
            || node_as_for_of_statement.expression != expression
            || node_as_for_of_statement.statement != statement
        {
            self.update(
                self.create_for_of_statement(await_modifier, initializer, expression, statement),
                node,
            )
        } else {
            node
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
            propagate_child_flags(node.label.clone(), self)
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        node
    }

    pub fn update_continue_statement(
        &self,
        node: Id<Node>, /*ContinueStatement*/
        label: Option<Id<Node /*Identifier*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_continue_statement = node_ref.as_continue_statement();
        if node_as_continue_statement.label != label {
            self.update(self.create_continue_statement(label), node)
        } else {
            node
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
            propagate_child_flags(node.label.clone(), self)
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        node
    }

    pub fn update_break_statement(
        &self,
        node: Id<Node>, /*BreakStatement*/
        label: Option<Id<Node /*Identifier*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_break_statement = node_ref.as_break_statement();
        if node_as_break_statement.label != label {
            self.update(self.create_break_statement(label), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_return_statement_raw(
        &self,
        expression: Option<Id<Node /*Expression*/>>,
    ) -> ReturnStatement {
        let node = self.create_base_node(SyntaxKind::ReturnStatement);
        let node = ReturnStatement::new(node, expression);
        node.add_transform_flags(
            propagate_child_flags(node.expression.clone(), self)
                | TransformFlags::ContainsES2018
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        node
    }

    pub fn update_return_statement(
        &self,
        node: Id<Node>, /*ReturnStatement*/
        expression: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_return_statement = node_ref.as_return_statement();
        if node_as_return_statement.expression != expression {
            self.update(self.create_return_statement(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_with_statement_raw(
        &self,
        expression: Id<Node /*Expression*/>,
        statement: Id<Node /*Statement*/>,
    ) -> WithStatement {
        let node = self.create_base_node(SyntaxKind::WithStatement);
        let node = WithStatement::new(
            node,
            expression,
            self.as_embedded_statement(Some(statement)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(Some(node.statement), self),
        );
        node
    }

    pub fn update_with_statement(
        &self,
        node: Id<Node>, /*WithStatement*/
        expression: Id<Node /*Expression*/>,
        statement: Id<Node /*Statement*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_with_statement = node_ref.as_with_statement();
        if node_as_with_statement.expression != expression
            || node_as_with_statement.statement != statement
        {
            self.update(self.create_with_statement(expression, statement), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_switch_statement_raw(
        &self,
        expression: Id<Node /*Expression*/>,
        case_block: Id<Node /*CaseBlock*/>,
    ) -> SwitchStatement {
        let node = self.create_base_node(SyntaxKind::SwitchStatement);
        let node = SwitchStatement::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_expression_for_disallowed_comma(expression),
            case_block,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(Some(node.case_block), self),
        );
        node
    }

    pub fn update_switch_statement(
        &self,
        node: Id<Node>, /*SwitchStatement*/
        expression: Id<Node /*Expression*/>,
        case_block: Id<Node /*CaseBlock*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_switch_statement = node_ref.as_switch_statement();
        if node_as_switch_statement.expression != expression
            || node_as_switch_statement.case_block != case_block
        {
            self.update(self.create_switch_statement(expression, case_block), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_labeled_statement_raw<'label>(
        &self,
        label: impl Into<StrOrRcNode<'label>>,
        statement: Id<Node /*Statement*/>,
    ) -> LabeledStatement {
        let node = self.create_base_node(SyntaxKind::LabeledStatement);
        let node = LabeledStatement::new(
            node,
            self.as_name(Some(label)).unwrap(),
            self.as_embedded_statement(Some(statement)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.label), self)
                | propagate_child_flags(Some(node.statement), self),
        );
        node
    }

    pub fn update_labeled_statement(
        &self,
        node: Id<Node>, /*LabeledStatement*/
        label: Id<Node /*Identifier*/>,
        statement: Id<Node /*Statement*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_labeled_statement = node_ref.as_labeled_statement();
        if node_as_labeled_statement.label != label
            || node_as_labeled_statement.statement != statement
        {
            self.update(self.create_labeled_statement(label, statement), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_throw_statement_raw(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> ThrowStatement {
        let node = self.create_base_node(SyntaxKind::ThrowStatement);
        let node = ThrowStatement::new(node, expression);
        node.add_transform_flags(propagate_child_flags(Some(node.expression), self));
        node
    }

    pub fn update_throw_statement(
        &self,
        node: Id<Node>, /*ThrowStatement*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_throw_statement = node_ref.as_throw_statement();
        if node_as_throw_statement.expression != expression {
            self.update(self.create_throw_statement(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_try_statement_raw(
        &self,
        try_block: Id<Node /*Block*/>,
        catch_clause: Option<Id<Node /*CatchClause*/>>,
        finally_block: Option<Id<Node /*Block*/>>,
    ) -> TryStatement {
        let node = self.create_base_node(SyntaxKind::TryStatement);
        let node = TryStatement::new(node, try_block, catch_clause, finally_block);
        node.add_transform_flags(
            propagate_child_flags(Some(node.try_block), self)
                | propagate_child_flags(node.catch_clause.clone(), self)
                | propagate_child_flags(node.finally_block.clone(), self),
        );
        node
    }

    pub fn update_try_statement(
        &self,
        node: Id<Node>, /*TryStatement*/
        try_block: Id<Node /*Block*/>,
        catch_clause: Option<Id<Node /*CatchClause*/>>,
        finally_block: Option<Id<Node /*Block*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_try_statement = node_ref.as_try_statement();
        if node_as_try_statement.try_block != try_block
            || node_as_try_statement.catch_clause != catch_clause
            || node_as_try_statement.finally_block != finally_block
        {
            self.update(
                self.create_try_statement(try_block, catch_clause, finally_block),
                node,
            )
        } else {
            node
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
        exclamation_token: Option<Id<Node /*ExclamationToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> VariableDeclaration {
        let node = self.create_base_variable_like_declaration(
            SyntaxKind::VariableDeclaration,
            Option::<Id<NodeArray>>::None,
            Option::<Id<NodeArray>>::None,
            name,
            type_,
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .ref_(self)
                    .parenthesize_expression_for_disallowed_comma(initializer)
            }),
        );
        let exclamation_token_is_some = exclamation_token.is_some();
        let node = VariableDeclaration::new(node, exclamation_token);
        node.add_transform_flags(propagate_child_flags(node.exclamation_token.clone(), self));
        if exclamation_token_is_some {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_variable_declaration(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
        name: Option<Id<Node /*BindingName*/>>,
        exclamation_token: Option<Id<Node /*ExclamationToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        if node.ref_(self).as_variable_declaration().maybe_name() != name
            || node.ref_(self).as_variable_declaration().maybe_type() != type_
            || node.ref_(self).as_variable_declaration().exclamation_token != exclamation_token
            || node
                .ref_(self)
                .as_variable_declaration()
                .maybe_initializer()
                != initializer
        {
            self.update(
                self.create_variable_declaration(name, exclamation_token, type_, initializer),
                node,
            )
        } else {
            node
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
            propagate_children_flags(Some(&node.declarations.ref_(self)))
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
        node: Id<Node>, /*VariableDeclarationList*/
        declarations: impl Into<NodeArrayOrVec>,
    ) -> Id<Node> {
        let declarations = declarations.into();
        if has_node_array_changed(
            node.ref_(self).as_variable_declaration_list().declarations,
            &declarations,
        ) {
            self.update(
                self.create_variable_declaration_list(
                    declarations,
                    Some(released!(node.ref_(self).flags())),
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_function_declaration_raw<'name>(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Id<Node /*AsteriskToken*/>>,
        name: Option<
            impl Into<StrOrRcNode<'name>>,
            /*Identifier*/
        >,
        type_parameters: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeParameterDeclaration>*/
        >,
        parameters: impl Into<NodeArrayOrVec>, /*<ParameterDeclaration>*/
        type_: Option<Id<Node /*TypeNode*/>>,
        body: Option<Id<Node /*Block*/>>,
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
            || modifiers_to_flags(node.maybe_modifiers().refed(self).as_double_deref(), self)
                .intersects(ModifierFlags::Ambient)
        {
            node.set_transform_flags(TransformFlags::ContainsTypeScript);
        } else {
            node.add_transform_flags(
                propagate_child_flags(node.maybe_asterisk_token(), self)
                    | TransformFlags::ContainsHoistedDeclarationOrCompletion,
            );
            if modifiers_to_flags(node.maybe_modifiers().refed(self).as_double_deref(), self)
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
        node: Id<Node>, /*FunctionDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Id<Node /*AsteriskToken*/>>,
        name: Option<Id<Node /*Identifier*/>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Id<Node /*TypeNode*/>>,
        body: Option<Id<Node /*Block*/>>,
    ) -> Id<Node> {
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let parameters = parameters.into();
        if has_option_node_array_changed(node.ref_(self).maybe_decorators(), decorators.as_ref())
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || node
                .ref_(self)
                .as_function_declaration()
                .maybe_asterisk_token()
                != asterisk_token
            || node.ref_(self).as_function_declaration().maybe_name() != name
            || has_option_node_array_changed(
                node.ref_(self)
                    .as_function_declaration()
                    .maybe_type_parameters(),
                type_parameters.as_ref(),
            )
            || has_node_array_changed(
                node.ref_(self).as_function_declaration().parameters(),
                &parameters,
            )
            || node.ref_(self).as_function_declaration().maybe_type() != type_
            || node.ref_(self).as_function_declaration().maybe_body() != body
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
            node
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
        if modifiers_to_flags(node.maybe_modifiers().refed(self).as_double_deref(), self)
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
        node: Id<Node>, /*ClassDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<Id<Node>>,
        type_parameters: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeParameterDeclaration>*/
        >,
        heritage_clauses: Option<
            impl Into<NodeArrayOrVec>,
            /*<HeritageClause>*/
        >,
        members: impl Into<NodeArrayOrVec>, /*<ClassElement>*/
    ) -> Id<Node> {
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let heritage_clauses = heritage_clauses.map(Into::into);
        let members = members.into();
        if has_option_node_array_changed(node.ref_(self).maybe_decorators(), decorators.as_ref())
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || node.ref_(self).as_class_declaration().maybe_name() != name
            || has_option_node_array_changed(
                node.ref_(self)
                    .as_class_declaration()
                    .maybe_type_parameters(),
                type_parameters.as_ref(),
            )
            || has_option_node_array_changed(
                node.ref_(self)
                    .as_class_declaration()
                    .maybe_heritage_clauses(),
                heritage_clauses.as_ref(),
            )
            || has_node_array_changed(node.ref_(self).as_class_declaration().members(), &members)
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
            node
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
        node: Id<Node>, /*InterfaceDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Id<Node>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        heritage_clauses: Option<impl Into<NodeArrayOrVec>>,
        members: impl Into<NodeArrayOrVec>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_interface_declaration = node_ref.as_interface_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let heritage_clauses = heritage_clauses.map(Into::into);
        let members = members.into();
        if has_option_node_array_changed(node.ref_(self).maybe_decorators(), decorators.as_ref())
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || node_as_interface_declaration.name() != name
            || has_option_node_array_changed(
                node_as_interface_declaration.maybe_type_parameters(),
                type_parameters.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_interface_declaration.maybe_heritage_clauses(),
                heritage_clauses.as_ref(),
            )
            || has_node_array_changed(node_as_interface_declaration.members(), &members)
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
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_alias_declaration_raw<'name>(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        type_: Id<Node /*TypeNode*/>,
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
        node: Id<Node>, /*TypeAliasDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Id<Node>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        type_: Id<Node /*TypeNode*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_type_alias_declaration = node_ref.as_type_alias_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        if has_option_node_array_changed(node.ref_(self).maybe_decorators(), decorators.as_ref())
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || node_as_type_alias_declaration.name() != name
            || has_option_node_array_changed(
                node_as_type_alias_declaration.maybe_type_parameters(),
                type_parameters.as_ref(),
            )
            || node_as_type_alias_declaration.maybe_type().unwrap() != type_
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
            node
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
            propagate_children_flags(Some(&node.members.ref_(self)))
                | TransformFlags::ContainsTypeScript,
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_enum_declaration(
        &self,
        node: Id<Node>, /*EnumDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Id<Node>,
        members: Option<impl Into<NodeArrayOrVec>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_enum_declaration = node_ref.as_enum_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let members = members.map(Into::into);
        if has_option_node_array_changed(node.ref_(self).maybe_decorators(), decorators.as_ref())
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || node_as_enum_declaration.name() != name
            || match members.as_ref() {
                None => true,
                Some(members) => has_node_array_changed(node_as_enum_declaration.members, members),
            }
        {
            self.update(
                self.create_enum_declaration(decorators, modifiers, name, members),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_module_declaration_raw(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Id<Node /*ModuleName*/>,
        body: Option<Id<Node /*ModuleBody*/>>,
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
        if modifiers_to_flags(node.maybe_modifiers().refed(self).as_double_deref(), self)
            .intersects(ModifierFlags::Ambient)
        {
            node.set_transform_flags(TransformFlags::ContainsTypeScript);
        } else {
            node.add_transform_flags(
                propagate_child_flags(Some(node.name), self)
                    | propagate_child_flags(node.body.clone(), self)
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
        node: Id<Node>, /*ModuleDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Id<Node /*ModuleName*/>,
        body: Option<Id<Node /*ModuleBody*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_module_declaration = node_ref.as_module_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.ref_(self).maybe_decorators(), decorators.as_ref())
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || node_as_module_declaration.name != name
            || node_as_module_declaration.body != body
        {
            self.update(
                self.create_module_declaration(
                    decorators,
                    modifiers,
                    name,
                    body,
                    Some(node.ref_(self).flags()),
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_module_block_raw(
        &self,
        statements: Option<impl Into<NodeArrayOrVec>>,
    ) -> ModuleBlock {
        let node = self.create_base_node(SyntaxKind::ModuleBlock);
        let node = ModuleBlock::new(node, self.create_node_array(statements, None));
        node.add_transform_flags(propagate_children_flags(Some(&node.statements.ref_(self))));
        node
    }

    pub fn update_module_block(
        &self,
        node: Id<Node>, /*ModuleBlock*/
        statements: impl Into<NodeArrayOrVec>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_module_block = node_ref.as_module_block();
        let statements = statements.into();
        if has_node_array_changed(node_as_module_block.statements, &statements) {
            self.update(self.create_module_block(Some(statements)), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_case_block_raw(
        &self,
        clauses: impl Into<NodeArrayOrVec>, /*<CaseOrDefaultClause>*/
    ) -> CaseBlock {
        let node = self.create_base_node(SyntaxKind::CaseBlock);
        let node = CaseBlock::new(node, self.create_node_array(Some(clauses), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.clauses.ref_(self))));
        node
    }

    pub fn update_case_block(
        &self,
        node: Id<Node>,                     /*CaseBlock*/
        clauses: impl Into<NodeArrayOrVec>, /*<CaseOrDefaultClause>*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_case_block = node_ref.as_case_block();
        let clauses = clauses.into();
        if has_node_array_changed(node_as_case_block.clauses, &clauses) {
            self.update(self.create_case_block(clauses), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_namespace_export_declaration_raw<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>,
    ) -> NamespaceExportDeclaration {
        let node = self.create_base_named_declaration(
            SyntaxKind::NamespaceExportDeclaration,
            Option::<Id<NodeArray>>::None,
            Option::<Id<NodeArray>>::None,
            Some(name),
        );
        let node = NamespaceExportDeclaration::new(node);
        node.set_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_namespace_export_declaration(
        &self,
        _node: Id<Node>, /*NamespaceExportDeclaration*/
        _name: Id<Node /*Identifier*/>,
    ) -> Id<Node> {
        unimplemented!()
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_import_equals_declaration_raw<'name>(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_type_only: bool,
        name: impl Into<StrOrRcNode<'name>>,
        module_reference: Id<Node /*ModuleReference*/>,
    ) -> ImportEqualsDeclaration {
        let node = self.create_base_named_declaration(
            SyntaxKind::ImportEqualsDeclaration,
            decorators,
            modifiers,
            Some(name),
        );
        let node = ImportEqualsDeclaration::new(node, is_type_only, module_reference);
        node.add_transform_flags(propagate_child_flags(Some(node.module_reference), self));
        if !is_external_module_reference(&node.module_reference.ref_(self)) {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_import_equals_declaration(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_type_only: bool,
        name: Id<Node /*Identifier*/>,
        module_reference: Id<Node /*ModuleReference*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_import_equals_declaration = node_ref.as_import_equals_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.ref_(self).maybe_decorators(), decorators.as_ref())
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || node_as_import_equals_declaration.is_type_only != is_type_only
            || node_as_import_equals_declaration.module_reference != module_reference
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
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_import_declaration_raw(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        import_clause: Option<Id<Node /*ImportClause*/>>,
        module_specifier: Id<Node /*Expression*/>,
        assert_clause: Option<Id<Node /*AssertClause*/>>,
    ) -> ImportDeclaration {
        let node =
            self.create_base_declaration(SyntaxKind::ImportDeclaration, decorators, modifiers);
        let node = ImportDeclaration::new(node, import_clause, module_specifier, assert_clause);
        node.add_transform_flags(
            propagate_child_flags(node.import_clause.clone(), self)
                | propagate_child_flags(Some(node.module_specifier), self),
        );
        node
    }

    pub fn update_import_declaration(
        &self,
        node: Id<Node>, /*ImportDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        import_clause: Option<Id<Node /*ImportClause*/>>,
        module_specifier: Id<Node /*Expression*/>,
        assert_clause: Option<Id<Node /*AssertClause*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_import_declaration = node_ref.as_import_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.ref_(self).maybe_decorators(), decorators.as_ref())
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || node_as_import_declaration.import_clause != import_clause
            || node_as_import_declaration.module_specifier != module_specifier
            || node_as_import_declaration.assert_clause != assert_clause
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
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_import_clause_raw(
        &self,
        is_type_only: bool,
        name: Option<Id<Node /*Identifier*/>>,
        named_bindings: Option<Id<Node /*NamedImportBindings*/>>,
    ) -> ImportClause {
        let node = self.create_base_node(SyntaxKind::ImportClause);
        let node = ImportClause::new(node, is_type_only, name, named_bindings);
        node.add_transform_flags(
            propagate_child_flags(node.name.clone(), self)
                | propagate_child_flags(node.named_bindings.clone(), self),
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
        node: Id<Node>, /*ImportClause*/
        is_type_only: bool,
        name: Option<Id<Node /*Identifier*/>>,
        named_bindings: Option<Id<Node /*NamedImportBindings*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_import_clause = node_ref.as_import_clause();
        if node_as_import_clause.is_type_only != is_type_only
            || node_as_import_clause.name != name
            || node_as_import_clause.named_bindings != named_bindings
        {
            self.update(
                self.create_import_clause(is_type_only, name, named_bindings),
                node,
            )
        } else {
            node
        }
    }
}
