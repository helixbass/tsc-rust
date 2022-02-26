#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{propagate_child_flags, propagate_children_flags};
use crate::{
    is_external_module_reference, modifiers_to_flags, AsExpression, BaseNodeFactory, Block,
    BreakStatement, CaseBlock, ClassDeclaration, ContinueStatement, Debug_, DebuggerStatement,
    DoStatement, EmptyStatement, EnumDeclaration, ExpressionStatement, ExpressionWithTypeArguments,
    ForInStatement, ForOfStatement, ForStatement, FunctionDeclaration,
    FunctionLikeDeclarationInterface, IfStatement, ImportClause, ImportDeclaration,
    ImportEqualsDeclaration, InterfaceDeclaration, LabeledStatement, MetaProperty, ModifierFlags,
    ModuleBlock, ModuleDeclaration, NamespaceExportDeclaration, Node, NodeArray, NodeArrayOrVec,
    NodeFactory, NodeFlags, NodeInterface, NonNullExpression, OmittedExpression,
    RcNodeOrNodeArrayOrVec, ReturnStatement, SemicolonClassElement, StringOrRcNode,
    SwitchStatement, SyntaxKind, TemplateSpan, ThrowStatement, TransformFlags, TryStatement,
    TypeAliasDeclaration, VariableDeclaration, VariableDeclarationList, VariableStatement,
    WhileStatement, WithStatement,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_omitted_expression(&self, base_factory: &TBaseNodeFactory) -> OmittedExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::OmittedExpression);
        OmittedExpression::new(node)
    }

    pub fn create_expression_with_type_arguments<TTypeArguments: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node>, /*Expression*/
        type_arguments: Option<TTypeArguments>,
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
                | propagate_children_flags(node.type_arguments.as_ref())
                | TransformFlags::ContainsES2015,
        );
        node
    }

    pub fn create_as_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node>, /*Expression*/
        type_: Rc<Node /*TypeNode*/>,
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

    pub fn create_non_null_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node>, /*Expression*/
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

    pub fn create_non_null_chain(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node>, /*Expression*/
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

    pub fn create_meta_property(
        &self,
        base_factory: &TBaseNodeFactory,
        keyword_token: SyntaxKind, /*MetaProperty["keywordToken"]*/
        name: Rc<Node>,            /*Identifier*/
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

    pub fn create_template_span(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
        literal: Rc<Node /*TemplateMiddle | TemplateTail*/>,
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

    pub fn create_semicolon_class_element(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> SemicolonClassElement {
        let node = self.create_base_node(base_factory, SyntaxKind::SemicolonClassElement);
        let mut node = SemicolonClassElement::new(node);
        node.add_transform_flags(TransformFlags::ContainsES2015);
        node
    }

    pub fn create_block<TStatements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: TStatements, /*Statement*/
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

    pub fn create_variable_statement<
        TModifiers: Into<NodeArrayOrVec>,
        TDeclarationList: Into<RcNodeOrNodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<TModifiers>,
        declaration_list: TDeclarationList,
    ) -> VariableStatement {
        let node = self.create_base_declaration(
            base_factory,
            SyntaxKind::VariableStatement,
            Option::<NodeArray>::None,
            modifiers,
        );
        let mut node = VariableStatement::new(
            node,
            match declaration_list.into() {
                RcNodeOrNodeArrayOrVec::RcNode(declaration_list) => declaration_list,
                RcNodeOrNodeArrayOrVec::NodeArray(declaration_list) => self
                    .create_variable_declaration_list(base_factory, declaration_list, None)
                    .into(),
                RcNodeOrNodeArrayOrVec::Vec(declaration_list) => self
                    .create_variable_declaration_list(base_factory, declaration_list, None)
                    .into(),
            },
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.declaration_list)));
        if modifiers_to_flags(node.maybe_modifiers().as_ref()).intersects(ModifierFlags::Ambient) {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn create_empty_statement(&self, base_factory: &TBaseNodeFactory) -> EmptyStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::EmptyStatement);
        EmptyStatement::new(node)
    }

    pub fn create_expression_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
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

    pub fn create_if_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
        then_statement: Rc<Node /*Statement*/>,
        else_statement: Option<Rc<Node /*Statement*/>>,
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

    pub fn create_do_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        statement: Rc<Node /*Statement*/>,
        expression: Rc<Node /*Expression*/>,
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

    pub fn create_while_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
        statement: Rc<Node /*Statement*/>,
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

    pub fn create_for_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        initializer: Option<Rc<Node /*ForInitializer*/>>,
        condition: Option<Rc<Node /*Expression*/>>,
        incrementor: Option<Rc<Node /*Expression*/>>,
        statement: Rc<Node /*Statement*/>,
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

    pub fn create_for_in_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        initializer: Rc<Node /*ForInitializer*/>,
        expression: Rc<Node /*Expression*/>,
        statement: Rc<Node /*Statement*/>,
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

    pub fn create_for_of_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        await_modifier: Option<Rc<Node /*AwaitKeyword*/>>,
        initializer: Rc<Node /*ForInitializer*/>,
        expression: Rc<Node /*Expression*/>,
        statement: Rc<Node /*Statement*/>,
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

    pub fn create_continue_statement<TLabel: Into<StringOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        label: Option<TLabel>,
    ) -> ContinueStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::ContinueStatement);
        let mut node = ContinueStatement::new(node, self.as_name(base_factory, label));
        node.add_transform_flags(
            propagate_child_flags(node.label.clone())
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        node
    }

    pub fn create_break_statement<TLabel: Into<StringOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        label: Option<TLabel>,
    ) -> BreakStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::BreakStatement);
        let mut node = BreakStatement::new(node, self.as_name(base_factory, label));
        node.add_transform_flags(
            propagate_child_flags(node.label.clone())
                | TransformFlags::ContainsHoistedDeclarationOrCompletion,
        );
        node
    }

    pub fn create_return_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Option<Rc<Node /*Expression*/>>,
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

    pub fn create_with_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
        statement: Rc<Node /*Statement*/>,
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

    pub fn create_switch_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
        case_block: Rc<Node /*CaseBlock*/>,
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

    pub fn create_labeled_statement<TLabel: Into<StringOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        label: TLabel,
        statement: Rc<Node /*Statement*/>,
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

    pub fn create_throw_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
    ) -> ThrowStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::ThrowStatement);
        let mut node = ThrowStatement::new(node, expression);
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn create_try_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        try_block: Rc<Node /*Block*/>,
        catch_clause: Option<Rc<Node /*CatchClause*/>>,
        finally_block: Option<Rc<Node /*Block*/>>,
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

    pub fn create_debugger_statement(&self, base_factory: &TBaseNodeFactory) -> DebuggerStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::DebuggerStatement);
        DebuggerStatement::new(node)
    }

    pub fn create_variable_declaration<TName: Into<StringOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Option<TName /*BindingName*/>,
        exclamation_token: Option<Rc<Node /*ExclamationToken*/>>,
        type_: Option<Rc<Node /*TypeNode*/>>,
        initializer: Option<Rc<Node /*Expression*/>>,
    ) -> VariableDeclaration {
        let node = self.create_base_variable_like_declaration(
            base_factory,
            SyntaxKind::VariableDeclaration,
            Option::<NodeArray>::None,
            Option::<NodeArray>::None,
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

    pub fn create_variable_declaration_list<TDeclarations: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        declarations: TDeclarations,
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

    pub fn create_function_declaration<
        TModifiers: Into<NodeArrayOrVec>,
        TDecorators: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
        TTypeParameters: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        asterisk_token: Option<Rc<Node /*AsteriskToken*/>>,
        name: Option<TName /*Identifier*/>,
        type_parameters: Option<TTypeParameters /*<TypeParameterDeclaration>*/>,
        parameters: TParameters, /*<ParameterDeclaration>*/
        type_: Option<Rc<Node /*TypeNode*/>>,
        body: Option<Rc<Node /*Block*/>>,
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
            || modifiers_to_flags(node.maybe_modifiers().as_ref())
                .intersects(ModifierFlags::Ambient)
        {
            node.set_transform_flags(TransformFlags::ContainsTypeScript);
        } else {
            node.add_transform_flags(
                propagate_child_flags(node.maybe_asterisk_token())
                    | TransformFlags::ContainsHoistedDeclarationOrCompletion,
            );
            if modifiers_to_flags(node.maybe_modifiers().as_ref()).intersects(ModifierFlags::Async)
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

    pub fn create_class_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
        TTypeParameters: Into<NodeArrayOrVec>,
        THeritageClauses: Into<NodeArrayOrVec>,
        TMembers: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: Option<TName /*string | Identifier*/>,
        type_parameters: Option<TTypeParameters /*<TypeParameterDeclaration>*/>,
        heritage_clauses: Option<THeritageClauses /*<HeritageClause>*/>,
        members: TMembers, /*<ClassElement>*/
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
        if modifiers_to_flags(node.maybe_modifiers().as_ref()).intersects(ModifierFlags::Ambient) {
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

    pub fn create_interface_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
        TTypeParameters: Into<NodeArrayOrVec>,
        THeritageClauses: Into<NodeArrayOrVec>,
        TMembers: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: TName,
        type_parameters: Option<TTypeParameters>,
        heritage_clauses: Option<THeritageClauses>,
        members: TMembers,
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

    pub fn create_type_alias_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
        TTypeParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: TName,
        type_parameters: Option<TTypeParameters>,
        type_: Rc<Node /*TypeNode*/>,
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

    pub fn create_enum_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
        TMembers: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: TName,
        members: Option<TMembers>,
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

    pub fn create_module_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: Rc<Node /*ModuleName*/>,
        body: Option<Rc<Node /*ModuleBody*/>>,
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
        if modifiers_to_flags(node.maybe_modifiers().as_ref()).intersects(ModifierFlags::Ambient) {
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

    pub fn create_module_block<TStatements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: Option<TStatements>,
    ) -> ModuleBlock {
        let node = self.create_base_node(base_factory, SyntaxKind::ModuleBlock);
        let mut node = ModuleBlock::new(node, self.create_node_array(statements, None));
        node.add_transform_flags(propagate_children_flags(Some(&node.statements)));
        node
    }

    pub fn create_case_block<TClauses: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        clauses: TClauses, /*<CaseOrDefaultClause>*/
    ) -> CaseBlock {
        let node = self.create_base_node(base_factory, SyntaxKind::CaseBlock);
        let mut node = CaseBlock::new(node, self.create_node_array(Some(clauses), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.clauses)));
        node
    }

    pub fn create_namespace_export_declaration<TName: Into<StringOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: TName,
    ) -> NamespaceExportDeclaration {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::NamespaceExportDeclaration,
            Option::<NodeArray>::None,
            Option::<NodeArray>::None,
            Some(name),
        );
        let mut node = NamespaceExportDeclaration::new(node);
        node.set_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_import_equals_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        is_type_only: bool,
        name: TName,
        module_reference: Rc<Node /*ModuleReference*/>,
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

    pub fn create_import_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        import_clause: Option<Rc<Node /*ImportClause*/>>,
        module_specifier: Rc<Node /*Expression*/>,
        assert_clause: Option<Rc<Node /*AssertClause*/>>,
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

    pub fn create_import_clause(
        &self,
        base_factory: &TBaseNodeFactory,
        is_type_only: bool,
        name: Option<Rc<Node /*Identifier*/>>,
        named_bindings: Option<Rc<Node /*NamedImportBindings*/>>,
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
}
