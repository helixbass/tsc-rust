#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use parking_lot::MappedRwLockWriteGuard;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::{
    create_detached_diagnostic, create_node_factory, create_scanner,
    get_binary_operator_precedence, last_or_undefined, normalize_path, object_allocator,
    set_text_range_pos_end, BaseNode, BaseNodeFactory, BinaryExpression,
    BindingLikeDeclarationInterface, Debug_, DiagnosticMessage,
    DiagnosticRelatedInformationInterface, DiagnosticWithDetachedLocation, Diagnostics, Expression,
    Identifier, KeywordTypeNode, LiteralLikeNode, NamedDeclarationInterface, Node, NodeArray,
    NodeArrayOrVec, NodeFactory, NodeFlags, NodeInterface, OperatorPrecedence, ReadonlyTextRange,
    Scanner, SourceFile, Statement, Symbol, SymbolTable, SyntaxKind, TypeNode, VariableDeclaration,
    VariableDeclarationList, VariableLikeDeclarationInterface,
};

#[derive(Eq, PartialEq)]
enum SpeculationKind {
    TryParse,
    Lookahead,
    Reparse,
}

fn visit_node<TNodeCallback: FnMut(Option<Rc<Node>>)>(
    mut cb_node: &mut TNodeCallback,
    node: Option<Rc<Node>>,
) {
    cb_node(node)
}

fn visit_nodes<TNodeCallback: FnMut(Option<Rc<Node>>), TNodesCallback: FnMut(&NodeArray)>(
    mut cb_node: TNodeCallback,
    mut cb_nodes: TNodesCallback,
    nodes: &NodeArray,
) {
    if true {
        if true {
            return cb_nodes(nodes);
        }
    }
    unimplemented!()
}

pub fn for_each_child<TNodeCallback: FnMut(Option<Rc<Node>>), TNodesCallback: FnMut(&NodeArray)>(
    node: Rc<Node>,
    mut cb_node: TNodeCallback,
    cb_nodes: TNodesCallback,
) {
    if node.kind() <= SyntaxKind::LastToken {
        return;
    }
    match &*node {
        Node::VariableDeclaration(variable_declaration) => {
            visit_node(&mut cb_node, Some(variable_declaration.name()));
            visit_node(&mut cb_node, variable_declaration.type_());
            return visit_node(&mut cb_node, variable_declaration.initializer());
        }
        Node::Expression(expression) => match expression {
            Expression::PrefixUnaryExpression(prefix_unary_expression) => {
                return visit_node(&mut cb_node, Some(prefix_unary_expression.operand.clone()));
            }
            _ => unimplemented!(),
        },
        Node::Statement(statement) => match statement {
            Statement::VariableStatement(variable_statement) => {
                return visit_node(
                    &mut cb_node,
                    Some(variable_statement.declaration_list.clone()),
                );
            }
            _ => unimplemented!(),
        },
        Node::VariableDeclarationList(variable_declaration_list) => {
            return visit_nodes(cb_node, cb_nodes, &variable_declaration_list.declarations);
        }
        _ => unimplemented!(),
    }
}

pub fn create_source_file(file_name: &str, source_text: &str) -> SourceFile {
    Parser().parse_source_file(file_name, source_text)
}

enum MissingNode {
    Identifier(Identifier),
}

impl NodeInterface for MissingNode {
    fn kind(&self) -> SyntaxKind {
        match self {
            MissingNode::Identifier(identifier) => identifier.kind(),
        }
    }

    fn parent(&self) -> Rc<Node> {
        match self {
            MissingNode::Identifier(identifier) => identifier.parent(),
        }
    }

    fn set_parent(&self, parent: Rc<Node>) {
        match self {
            MissingNode::Identifier(identifier) => identifier.set_parent(parent),
        }
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        match self {
            MissingNode::Identifier(identifier) => identifier.maybe_symbol(),
        }
    }

    fn symbol(&self) -> Rc<Symbol> {
        match self {
            MissingNode::Identifier(identifier) => identifier.symbol(),
        }
    }

    fn set_symbol(&self, symbol: Rc<Symbol>) {
        match self {
            MissingNode::Identifier(identifier) => identifier.set_symbol(symbol),
        }
    }

    fn locals(&self) -> MappedRwLockWriteGuard<SymbolTable> {
        match self {
            MissingNode::Identifier(identifier) => identifier.locals(),
        }
    }

    fn set_locals(&self, locals: SymbolTable) {
        match self {
            MissingNode::Identifier(identifier) => identifier.set_locals(locals),
        }
    }
}

impl ReadonlyTextRange for MissingNode {
    fn pos(&self) -> usize {
        match self {
            MissingNode::Identifier(identifier) => identifier.pos(),
        }
    }

    fn set_pos(&self, pos: usize) {
        match self {
            MissingNode::Identifier(identifier) => identifier.set_pos(pos),
        }
    }

    fn end(&self) -> usize {
        match self {
            MissingNode::Identifier(identifier) => identifier.end(),
        }
    }

    fn set_end(&self, end: usize) {
        match self {
            MissingNode::Identifier(identifier) => identifier.set_end(end),
        }
    }
}

#[allow(non_snake_case)]
struct ParserType {
    scanner: RwLock<Scanner>,
    NodeConstructor: Option<fn(SyntaxKind, usize, usize) -> BaseNode>,
    IdentifierConstructor: Option<fn(SyntaxKind, usize, usize) -> BaseNode>,
    TokenConstructor: Option<fn(SyntaxKind, usize, usize) -> BaseNode>,
    SourceFileConstructor: Option<fn(SyntaxKind, usize, usize) -> BaseNode>,
    factory: NodeFactory,
    file_name: Option<String>,
    parse_diagnostics: Option<RwLock<Vec<DiagnosticWithDetachedLocation>>>,
    current_token: RwLock<Option<SyntaxKind>>,
    parsing_context: Option<ParsingContext>,
    parse_error_before_next_finished_node: AtomicBool,
}

impl ParserType {
    fn new() -> Self {
        ParserType {
            scanner: RwLock::new(create_scanner(true)),
            NodeConstructor: None,
            IdentifierConstructor: None,
            TokenConstructor: None,
            SourceFileConstructor: None,
            factory: create_node_factory(),
            file_name: None,
            parse_diagnostics: None,
            current_token: RwLock::new(None),
            parsing_context: None,
            parse_error_before_next_finished_node: AtomicBool::new(false),
        }
    }

    fn scanner(&self) -> RwLockReadGuard<Scanner> {
        self.scanner.try_read().unwrap()
    }

    fn scanner_mut(&self) -> RwLockWriteGuard<Scanner> {
        self.scanner.try_write().unwrap()
    }

    #[allow(non_snake_case)]
    fn NodeConstructor(&self) -> fn(SyntaxKind, usize, usize) -> BaseNode {
        self.NodeConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_NodeConstructor(&mut self, NodeConstructor: fn(SyntaxKind, usize, usize) -> BaseNode) {
        self.NodeConstructor = Some(NodeConstructor);
    }

    #[allow(non_snake_case)]
    fn IdentifierConstructor(&self) -> fn(SyntaxKind, usize, usize) -> BaseNode {
        self.IdentifierConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_IdentifierConstructor(
        &mut self,
        IdentifierConstructor: fn(SyntaxKind, usize, usize) -> BaseNode,
    ) {
        self.IdentifierConstructor = Some(IdentifierConstructor);
    }

    #[allow(non_snake_case)]
    fn TokenConstructor(&self) -> fn(SyntaxKind, usize, usize) -> BaseNode {
        self.TokenConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_TokenConstructor(&mut self, TokenConstructor: fn(SyntaxKind, usize, usize) -> BaseNode) {
        self.TokenConstructor = Some(TokenConstructor);
    }

    #[allow(non_snake_case)]
    fn SourceFileConstructor(&self) -> fn(SyntaxKind, usize, usize) -> BaseNode {
        self.SourceFileConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_SourceFileConstructor(
        &mut self,
        SourceFileConstructor: fn(SyntaxKind, usize, usize) -> BaseNode,
    ) {
        self.SourceFileConstructor = Some(SourceFileConstructor);
    }

    fn file_name(&self) -> &str {
        self.file_name.as_ref().unwrap()
    }

    fn set_file_name(&mut self, file_name: &str) {
        self.file_name = Some(file_name.to_string());
    }

    fn parse_diagnostics(&self) -> RwLockWriteGuard<Vec<DiagnosticWithDetachedLocation>> {
        self.parse_diagnostics
            .as_ref()
            .unwrap()
            .try_write()
            .unwrap()
    }

    fn set_parse_diagnostics(&mut self, parse_diagnostics: Vec<DiagnosticWithDetachedLocation>) {
        self.parse_diagnostics = Some(RwLock::new(parse_diagnostics));
    }

    fn current_token(&self) -> SyntaxKind {
        self.current_token.try_read().unwrap().unwrap()
    }

    fn set_current_token(&self, token: SyntaxKind) {
        *self.current_token.try_write().unwrap() = Some(token);
    }

    fn parsing_context(&self) -> ParsingContext {
        self.parsing_context.unwrap()
    }

    fn set_parsing_context(&mut self, parsing_context: ParsingContext) {
        self.parsing_context = Some(parsing_context);
    }

    fn parse_error_before_next_finished_node(&self) -> bool {
        self.parse_error_before_next_finished_node
            .load(Ordering::Relaxed)
    }

    fn set_parse_error_before_next_finished_node(&self, value: bool) {
        self.parse_error_before_next_finished_node
            .store(value, Ordering::Relaxed);
    }

    fn parse_source_file(&mut self, file_name: &str, source_text: &str) -> SourceFile {
        self.initialize_state(file_name, source_text);
        self.parse_source_file_worker()
    }

    fn initialize_state(&mut self, _file_name: &str, _source_text: &str) {
        self.set_NodeConstructor(object_allocator.get_node_constructor());
        self.set_IdentifierConstructor(object_allocator.get_identifier_constructor());
        self.set_TokenConstructor(object_allocator.get_token_constructor());
        self.set_SourceFileConstructor(object_allocator.get_source_file_constructor());

        self.set_file_name(&normalize_path(_file_name));

        self.set_parse_diagnostics(vec![]);
        self.set_parsing_context(ParsingContext::None);

        self.scanner_mut().set_text(Some(_source_text), None, None);
    }

    fn parse_source_file_worker(&mut self) -> SourceFile {
        self.next_token();

        let statements =
            self.parse_list(ParsingContext::SourceElements, ParserType::parse_statement);
        Debug_.assert(matches!(self.token(), SyntaxKind::EndOfFileToken));

        let source_file = self.create_source_file(self.file_name(), statements);

        source_file
    }

    fn create_source_file<TNodes: Into<NodeArrayOrVec>>(
        &self,
        file_name: &str,
        statements: TNodes,
    ) -> SourceFile {
        let mut source_file = self.factory.create_source_file(self, statements);

        source_file.file_name = file_name.to_string();

        source_file
    }

    fn do_outside_of_context<TReturn>(
        &mut self,
        context: NodeFlags,
        func: fn(&mut ParserType) -> TReturn,
    ) -> TReturn {
        func(self)
    }

    fn parse_error_at_current_token(&self, message: &DiagnosticMessage) {
        self.parse_error_at(
            self.scanner().get_token_pos(),
            self.scanner().get_text_pos(),
            message,
        );
    }

    fn parse_error_at_position(&self, start: usize, length: usize, message: &DiagnosticMessage) {
        {
            let mut parse_diagnostics = self.parse_diagnostics();
            let last_error = last_or_undefined(&*parse_diagnostics);
            if last_error.map_or(true, |last_error| last_error.start() != start) {
                let file_name = self.file_name().to_string();
                parse_diagnostics.push(create_detached_diagnostic(
                    &file_name, start, length, message,
                ));
            }
        }

        self.set_parse_error_before_next_finished_node(true);
    }

    fn parse_error_at(&self, start: usize, end: usize, message: &DiagnosticMessage) {
        self.parse_error_at_position(start, end - start, message);
    }

    fn get_node_pos(&self) -> usize {
        self.scanner().get_start_pos()
    }

    fn token(&self) -> SyntaxKind {
        self.current_token()
    }

    fn next_token_without_check(&self) -> SyntaxKind {
        let current_token = self.scanner().scan();
        self.set_current_token(current_token);
        self.current_token()
    }

    fn next_token_and<TReturn>(&mut self, func: fn(&mut ParserType) -> TReturn) -> TReturn {
        self.next_token();
        func(self)
    }

    fn next_token(&self) -> SyntaxKind {
        self.next_token_without_check()
    }

    fn speculation_helper<TReturn, TCallback: FnMut() -> Option<TReturn>>(
        &self,
        callback: TCallback,
        speculation_kind: SpeculationKind,
    ) -> Option<TReturn> {
        let save_token = self.current_token();
        let save_parse_diagnostics_length = self.parse_diagnostics().len();
        let save_parse_error_before_next_finished_node =
            self.parse_error_before_next_finished_node();

        let result = if speculation_kind != SpeculationKind::TryParse {
            self.scanner().look_ahead(callback)
        } else {
            self.scanner().try_scan(callback)
        };

        if result.is_none() || speculation_kind != SpeculationKind::TryParse {
            self.set_current_token(save_token);
            if speculation_kind != SpeculationKind::Reparse {
                self.parse_diagnostics()
                    .truncate(save_parse_diagnostics_length);
            }
            self.set_parse_error_before_next_finished_node(
                save_parse_error_before_next_finished_node,
            );
        }

        result
    }

    fn look_ahead<TReturn, TCallback: FnMut() -> Option<TReturn>>(
        &self,
        callback: TCallback,
    ) -> Option<TReturn> {
        self.speculation_helper(callback, SpeculationKind::Lookahead)
    }

    fn try_parse<TReturn, TCallback: FnMut() -> Option<TReturn>>(
        &self,
        callback: TCallback,
    ) -> Option<TReturn> {
        self.speculation_helper(callback, SpeculationKind::TryParse)
    }

    fn is_binding_identifier(&self) -> bool {
        if self.token() == SyntaxKind::Identifier {
            return true;
        }

        self.token() > SyntaxKind::LastReservedWord
    }

    fn is_identifier(&self) -> bool {
        if self.token() == SyntaxKind::Identifier {
            return true;
        }

        self.token() > SyntaxKind::LastReservedWord
    }

    fn parse_expected(&mut self, kind: SyntaxKind, should_advance: Option<bool>) -> bool {
        let should_advance = should_advance.unwrap_or(true);
        if self.token() == kind {
            if should_advance {
                self.next_token();
            }
            return true;
        }

        if false {
        } else {
            self.parse_error_at_current_token(&Diagnostics::_0_expected);
        }
        false
    }

    fn parse_error_for_missing_semicolon_after(&self, node: &Expression) {
        unimplemented!()
    }

    fn parse_optional(&mut self, t: SyntaxKind) -> bool {
        if self.token() == t {
            self.next_token();
            return true;
        }
        false
    }

    fn parse_token_node(&self) -> BaseNode {
        let pos = self.get_node_pos();
        let kind = self.token();
        self.next_token();
        self.finish_node(self.factory.create_token(self, kind), pos, None)
    }

    fn can_parse_semicolon(&self) -> bool {
        if self.token() == SyntaxKind::SemicolonToken {
            return true;
        }

        self.token() == SyntaxKind::CloseBraceToken
            || self.token() == SyntaxKind::EndOfFileToken
            || self.scanner().has_preceding_line_break()
    }

    fn try_parse_semicolon(&mut self) -> bool {
        if !self.can_parse_semicolon() {
            return false;
        }

        if self.token() == SyntaxKind::SemicolonToken {
            self.next_token();
        }

        true
    }

    fn parse_semicolon(&mut self) -> bool {
        self.try_parse_semicolon() || self.parse_expected(SyntaxKind::SemicolonToken, None)
    }

    fn create_node_array(&self, elements: Vec<Node>) -> NodeArray {
        self.factory
            .create_node_array(elements.into_iter().map(Rc::new).collect::<Vec<Rc<Node>>>())
    }

    fn finish_node<TParsedNode: NodeInterface>(
        &self,
        mut node: TParsedNode,
        pos: usize,
        end: Option<usize>,
    ) -> TParsedNode {
        set_text_range_pos_end(
            &mut node,
            pos,
            end.unwrap_or_else(|| self.scanner().get_start_pos()),
        );

        if self.parse_error_before_next_finished_node() {
            self.set_parse_error_before_next_finished_node(false);
        }

        node
    }

    fn create_missing_node(
        &mut self,
        kind: SyntaxKind,
        diagnostic_message: DiagnosticMessage,
    ) -> MissingNode {
        self.parse_error_at_current_token(&diagnostic_message);

        let pos = self.get_node_pos();
        let result = MissingNode::Identifier(self.factory.create_identifier(self, ""));
        self.finish_node(result, pos, None)
    }

    fn intern_identifier(&self, text: &str) -> String {
        text.to_string()
    }

    fn create_identifier(
        &mut self,
        is_identifier: bool,
        diagnostic_message: Option<DiagnosticMessage>,
    ) -> Identifier {
        if is_identifier {
            let pos = self.get_node_pos();
            let text = self.intern_identifier(&self.scanner().get_token_value());
            self.next_token_without_check();
            return self.finish_node(self.factory.create_identifier(self, &text), pos, None);
        }

        let default_message = Diagnostics::Identifier_expected;

        match self.create_missing_node(
            SyntaxKind::Identifier,
            diagnostic_message.unwrap_or(default_message),
        ) {
            MissingNode::Identifier(identifier) => identifier,
            _ => panic!("Expected identifier"),
        }
    }

    fn parse_binding_identifier(&mut self) -> Identifier {
        self.create_identifier(self.is_binding_identifier(), None)
    }

    fn parse_identifier(&mut self, diagnostic_message: Option<DiagnosticMessage>) -> Identifier {
        self.create_identifier(self.is_identifier(), diagnostic_message)
    }

    fn is_list_element(&mut self, kind: ParsingContext) -> bool {
        match kind {
            ParsingContext::SourceElements => self.is_start_of_statement(),
            ParsingContext::VariableDeclarations => {
                self.is_binding_identifier_or_private_identifier_or_pattern()
            }
            _ => unimplemented!(),
        }
    }

    fn is_list_terminator(&self, kind: ParsingContext) -> bool {
        if self.token() == SyntaxKind::EndOfFileToken {
            return true;
        }
        match kind {
            ParsingContext::VariableDeclarations => self.is_variable_declarator_list_terminator(),
            _ => false,
        }
    }

    fn parse_delimited_list<TItem: Into<Node>>(
        &mut self,
        kind: ParsingContext,
        parse_element: fn(&mut ParserType) -> TItem,
    ) -> NodeArray {
        let save_parsing_context = self.parsing_context();
        self.set_parsing_context(self.parsing_context() | kind);
        let mut list: Vec<Node> = vec![];
        let list_pos = self.get_node_pos();

        loop {
            if self.is_list_element(kind) {
                let start_pos = self.scanner().get_start_pos();
                list.push(self.parse_list_element(kind, parse_element).into());

                if self.is_list_terminator(kind) {
                    break;
                }

                unimplemented!()
            }

            if self.is_list_terminator(kind) {
                break;
            }
        }

        self.set_parsing_context(save_parsing_context);
        self.create_node_array(list)
    }

    fn is_variable_declarator_list_terminator(&self) -> bool {
        if self.can_parse_semicolon() {
            return true;
        }

        false
    }

    fn parse_list<TItem: Into<Node>>(
        &mut self,
        kind: ParsingContext,
        parse_element: fn(&mut ParserType) -> TItem,
    ) -> NodeArray {
        let mut list = vec![];

        while !self.is_list_terminator(kind) {
            if self.is_list_element(kind) {
                list.push(self.parse_list_element(kind, parse_element).into());

                continue;
            }
        }

        self.create_node_array(list)
    }

    fn parse_list_element<TItem: Into<Node>>(
        &mut self,
        _parsing_context: ParsingContext,
        parse_element: fn(&mut ParserType) -> TItem,
    ) -> TItem {
        parse_element(self)
    }

    fn parse_literal_node(&mut self) -> LiteralLikeNode {
        self.parse_literal_like_node(self.token())
    }

    fn parse_literal_like_node(&mut self, kind: SyntaxKind) -> LiteralLikeNode {
        let pos = self.get_node_pos();
        let node: LiteralLikeNode = if kind == SyntaxKind::NumericLiteral {
            self.factory
                .create_numeric_literal(self, &self.scanner().get_token_value())
                .into()
        } else {
            Debug_.fail(None)
        };

        self.next_token();
        self.finish_node(node, pos, None)
    }

    fn parse_keyword_and_no_dot(&self) -> Option<TypeNode> {
        let node = self.parse_token_node();
        if false {
            None
        } else {
            Some(Into::<KeywordTypeNode>::into(node).into())
        }
    }

    fn parse_non_array_type(&mut self) -> TypeNode {
        match self.token() {
            SyntaxKind::NumberKeyword => self
                .try_parse(|| self.parse_keyword_and_no_dot())
                .unwrap_or_else(|| unimplemented!()),
            _ => unimplemented!(),
        }
    }

    fn parse_postfix_type_or_higher(&mut self) -> TypeNode {
        let pos = self.get_node_pos();
        let type_ = self.parse_non_array_type();
        type_
    }

    fn parse_type_operator_or_higher(&mut self) -> TypeNode {
        // let operator = self.token();
        // match operator {

        // }
        self.parse_postfix_type_or_higher()
    }

    fn parse_union_or_intersection_type(
        &mut self,
        parse_constituent_type: fn(&mut ParserType) -> TypeNode,
    ) -> TypeNode {
        parse_constituent_type(self)
    }

    fn parse_intersection_type_or_higher(&mut self) -> TypeNode {
        self.parse_union_or_intersection_type(ParserType::parse_type_operator_or_higher)
    }

    fn parse_union_type_or_higher(&mut self) -> TypeNode {
        self.parse_union_or_intersection_type(ParserType::parse_intersection_type_or_higher)
    }

    fn parse_type(&mut self) -> TypeNode {
        self.do_outside_of_context(NodeFlags::TypeExcludesFlags, ParserType::parse_type_worker)
    }

    fn parse_type_worker(&mut self) -> TypeNode {
        let pos = self.get_node_pos();
        let type_ = self.parse_union_type_or_higher();
        type_
    }

    fn parse_type_annotation(&mut self) -> Option<TypeNode> {
        if self.parse_optional(SyntaxKind::ColonToken) {
            Some(self.parse_type())
        } else {
            None
        }
    }

    fn is_start_of_left_hand_side_expression(&self) -> bool {
        match self.token() {
            SyntaxKind::NumericLiteral => true,
            _ => self.is_identifier(),
        }
    }

    fn is_start_of_expression(&self) -> bool {
        if self.is_start_of_left_hand_side_expression() {
            return true;
        }

        match self.token() {
            SyntaxKind::PlusPlusToken => true,
            _ => {
                if self.is_binary_operator() {
                    return true;
                }

                self.is_identifier()
            }
        }
    }

    fn parse_expression(&mut self) -> Expression {
        let expr = self.parse_assignment_expression_or_higher();

        expr
    }

    fn parse_initializer(&mut self) -> Option<Expression> {
        if self.parse_optional(SyntaxKind::EqualsToken) {
            Some(self.parse_assignment_expression_or_higher())
        } else {
            None
        }
    }

    fn parse_assignment_expression_or_higher(&mut self) -> Expression {
        let expr = self.parse_binary_expression_or_higher(OperatorPrecedence::Lowest);

        self.parse_conditional_expression_rest(expr)
    }

    fn parse_conditional_expression_rest(&self, left_operand: Expression) -> Expression {
        left_operand
    }

    fn parse_binary_expression_or_higher(&mut self, precedence: OperatorPrecedence) -> Expression {
        let pos = self.get_node_pos();
        let left_operand = self.parse_unary_expression_or_higher();
        self.parse_binary_expression_rest(precedence, left_operand, pos)
    }

    fn parse_binary_expression_rest(
        &mut self,
        precedence: OperatorPrecedence,
        mut left_operand: Expression,
        pos: usize,
    ) -> Expression {
        loop {
            let new_precedence = get_binary_operator_precedence(self.token());

            let consume_current_operator = new_precedence > precedence;

            if !consume_current_operator {
                break;
            }

            let operator_token = self.parse_token_node();
            let right = self.parse_binary_expression_or_higher(new_precedence);
            left_operand = self
                .make_binary_expression(left_operand, operator_token, right, pos)
                .into();
        }

        left_operand
    }

    fn is_binary_operator(&self) -> bool {
        get_binary_operator_precedence(self.token()) > OperatorPrecedence::Comma
    }

    fn make_binary_expression<TNode: Into<Node>>(
        &mut self,
        left: Expression,
        operator_token: TNode,
        right: Expression,
        pos: usize,
    ) -> BinaryExpression {
        self.finish_node(
            self.factory
                .create_binary_expression(self, left, operator_token.into(), right),
            pos,
            None,
        )
    }

    fn parse_unary_expression_or_higher(&mut self) -> Expression {
        if self.is_update_expression() {
            let update_expression = self.parse_update_expression();
            return update_expression;
        }

        panic!("Unimplemented");
    }

    fn is_update_expression(&self) -> bool {
        match self.token() {
            _ => true,
        }
    }

    fn parse_update_expression(&mut self) -> Expression {
        if self.token() == SyntaxKind::PlusPlusToken {
            let pos = self.get_node_pos();
            let operator = self.token();
            let operand =
                self.next_token_and(ParserType::parse_left_hand_side_expression_or_higher);
            return self
                .finish_node(
                    self.factory
                        .create_prefix_unary_expression(self, operator, operand),
                    pos,
                    None,
                )
                .into();
        }

        let expression = self.parse_left_hand_side_expression_or_higher();

        expression
    }

    fn parse_left_hand_side_expression_or_higher(&mut self) -> Expression {
        let expression = self.parse_member_expression_or_higher();

        self.parse_call_expression_rest(expression)
    }

    fn parse_member_expression_or_higher(&mut self) -> Expression {
        let expression = self.parse_primary_expression();
        self.parse_member_expression_rest(expression)
    }

    fn parse_member_expression_rest(&self, expression: Expression) -> Expression {
        loop {
            return expression;
        }
    }

    fn parse_call_expression_rest(&self, expression: Expression) -> Expression {
        expression
    }

    fn parse_primary_expression(&mut self) -> Expression {
        match self.token() {
            SyntaxKind::NumericLiteral => return self.parse_literal_node().into(),
            SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword => {
                return self.parse_token_node().into()
            }
            _ => (),
        }

        self.parse_identifier(Some(Diagnostics::Expression_expected))
            .into()
    }

    fn parse_expression_or_labeled_statement(&mut self) -> Statement {
        let pos = self.get_node_pos();
        let expression = self.parse_expression();
        let node: Statement = if false {
            unimplemented!()
        } else {
            if !self.try_parse_semicolon() {
                self.parse_error_for_missing_semicolon_after(&expression);
            }
            self.factory
                .create_expression_statement(self, expression)
                .into()
        };
        self.finish_node(node, pos, None)
    }

    fn parse_empty_statement(&mut self) -> Statement {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::SemicolonToken, None);
        self.finish_node(self.factory.create_empty_statement(self).into(), pos, None)
    }

    fn is_declaration(&self) -> bool {
        loop {
            match self.token() {
                SyntaxKind::ConstKeyword => {
                    return true;
                }
                _ => unimplemented!(),
            }
        }
    }

    fn is_start_of_declaration(&mut self) -> bool {
        self.look_ahead(|| Some(self.is_declaration())).unwrap()
    }

    fn is_start_of_statement(&mut self) -> bool {
        match self.token() {
            SyntaxKind::SemicolonToken => true,
            SyntaxKind::ConstKeyword => self.is_start_of_declaration(),
            _ => self.is_start_of_expression(),
        }
    }

    fn parse_statement(&mut self) -> Statement {
        match self.token() {
            SyntaxKind::SemicolonToken => return self.parse_empty_statement(),
            SyntaxKind::ConstKeyword => {
                if self.is_start_of_declaration() {
                    return self.parse_declaration();
                }
            }
            _ => (),
        }
        return self.parse_expression_or_labeled_statement();
    }

    fn parse_declaration(&mut self) -> Statement {
        let pos = self.get_node_pos();
        if false {
            unimplemented!()
        } else {
            self.parse_declaration_worker(pos)
        }
    }

    fn parse_declaration_worker(&mut self, pos: usize) -> Statement {
        match self.token() {
            SyntaxKind::ConstKeyword => self.parse_variable_statement(pos),
            _ => unimplemented!(),
        }
    }

    fn is_binding_identifier_or_private_identifier_or_pattern(&self) -> bool {
        self.is_binding_identifier()
    }

    fn parse_identifier_or_pattern(&mut self) -> Rc<Node> {
        Rc::new(self.parse_binding_identifier().into())
    }

    fn parse_variable_declaration_no_exclamation(&mut self) -> VariableDeclaration {
        self.parse_variable_declaration(false)
    }

    fn parse_variable_declaration_allow_exclamation(&mut self) -> VariableDeclaration {
        self.parse_variable_declaration(true)
    }

    fn parse_variable_declaration(&mut self, allow_exclamation: bool) -> VariableDeclaration {
        let pos = self.get_node_pos();
        let name = self.parse_identifier_or_pattern();
        let type_ = self.parse_type_annotation();
        let initializer = if false {
            None
        } else {
            self.parse_initializer()
        };
        let node = self.factory.create_variable_declaration(
            self,
            Some(name),
            type_.map(|type_| Rc::new(type_.into())),
            initializer.map(|initializer| Rc::new(initializer.into())),
        );
        self.finish_node(node, pos, None)
    }

    fn parse_variable_declaration_list(&mut self) -> VariableDeclarationList {
        let pos = self.get_node_pos();

        let mut flags = NodeFlags::None;
        match self.token() {
            SyntaxKind::ConstKeyword => {
                flags |= NodeFlags::Const;
            }
            _ => Debug_.fail(None),
        }

        self.next_token();

        let declarations: NodeArray;
        if false {
            unimplemented!()
        } else {
            declarations = self.parse_delimited_list(
                ParsingContext::VariableDeclarations,
                ParserType::parse_variable_declaration_allow_exclamation,
            );
        }

        self.finish_node(
            self.factory
                .create_variable_declaration_list(self, declarations, Some(flags)),
            pos,
            None,
        )
    }

    fn parse_variable_statement(&mut self, pos: usize) -> Statement {
        let declaration_list = self.parse_variable_declaration_list();
        self.parse_semicolon();
        let node = self
            .factory
            .create_variable_statement(self, declaration_list);
        self.finish_node(node.into(), pos, None)
    }
}

impl BaseNodeFactory for ParserType {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        self.SourceFileConstructor()(kind, 0, 0)
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        self.IdentifierConstructor()(kind, 0, 0)
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        self.TokenConstructor()(kind, 0, 0)
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        self.NodeConstructor()(kind, 0, 0)
    }
}

// lazy_static! {
//     static ref ParserMut: Mutex<ParserType> = Mutex::new(ParserType::new());
// }

#[allow(non_snake_case)]
fn Parser() -> ParserType {
    ParserType::new()
}
// fn Parser() -> MutexGuard<'static, ParserType> {
//     ParserMut.lock().unwrap()
// }

bitflags! {
    pub struct ParsingContext: u32 {
        const None = 0;
        const SourceElements = 1 << 0;
        const VariableDeclarations = 1 << 8;
    }
}
