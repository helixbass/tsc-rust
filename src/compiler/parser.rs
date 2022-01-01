#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    create_detached_diagnostic, create_node_factory, create_scanner,
    get_binary_operator_precedence, last_or_undefined, normalize_path, object_allocator,
    set_text_range_pos_end, token_is_identifier_or_keyword, ArrayLiteralExpression, BaseNode,
    BaseNodeFactory, BinaryExpression, Debug_, DiagnosticMessage,
    DiagnosticRelatedInformationInterface, DiagnosticWithDetachedLocation, Diagnostics, Expression,
    HasExpressionInitializerInterface, HasTypeInterface, Identifier, InterfaceDeclaration,
    KeywordTypeNode, LiteralLikeNode, NamedDeclarationInterface, Node, NodeArray, NodeArrayOrVec,
    NodeFactory, NodeFlags, NodeInterface, ObjectLiteralExpression, OperatorPrecedence,
    PropertyAssignment, ReadonlyTextRange, Scanner, SourceFile, Statement, Symbol, SymbolTable,
    SyntaxKind, TypeElement, TypeNode, VariableDeclaration, VariableDeclarationList,
};

#[derive(Eq, PartialEq)]
enum SpeculationKind {
    TryParse,
    Lookahead,
    Reparse,
}

fn visit_node<TNodeRef: Borrow<Node>, TNodeCallback: FnMut(Option<TNodeRef>)>(
    cb_node: &mut TNodeCallback,
    node: Option<TNodeRef>,
) {
    cb_node(node)
}

fn visit_nodes<TNodeCallback: FnMut(Option<Rc<Node>>), TNodesCallback: FnMut(&NodeArray)>(
    cb_node: TNodeCallback,
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
    node: &Node,
    mut cb_node: TNodeCallback,
    cb_nodes: TNodesCallback,
) {
    if node.kind() <= SyntaxKind::LastToken {
        return;
    }
    match node {
        Node::TypeElement(TypeElement::PropertySignature(property_signature)) => {
            visit_node(&mut cb_node, Some(property_signature.name()));
            visit_node(&mut cb_node, property_signature.type_())
        }
        Node::PropertyAssignment(property_assignment) => {
            visit_node(&mut cb_node, Some(property_assignment.name()));
            visit_node(&mut cb_node, Some(property_assignment.initializer.clone()))
        }
        Node::VariableDeclaration(variable_declaration) => {
            visit_node(&mut cb_node, Some(variable_declaration.name()));
            visit_node(&mut cb_node, variable_declaration.type_());
            visit_node(&mut cb_node, variable_declaration.initializer())
        }
        Node::TypeNode(TypeNode::TypeReferenceNode(type_reference_node)) => {
            visit_node(&mut cb_node, Some(type_reference_node.type_name.clone()))
        }
        Node::TypeNode(TypeNode::ArrayTypeNode(array_type)) => {
            visit_node(&mut cb_node, Some(array_type.element_type.clone()))
        }
        Node::Expression(Expression::ArrayLiteralExpression(array_literal_expression)) => {
            visit_nodes(&mut cb_node, cb_nodes, &array_literal_expression.elements)
        }
        Node::Expression(Expression::ObjectLiteralExpression(object_literal_expression)) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes,
                &object_literal_expression.properties,
            )
        }
        Node::Expression(Expression::PrefixUnaryExpression(prefix_unary_expression)) => {
            visit_node(&mut cb_node, Some(prefix_unary_expression.operand.clone()))
        }
        Node::Statement(Statement::VariableStatement(variable_statement)) => visit_node(
            &mut cb_node,
            Some(variable_statement.declaration_list.clone()),
        ),
        Node::VariableDeclarationList(variable_declaration_list) => {
            visit_nodes(cb_node, cb_nodes, &variable_declaration_list.declarations)
        }
        Node::Statement(Statement::InterfaceDeclaration(interface_declaration)) => {
            visit_node(&mut cb_node, Some(interface_declaration.name()));
            visit_nodes(cb_node, cb_nodes, &interface_declaration.members)
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
    fn node_wrapper(&self) -> Rc<Node> {
        match self {
            MissingNode::Identifier(identifier) => identifier.node_wrapper(),
        }
    }

    fn set_node_wrapper(&self, wrapper: Rc<Node>) {
        match self {
            MissingNode::Identifier(identifier) => identifier.set_node_wrapper(wrapper),
        }
    }

    fn kind(&self) -> SyntaxKind {
        match self {
            MissingNode::Identifier(identifier) => identifier.kind(),
        }
    }

    fn maybe_parent(&self) -> Option<Rc<Node>> {
        match self {
            MissingNode::Identifier(identifier) => identifier.maybe_parent(),
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

    fn maybe_locals(&self) -> RefMut<Option<SymbolTable>> {
        match self {
            MissingNode::Identifier(identifier) => identifier.maybe_locals(),
        }
    }

    fn locals(&self) -> RefMut<SymbolTable> {
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
    fn pos(&self) -> isize {
        match self {
            MissingNode::Identifier(identifier) => identifier.pos(),
        }
    }

    fn set_pos(&self, pos: isize) {
        match self {
            MissingNode::Identifier(identifier) => identifier.set_pos(pos),
        }
    }

    fn end(&self) -> isize {
        match self {
            MissingNode::Identifier(identifier) => identifier.end(),
        }
    }

    fn set_end(&self, end: isize) {
        match self {
            MissingNode::Identifier(identifier) => identifier.set_end(end),
        }
    }
}

#[allow(non_snake_case)]
struct ParserType {
    scanner: RefCell<Scanner>,
    NodeConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    IdentifierConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    TokenConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    SourceFileConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    factory: NodeFactory,
    file_name: Option<String>,
    source_text: Option<String>,
    parse_diagnostics: Option<RefCell<Vec<DiagnosticWithDetachedLocation>>>,
    current_token: RefCell<Option<SyntaxKind>>,
    parsing_context: Option<ParsingContext>,
    parse_error_before_next_finished_node: Cell<bool>,
}

impl ParserType {
    fn new() -> Self {
        ParserType {
            scanner: RefCell::new(create_scanner(true)),
            NodeConstructor: None,
            IdentifierConstructor: None,
            TokenConstructor: None,
            SourceFileConstructor: None,
            factory: create_node_factory(),
            file_name: None,
            source_text: None,
            parse_diagnostics: None,
            current_token: RefCell::new(None),
            parsing_context: None,
            parse_error_before_next_finished_node: Cell::new(false),
        }
    }

    fn scanner(&self) -> Ref<Scanner> {
        self.scanner.borrow()
    }

    fn scanner_mut(&self) -> RefMut<Scanner> {
        self.scanner.borrow_mut()
    }

    #[allow(non_snake_case)]
    fn NodeConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.NodeConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_NodeConstructor(&mut self, NodeConstructor: fn(SyntaxKind, isize, isize) -> BaseNode) {
        self.NodeConstructor = Some(NodeConstructor);
    }

    #[allow(non_snake_case)]
    fn IdentifierConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.IdentifierConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_IdentifierConstructor(
        &mut self,
        IdentifierConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.IdentifierConstructor = Some(IdentifierConstructor);
    }

    #[allow(non_snake_case)]
    fn TokenConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.TokenConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_TokenConstructor(&mut self, TokenConstructor: fn(SyntaxKind, isize, isize) -> BaseNode) {
        self.TokenConstructor = Some(TokenConstructor);
    }

    #[allow(non_snake_case)]
    fn SourceFileConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.SourceFileConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_SourceFileConstructor(
        &mut self,
        SourceFileConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.SourceFileConstructor = Some(SourceFileConstructor);
    }

    fn file_name(&self) -> &str {
        self.file_name.as_ref().unwrap()
    }

    fn set_file_name(&mut self, file_name: String) {
        self.file_name = Some(file_name);
    }

    fn source_text(&self) -> &str {
        self.source_text.as_ref().unwrap()
    }

    fn set_source_text(&mut self, source_text: String) {
        self.source_text = Some(source_text);
    }

    fn parse_diagnostics(&self) -> RefMut<Vec<DiagnosticWithDetachedLocation>> {
        self.parse_diagnostics.as_ref().unwrap().borrow_mut()
    }

    fn set_parse_diagnostics(&mut self, parse_diagnostics: Vec<DiagnosticWithDetachedLocation>) {
        self.parse_diagnostics = Some(RefCell::new(parse_diagnostics));
    }

    fn current_token(&self) -> SyntaxKind {
        self.current_token.borrow().unwrap()
    }

    fn set_current_token(&self, token: SyntaxKind) {
        *self.current_token.borrow_mut() = Some(token);
    }

    fn parsing_context(&self) -> ParsingContext {
        self.parsing_context.unwrap()
    }

    fn set_parsing_context(&mut self, parsing_context: ParsingContext) {
        self.parsing_context = Some(parsing_context);
    }

    fn parse_error_before_next_finished_node(&self) -> bool {
        self.parse_error_before_next_finished_node.get()
    }

    fn set_parse_error_before_next_finished_node(&self, value: bool) {
        self.parse_error_before_next_finished_node.set(value);
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

        self.set_file_name(normalize_path(_file_name));
        self.set_source_text(_source_text.to_string());

        self.set_parse_diagnostics(vec![]);
        self.set_parsing_context(ParsingContext::None);

        self.scanner_mut().set_text(Some(_source_text), None, None);
    }

    fn parse_source_file_worker(&mut self) -> SourceFile {
        self.next_token();

        let statements =
            self.parse_list(ParsingContext::SourceElements, ParserType::parse_statement);
        Debug_.assert(matches!(self.token(), SyntaxKind::EndOfFileToken), None);

        let source_file = self.create_source_file(self.file_name(), statements);

        source_file
    }

    fn create_source_file<TNodes: Into<NodeArrayOrVec>>(
        &self,
        file_name: &str,
        statements: TNodes,
    ) -> SourceFile {
        let mut source_file = self.factory.create_source_file(self, statements);

        source_file.text = self.source_text().to_string();
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

    fn allow_in_and<TReturn>(&mut self, func: fn(&mut ParserType) -> TReturn) -> TReturn {
        self.do_outside_of_context(NodeFlags::DisallowInContext, func)
    }

    fn parse_error_at_current_token(&self, message: &DiagnosticMessage) {
        self.parse_error_at(
            self.scanner().get_token_pos().try_into().unwrap(),
            self.scanner().get_text_pos().try_into().unwrap(),
            message,
        );
    }

    fn parse_error_at_position(&self, start: isize, length: isize, message: &DiagnosticMessage) {
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

    fn parse_error_at(&self, start: isize, end: isize, message: &DiagnosticMessage) {
        self.parse_error_at_position(start, end - start, message);
    }

    fn get_node_pos(&self) -> isize {
        self.scanner().get_start_pos().try_into().unwrap()
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
        self.factory.create_node_array(
            elements
                .into_iter()
                .map(Node::wrap)
                .collect::<Vec<Rc<Node>>>(),
        )
    }

    fn finish_node<TParsedNode: NodeInterface>(
        &self,
        mut node: TParsedNode,
        pos: isize,
        end: Option<isize>,
    ) -> TParsedNode {
        set_text_range_pos_end(
            &mut node,
            pos,
            end.unwrap_or_else(|| self.scanner().get_start_pos().try_into().unwrap()),
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

    fn parse_identifier_name(
        &mut self,
        diagnostic_message: Option<DiagnosticMessage>,
    ) -> Identifier {
        self.create_identifier(
            token_is_identifier_or_keyword(self.token()),
            diagnostic_message,
        )
    }

    fn is_literal_property_name(&self) -> bool {
        token_is_identifier_or_keyword(self.token())
            || self.token() == SyntaxKind::StringLiteral
            || self.token() == SyntaxKind::NumericLiteral
    }

    fn parse_property_name_worker(&mut self) -> Node /*PropertyName*/ {
        self.parse_identifier_name(None).into()
    }

    fn parse_property_name(&mut self) -> Node /*PropertyName*/ {
        self.parse_property_name_worker()
    }

    fn is_list_element(&mut self, kind: ParsingContext) -> bool {
        match kind {
            ParsingContext::SourceElements => self.is_start_of_statement(),
            ParsingContext::TypeMembers => self
                .look_ahead(|| Some(self.is_type_member_start()))
                .unwrap(),
            ParsingContext::ObjectLiteralMembers => match self.token() {
                SyntaxKind::OpenBracketToken
                | SyntaxKind::AsteriskToken
                | SyntaxKind::DotDotDotToken
                | SyntaxKind::DotToken => true,
                _ => self.is_literal_property_name(),
            },
            ParsingContext::VariableDeclarations => {
                self.is_binding_identifier_or_private_identifier_or_pattern()
            }
            ParsingContext::ArrayLiteralMembers => {
                self.token() == SyntaxKind::CommaToken
                    || self.token() == SyntaxKind::DotToken
                    || self.token() == SyntaxKind::DotDotDotToken
                    || self.is_start_of_expression()
            }
            _ => unimplemented!(),
        }
    }

    fn is_list_terminator(&self, kind: ParsingContext) -> bool {
        if self.token() == SyntaxKind::EndOfFileToken {
            return true;
        }
        match kind {
            ParsingContext::TypeMembers | ParsingContext::ObjectLiteralMembers => {
                self.token() == SyntaxKind::CloseBraceToken
            }
            ParsingContext::VariableDeclarations => self.is_variable_declarator_list_terminator(),
            ParsingContext::ArrayLiteralMembers => self.token() == SyntaxKind::CloseBracketToken,
            _ => false,
        }
    }

    fn parse_delimited_list<TItem: Into<Node>>(
        &mut self,
        kind: ParsingContext,
        parse_element: fn(&mut ParserType) -> TItem,
        consider_semicolon_as_delimiter: Option<bool>,
    ) -> NodeArray {
        let consider_semicolon_as_delimiter = consider_semicolon_as_delimiter.unwrap_or(false);
        let save_parsing_context = self.parsing_context();
        self.set_parsing_context(self.parsing_context() | kind);
        let mut list: Vec<Node> = vec![];
        let list_pos = self.get_node_pos();

        loop {
            if self.is_list_element(kind) {
                let start_pos = self.scanner().get_start_pos();
                list.push(self.parse_list_element(kind, parse_element).into());

                if self.parse_optional(SyntaxKind::CommaToken) {
                    continue;
                }

                if self.is_list_terminator(kind) {
                    break;
                }

                unimplemented!()
            }

            if self.is_list_terminator(kind) {
                break;
            }

            unimplemented!()
        }

        self.set_parsing_context(save_parsing_context);
        self.create_node_array(list)
    }

    fn parse_entity_name(
        &mut self,
        allow_reserved_words: bool,
        diagnostic_message: Option<DiagnosticMessage>,
    ) -> Node /*EntityName*/ {
        let pos = self.get_node_pos();
        let entity: Node = if allow_reserved_words {
            self.parse_identifier_name(diagnostic_message).into()
        } else {
            self.parse_identifier(diagnostic_message).into()
        };
        entity
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

            unimplemented!()
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

    fn parse_entity_name_of_type_reference(&mut self) -> Node /*EntityName*/ {
        self.parse_entity_name(true, Some(Diagnostics::Type_expected))
    }

    fn parse_type_reference(&mut self) -> TypeNode {
        let pos = self.get_node_pos();
        let name = self.parse_entity_name_of_type_reference().wrap();
        self.finish_node(
            self.factory.create_type_reference_node(self, name).into(),
            pos,
            None,
        )
    }

    fn parse_type_member_semicolon(&mut self) {
        if self.parse_optional(SyntaxKind::CommaToken) {
            return;
        }

        self.parse_semicolon();
    }

    fn parse_property_or_method_signature(&mut self, pos: isize) -> TypeElement {
        let name = self.parse_property_name();
        let node: TypeElement;
        if false {
            unimplemented!()
        } else {
            let type_ = self.parse_type_annotation();
            node = self
                .factory
                .create_property_signature(self, name.wrap(), type_.map(Into::into))
                .into();
        }
        self.parse_type_member_semicolon();
        self.finish_node(node, pos, None)
    }

    fn is_type_member_start(&self) -> bool {
        let mut id_token = false;

        if self.is_literal_property_name() {
            id_token = true;
            self.next_token();
        }

        if id_token {
            return self.token() == SyntaxKind::OpenParenToken
                || self.token() == SyntaxKind::LessThanToken
                || self.token() == SyntaxKind::QuestionToken
                || self.token() == SyntaxKind::ColonToken
                || self.token() == SyntaxKind::CommaToken
                || self.can_parse_semicolon();
        }
        false
    }

    fn parse_type_member(&mut self) -> TypeElement {
        let pos = self.get_node_pos();
        self.parse_property_or_method_signature(pos)
    }

    fn parse_object_type_members(&mut self) -> NodeArray /*<TypeElement>*/ {
        let members: NodeArray;
        if self.parse_expected(SyntaxKind::OpenBraceToken, None) {
            members = self.parse_list(ParsingContext::TypeMembers, ParserType::parse_type_member);
            self.parse_expected(SyntaxKind::CloseBraceToken, None);
        } else {
            unimplemented!()
        }

        members
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
            _ => self.parse_type_reference(),
        }
    }

    fn is_start_of_type(&self) -> bool {
        match self.token() {
            _ => self.is_identifier(),
        }
    }

    fn parse_postfix_type_or_higher(&mut self) -> TypeNode {
        let pos = self.get_node_pos();
        let mut type_ = self.parse_non_array_type();
        while !self.scanner().has_preceding_line_break() {
            match self.token() {
                SyntaxKind::ExclamationToken => {
                    unimplemented!()
                }
                SyntaxKind::QuestionToken => {
                    unimplemented!()
                }
                SyntaxKind::OpenBracketToken => {
                    self.parse_expected(SyntaxKind::OpenBracketToken, None);
                    if self.is_start_of_type() {
                        unimplemented!()
                    } else {
                        self.parse_expected(SyntaxKind::CloseBracketToken, None);
                        type_ = self.finish_node(
                            self.factory
                                .create_array_type_node(self, type_.into())
                                .into(),
                            pos,
                            None,
                        );
                    }
                    break;
                }
                _ => {
                    return type_;
                }
            }
        }
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
            SyntaxKind::TrueKeyword => true,
            SyntaxKind::FalseKeyword => true,
            SyntaxKind::NumericLiteral => true,
            SyntaxKind::OpenBracketToken => true,
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

    fn next_token_is_identifier_on_same_line(&self) -> bool {
        self.next_token();
        !self.scanner().has_preceding_line_break() && self.is_identifier()
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
        pos: isize,
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
        pos: isize,
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
            SyntaxKind::OpenBracketToken => return self.parse_array_literal_expression().into(),
            SyntaxKind::OpenBraceToken => return self.parse_object_literal_expression().into(),
            _ => (),
        }

        self.parse_identifier(Some(Diagnostics::Expression_expected))
            .into()
    }

    fn parse_argument_or_array_literal_element(&mut self) -> Expression {
        if false {
            unimplemented!()
        } else if false {
            unimplemented!()
        } else {
            self.parse_assignment_expression_or_higher()
        }
    }

    fn parse_array_literal_expression(&mut self) -> ArrayLiteralExpression {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBracketToken, None);
        let elements = self.parse_delimited_list(
            ParsingContext::ArrayLiteralMembers,
            ParserType::parse_argument_or_array_literal_element,
            None,
        );
        self.parse_expected(SyntaxKind::CloseBracketToken, None);
        self.finish_node(
            self.factory.create_array_literal_expression(self, elements),
            pos,
            None,
        )
    }

    fn parse_object_literal_element(&mut self) -> Node {
        let pos = self.get_node_pos();

        let token_is_identifier = self.is_identifier();
        let name = self.parse_property_name();

        let node: PropertyAssignment;
        if false {
            unimplemented!()
        } else {
            self.parse_expected(SyntaxKind::ColonToken, None);
            let initializer = self.allow_in_and(ParserType::parse_assignment_expression_or_higher);
            node = self
                .factory
                .create_property_assignment(self, name.wrap(), initializer.into());
        }
        self.finish_node(node.into(), pos, None)
    }

    fn parse_object_literal_expression(&mut self) -> ObjectLiteralExpression {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBraceToken, None);
        let properties = self.parse_delimited_list(
            ParsingContext::ObjectLiteralMembers,
            ParserType::parse_object_literal_element,
            Some(true),
        );
        if !self.parse_expected(SyntaxKind::CloseBraceToken, None) {
            unimplemented!()
        }
        self.finish_node(
            self.factory
                .create_object_literal_expression(self, properties),
            pos,
            None,
        )
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
                SyntaxKind::InterfaceKeyword => {
                    return self.next_token_is_identifier_on_same_line();
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
            SyntaxKind::InterfaceKeyword => true,
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
            SyntaxKind::InterfaceKeyword => {
                if self.is_start_of_declaration() {
                    return self.parse_declaration();
                }
            }
            _ => (),
        }
        self.parse_expression_or_labeled_statement()
    }

    fn parse_declaration(&mut self) -> Statement {
        let pos = self.get_node_pos();
        if false {
            unimplemented!()
        } else {
            self.parse_declaration_worker(pos)
        }
    }

    fn parse_declaration_worker(&mut self, pos: isize) -> Statement {
        match self.token() {
            SyntaxKind::ConstKeyword => self.parse_variable_statement(pos),
            SyntaxKind::InterfaceKeyword => self.parse_interface_declaration(pos).into(),
            _ => unimplemented!(),
        }
    }

    fn is_binding_identifier_or_private_identifier_or_pattern(&self) -> bool {
        self.is_binding_identifier()
    }

    fn parse_identifier_or_pattern(&mut self) -> Rc<Node> {
        self.parse_binding_identifier().into()
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
            type_.map(|type_| type_.into()),
            initializer.map(|initializer| initializer.into()),
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
                None,
            );
        }

        self.finish_node(
            self.factory
                .create_variable_declaration_list(self, declarations, Some(flags)),
            pos,
            None,
        )
    }

    fn parse_variable_statement(&mut self, pos: isize) -> Statement {
        let declaration_list = self.parse_variable_declaration_list();
        self.parse_semicolon();
        let node = self
            .factory
            .create_variable_statement(self, declaration_list);
        self.finish_node(node.into(), pos, None)
    }

    fn parse_interface_declaration(&mut self, pos: isize) -> InterfaceDeclaration {
        self.parse_expected(SyntaxKind::InterfaceKeyword, None);
        let name = self.parse_identifier(None);
        let members = self.parse_object_type_members();
        let node = self
            .factory
            .create_interface_declaration(self, name.into(), members);
        self.finish_node(node, pos, None)
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
        const TypeMembers = 1 << 4;
        const VariableDeclarations = 1 << 8;
        const ObjectLiteralMembers = 1 << 12;
        const ArrayLiteralMembers = 1 << 15;
    }
}
