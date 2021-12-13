#![allow(non_upper_case_globals)]

use std::sync::{Mutex, MutexGuard};

use crate::{
    create_detached_diagnostic, create_node_factory, create_scanner,
    get_binary_operator_precedence, is_same_variant, last_or_undefined, normalize_path,
    object_allocator, BaseNode, BaseNodeFactory, Debug_, DiagnosticMessage,
    DiagnosticWithDetachedLocation, Diagnostics, Expression, Identifier, LiteralLikeNode, Node,
    NodeArray, NodeArrayOrVec, NodeFactory, NodeInterface, OperatorPrecedence, Scanner, SourceFile,
    Statement, SyntaxKind,
};

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
}

#[allow(non_snake_case)]
struct ParserType {
    scanner: Scanner,
    NodeConstructor: Option<fn(SyntaxKind) -> BaseNode>,
    IdentifierConstructor: Option<fn(SyntaxKind) -> BaseNode>,
    TokenConstructor: Option<fn(SyntaxKind) -> BaseNode>,
    SourceFileConstructor: Option<fn(SyntaxKind) -> BaseNode>,
    factory: NodeFactory,
    file_name: Option<String>,
    parse_diagnostics: Option<Vec<DiagnosticWithDetachedLocation>>,
    current_token: Option<SyntaxKind>,
    parse_error_before_next_finished_node: bool,
}

impl ParserType {
    fn new() -> Self {
        ParserType {
            scanner: create_scanner(),
            NodeConstructor: None,
            IdentifierConstructor: None,
            TokenConstructor: None,
            SourceFileConstructor: None,
            factory: create_node_factory(),
            file_name: None,
            parse_diagnostics: None,
            current_token: None,
            parse_error_before_next_finished_node: false,
        }
    }

    #[allow(non_snake_case)]
    fn NodeConstructor(&self) -> fn(SyntaxKind) -> BaseNode {
        self.NodeConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_NodeConstructor(&mut self, NodeConstructor: fn(SyntaxKind) -> BaseNode) {
        self.NodeConstructor = Some(NodeConstructor);
    }

    #[allow(non_snake_case)]
    fn IdentifierConstructor(&self) -> fn(SyntaxKind) -> BaseNode {
        self.IdentifierConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_IdentifierConstructor(&mut self, IdentifierConstructor: fn(SyntaxKind) -> BaseNode) {
        self.IdentifierConstructor = Some(IdentifierConstructor);
    }

    #[allow(non_snake_case)]
    fn TokenConstructor(&self) -> fn(SyntaxKind) -> BaseNode {
        self.TokenConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_TokenConstructor(&mut self, TokenConstructor: fn(SyntaxKind) -> BaseNode) {
        self.TokenConstructor = Some(TokenConstructor);
    }

    #[allow(non_snake_case)]
    fn SourceFileConstructor(&self) -> fn(SyntaxKind) -> BaseNode {
        self.SourceFileConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_SourceFileConstructor(&mut self, SourceFileConstructor: fn(SyntaxKind) -> BaseNode) {
        self.SourceFileConstructor = Some(SourceFileConstructor);
    }

    fn file_name(&self) -> &str {
        self.file_name.as_ref().unwrap()
    }

    fn set_file_name(&mut self, file_name: &str) {
        self.file_name = Some(file_name.to_string());
    }

    fn parse_diagnostics(&mut self) -> &mut Vec<DiagnosticWithDetachedLocation> {
        self.parse_diagnostics.as_mut().unwrap()
    }

    fn set_parse_diagnostics(&mut self, parse_diagnostics: Vec<DiagnosticWithDetachedLocation>) {
        self.parse_diagnostics = Some(parse_diagnostics);
    }

    fn current_token(&self) -> SyntaxKind {
        self.current_token.unwrap()
    }

    fn set_current_token(&mut self, token: SyntaxKind) {
        self.current_token = Some(token);
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

        self.scanner.set_text(Some(_source_text), None, None);
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
        _file_name: &str,
        statements: TNodes,
    ) -> SourceFile {
        self.factory.create_source_file(self, statements)
    }

    fn parse_error_at_current_token(&mut self, message: &DiagnosticMessage) {
        self.parse_error_at(
            self.scanner.get_token_pos(),
            self.scanner.get_text_pos(),
            message,
        );
    }

    fn parse_error_at_position(
        &mut self,
        start: usize,
        length: usize,
        message: &DiagnosticMessage,
    ) {
        let last_error = last_or_undefined(self.parse_diagnostics());
        if last_error.map_or(true, |last_error| last_error.start != start) {
            let file_name = self.file_name().to_string();
            self.parse_diagnostics().push(create_detached_diagnostic(
                &file_name, start, length, message,
            ));
        }

        self.parse_error_before_next_finished_node = true;
    }

    fn parse_error_at(&mut self, start: usize, end: usize, message: &DiagnosticMessage) {
        self.parse_error_at_position(start, end - start, message);
    }

    fn token(&self) -> SyntaxKind {
        self.current_token()
    }

    fn next_token_without_check(&mut self) -> SyntaxKind {
        let current_token = self.scanner.scan();
        self.set_current_token(current_token);
        self.current_token()
    }

    fn next_token(&mut self) -> SyntaxKind {
        self.next_token_without_check()
    }

    fn is_identifier(&self) -> bool {
        false
    }

    fn parse_expected(&mut self, kind: SyntaxKind, should_advance: Option<bool>) -> bool {
        let should_advance = should_advance.unwrap_or(true);
        if is_same_variant(&self.token(), &kind) {
            if should_advance {
                self.next_token();
            }
            return true;
        }

        self.parse_error_at_current_token(&Diagnostics::_0_expected);
        false
    }

    fn create_node_array(&self, elements: Vec<Node>) -> NodeArray {
        self.factory.create_node_array(elements)
    }

    fn finish_node<TParsedNode: NodeInterface>(&mut self, node: TParsedNode) -> TParsedNode {
        if self.parse_error_before_next_finished_node {
            self.parse_error_before_next_finished_node = false;
        }

        node
    }

    fn create_missing_node(
        &mut self,
        kind: SyntaxKind,
        diagnostic_message: DiagnosticMessage,
    ) -> MissingNode {
        self.parse_error_at_current_token(&diagnostic_message);

        let result = MissingNode::Identifier(self.factory.create_identifier(self, ""));
        self.finish_node(result)
    }

    fn create_identifier(
        &mut self,
        is_identifier: bool,
        diagnostic_message: Option<DiagnosticMessage>,
    ) -> Identifier {
        let default_message = Diagnostics::Identifier_expected;

        match self.create_missing_node(
            SyntaxKind::Identifier,
            diagnostic_message.unwrap_or(default_message),
        ) {
            MissingNode::Identifier(identifier) => identifier,
            _ => panic!("Expected identifier"),
        }
    }

    fn parse_identifier(&mut self, diagnostic_message: Option<DiagnosticMessage>) -> Identifier {
        self.create_identifier(self.is_identifier(), diagnostic_message)
    }

    fn is_list_element(&self, kind: ParsingContext) -> bool {
        match kind {
            ParsingContext::SourceElements => self.is_start_of_statement(),
            _ => unimplemented!(),
        }
    }

    fn is_list_terminator(&self) -> bool {
        if self.token() == SyntaxKind::EndOfFileToken {
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

        while !self.is_list_terminator() {
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
        let node: LiteralLikeNode = if kind == SyntaxKind::NumericLiteral {
            self.factory
                .create_numeric_literal(self, self.scanner.get_token_value())
                .into()
        } else {
            Debug_.fail(None)
        };

        self.next_token();
        self.finish_node(node)
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

    fn parse_assignment_expression_or_higher(&mut self) -> Expression {
        let expr = self.parse_binary_expression_or_higher(OperatorPrecedence::Lowest);

        self.parse_conditional_expression_rest(expr)
    }

    fn parse_conditional_expression_rest(&self, left_operand: Expression) -> Expression {
        left_operand
    }

    fn parse_binary_expression_or_higher(&mut self, precedence: OperatorPrecedence) -> Expression {
        let left_operand = self.parse_unary_expression_or_higher();
        self.parse_binary_expression_rest(precedence, left_operand)
    }

    fn parse_binary_expression_rest(
        &self,
        precedence: OperatorPrecedence,
        left_operand: Expression,
    ) -> Expression {
        loop {
            let new_precedence = get_binary_operator_precedence(self.token());

            let consume_current_operator = new_precedence > precedence;

            if !consume_current_operator {
                break;
            }
        }

        left_operand
    }

    fn is_binary_operator(&self) -> bool {
        get_binary_operator_precedence(self.token()) > OperatorPrecedence::Comma
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
            _ => (),
        }

        self.parse_identifier(Some(Diagnostics::Expression_expected))
            .into()
    }

    fn parse_expression_or_labeled_statement(&mut self) -> Statement {
        let expression = self.parse_expression();
        let node = self.factory.create_expression_statement(self, expression);
        self.finish_node(node.into())
    }

    fn parse_empty_statement(&mut self) -> Statement {
        self.parse_expected(SyntaxKind::SemicolonToken, None);
        self.finish_node(self.factory.create_empty_statement(self).into())
    }

    fn is_start_of_statement(&self) -> bool {
        match self.token() {
            SyntaxKind::SemicolonToken => true,
            _ => self.is_start_of_expression(),
        }
    }

    fn parse_statement(&mut self) -> Statement {
        match self.token() {
            SyntaxKind::SemicolonToken => self.parse_empty_statement(),
            _ => self.parse_expression_or_labeled_statement(),
        }
    }
}

impl BaseNodeFactory for ParserType {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        self.SourceFileConstructor()(kind)
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        self.IdentifierConstructor()(kind)
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        self.TokenConstructor()(kind)
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        self.NodeConstructor()(kind)
    }
}

lazy_static! {
    static ref ParserMut: Mutex<ParserType> = Mutex::new(ParserType::new());
}

#[allow(non_snake_case)]
fn Parser() -> MutexGuard<'static, ParserType> {
    ParserMut.lock().unwrap()
}

#[derive(Copy, Clone)]
enum ParsingContext {
    SourceElements,
}
