use crate::{
    create_detached_diagnostic, create_node_factory, create_scanner, last_or_undefined,
    object_allocator, BaseNode, BaseNodeFactory, DiagnosticMessage, DiagnosticWithDetachedLocation,
    Diagnostics, Node, NodeArray, NodeFactory, Scanner, SourceFile, Statement, SyntaxKind,
};

pub fn create_source_file(file_name: &str, source_text: &str) -> SourceFile {
    Parser.parse_source_file(file_name, source_text)
}

struct ParserType {
    scanner: Scanner,
    NodeConstructor: Option<fn(SyntaxKind) -> BaseNode>,
    factory: NodeFactory,
    file_name: Option<String>,
    parse_diagnostics: Option<Vec<DiagnosticWithDetachedLocation>>,
    current_token: Option<SyntaxKind>,
    parse_error_before_next_finished_node: bool,
}

impl ParserType {
    fn new() -> ParserType {
        ParserType {
            scanner: create_scanner(),
            NodeConstructor: None,
            factory: create_node_factory(),
            file_name: None,
            parse_diagnostics: None,
            current_token: None,
            parse_error_before_next_finished_node: false,
        }
    }

    fn NodeConstructor(&self) -> fn(SyntaxKind) -> BaseNode {
        self.NodeConstructor.unwrap()
    }

    fn set_NodeConstructor(&mut self, NodeConstructor: fn(SyntaxKind) -> BaseNode) {
        self.NodeConstructor = Some(NodeConstructor);
    }

    fn file_name(&self) -> &str {
        &self.file_name.unwrap()
    }

    fn set_file_name(&mut self, file_name: &str) {
        self.file_name = Some(file_name.to_string());
    }

    fn parse_diagnostics(&self) -> &Vec<DiagnosticWithDetachedLocation> {
        &self.parse_diagnostics.unwrap()
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
    }

    fn initialize_node_factory(&mut self) {}

    fn parse_source_file_worker(&mut self) -> SourceFile {
        self.next_token();

        let statements = self.parse_list(ParserType::parse_statement);
    }

    fn parse_error_at_current_token(&self, message: &DiagnosticMessage) {
        self.parse_error_at(
            self.scanner.get_token_pos(),
            self.scanner.get_text_pos(),
            message,
        );
    }

    fn parse_error_at_position(&self, start: usize, length: usize, message: &DiagnosticMessage) {
        let last_error = last_or_undefined(self.parse_diagnostics());
        if match last_error {
            None => true,
            Some(last_error) => last_error.start != start,
        } {
            self.parse_diagnostics().push(create_detached_diagnostic(
                self.file_name(),
                start,
                length,
                message,
            ));
        }

        self.parse_error_before_next_finished_node = true;
    }

    fn parse_error_at(&self, start: usize, end: usize, message: &DiagnosticMessage) {
        self.parse_error_at_position(start, end - start, message);
    }

    fn token(&self) -> SyntaxKind {
        self.current_token()
    }

    fn next_token_without_check(&mut self) -> SyntaxKind {
        self.set_current_token(self.scanner.scan());
        self.current_token()
    }

    fn next_token(&mut self) -> SyntaxKind {
        self.next_token_without_check()
    }

    fn parse_expected(&self, kind: SyntaxKind, should_advance: Option<bool>) -> bool {
        let should_advance = should_advance.unwrap_or(true);
        if matches!(self.token(), kind) {
            if should_advance {
                self.next_token();
            }
            return true;
        }

        self.parse_error_at_current_token(&Diagnostics::_0_expected);
        false
    }

    fn create_node_array<TParsedNode: Node>(
        &self,
        elements: Vec<TParsedNode>,
    ) -> NodeArray<TParsedNode> {
        self.factory.create_node_array(elements)
    }

    fn finish_node<TParsedNode: Node>(&self, node: TParsedNode) -> Box<TParsedNode> {
        if self.parse_error_before_next_finished_node {
            self.parse_error_before_next_finished_node = false;
        }

        Box::new(node)
    }

    fn is_list_terminator(&self) -> bool {
        if matches!(self.token(), SyntaxKind::EndOfFileToken) {
            return true;
        }
        false
    }

    fn parse_list<TParsedNode: Node>(
        &self,
        parse_element: fn(&ParserType) -> TParsedNode,
    ) -> NodeArray<TParsedNode> {
        let mut list = vec![];

        while !self.is_list_terminator() {}

        self.create_node_array(list)
    }

    // fn parse_expression(&self) -> Box<dyn Expression> {}

    // fn parse_expression_or_labeled_statement(&self) -> Box<dyn Statement> {
    //     let expression = self.parse_expression();
    //     let node = self.factory.create_expression_statement(expression);
    //     Box::new(self.finish_node(node))
    // }

    fn parse_empty_statement(&self) -> Box<dyn Statement> {
        self.parse_expected(SyntaxKind::SemicolonToken, None);
        self.finish_node(self.factory.create_empty_statement(self))
    }

    fn parse_statement(&self) -> Box<dyn Statement> {
        match self.token() {
            SyntaxKind::SemicolonToken => self.parse_empty_statement(),
            _ => {
                // self.parse_expression_or_labeled_statement()
                panic!("Unimplemented")
            }
        }
    }
}

impl BaseNodeFactory for ParserType {
    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        self.NodeConstructor()(kind)
    }
}

lazy_static! {
    static ref Parser: ParserType = ParserType::new();
}
