use crate::{
    create_node_factory, create_scanner, BaseNodeFactory, Node, NodeArray, NodeFactory, Scanner,
    SourceFile, Statement, SyntaxKind,
};

pub fn create_source_file(file_name: &str, source_text: &str) -> SourceFile {
    Parser.parse_source_file(file_name, source_text)
}

struct ParserType {
    scanner: Scanner,
    NodeConstructor: Option<fn() -> Box<dyn Node>>,
    factory: NodeFactory,
    current_token: Option<SyntaxKind>,
}

impl ParserType {
    fn new() -> ParserType {
        ParserType {
            scanner: create_scanner(),
            NodeConstructor: None,
            factory: create_node_factory(),
            current_token: None,
        }
    }

    fn NodeConstructor(&self) -> fn() -> Box<dyn Node> {
        self.NodeConstructor.unwrap()
    }

    fn set_NodeConstructor(&mut self, NodeConstructor: fn() -> Box<dyn Node>) {
        self.NodeConstructor = Some(NodeConstructor);
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
        self.NodeConstructor = Some(object_allocator.get_node_constructor());
    }

    fn initialize_node_factory(&mut self) {}

    fn parse_source_file_worker(&mut self) -> SourceFile {
        self.next_token();

        let statements = self.parse_list(ParserType::parse_statement);
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

    fn create_node_array<TParsedNode: Node>(
        &self,
        elements: Vec<TParsedNode>,
    ) -> NodeArray<TParsedNode> {
        self.factory.create_node_array(elements)
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
    fn create_base_node(&self, kind: SyntaxKind) -> Box<dyn Node> {
        self.NodeConstructor()(kind);
    }
}

lazy_static! {
    static ref Parser: ParserType = ParserType::new();
}
