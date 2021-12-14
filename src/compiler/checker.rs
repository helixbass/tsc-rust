use crate::{
    create_diagnostic_collection, for_each, Diagnostic, Expression, ExpressionStatement, Node,
    SourceFile, Statement, TypeChecker,
};

pub fn create_type_checker(produce_diagnostics: bool) -> TypeChecker {
    TypeChecker {
        diagnostics: create_diagnostic_collection(),
    }
}

impl TypeChecker {
    fn check_source_element(&self, node: &Node) {
        self.check_source_element_worker(node)
    }

    fn check_source_element_worker(&self, node: &Node) {
        match node {
            Node::Statement(statement) => {
                match statement {
                    Statement::ExpressionStatement(expression_statement) => {
                        return self.check_expression_statement(expression_statement);
                    }
                    _ => unimplemented!(),
                };
            }
            _ => unimplemented!(),
        };
    }

    fn check_source_file(&self, source_file: &SourceFile) {
        self.check_source_file_worker(source_file)
    }

    fn check_source_file_worker(&self, node: &SourceFile) {
        if true {
            for_each(&node.statements, |statement, _index| {
                self.check_source_element(statement);
                Option::<()>::None
            });
        }
    }

    pub fn get_diagnostics(&self, source_file: &SourceFile) -> Vec<Box<dyn Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    fn get_diagnostics_worker(&self, source_file: &SourceFile) -> Vec<Box<dyn Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self.diagnostics.get_diagnostics(&source_file.file_name);

        semantic_diagnostics
            .into_iter()
            .map(|diagnostic| {
                let boxed: Box<dyn Diagnostic> = Box::new(diagnostic);
                boxed
            })
            .collect()
    }

    fn check_expression(&self, node: &Expression) {
        self.check_expression_worker(node);
    }

    fn check_expression_worker(&self, node: &Expression) {
        match node {
            // Expression::BinaryExpression(binary_expression) => {
            //     return self.check_binary_expression(binary_expression);
            // }
            _ => unimplemented!(),
        }
    }

    fn check_expression_statement(&self, node: &ExpressionStatement) {
        self.check_expression(&node.expression);
    }
}
