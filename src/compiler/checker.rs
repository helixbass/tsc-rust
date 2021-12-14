use crate::{
    create_diagnostic_collection, for_each, object_allocator, BaseIntrinsicType, BaseType,
    Diagnostic, DiagnosticMessage, Diagnostics, Expression, ExpressionStatement,
    FreshableIntrinsicType, Node, NodeInterface, PrefixUnaryExpression, SourceFile, Statement,
    SyntaxKind, Type, TypeChecker, TypeFlags,
};

pub fn create_type_checker(produce_diagnostics: bool) -> TypeChecker {
    let mut type_checker = TypeChecker {
        Type: object_allocator.get_type_constructor(),

        number_type: None,
        bigint_type: None,
        true_type: None,
        number_or_big_int_type: None,

        diagnostics: create_diagnostic_collection(),
    };
    type_checker.number_type =
        Some(type_checker.create_intrinsic_type(TypeFlags::Number, "number"));
    type_checker.bigint_type =
        Some(type_checker.create_intrinsic_type(TypeFlags::BigInt, "bigint"));
    type_checker.true_type = Some(FreshableIntrinsicType::new(
        type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "true"),
    ));
    type_checker.number_or_big_int_type = Some(type_checker.get_union_type(vec![
        Box::new(type_checker.number_type()),
        Box::new(type_checker.bigint_type()),
    ]));
    type_checker
}

impl TypeChecker {
    fn number_type(&self) -> BaseIntrinsicType {
        self.number_type.as_ref().unwrap().clone()
    }

    fn bigint_type(&self) -> BaseIntrinsicType {
        self.bigint_type.as_ref().unwrap().clone()
    }

    fn true_type(&self) -> FreshableIntrinsicType {
        self.true_type.as_ref().unwrap().clone()
    }

    fn number_or_big_int_type(&self) -> Box<dyn Type> {
        self.number_or_big_int_type.unwrap().clone()
    }

    fn create_type(&self, flags: TypeFlags) -> BaseType {
        let result = (self.Type)(flags);
        result
    }

    fn create_intrinsic_type(&self, kind: TypeFlags, intrinsic_name: &str) -> BaseIntrinsicType {
        let type_ = self.create_type(kind);
        let type_ = BaseIntrinsicType::new(type_);
        type_
    }

    fn get_union_type(&self, types: Vec<Box<dyn Type>>) -> Box<dyn Type> {}

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

    fn check_arithmetic_operand_type(
        &self,
        operand: /*&Node*/ &Expression,
        type_: &Box<dyn Type>,
        diagnostic: &DiagnosticMessage,
    ) -> bool {
        if !self.is_type_assignable_to(type_, self.number_or_big_int_type()) {
            self.error_and_maybe_suggest_await(operand, diagnostic);
            return false;
        }
        true
    }

    fn check_prefix_unary_expression(&self, node: &PrefixUnaryExpression) -> Box<dyn Type> {
        let operand_type = self.check_expression(&node.operand);
        match node.operator {
            SyntaxKind::PlusPlusToken => {
                self.check_arithmetic_operand_type(&node.operand, &operand_type, &Diagnostics::An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type);
                return self.get_unary_result_type(&operand_type);
            }
            _ => {
                unimplemented!();
            }
        }
    }

    fn get_unary_result_type(&self, operand_type: &Box<dyn Type>) -> Box<dyn Type> {
        Box::new(self.number_type().clone())
    }

    fn check_expression(&self, node: &Expression) -> Box<dyn Type> {
        self.check_expression_worker(node)
    }

    fn check_expression_worker(&self, node: &Expression) -> Box<dyn Type> {
        match node {
            Expression::TokenExpression(token_expression) => match token_expression.kind() {
                SyntaxKind::TrueKeyword => {
                    return Box::new(self.true_type().clone());
                }
                _ => unimplemented!(),
            },
            Expression::PrefixUnaryExpression(prefix_unary_expression) => {
                return self.check_prefix_unary_expression(prefix_unary_expression);
            }
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
