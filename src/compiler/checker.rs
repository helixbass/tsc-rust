use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;
use std::sync::RwLock;

use crate::{
    create_diagnostic_collection, create_diagnostic_for_node, for_each, object_allocator,
    BaseIntrinsicType, BaseType, BaseUnionOrIntersectionType, Diagnostic, DiagnosticCollection,
    DiagnosticMessage, Diagnostics, Expression, ExpressionStatement, FreshableIntrinsicType,
    IntrinsicType, Node, NodeInterface, PrefixUnaryExpression, RelationComparisonResult,
    SourceFile, Statement, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, UnionType,
};

pub fn create_type_checker(produce_diagnostics: bool) -> TypeChecker {
    let mut type_checker = TypeChecker {
        Type: object_allocator.get_type_constructor(),

        number_type: None,
        bigint_type: None,
        true_type: None,
        regular_true_type: None,
        number_or_big_int_type: None,

        diagnostics: RwLock::new(create_diagnostic_collection()),

        assignable_relation: HashMap::new(),
    };
    type_checker.number_type = Some(Rc::new(
        type_checker
            .create_intrinsic_type(TypeFlags::Number, "number")
            .into(),
    ));
    type_checker.bigint_type = Some(Rc::new(
        type_checker
            .create_intrinsic_type(TypeFlags::BigInt, "bigint")
            .into(),
    ));
    let true_type: Rc<Type> = Rc::new(
        FreshableIntrinsicType::new(
            type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "true"),
        )
        .into(),
    );
    let regular_true_type: Rc<Type> = Rc::new(
        FreshableIntrinsicType::new(
            type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "true"),
        )
        .into(),
    );
    match &*true_type {
        Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type.regular_type = Some(Rc::downgrade(&regular_true_type));
                freshable_intrinsic_type.fresh_type = Some(Rc::downgrade(&true_type));
            }
            _ => panic!("Expected FreshableIntrinsicType"),
        },
        _ => panic!("Expected IntrinsicType"),
    }
    type_checker.true_type = Some(true_type);
    match &*regular_true_type {
        Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type.regular_type = Some(Rc::downgrade(&regular_true_type));
                freshable_intrinsic_type.fresh_type = Some(Rc::downgrade(&true_type));
            }
            _ => panic!("Expected FreshableIntrinsicType"),
        },
        _ => panic!("Expected IntrinsicType"),
    }
    type_checker.regular_true_type = Some(regular_true_type);
    type_checker.number_or_big_int_type = Some(
        type_checker
            .get_union_type(vec![
                type_checker.number_type().clone(),
                type_checker.bigint_type().clone(),
            ])
            .into(),
    );
    type_checker
}

impl TypeChecker {
    fn number_type(&self) -> Rc<Type> {
        self.number_type.as_ref().unwrap().clone()
    }

    fn bigint_type(&self) -> Rc<Type> {
        self.bigint_type.as_ref().unwrap().clone()
    }

    fn true_type(&self) -> Rc<Type> {
        self.true_type.as_ref().unwrap().clone()
    }

    fn number_or_big_int_type(&self) -> Rc<Type> {
        self.number_or_big_int_type.as_ref().unwrap().clone()
    }

    fn diagnostics(&self) -> &RwLock<DiagnosticCollection> {
        &self.diagnostics
    }

    fn create_error<TNode: NodeInterface>(
        &self,
        location: Option<&TNode>,
        message: &DiagnosticMessage,
    ) -> Rc<Diagnostic> {
        if let Some(location) = location {
            Rc::new(create_diagnostic_for_node(location, message).into())
        } else {
            unimplemented!()
        }
    }

    fn error<TNode: NodeInterface>(
        &self,
        location: Option<&TNode>,
        message: &DiagnosticMessage,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.create_error(location, message);
        self.diagnostics().write().unwrap().add(diagnostic.clone());
        diagnostic
    }

    fn error_and_maybe_suggest_await<TNode: NodeInterface>(
        &self,
        location: &TNode,
        message: &DiagnosticMessage,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.error(Some(location), message);
        diagnostic
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

    fn add_type_to_union(&self, type_set: &mut Vec<Rc<Type>>, type_: Rc<Type>) {
        type_set.push(type_);
    }

    fn add_types_to_union(&self, type_set: &mut Vec<Rc<Type>>, types: &[Rc<Type>]) {
        for type_ in types {
            self.add_type_to_union(type_set, type_.clone());
        }
    }

    fn get_union_type(&self, types: Vec<Rc<Type>>) -> Rc<Type> {
        let mut type_set: Vec<Rc<Type>> = vec![];
        self.add_types_to_union(&mut type_set, /*TypeFlags::empty(), */ &types);
        self.get_union_type_from_sorted_list(type_set)
    }

    fn get_union_type_from_sorted_list(&self, types: Vec<Rc<Type>>) -> Rc<Type> {
        let mut type_: Option<Rc<Type>> = None;
        if type_.is_none() {
            let base_type = self.create_type(TypeFlags::Union);
            type_ = Some(Rc::new(
                (UnionType {
                    _union_or_intersection_type: BaseUnionOrIntersectionType {
                        _type: base_type,
                        types,
                    },
                })
                .into(),
            ));
        }
        type_.unwrap()
    }

    fn is_fresh_literal_type(&self, type_: Rc<Type>) -> bool {
        if !type_.flags().intersects(TypeFlags::Literal) {
            return false;
        }
        match &*type_ {
            Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
                IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                    ptr::eq(&*type_, freshable_intrinsic_type.fresh_type().as_ptr())
                }
                _ => panic!("Expected FreshableIntrinsicType"),
            },
            _ => panic!("Expected IntrinsicType"),
        }
    }

    fn is_type_assignable_to(&self, source: Rc<Type>, target: Rc<Type>) -> bool {
        self.is_type_related_to(source, target, &self.assignable_relation)
    }

    fn is_type_related_to(
        &self,
        mut source: Rc<Type>,
        target: Rc<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> bool {
        if self.is_fresh_literal_type(source.clone()) {
            source = match &*source {
                Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
                    IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                        freshable_intrinsic_type.regular_type().upgrade().unwrap()
                    }
                    _ => panic!("Expected IntrinsicType"),
                },
                _ => panic!("Expected IntrinsicType"),
            };
        }
        if source
            .flags()
            .intersects(TypeFlags::StructuredOrInstantiable)
            || target
                .flags()
                .intersects(TypeFlags::StructuredOrInstantiable)
        {
            return self.check_type_related_to(source, target, relation);
        }
        false
    }

    fn check_type_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> bool {
        false
    }

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

    pub fn get_diagnostics(&self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    fn get_diagnostics_worker(&self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self
            .diagnostics()
            .read()
            .unwrap()
            .get_diagnostics(&source_file.file_name);

        semantic_diagnostics
    }

    fn check_arithmetic_operand_type(
        &self,
        operand: /*&Node*/ &Expression,
        type_: Rc<Type>,
        diagnostic: &DiagnosticMessage,
    ) -> bool {
        if !self.is_type_assignable_to(type_, self.number_or_big_int_type()) {
            self.error_and_maybe_suggest_await(operand, diagnostic);
            return false;
        }
        true
    }

    fn check_prefix_unary_expression(&self, node: &PrefixUnaryExpression) -> Rc<Type> {
        let operand_type = self.check_expression(&node.operand);
        match node.operator {
            SyntaxKind::PlusPlusToken => {
                self.check_arithmetic_operand_type(&node.operand, operand_type.clone(), &Diagnostics::An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type);
                return self.get_unary_result_type(&operand_type);
            }
            _ => {
                unimplemented!();
            }
        }
    }

    fn get_unary_result_type(&self, operand_type: &Type) -> Rc<Type> {
        self.number_type().clone()
    }

    fn check_expression(&self, node: &Expression) -> Rc<Type> {
        self.check_expression_worker(node)
    }

    fn check_expression_worker(&self, node: &Expression) -> Rc<Type> {
        match node {
            Expression::TokenExpression(token_expression) => match token_expression.kind() {
                SyntaxKind::TrueKeyword => {
                    return self.true_type().clone();
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
