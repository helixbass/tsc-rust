use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;
use std::sync::RwLock;

use crate::{
    bind_source_file, create_diagnostic_collection, create_diagnostic_for_node, for_each,
    object_allocator, BaseIntrinsicType, BaseLiteralType, BaseType, BaseUnionOrIntersectionType,
    Diagnostic, DiagnosticCollection, DiagnosticMessage, Diagnostics, Expression,
    ExpressionStatement, FreshableIntrinsicType, IntrinsicType, LiteralLikeNode,
    LiteralLikeNodeInterface, LiteralTypeInterface, Node, NodeInterface, Number, NumberLiteralType,
    NumericLiteral, PrefixUnaryExpression, RelationComparisonResult, SourceFile, Statement,
    SyntaxKind, Ternary, Type, TypeChecker, TypeCheckerHost, TypeFlags, TypeInterface,
    UnionOrIntersectionType, UnionOrIntersectionTypeInterface, UnionType,
};

pub fn create_type_checker<TTypeCheckerHost: TypeCheckerHost>(
    host: &TTypeCheckerHost,
    produce_diagnostics: bool,
) -> TypeChecker {
    let mut type_checker = TypeChecker {
        Type: object_allocator.get_type_constructor(),

        number_literal_types: HashMap::new(),

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
                freshable_intrinsic_type
                    .regular_type
                    .init(&regular_true_type, true);
                freshable_intrinsic_type.fresh_type.init(&true_type, true);
            }
            _ => panic!("Expected FreshableIntrinsicType"),
        },
        _ => panic!("Expected IntrinsicType"),
    }
    type_checker.true_type = Some(true_type);
    match &*regular_true_type {
        Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type
                    .regular_type
                    .init(&regular_true_type, false);
                freshable_intrinsic_type
                    .fresh_type
                    .init(type_checker.true_type.as_ref().unwrap(), false);
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
    type_checker.initialize_type_checker(host);
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

    // pub fn create_literal_type(
    pub fn create_number_literal_type(
        &self,
        flags: TypeFlags,
        value: Number,
        regular_type: Option<Rc<Type>>,
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = Rc::new(NumberLiteralType::new(type_, value).into());
        match &*type_ {
            Type::LiteralType(literal_type) => {
                literal_type.set_regular_type(&regular_type.unwrap_or_else(|| type_.clone()));
            }
            _ => panic!("Expected LiteralType"),
        }
        type_
    }

    fn get_fresh_type_of_literal_type(&self, type_: Rc<Type>) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Literal) {
            match &*type_ {
                Type::LiteralType(literal_type) => {
                    return literal_type.get_or_initialize_fresh_type(self, &type_);
                }
                _ => panic!("Expected LiteralType"),
            }
        }
        type_
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
            Type::LiteralType(literal_type) => {
                ptr::eq(&*type_, literal_type.fresh_type().unwrap().as_ptr())
            }
            _ => panic!("Expected IntrinsicType or LiteralType"),
        }
    }

    fn get_number_literal_type(&mut self, value: Number) -> Rc<Type> {
        if self.number_literal_types.contains_key(&value) {
            return self.number_literal_types.get(&value).unwrap().clone();
        }
        let type_ = self.create_number_literal_type(TypeFlags::NumberLiteral, value, None);
        self.number_literal_types.insert(value, type_.clone());
        type_
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
                Type::LiteralType(literal_type) => literal_type.regular_type(),
                _ => panic!("Expected IntrinsicType or LiteralType"),
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

    fn get_normalized_type(&self, mut type_: Rc<Type>) -> Rc<Type> {
        loop {
            let t: Rc<Type> = if self.is_fresh_literal_type(type_.clone()) {
                match &*type_ {
                    Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
                        IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                            freshable_intrinsic_type.regular_type().upgrade().unwrap()
                        }
                        _ => panic!("Expected FreshableIntrinsicType"),
                    },
                    Type::LiteralType(literal_type) => literal_type.regular_type(),
                    _ => panic!("Expected IntrinsicType or LiteralType"),
                }
            } else {
                type_.clone()
            };
            if Rc::ptr_eq(&t, &type_) {
                break;
            }
            type_ = t.clone();
        }
        type_
    }

    fn check_type_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> bool {
        CheckTypeRelatedTo::new(self, source, target, relation).call()
    }

    fn get_constituent_count(&self, type_: Rc<Type>) -> usize {
        if type_.flags().intersects(TypeFlags::Union) {
            match &*type_ {
                Type::UnionOrIntersectionType(union_or_intersection_type) => {
                    match union_or_intersection_type {
                        UnionOrIntersectionType::UnionType(union_type) => union_type.types().len(),
                    }
                }
                _ => panic!("Expected UnionOrIntersectionType"),
            }
        } else {
            1
        }
    }

    fn check_source_element(&mut self, node: &Node) {
        self.check_source_element_worker(node)
    }

    fn check_source_element_worker(&mut self, node: &Node) {
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

    fn check_source_file(&mut self, source_file: &SourceFile) {
        self.check_source_file_worker(source_file)
    }

    fn check_source_file_worker(&mut self, node: &SourceFile) {
        if true {
            for_each(&node.statements, |statement, _index| {
                self.check_source_element(statement);
                Option::<()>::None
            });
        }
    }

    pub fn get_diagnostics(&mut self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    fn get_diagnostics_worker(&mut self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
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

    fn check_prefix_unary_expression(&mut self, node: &PrefixUnaryExpression) -> Rc<Type> {
        let operand_expression = match &*node.operand {
            Node::Expression(expression) => expression,
            _ => panic!("Expected Expression"),
        };
        let operand_type = self.check_expression(operand_expression);
        match node.operator {
            SyntaxKind::PlusPlusToken => {
                self.check_arithmetic_operand_type(operand_expression, operand_type.clone(), &Diagnostics::An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type);
                return self.get_unary_result_type(&operand_type);
            }
            _ => {
                unimplemented!();
            }
        }
    }

    fn get_unary_result_type(&self, operand_type: &Type) -> Rc<Type> {
        self.number_type()
    }

    fn check_expression(&mut self, node: &Expression) -> Rc<Type> {
        self.check_expression_worker(node)
    }

    fn check_expression_worker(&mut self, node: &Expression) -> Rc<Type> {
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
            Expression::LiteralLikeNode(literal_like_node) => match literal_like_node {
                LiteralLikeNode::NumericLiteral(numeric_literal) => {
                    self.check_grammar_numeric_literal(numeric_literal);
                    let type_: Rc<Type> =
                        self.get_number_literal_type(numeric_literal.text().into());
                    return self.get_fresh_type_of_literal_type(type_);
                }
            },
            _ => unimplemented!(),
        }
    }

    fn check_expression_statement(&mut self, node: &ExpressionStatement) {
        let expression = match &*node.expression {
            Node::Expression(expression) => expression,
            _ => panic!("Expected Expression"),
        };
        self.check_expression(expression);
    }

    fn initialize_type_checker<TTypeCheckerHost: TypeCheckerHost>(&self, host: &TTypeCheckerHost) {
        for file in host.get_source_files() {
            bind_source_file(file);
        }
    }

    fn check_grammar_numeric_literal(&self, node: &NumericLiteral) -> bool {
        false
    }
}

struct CheckTypeRelatedTo<'type_checker> {
    type_checker: &'type_checker TypeChecker,
    source: Rc<Type>,
    target: Rc<Type>,
    relation: &'type_checker HashMap<String, RelationComparisonResult>,
}

impl<'type_checker> CheckTypeRelatedTo<'type_checker> {
    fn new(
        type_checker: &'type_checker TypeChecker,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &'type_checker HashMap<String, RelationComparisonResult>,
    ) -> Self {
        Self {
            type_checker,
            source,
            target,
            relation,
        }
    }

    fn call(&self) -> bool {
        let result = self.is_related_to(self.source.clone(), self.target.clone());

        result != Ternary::False
    }

    fn is_related_to(&self, original_source: Rc<Type>, original_target: Rc<Type>) -> Ternary {
        let source = self.type_checker.get_normalized_type(original_source);
        let target = self.type_checker.get_normalized_type(original_target);

        let mut result = Ternary::False;

        if (source.flags().intersects(TypeFlags::Union)
            || target.flags().intersects(TypeFlags::Union))
            && self.type_checker.get_constituent_count(source.clone())
                * self.type_checker.get_constituent_count(target.clone())
                < 4
        {
            result = self.structured_type_related_to(source, target);
        }

        result
    }

    fn structured_type_related_to(&self, source: Rc<Type>, target: Rc<Type>) -> Ternary {
        let result = self.structured_type_related_to_worker(source, target);
        result
    }

    fn structured_type_related_to_worker(&self, source: Rc<Type>, target: Rc<Type>) -> Ternary {
        Ternary::False
    }
}
