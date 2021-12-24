use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use crate::{
    bind_source_file, chain_diagnostic_messages, create_diagnostic_collection,
    create_diagnostic_for_node, create_diagnostic_for_node_from_message_chain, create_text_writer,
    every, factory, for_each, get_effective_initializer, get_effective_type_annotation_node,
    get_synthetic_factory, is_variable_declaration, object_allocator, BaseIntrinsicType,
    BaseLiteralType, BaseType, BaseUnionOrIntersectionType, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, Expression,
    ExpressionStatement, FreshableIntrinsicType, HasTypeInterface, IntrinsicType, KeywordTypeNode,
    LiteralLikeNode, LiteralLikeNodeInterface, LiteralTypeInterface, Node, NodeInterface, Number,
    NumberLiteralType, NumericLiteral, PrefixUnaryExpression, RelationComparisonResult, SourceFile,
    Statement, Symbol, SymbolFlags, SymbolTracker, SyntaxKind, Ternary, Type, TypeChecker,
    TypeCheckerHost, TypeFlags, TypeInterface, TypeNode, UnionOrIntersectionType,
    UnionOrIntersectionTypeInterface, UnionType, VariableDeclaration, VariableStatement,
};

bitflags! {
    struct IntersectionState: u32 {
        const None = 0;
        const Source = 1 << 0;
        const Target = 1 << 1;
        const PropertyCheck = 1 << 2;
        const UnionIntersectionCheck = 1 << 3;
        const InPropertyCheck = 1 << 4;
    }
}

pub fn create_type_checker<TTypeCheckerHost: TypeCheckerHost>(
    host: &TTypeCheckerHost,
    produce_diagnostics: bool,
) -> TypeChecker {
    let mut type_checker = TypeChecker {
        Type: object_allocator.get_type_constructor(),

        node_builder: create_node_builder(),

        number_literal_types: HashMap::new(),

        number_type: None,
        bigint_type: None,
        true_type: None,
        regular_true_type: None,
        number_or_big_int_type: None,

        diagnostics: RefCell::new(create_diagnostic_collection()),

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

    fn diagnostics(&self) -> RefMut<DiagnosticCollection> {
        self.diagnostics.borrow_mut()
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
        self.diagnostics().add(diagnostic.clone());
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

    fn get_merged_symbol(&self, symbol: Option<Rc<Symbol>>) -> Option<Rc<Symbol>> {
        symbol
    }

    fn get_symbol_of_node<TNode: NodeInterface>(&self, node: &TNode) -> Option<Rc<Symbol>> {
        self.get_merged_symbol(node.maybe_symbol())
    }

    fn create_type(&self, flags: TypeFlags) -> BaseType {
        let result = (self.Type)(flags);
        result
    }

    fn create_intrinsic_type(&self, kind: TypeFlags, intrinsic_name: &str) -> BaseIntrinsicType {
        let type_ = self.create_type(kind);
        let type_ = BaseIntrinsicType::new(type_, intrinsic_name.to_string());
        type_
    }

    fn type_to_string(&self, type_: &Type) -> String {
        let writer = create_text_writer("");
        let type_node = self.node_builder.type_to_type_node(type_, Some(&writer));
    }

    fn get_type_names_for_error_display(&self, left: &Type, right: &Type) -> (String, String) {
        let left_str = if false {
            unimplemented!()
        } else {
            self.type_to_string(left)
        };
        let right_str = if false {
            unimplemented!()
        } else {
            self.type_to_string(right)
        };
        (left_str, right_str)
    }

    fn get_type_name_for_error_display(&self, type_: &Type) -> String {
        self.type_to_string(type_)
    }

    fn add_optionality(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    fn get_type_for_variable_like_declaration(&self, declaration: &Node) -> Option<Rc<Type>> {
        let declared_type = self.try_get_type_from_effective_type_node(declaration);
        if let Some(declared_type) = declared_type {
            return Some(self.add_optionality(declared_type));
        }
        unimplemented!()
    }

    fn get_widened_type_for_variable_like_declaration(&self, declaration: &Node) -> Rc<Type> {
        self.widen_type_for_variable_like_declaration(
            self.get_type_for_variable_like_declaration(declaration),
            declaration,
        )
    }

    fn widen_type_for_variable_like_declaration(
        &self,
        type_: Option<Rc<Type>>,
        declaration: &Node,
    ) -> Rc<Type> {
        if let Some(type_) = type_ {
            return self.get_widened_type(type_);
        }
        unimplemented!()
    }

    fn try_get_type_from_effective_type_node(
        &self,
        declaration: &Node, /*Declaration*/
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(declaration);
        type_node.map(|type_node| self.get_type_from_type_node(&*type_node))
    }

    fn get_type_of_variable_or_parameter_or_property(&self, symbol: &Symbol) -> Rc<Type> {
        // let links = self.get_symbol_links(symbol);
        // if links.type_.is_none() {
        self.get_type_of_variable_or_parameter_or_property_worker(symbol)
        // }
        // links.type.unwrap().clone()
    }

    fn get_type_of_variable_or_parameter_or_property_worker(&self, symbol: &Symbol) -> Rc<Type> {
        Debug_.assert_is_defined(symbol.maybe_value_declaration().as_ref(), None);
        let declaration = symbol
            .maybe_value_declaration()
            .as_ref()
            .unwrap()
            .upgrade()
            .unwrap();

        let type_: Rc<Type>;
        if false {
            unimplemented!()
        } else if is_variable_declaration(&*declaration) {
            type_ = self.get_widened_type_for_variable_like_declaration(&*declaration);
        } else {
            unimplemented!()
        }

        type_
    }

    fn get_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        if symbol
            .flags()
            .intersects(SymbolFlags::Variable | SymbolFlags::Property)
        {
            return self.get_type_of_variable_or_parameter_or_property(symbol);
        }
        unimplemented!()
    }

    fn get_conditional_flow_type_of_type(&self, type_: Rc<Type>, node: &Node) -> Rc<Type> {
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

    fn get_type_from_type_node(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
        self.get_conditional_flow_type_of_type(self.get_type_from_type_node_worker(node), node)
    }

    fn get_type_from_type_node_worker(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
        match node.kind() {
            SyntaxKind::NumberKeyword => self.number_type(),
            _ => unimplemented!(),
        }
    }

    fn is_type_assignable_to(&self, source: Rc<Type>, target: Rc<Type>) -> bool {
        self.is_type_related_to(source, target, &self.assignable_relation)
    }

    fn check_type_assignable_to_and_optionally_elaborate(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        error_node: Option<&Node>,
        expr: Option<&Expression>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        self.check_type_related_to_and_optionally_elaborate(
            source,
            target,
            &self.assignable_relation,
            error_node,
            expr,
            head_message,
        )
    }

    fn check_type_related_to_and_optionally_elaborate(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
        error_node: Option<&Node>,
        expr: Option<&Expression>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        if self.is_type_related_to(source.clone(), target.clone(), relation) {
            return true;
        }
        if true {
            return self.check_type_related_to(source, target, relation, error_node, head_message);
        }
        false
    }

    fn is_simple_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        error_reporter: Option<ErrorReporter>,
    ) -> bool {
        let s = source.flags();
        let t = target.flags();
        if s.intersects(TypeFlags::NumberLike) && t.intersects(TypeFlags::Number) {
            return true;
        }
        false
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
        if true {
            if self.is_simple_type_related_to(&*source, &*target, relation, None) {
                return true;
            }
        } else {
            unimplemented!()
        }
        if source
            .flags()
            .intersects(TypeFlags::StructuredOrInstantiable)
            || target
                .flags()
                .intersects(TypeFlags::StructuredOrInstantiable)
        {
            return self.check_type_related_to(source, target, relation, None, None);
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
        error_node: Option<&Node>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        CheckTypeRelatedTo::new(self, source, target, relation, error_node, head_message).call()
    }

    fn type_could_have_top_level_singleton_types(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Boolean) {
            return false;
        }

        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            return for_each(type_.as_union_or_intersection_type().types(), |type_, _| {
                if self.type_could_have_top_level_singleton_types(type_) {
                    Some(())
                } else {
                    None
                }
            })
            .is_some();
        }

        if type_.flags().intersects(TypeFlags::Instantiable) {
            unimplemented!()
        }

        self.is_unit_type(type_) || type_.flags().intersects(TypeFlags::TemplateLiteral)
    }

    fn is_unit_type(&self, type_: &Type) -> bool {
        type_.flags().intersects(TypeFlags::Unit)
    }

    fn is_literal_type(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Boolean) {
            true
        } else if type_.flags().intersects(TypeFlags::Union) {
            if type_.flags().intersects(TypeFlags::EnumLiteral) {
                true
            } else {
                every(type_.as_union_or_intersection_type().types(), |type_, _| {
                    self.is_unit_type(&**type_)
                })
            }
        } else {
            self.is_unit_type(type_)
        }
    }

    fn get_base_type_of_literal_type(&self, type_: Rc<Type>) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::EnumLiteral) {
            unimplemented!()
        } else if type_.flags().intersects(TypeFlags::StringLiteral) {
            unimplemented!()
        } else if type_.flags().intersects(TypeFlags::NumberLiteral) {
            self.number_type()
        } else if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            self.bigint_type()
        } else if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            unimplemented!()
        } else if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(type_, |type_| self.get_base_type_of_literal_type(type_))
        } else {
            type_
        }
    }

    fn get_regular_type_of_object_literal(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    fn get_widened_type(&self, type_: Rc<Type>) -> Rc<Type> {
        self.get_widened_type_with_context(type_)
    }

    fn get_widened_type_with_context(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    fn map_type<TMapper: FnMut(Rc<Type>) -> Rc<Type>>(
        &self,
        type_: Rc<Type>,
        mut mapper: TMapper,
    ) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Never) {
            return type_;
        }
        if !type_.flags().intersects(TypeFlags::Union) {
            return mapper(type_);
        }
        let types = type_.as_union_or_intersection_type().types();
        unimplemented!()
    }

    fn get_constituent_count(&self, type_: Rc<Type>) -> usize {
        if type_.flags().intersects(TypeFlags::Union) {
            type_.as_union_or_intersection_type().types().len()
        } else {
            1
        }
    }

    fn check_source_element<TNodeRef: Borrow<Node>>(&mut self, node: Option<TNodeRef>) {
        if let Some(node) = node {
            let node = node.borrow();
            self.check_source_element_worker(node);
        }
    }

    fn check_source_element_worker(&mut self, node: &Node) {
        match node {
            Node::TypeNode(type_node) => match type_node {
                TypeNode::KeywordTypeNode(_) => (),
                _ => unimplemented!(),
            },
            Node::Statement(statement) => {
                match statement {
                    Statement::VariableStatement(variable_statement) => {
                        return self.check_variable_statement(variable_statement);
                    }
                    Statement::ExpressionStatement(expression_statement) => {
                        return self.check_expression_statement(expression_statement);
                    }
                    _ => unimplemented!(),
                };
            }
            Node::VariableDeclaration(variable_declaration) => {
                return self.check_variable_declaration(variable_declaration);
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
                self.check_source_element(Some(&**statement));
                Option::<()>::None
            });
        }
    }

    pub fn get_diagnostics(&mut self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    fn get_diagnostics_worker(&mut self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self.diagnostics().get_diagnostics(&source_file.file_name);

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

    fn check_expression_cached(&mut self, node: &Expression) -> Rc<Type> {
        self.check_expression(node)
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

    fn convert_auto_to_any(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    fn check_variable_like_declaration(&mut self, node: &VariableDeclaration) {
        if true {
            self.check_source_element(node.type_());
        }

        let symbol = self.get_symbol_of_node(node).unwrap();

        let type_ = self.convert_auto_to_any(self.get_type_of_symbol(&*symbol));
        let value_declaration = symbol.maybe_value_declaration();
        let wrapper = node.node_wrapper();
        if value_declaration.is_some()
            && Rc::ptr_eq(
                &wrapper,
                &value_declaration.as_ref().unwrap().upgrade().unwrap(),
            )
        {
            let initializer = get_effective_initializer(node);
            if let Some(initializer) = initializer {
                if true {
                    let initializer_type = self.check_expression_cached(match &*initializer {
                        Node::Expression(expression) => expression,
                        _ => panic!("Expected Expression"),
                    });
                    self.check_type_assignable_to_and_optionally_elaborate(
                        initializer_type,
                        type_,
                        Some(&*wrapper),
                        Some(match &*initializer {
                            Node::Expression(expression) => expression,
                            _ => panic!("Expected Expression"),
                        }),
                        None,
                    );
                }
            }
        } else {
            unimplemented!()
        }
    }

    fn check_variable_declaration(&mut self, node: &VariableDeclaration) {
        self.check_variable_like_declaration(node);
    }

    fn check_variable_statement(&mut self, node: &VariableStatement) {
        for_each(
            &match &*node.declaration_list {
                Node::VariableDeclarationList(variable_declaration_list) => {
                    variable_declaration_list
                }
                _ => panic!("Expected VariableDeclarationList"),
            }
            .declarations,
            |declaration, _| Some(self.check_source_element(Some(&**declaration))),
        );
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
            bind_source_file(&*file);
            println!("post-binding: {:#?}", file);
        }
    }

    fn check_grammar_numeric_literal(&self, node: &NumericLiteral) -> bool {
        false
    }
}

fn create_node_builder() -> NodeBuilder {
    NodeBuilder::new()
}

pub struct NodeBuilder {}

impl NodeBuilder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn type_to_type_node(
        &self,
        type_: &Type,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<TypeNode> {
        self.with_context(tracker, |context| {
            self.type_to_type_node_helper(type_, context)
        })
    }

    fn with_context<TReturn, TCallback: FnMut(&NodeBuilderContext) -> TReturn>(
        &self,
        tracker: Option<&dyn SymbolTracker>,
        cb: TCallback,
    ) -> Option<TReturn> {
        let context = NodeBuilderContext::new(tracker.unwrap());
        let resulting_node = cb(&context);
        Some(resulting_node)
    }

    pub fn type_to_type_node_helper(&self, type_: &Type, context: &NodeBuilderContext) -> TypeNode {
        if type_.flags().intersects(TypeFlags::Number) {
            return Into::<KeywordTypeNode>::into(
                factory
                    .create_keyword_type_node(get_synthetic_factory(), SyntaxKind::NumberKeyword),
            )
            .into();
        }
        if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            return factory
                .create_literal_type_node(
                    get_synthetic_factory(),
                    &if type_.as_intrinsic_type().intrinsic_name() == "true" {
                        factory.create_true(get_synthetic_factory())
                    } else {
                        factory.create_false(get_synthetic_factory())
                    },
                )
                .into();
        }
        unimplemented!()
    }
}

struct NodeBuilderContext<'symbol_tracker> {
    tracker: &'symbol_tracker dyn SymbolTracker,
}

impl<'symbol_tracker> NodeBuilderContext<'symbol_tracker> {
    pub fn new(tracker: &'symbol_tracker dyn SymbolTracker) -> Self {
        Self { tracker }
    }
}

type ErrorReporter<'a> = &'a dyn FnMut(DiagnosticMessage);

struct CheckTypeRelatedTo<'type_checker> {
    type_checker: &'type_checker TypeChecker,
    source: Rc<Type>,
    target: Rc<Type>,
    relation: &'type_checker HashMap<String, RelationComparisonResult>,
    error_node: Option<&'type_checker Node>,
    head_message: Option<DiagnosticMessage>,
    error_info: RefCell<Option<DiagnosticMessageChain>>,
}

impl<'type_checker> CheckTypeRelatedTo<'type_checker> {
    fn new(
        type_checker: &'type_checker TypeChecker,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &'type_checker HashMap<String, RelationComparisonResult>,
        error_node: Option<&'type_checker Node>,
        head_message: Option<DiagnosticMessage>,
    ) -> Self {
        Self {
            type_checker,
            source,
            target,
            relation,
            error_node,
            head_message,
            error_info: RefCell::new(None),
        }
    }

    fn error_info(&self) -> Ref<Option<DiagnosticMessageChain>> {
        self.error_info.borrow()
    }

    fn set_error_info(&self, error_info: DiagnosticMessageChain) {
        *self.error_info.borrow_mut() = Some(error_info);
    }

    fn call(&self) -> bool {
        let result = self.is_related_to(
            self.source.clone(),
            self.target.clone(),
            self.error_node.is_some(),
            self.head_message.as_ref(),
            None,
        );

        if false {
            unimplemented!()
        } else if self.error_info().is_some() {
            let diag = create_diagnostic_for_node_from_message_chain(
                &*self.error_node.unwrap(),
                &*self.error_info().as_ref().unwrap(),
            );
            if true {
                self.type_checker.diagnostics().add(Rc::new(diag.into()));
            }
        }

        result != Ternary::False
    }

    fn report_error(&self, message: &DiagnosticMessage) {
        Debug_.assert(self.error_node.is_some(), None);
        self.set_error_info(chain_diagnostic_messages(
            self.error_info().clone(),
            message,
        ));
    }

    fn report_relation_error(
        &self,
        mut message: Option<&DiagnosticMessage>,
        source: Rc<Type>,
        target: Rc<Type>,
    ) {
        let (source_type, target_type) = self
            .type_checker
            .get_type_names_for_error_display(&*source, &*target);
        let mut generalized_source = source.clone();
        let mut generalized_source_type = source_type;

        if self.type_checker.is_literal_type(&*source)
            && !self
                .type_checker
                .type_could_have_top_level_singleton_types(&*target)
        {
            generalized_source = self
                .type_checker
                .get_base_type_of_literal_type(source.clone());
            Debug_.assert(
                !self
                    .type_checker
                    .is_type_assignable_to(generalized_source, target),
                Some("generalized source shouldn't be assignable"),
            );
            generalized_source_type = self.type_checker.get_type_name_for_error_display(&*source);
        }

        if message.is_none() {
            if false {
            } else {
                message = Some(&Diagnostics::Type_0_is_not_assignable_to_type_1);
            }
        }

        self.report_error(
            message.unwrap(), /*, generalized_source_type, target_type*/
        );
    }

    fn is_related_to(
        &self,
        original_source: Rc<Type>,
        original_target: Rc<Type>,
        report_errors: bool,
        head_message: Option<&DiagnosticMessage>,
        intersection_state: Option<IntersectionState>,
    ) -> Ternary {
        let intersection_state = intersection_state.unwrap_or(IntersectionState::None);

        let source = self.type_checker.get_normalized_type(original_source);
        let target = self.type_checker.get_normalized_type(original_target);

        let report_error_results = |source, target, result| {
            if result == Ternary::False && report_errors {
                let source = source;
                let target = target;
                self.report_relation_error(head_message, source, target);
            }
        };

        let report_error = |message: DiagnosticMessage| {
            self.report_error(&message);
        };

        if self.type_checker.is_simple_type_related_to(
            &*source,
            &*target,
            self.relation,
            if report_errors {
                Some(&report_error)
            } else {
                None
            },
        ) {
            return Ternary::True;
        }

        let mut result = Ternary::False;

        if (source.flags().intersects(TypeFlags::Union)
            || target.flags().intersects(TypeFlags::Union))
            && self.type_checker.get_constituent_count(source.clone())
                * self.type_checker.get_constituent_count(target.clone())
                < 4
        {
            result = self.structured_type_related_to(
                source.clone(),
                target.clone(),
                intersection_state | IntersectionState::UnionIntersectionCheck,
            );
        }

        report_error_results(source, target, result);

        result
    }

    fn type_related_to_some_type(
        &self,
        source: Rc<Type>,
        target: &UnionOrIntersectionType,
    ) -> Ternary {
        let target_types = target.types();

        for type_ in target_types {
            let related = self.is_related_to(source.clone(), type_.clone(), false, None, None);
            if related != Ternary::False {
                return related;
            }
        }

        Ternary::False
    }

    fn structured_type_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let result = self.structured_type_related_to_worker(source, target, intersection_state);
        result
    }

    fn structured_type_related_to_worker(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if intersection_state.intersects(IntersectionState::UnionIntersectionCheck) {
            if target.flags().intersects(TypeFlags::Union) {
                return self.type_related_to_some_type(
                    self.type_checker.get_regular_type_of_object_literal(source),
                    match &*target {
                        Type::UnionOrIntersectionType(union_or_intersection_type) => {
                            union_or_intersection_type
                        }
                        _ => panic!("Expected UnionOrIntersectionType"),
                    },
                );
            }
            unimplemented!()
        }

        Ternary::False
    }
}
