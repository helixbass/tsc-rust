#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, CheckTypeRelatedTo};
use crate::{
    get_check_flags, ArrayTypeNode, BaseLiteralType, CheckFlags, Debug_, DiagnosticMessage,
    Expression, IntrinsicType, LiteralTypeInterface, LiteralTypeNode, NamedDeclarationInterface,
    Node, NodeInterface, Number, NumberLiteralType, ObjectLiteralExpression,
    RelationComparisonResult, StringLiteralType, Symbol, SymbolInterface, SyntaxKind,
    TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper, TypeNode,
    UnionOrIntersectionType,
};

impl TypeChecker {
    // pub fn create_literal_type(
    pub fn create_string_literal_type<TTypeRef: Borrow<Type>>(
        &self,
        flags: TypeFlags,
        value: String,
        regular_type: Option<TTypeRef>,
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = StringLiteralType::new(type_, value).into();
        match &*type_ {
            Type::LiteralType(literal_type) => {
                literal_type.set_regular_type(&if let Some(regular_type) = regular_type {
                    regular_type.borrow().type_wrapper()
                } else {
                    type_.clone()
                });
            }
            _ => panic!("Expected LiteralType"),
        }
        type_
    }

    pub fn create_number_literal_type<TTypeRef: Borrow<Type>>(
        &self,
        flags: TypeFlags,
        value: Number,
        regular_type: Option<TTypeRef>,
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = NumberLiteralType::new(type_, value).into();
        match &*type_ {
            Type::LiteralType(literal_type) => {
                literal_type.set_regular_type(&if let Some(regular_type) = regular_type {
                    regular_type.borrow().type_wrapper()
                } else {
                    type_.clone()
                });
            }
            _ => panic!("Expected LiteralType"),
        }
        type_
    }

    pub(super) fn get_fresh_type_of_literal_type(&self, type_: &Type) -> Rc<Type> {
        match type_ {
            Type::LiteralType(literal_type) => {
                return literal_type.get_or_initialize_fresh_type(self);
            }
            _ => type_.type_wrapper(),
        }
    }

    pub(super) fn get_regular_type_of_literal_type(&self, type_: &Type) -> Rc<Type> {
        match type_ {
            Type::LiteralType(literal_type) => return literal_type.regular_type(),
            Type::UnionOrIntersectionType(UnionOrIntersectionType::UnionType(union_type)) => {
                unimplemented!()
            }
            _ => type_.type_wrapper(),
        }
    }

    pub(super) fn is_fresh_literal_type(&self, type_: &Type) -> bool {
        if !type_.flags().intersects(TypeFlags::Literal) {
            return false;
        }
        match type_ {
            Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
                IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                    ptr::eq(type_, freshable_intrinsic_type.fresh_type().as_ptr())
                }
                _ => panic!("Expected FreshableIntrinsicType"),
            },
            Type::LiteralType(literal_type) => {
                ptr::eq(type_, literal_type.fresh_type().unwrap().as_ptr())
            }
            _ => panic!("Expected IntrinsicType or LiteralType"),
        }
    }

    pub(super) fn get_string_literal_type(&self, value: &str) -> Rc<Type> {
        let mut string_literal_types = self.string_literal_types();
        if string_literal_types.contains_key(value) {
            return string_literal_types.get(value).unwrap().clone();
        }
        let type_ = self.create_string_literal_type(
            TypeFlags::StringLiteral,
            value.to_string(),
            Option::<&Type>::None,
        );
        string_literal_types.insert(value.to_string(), type_.clone());
        type_
    }

    pub(super) fn get_number_literal_type(&self, value: Number) -> Rc<Type> {
        let mut number_literal_types = self.number_literal_types();
        if number_literal_types.contains_key(&value) {
            return number_literal_types.get(&value).unwrap().clone();
        }
        let type_ =
            self.create_number_literal_type(TypeFlags::NumberLiteral, value, Option::<&Type>::None);
        number_literal_types.insert(value, type_.clone());
        type_
    }

    pub(super) fn get_type_from_literal_type_node(&self, node: &LiteralTypeNode) -> Rc<Type> {
        if node.literal.kind() == SyntaxKind::NullKeyword {
            unimplemented!()
        }
        let links = self.get_node_links(node);
        let mut links_ref = links.borrow_mut();
        if links_ref.resolved_type.is_none() {
            links_ref.resolved_type = Some(self.get_regular_type_of_literal_type(
                &self.check_expression(
                    match &*node.literal {
                        Node::Expression(expression) => expression,
                        _ => panic!("Expected Expression"),
                    },
                    None,
                ),
            ));
        }
        links_ref.resolved_type.clone().unwrap()
    }

    pub(super) fn get_array_element_type_node(
        &self,
        node: &ArrayTypeNode,
    ) -> Option<Rc<Node /*TypeNode*/>> {
        Some(node.element_type.clone())
    }

    pub(super) fn get_type_from_type_node(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
        self.get_conditional_flow_type_of_type(&self.get_type_from_type_node_worker(node), node)
    }

    pub(super) fn get_type_from_type_node_worker(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
        let node = match node {
            Node::TypeNode(type_node) => type_node,
            _ => panic!("Expected TypeNode"),
        };
        match node {
            TypeNode::KeywordTypeNode(_) => match node.kind() {
                SyntaxKind::NumberKeyword => self.number_type(),
                _ => unimplemented!(),
            },
            TypeNode::LiteralTypeNode(literal_type_node) => {
                self.get_type_from_literal_type_node(literal_type_node)
            }
            TypeNode::TypeReferenceNode(type_reference_node) => {
                self.get_type_from_type_reference(type_reference_node)
            }
            TypeNode::ArrayTypeNode(array_type_node) => {
                self.get_type_from_array_or_tuple_type_node(array_type_node)
            }
            TypeNode::UnionTypeNode(union_type_node) => {
                self.get_type_from_union_type_node(union_type_node)
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn create_type_mapper(
        &self,
        sources: Vec<Rc<Type /*TypeParameter*/>>,
        targets: Option<Vec<Rc<Type>>>,
    ) -> TypeMapper {
        if sources.len() == 1 {
            self.make_unary_type_mapper(
                &sources[0],
                &*if let Some(targets) = targets {
                    targets[0].clone()
                } else {
                    self.any_type()
                },
            )
        } else {
            self.make_array_type_mapper(sources, targets)
        }
    }

    pub(super) fn get_mapped_type(&self, type_: &Type, mapper: &TypeMapper) -> Rc<Type> {
        match mapper {
            TypeMapper::Simple(mapper) => {
                if ptr::eq(type_, Rc::as_ptr(&mapper.source)) {
                    mapper.target.clone()
                } else {
                    type_.type_wrapper()
                }
            }
            TypeMapper::Array(mapper) => {
                let sources = &mapper.sources;
                let targets = &mapper.targets;
                for (i, source) in sources.iter().enumerate() {
                    if ptr::eq(type_, Rc::as_ptr(&source)) {
                        return if let Some(targets) = targets {
                            targets[i].clone()
                        } else {
                            self.any_type()
                        };
                    }
                }
                type_.type_wrapper()
            }
            TypeMapper::Function(mapper) => (mapper.func)(self, type_),
            TypeMapper::Composite(composite_or_merged_mapper)
            | TypeMapper::Merged(composite_or_merged_mapper) => {
                let t1 = self.get_mapped_type(type_, &composite_or_merged_mapper.mapper1);
                if !ptr::eq(Rc::as_ptr(&t1), type_) && matches!(mapper, TypeMapper::Composite(_)) {
                    self.instantiate_type(Some(t1), Some(&composite_or_merged_mapper.mapper2))
                        .unwrap()
                } else {
                    self.get_mapped_type(&t1, &composite_or_merged_mapper.mapper2)
                }
            }
        }
    }

    pub(super) fn make_unary_type_mapper(&self, source: &Type, target: &Type) -> TypeMapper {
        TypeMapper::new_simple(source.type_wrapper(), target.type_wrapper())
    }

    pub(super) fn make_array_type_mapper(
        &self,
        sources: Vec<Rc<Type /*TypeParameter*/>>,
        targets: Option<Vec<Rc<Type>>>,
    ) -> TypeMapper {
        TypeMapper::new_array(sources, targets)
    }

    pub(super) fn make_composite_type_mapper(
        &self,
        mapper1: TypeMapper,
        mapper2: TypeMapper,
    ) -> TypeMapper {
        TypeMapper::new_composite(mapper1, mapper2)
    }

    pub(super) fn make_merged_type_mapper(
        &self,
        mapper1: TypeMapper,
        mapper2: TypeMapper,
    ) -> TypeMapper {
        TypeMapper::new_merged(mapper1, mapper2)
    }

    pub(super) fn combine_type_mappers(
        &self,
        mapper1: Option<TypeMapper>,
        mapper2: TypeMapper,
    ) -> TypeMapper {
        if let Some(mapper1) = mapper1 {
            self.make_composite_type_mapper(mapper1, mapper2)
        } else {
            mapper2
        }
    }

    pub(super) fn merge_type_mappers(
        &self,
        mapper1: Option<TypeMapper>,
        mapper2: TypeMapper,
    ) -> TypeMapper {
        if let Some(mapper1) = mapper1 {
            self.make_merged_type_mapper(mapper1, mapper2)
        } else {
            mapper2
        }
    }

    pub(super) fn instantiate_symbol(&self, symbol: &Symbol, mapper: &TypeMapper) -> Rc<Symbol> {
        let mut symbol = symbol.symbol_wrapper();
        let links = self.get_symbol_links(&symbol);
        let links = (*links).borrow();
        if let Some(type_) = links.type_.as_ref() {
            if !self.could_contain_type_variables(&type_) {
                return symbol;
            }
        }
        let mut mapper = (*mapper).clone();
        if get_check_flags(&symbol).intersects(CheckFlags::Instantiated) {
            symbol = links.target.clone().unwrap();
            mapper = self.combine_type_mappers(links.mapper.clone(), mapper);
        }
        let result = self.create_symbol(
            symbol.flags(),
            symbol.escaped_name().clone(),
            Some(
                CheckFlags::Instantiated
                    | get_check_flags(&*symbol)
                        & (CheckFlags::Readonly
                            | CheckFlags::Late
                            | CheckFlags::OptionalParameter
                            | CheckFlags::RestParameter),
            ),
        );
        if let Some(declarations) = &*symbol.maybe_declarations() {
            result.set_declarations(declarations.clone());
        }
        let symbol_links = result.symbol_links();
        let mut symbol_links_ref = symbol_links.borrow_mut();
        symbol_links_ref.target = Some(symbol.clone());
        symbol_links_ref.mapper = Some(mapper);
        if let Some(value_declaration) = &*symbol.maybe_value_declaration() {
            result.set_value_declaration(value_declaration.upgrade().unwrap());
        }
        Rc::new(result.into())
    }

    pub(super) fn instantiate_type<TTypeRef: Borrow<Type>>(
        &self,
        type_: Option<TTypeRef>,
        mapper: Option<&TypeMapper>,
    ) -> Option<Rc<Type>> {
        if let Some(type_) = type_.as_ref() {
            if let Some(mapper) = mapper {
                return Some(self.instantiate_type_with_alias(
                    type_.borrow(),
                    mapper,
                    Option::<&Symbol>::None,
                    None,
                ));
            }
        }
        type_.map(|type_| type_.borrow().type_wrapper())
    }

    pub(super) fn instantiate_type_with_alias<TSymbolRef: Borrow<Symbol>>(
        &self,
        type_: &Type,
        mapper: &TypeMapper,
        alias_symbol: Option<TSymbolRef>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let result =
            self.instantiate_type_worker(type_, mapper, alias_symbol, alias_type_arguments);
        result
    }

    pub(super) fn instantiate_type_worker<TSymbolRef: Borrow<Symbol>>(
        &self,
        type_: &Type,
        mapper: &TypeMapper,
        alias_symbol: Option<TSymbolRef>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::TypeParameter) {
            return self.get_mapped_type(type_, mapper);
        }
        unimplemented!()
    }

    pub(super) fn is_context_sensitive(
        &self,
        node: &Node, /*Expression | MethodDeclaration | ObjectLiteralElementLike | JsxAttributeLike | JsxChild*/
    ) -> bool {
        // match node {
        // }
        false
    }

    pub(super) fn is_type_assignable_to(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, &self.assignable_relation)
    }

    pub(super) fn check_type_assignable_to_and_optionally_elaborate(
        &self,
        source: &Type,
        target: &Type,
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

    pub(super) fn check_type_related_to_and_optionally_elaborate(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        error_node: Option<&Node>,
        expr: Option<&Expression>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        if self.is_type_related_to(source, target, relation) {
            return true;
        }
        if error_node.is_none()
            || !self.elaborate_error(expr, source, target, relation, head_message.clone())
        {
            return self.check_type_related_to(source, target, relation, error_node, head_message);
        }
        false
    }

    pub(super) fn elaborate_error(
        &self,
        node: Option<&Expression>,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        if node.is_none() || false {
            return false;
        }
        let node = node.unwrap();
        match node {
            Expression::ObjectLiteralExpression(object_literal_expression) => {
                return self.elaborate_object_literal(
                    object_literal_expression,
                    source,
                    target,
                    relation,
                );
            }
            _ => (),
        }
        false
    }

    pub(super) fn get_best_match_indexed_access_type_or_undefined(
        &self,
        source: &Type,
        target: &Type,
        name_type: &Type,
    ) -> Option<Rc<Type>> {
        let idx = self.get_indexed_access_type_or_undefined(target, name_type);
        if idx.is_some() {
            return idx;
        }
        if target.flags().intersects(TypeFlags::Union) {
            unimplemented!()
        }
        None
    }

    pub(super) fn check_expression_for_mutable_location_with_contextual_type(
        &self,
        next: &Node, /*Expression*/
        source_prop_type: &Type,
    ) -> Rc<Type> {
        self.check_expression_for_mutable_location(
            match next {
                Node::Expression(expression) => expression,
                _ => panic!("Expected Expression"),
            },
            Some(CheckMode::Contextual),
            Some(source_prop_type),
        )
    }

    pub(super) fn elaborate_elementwise(
        &self,
        iterator: Vec<ElaborationIteratorItem>,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> bool {
        let mut reported_error = false;
        for status in iterator {
            let ElaborationIteratorItem {
                error_node: prop,
                inner_expression: next,
                name_type,
                error_message,
            } = status;
            let target_prop_type =
                self.get_best_match_indexed_access_type_or_undefined(source, target, &name_type);
            if target_prop_type.is_none() {
                continue;
            }
            let target_prop_type = target_prop_type.unwrap();
            if target_prop_type
                .flags()
                .intersects(TypeFlags::IndexedAccess)
            {
                continue;
            }
            let source_prop_type = self.get_indexed_access_type_or_undefined(source, &name_type);
            if source_prop_type.is_none() {
                continue;
            }
            let source_prop_type = source_prop_type.unwrap();
            if !self.check_type_related_to(
                &source_prop_type,
                &target_prop_type,
                relation,
                None,
                None,
            ) {
                reported_error = true;
                if true {
                    let specific_source = if let Some(next) = next {
                        self.check_expression_for_mutable_location_with_contextual_type(
                            &*next,
                            &source_prop_type,
                        )
                    } else {
                        source_prop_type
                    };
                    if false {
                        unimplemented!()
                    } else {
                        let result = self.check_type_related_to(
                            &specific_source,
                            &target_prop_type,
                            relation,
                            Some(&*prop),
                            error_message,
                        );
                    }
                }
            }
        }
        reported_error
    }

    pub(super) fn generate_object_literal_elements(
        &self,
        node: &ObjectLiteralExpression,
        // ) -> impl Iterator<Item = ElaborationIteratorItem> {
    ) -> Vec<ElaborationIteratorItem> {
        // if node.properties.is_empty() {
        //     return vec![];
        // }
        node.properties
            .iter()
            .flat_map(|prop| {
                let type_ = self.get_literal_type_from_property(
                    &self.get_symbol_of_node(&**prop).unwrap(),
                    TypeFlags::StringOrNumberLiteralOrUnique,
                    None,
                );
                if type_.flags().intersects(TypeFlags::Never) {
                    return vec![];
                }
                match &**prop {
                    Node::PropertyAssignment(property_assignment) => {
                        vec![ElaborationIteratorItem {
                            error_node: property_assignment.name(),
                            inner_expression: Some(property_assignment.initializer.clone()),
                            name_type: type_,
                            error_message: if false { unimplemented!() } else { None },
                        }]
                    }
                    _ => Debug_.assert_never(prop, None),
                }
            })
            .collect()
    }

    pub(super) fn elaborate_object_literal(
        &self,
        node: &ObjectLiteralExpression,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> bool {
        if target.flags().intersects(TypeFlags::Primitive) {
            return false;
        }
        self.elaborate_elementwise(
            self.generate_object_literal_elements(node),
            source,
            target,
            relation,
        )
    }

    pub(super) fn is_simple_type_related_to(
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

    pub(super) fn is_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> bool {
        let mut source = source.type_wrapper();
        if self.is_fresh_literal_type(&source) {
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
        let mut target = target.type_wrapper();
        if self.is_fresh_literal_type(&target) {
            target = match &*target {
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
        if Rc::ptr_eq(&source, &target) {
            return true;
        }
        if true {
            if self.is_simple_type_related_to(&source, &target, relation, None) {
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
            return self.check_type_related_to(&source, &target, relation, None, None);
        }
        false
    }

    pub(super) fn get_normalized_type(&self, type_: &Type) -> Rc<Type> {
        let mut type_ = type_.type_wrapper();
        loop {
            let t: Rc<Type> = if self.is_fresh_literal_type(&type_) {
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
                type_.type_wrapper()
            };
            if Rc::ptr_eq(&t, &type_) {
                break;
            }
            type_ = t.clone();
        }
        type_
    }

    pub(super) fn check_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        error_node: Option<&Node>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        CheckTypeRelatedTo::new(self, source, target, relation, error_node, head_message).call()
    }
}

#[derive(Debug)]
pub(super) struct ElaborationIteratorItem {
    pub error_node: Rc<Node>,
    pub inner_expression: Option<Rc<Node /*Expression*/>>,
    name_type: Rc<Type>,
    error_message: Option<DiagnosticMessage>,
}

type ErrorReporter<'a> = &'a dyn FnMut(DiagnosticMessage, Option<Vec<String>>);
