#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

use super::{CheckTypeRelatedTo, ExpandingFlags, IntersectionState, RecursionFlags};
use crate::{
    TypeNode, UnionOrIntersectionType, UnionOrIntersectionTypeInterface, UnionType,
    VariableDeclaration, VariableStatement, __String, bind_source_file, chain_diagnostic_messages,
    create_diagnostic_collection, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, create_printer, create_symbol_table,
    create_text_writer, declaration_name_to_string, escape_leading_underscores, every, factory,
    first_defined, first_or_undefined, for_each, get_effective_initializer,
    get_effective_type_annotation_node, get_first_identifier, get_name_of_declaration,
    get_object_flags, get_source_file_of_node, get_synthetic_factory, has_dynamic_name,
    has_initializer, is_binding_element, is_external_or_common_js_module, is_identifier_text,
    is_object_literal_expression, is_private_identifier, is_property_assignment,
    is_property_declaration, is_property_signature, is_variable_declaration, node_is_missing,
    object_allocator, unescape_leading_underscores, using_single_line_string_writer, ArrayTypeNode,
    BaseInterfaceType, BaseIntrinsicType, BaseLiteralType, BaseNodeFactorySynthetic,
    BaseObjectType, BaseType, BaseUnionOrIntersectionType, CharacterCodes, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, EmitHint,
    EmitTextWriter, Expression, ExpressionStatement, FreshableIntrinsicType,
    HasExpressionInitializerInterface, InterfaceDeclaration, InterfaceType, IntrinsicType,
    KeywordTypeNode, LiteralLikeNode, LiteralLikeNodeInterface, LiteralType, LiteralTypeInterface,
    NamedDeclarationInterface, Node, NodeInterface, Number, NumberLiteralType, NumericLiteral,
    ObjectFlags, ObjectFlagsTypeInterface, ObjectLiteralExpression, PrefixUnaryExpression,
    PrinterOptions, PropertyAssignment, PropertySignature, RelationComparisonResult,
    ResolvableTypeInterface, ResolvedTypeInterface, SourceFile, Statement, StringLiteralType,
    Symbol, SymbolFlags, SymbolFormatFlags, SymbolTable, SymbolTracker, SyntaxKind, Ternary, Type,
    TypeChecker, TypeCheckerHost, TypeElement, TypeFlags, TypeInterface, TypeReferenceNode,
    UnionReduction, VariableLikeDeclarationInterface,
};

impl TypeChecker {
    // pub fn create_literal_type(
    pub fn create_string_literal_type(
        &self,
        flags: TypeFlags,
        value: String,
        regular_type: Option<Rc<Type>>,
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = Rc::new(StringLiteralType::new(type_, value).into());
        match &*type_ {
            Type::LiteralType(literal_type) => {
                literal_type.set_regular_type(&regular_type.unwrap_or_else(|| type_.clone()));
            }
            _ => panic!("Expected LiteralType"),
        }
        type_
    }

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

    pub(crate) fn get_fresh_type_of_literal_type(&self, type_: Rc<Type>) -> Rc<Type> {
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

    pub(crate) fn is_fresh_literal_type(&self, type_: Rc<Type>) -> bool {
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

    pub(crate) fn get_string_literal_type(&self, value: &str) -> Rc<Type> {
        let mut string_literal_types = self.string_literal_types();
        if string_literal_types.contains_key(value) {
            return string_literal_types.get(value).unwrap().clone();
        }
        let type_ =
            self.create_string_literal_type(TypeFlags::StringLiteral, value.to_string(), None);
        string_literal_types.insert(value.to_string(), type_.clone());
        type_
    }

    pub(crate) fn get_number_literal_type(&self, value: Number) -> Rc<Type> {
        let mut number_literal_types = self.number_literal_types();
        if number_literal_types.contains_key(&value) {
            return number_literal_types.get(&value).unwrap().clone();
        }
        let type_ = self.create_number_literal_type(TypeFlags::NumberLiteral, value, None);
        number_literal_types.insert(value, type_.clone());
        type_
    }

    pub(crate) fn get_array_element_type_node(
        &self,
        node: &ArrayTypeNode,
    ) -> Option<Rc<Node /*TypeNode*/>> {
        Some(node.element_type.clone())
    }

    pub(crate) fn get_type_from_type_node(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
        self.get_conditional_flow_type_of_type(self.get_type_from_type_node_worker(node), node)
    }

    pub(crate) fn get_type_from_type_node_worker(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
        let node = match node {
            Node::TypeNode(type_node) => type_node,
            _ => panic!("Expected TypeNode"),
        };
        match node {
            TypeNode::KeywordTypeNode(_) => match node.kind() {
                SyntaxKind::NumberKeyword => self.number_type(),
                _ => unimplemented!(),
            },
            TypeNode::TypeReferenceNode(type_reference_node) => {
                self.get_type_from_type_reference(type_reference_node)
            }
            TypeNode::ArrayTypeNode(array_type_node) => {
                self.get_type_from_array_or_tuple_type_node(array_type_node)
            }
            _ => unimplemented!(),
        }
    }

    pub(crate) fn is_type_assignable_to(&self, source: Rc<Type>, target: Rc<Type>) -> bool {
        self.is_type_related_to(source, target, &self.assignable_relation)
    }

    pub(crate) fn check_type_assignable_to_and_optionally_elaborate(
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

    pub(crate) fn check_type_related_to_and_optionally_elaborate(
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
        if error_node.is_none()
            || !self.elaborate_error(
                expr,
                source.clone(),
                target.clone(),
                relation,
                head_message.clone(),
            )
        {
            return self.check_type_related_to(source, target, relation, error_node, head_message);
        }
        false
    }

    pub(crate) fn elaborate_error(
        &self,
        node: Option<&Expression>,
        source: Rc<Type>,
        target: Rc<Type>,
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

    pub(crate) fn get_best_match_indexed_access_type_or_undefined(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        name_type: Rc<Type>,
    ) -> Option<Rc<Type>> {
        let idx = self.get_indexed_access_type_or_undefined(target.clone(), name_type);
        if idx.is_some() {
            return idx;
        }
        if target.flags().intersects(TypeFlags::Union) {
            unimplemented!()
        }
        None
    }

    pub(crate) fn check_expression_for_mutable_location_with_contextual_type(
        &self,
        next: &Node, /*Expression*/
        source_prop_type: Rc<Type>,
    ) -> Rc<Type> {
        self.check_expression_for_mutable_location(
            match next {
                Node::Expression(expression) => expression,
                _ => panic!("Expected Expression"),
            },
            Some(source_prop_type),
        )
    }

    pub(crate) fn elaborate_elementwise(
        &self,
        iterator: Vec<ElaborationIteratorItem>,
        source: Rc<Type>,
        target: Rc<Type>,
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
            let target_prop_type = self.get_best_match_indexed_access_type_or_undefined(
                source.clone(),
                target.clone(),
                name_type.clone(),
            );
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
            let source_prop_type =
                self.get_indexed_access_type_or_undefined(source.clone(), name_type);
            if source_prop_type.is_none() {
                continue;
            }
            let source_prop_type = source_prop_type.unwrap();
            if !self.check_type_related_to(
                source_prop_type.clone(),
                target_prop_type.clone(),
                relation,
                None,
                None,
            ) {
                reported_error = true;
                if true {
                    let specific_source = if let Some(next) = next {
                        self.check_expression_for_mutable_location_with_contextual_type(
                            &*next,
                            source_prop_type,
                        )
                    } else {
                        source_prop_type
                    };
                    if false {
                        unimplemented!()
                    } else {
                        let result = self.check_type_related_to(
                            specific_source,
                            target_prop_type,
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

    pub(crate) fn generate_object_literal_elements(
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
                    self.get_symbol_of_node(&**prop).unwrap(),
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

    pub(crate) fn elaborate_object_literal(
        &self,
        node: &ObjectLiteralExpression,
        source: Rc<Type>,
        target: Rc<Type>,
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

    pub(crate) fn is_simple_type_related_to(
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

    pub(crate) fn is_type_related_to(
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

    pub(crate) fn get_normalized_type(&self, mut type_: Rc<Type>) -> Rc<Type> {
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

    pub(crate) fn check_type_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
        error_node: Option<&Node>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        CheckTypeRelatedTo::new(self, source, target, relation, error_node, head_message).call()
    }
}

#[derive(Debug)]
pub(crate) struct ElaborationIteratorItem {
    pub error_node: Rc<Node>,
    pub inner_expression: Option<Rc<Node /*Expression*/>>,
    name_type: Rc<Type>,
    error_message: Option<DiagnosticMessage>,
}

type ErrorReporter<'a> = &'a dyn FnMut(DiagnosticMessage, Option<Vec<String>>);
