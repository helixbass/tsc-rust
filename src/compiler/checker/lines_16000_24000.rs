#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

use super::{ExpandingFlags, IntersectionState, RecursionFlags};
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

    pub(crate) fn type_could_have_top_level_singleton_types(&self, type_: &Type) -> bool {
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

    pub(crate) fn is_unit_type(&self, type_: &Type) -> bool {
        type_.flags().intersects(TypeFlags::Unit)
    }

    pub(crate) fn is_literal_type(&self, type_: &Type) -> bool {
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

    pub(crate) fn get_base_type_of_literal_type(&self, type_: Rc<Type>) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::EnumLiteral) {
            unimplemented!()
        } else if type_.flags().intersects(TypeFlags::StringLiteral) {
            unimplemented!()
        } else if type_.flags().intersects(TypeFlags::NumberLiteral) {
            self.number_type()
        } else if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            self.bigint_type()
        } else if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            self.boolean_type()
        } else if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.get_base_type_of_literal_type(type_)),
                None,
            )
            .unwrap()
        } else {
            type_
        }
    }

    pub(crate) fn get_widened_literal_type(&self, type_: Rc<Type>) -> Rc<Type> {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::EnumLiteral) && self.is_fresh_literal_type(type_.clone()) {
            unimplemented!()
        } else if flags.intersects(TypeFlags::StringLiteral)
            && self.is_fresh_literal_type(type_.clone())
        {
            unimplemented!()
        } else if flags.intersects(TypeFlags::NumberLiteral)
            && self.is_fresh_literal_type(type_.clone())
        {
            self.number_type()
        } else if flags.intersects(TypeFlags::BigIntLiteral)
            && self.is_fresh_literal_type(type_.clone())
        {
            self.bigint_type()
        } else if flags.intersects(TypeFlags::BooleanLiteral)
            && self.is_fresh_literal_type(type_.clone())
        {
            self.boolean_type()
        } else if flags.intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.get_widened_literal_type(type_)),
                None,
            )
            .unwrap()
        } else {
            type_
        }
    }

    pub(crate) fn get_widened_unique_es_symbol_type(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    pub(crate) fn get_widened_literal_like_type_for_contextual_type(
        &self,
        mut type_: Rc<Type>,
        contextual_type: Option<Rc<Type>>,
    ) -> Rc<Type> {
        if !self.is_literal_of_contextual_type(type_.clone(), contextual_type) {
            type_ = self.get_widened_unique_es_symbol_type(self.get_widened_literal_type(type_));
        }
        type_
    }

    pub(crate) fn get_optional_type(&self, type_: Rc<Type>, is_property: Option<bool>) -> Rc<Type> {
        let is_property = is_property.unwrap_or(false);
        Debug_.assert(self.strict_null_checks, None);
        if type_.flags().intersects(TypeFlags::Undefined) {
            type_
        } else {
            unimplemented!()
        }
    }

    pub(crate) fn remove_missing_type(&self, type_: Rc<Type>, is_optional: bool) -> Rc<Type> {
        if self.exact_optional_property_types && is_optional {
            unimplemented!()
        } else {
            type_
        }
    }

    pub(crate) fn get_regular_type_of_object_literal(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    pub(crate) fn get_widened_type(&self, type_: Rc<Type>) -> Rc<Type> {
        self.get_widened_type_with_context(type_)
    }

    pub(crate) fn get_widened_type_with_context(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    pub(crate) fn is_object_literal_type(&self, type_: Rc<Type>) -> bool {
        get_object_flags(&*type_).intersects(ObjectFlags::ObjectLiteral)
    }

    pub(crate) fn get_cannot_find_name_diagnostic_for_name(
        &self,
        node: &Node,
    ) -> DiagnosticMessage {
        match node {
            Node::Expression(Expression::Identifier(identifier)) => match identifier.escaped_text {
                _ => {
                    if false {
                        unimplemented!()
                    } else {
                        Diagnostics::Cannot_find_name_0
                    }
                }
            },
            _ => panic!("Expected Identifier"),
        }
    }

    pub(crate) fn filter_type(
        &self,
        type_: Rc<Type>,
        f: fn(&TypeChecker, Rc<Type>) -> bool,
    ) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            unimplemented!()
        }
        if type_.flags().intersects(TypeFlags::Never) || f(self, type_.clone()) {
            type_
        } else {
            self.never_type()
        }
    }

    pub(crate) fn map_type<TMapper: FnMut(Rc<Type>) -> Option<Rc<Type>>>(
        &self,
        type_: Rc<Type>,
        mapper: &mut TMapper,
        no_reductions: Option<bool>,
    ) -> Option<Rc<Type>> {
        let no_reductions = no_reductions.unwrap_or(false);
        if type_.flags().intersects(TypeFlags::Never) {
            return Some(type_);
        }
        if !type_.flags().intersects(TypeFlags::Union) {
            return mapper(type_);
        }
        let types = type_.as_union_or_intersection_type().types();
        let mut mapped_types: Vec<Rc<Type>> = vec![];
        let mut changed = false;
        for t in types {
            let mapped = if t.flags().intersects(TypeFlags::Union) {
                self.map_type(t.clone(), mapper, Some(no_reductions))
            } else {
                mapper(t.clone())
            };
            changed = changed
                || match mapped.as_ref() {
                    None => true,
                    Some(mapped) => !Rc::ptr_eq(t, mapped),
                };
            if let Some(mapped) = mapped {
                mapped_types.push(mapped);
            }
        }
        if changed {
            if !mapped_types.is_empty() {
                Some(self.get_union_type(
                    mapped_types,
                    Some(if no_reductions {
                        UnionReduction::None
                    } else {
                        UnionReduction::Literal
                    }),
                ))
            } else {
                None
            }
        } else {
            Some(type_)
        }
    }

    pub(crate) fn get_constituent_count(&self, type_: Rc<Type>) -> usize {
        if type_.flags().intersects(TypeFlags::Union) {
            type_.as_union_or_intersection_type().types().len()
        } else {
            1
        }
    }
}

struct CheckTypeRelatedTo<'type_checker> {
    type_checker: &'type_checker TypeChecker,
    source: Rc<Type>,
    target: Rc<Type>,
    relation: &'type_checker HashMap<String, RelationComparisonResult>,
    error_node: Option<&'type_checker Node>,
    head_message: Option<DiagnosticMessage>,
    error_info: RefCell<Option<DiagnosticMessageChain>>,
    expanding_flags: ExpandingFlags,
    incompatible_stack: RefCell<Vec<(DiagnosticMessage, Option<Vec<String>>)>>,
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
            expanding_flags: ExpandingFlags::None,
            incompatible_stack: RefCell::new(vec![]),
        }
    }

    fn error_info(&self) -> Ref<Option<DiagnosticMessageChain>> {
        self.error_info.borrow()
    }

    fn set_error_info(&self, error_info: DiagnosticMessageChain) {
        *self.error_info.borrow_mut() = Some(error_info);
    }

    fn incompatible_stack(&self) -> RefMut<Vec<(DiagnosticMessage, Option<Vec<String>>)>> {
        self.incompatible_stack.borrow_mut()
    }

    fn call(&self) -> bool {
        let result = self.is_related_to(
            self.source.clone(),
            self.target.clone(),
            Some(RecursionFlags::Both),
            self.error_node.is_some(),
            self.head_message.as_ref(),
            None,
        );

        if !self.incompatible_stack().is_empty() {
            self.report_incompatible_stack();
        } else if false {
            unimplemented!()
        } else if self.error_info().is_some() {
            let diag = create_diagnostic_for_node_from_message_chain(
                &*self.error_node.unwrap(),
                self.error_info().clone().unwrap(),
            );
            if true {
                self.type_checker.diagnostics().add(Rc::new(diag.into()));
            }
        }

        result != Ternary::False
    }

    fn report_incompatible_error(&self, message: DiagnosticMessage, args: Option<Vec<String>>) {
        self.incompatible_stack().push((message, args));
    }

    fn report_incompatible_stack(&self) {
        unimplemented!()
    }

    fn report_error(&self, message: &DiagnosticMessage, args: Option<Vec<String>>) {
        Debug_.assert(self.error_node.is_some(), None);
        let error_info = { chain_diagnostic_messages(self.error_info().clone(), message, args) };
        self.set_error_info(error_info);
    }

    fn report_relation_error(
        &self,
        mut message: Option<&DiagnosticMessage>,
        source: Rc<Type>,
        target: Rc<Type>,
    ) {
        let (source_type, target_type) = self
            .type_checker
            .get_type_names_for_error_display(source.clone(), target.clone());
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
                    .is_type_assignable_to(generalized_source.clone(), target),
                Some("generalized source shouldn't be assignable"),
            );
            generalized_source_type = self
                .type_checker
                .get_type_name_for_error_display(generalized_source);
        }

        if message.is_none() {
            if false {
            } else {
                message = Some(&Diagnostics::Type_0_is_not_assignable_to_type_1);
            }
        }

        self.report_error(
            message.unwrap(),
            Some(vec![generalized_source_type, target_type]),
        );
    }

    fn is_related_to(
        &self,
        original_source: Rc<Type>,
        original_target: Rc<Type>,
        recursion_flags: Option<RecursionFlags>,
        report_errors: bool,
        head_message: Option<&DiagnosticMessage>,
        intersection_state: Option<IntersectionState>,
    ) -> Ternary {
        let intersection_state = intersection_state.unwrap_or(IntersectionState::None);
        let recursion_flags = recursion_flags.unwrap_or(RecursionFlags::Both);

        let source = self.type_checker.get_normalized_type(original_source);
        let target = self
            .type_checker
            .get_normalized_type(original_target.clone());

        let report_error_results = |source, target, result| {
            if result == Ternary::False && report_errors {
                let source = source;
                let target = target;
                self.report_relation_error(head_message, source, target);
            }
        };

        let report_error = |message: DiagnosticMessage, args: Option<Vec<String>>| {
            self.report_error(&message, args);
        };

        if false
            || self.type_checker.is_simple_type_related_to(
                &*source,
                &*target,
                self.relation,
                if report_errors {
                    Some(&report_error)
                } else {
                    None
                },
            )
        {
            return Ternary::True;
        }

        let is_performing_excess_property_checks = !intersection_state
            .intersects(IntersectionState::Target)
            && (self.type_checker.is_object_literal_type(source.clone())
                && get_object_flags(&*source).intersects(ObjectFlags::FreshLiteral));
        if is_performing_excess_property_checks {
            if self.has_excess_properties(source.clone(), target.clone(), report_errors) {
                if report_errors {
                    self.report_relation_error(
                        head_message,
                        source,
                        if false { original_target } else { target },
                    );
                }
                return Ternary::False;
            }
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
                report_errors,
                intersection_state | IntersectionState::UnionIntersectionCheck,
            );
        }
        if result == Ternary::False
            && !(source.flags().intersects(TypeFlags::Union))
            && (source
                .flags()
                .intersects(TypeFlags::StructuredOrInstantiable)
                || source
                    .flags()
                    .intersects(TypeFlags::StructuredOrInstantiable))
        {
            result = self.recursive_type_related_to(
                source.clone(),
                target.clone(),
                report_errors,
                intersection_state,
                recursion_flags,
            );
        }

        report_error_results(source, target, result);

        result
    }

    fn has_excess_properties(
        &self,
        source: Rc<Type /*FreshObjectLiteralType*/>,
        target: Rc<Type>,
        report_errors: bool,
    ) -> bool {
        if !self
            .type_checker
            .is_excess_property_check_target(target.clone())
            || false
        {
            return false;
        }
        let is_comparing_jsx_attributes = false;
        let reduced_target = target.clone();
        for prop in self.type_checker.get_properties_of_type(source.clone()) {
            if self.should_check_as_excess_property(prop.clone(), source.symbol()) && true {
                if !self.type_checker.is_known_property(
                    reduced_target.clone(),
                    &prop.escaped_name,
                    is_comparing_jsx_attributes,
                ) {
                    if report_errors {
                        let error_target = self.type_checker.filter_type(
                            reduced_target,
                            TypeChecker::is_excess_property_check_target,
                        );
                        let error_node = match self.error_node {
                            None => Debug_.fail(None),
                            Some(error_node) => error_node,
                        };
                        if false {
                            unimplemented!()
                        } else {
                            let object_literal_declaration =
                                if let Some(symbol) = source.maybe_symbol() {
                                    if let Some(declarations) = &*symbol.maybe_declarations() {
                                        first_or_undefined(declarations).map(Clone::clone)
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                };
                            if false {
                                unimplemented!()
                            } else {
                                self.report_error(
                                    &Diagnostics::Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1,
                                    Some(vec![self.type_checker.symbol_to_string(prop, None, None, None, None), self.type_checker.type_to_string(error_target)])
                                );
                            }
                        }
                    }
                    return true;
                }
            }
        }
        false
    }

    fn should_check_as_excess_property(&self, prop: Rc<Symbol>, container: Rc<Symbol>) -> bool {
        if let Some(prop_value_declaration) = prop.maybe_value_declaration().as_ref() {
            if let Some(container_value_declaration) = container.maybe_value_declaration().as_ref()
            {
                return Rc::ptr_eq(
                    &prop_value_declaration.upgrade().unwrap().parent(),
                    &container_value_declaration.upgrade().unwrap(),
                );
            }
        }
        false
    }

    fn type_related_to_some_type(
        &self,
        source: Rc<Type>,
        target: &UnionOrIntersectionType,
    ) -> Ternary {
        let target_types = target.types();

        for type_ in target_types {
            let related = self.is_related_to(
                source.clone(),
                type_.clone(),
                Some(RecursionFlags::Target),
                false,
                None,
                None,
            );
            if related != Ternary::False {
                return related;
            }
        }

        Ternary::False
    }

    fn each_type_related_to_type(
        &self,
        source: &UnionOrIntersectionType,
        target: Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::True;
        let source_types = source.types();
        for source_type in source_types {
            let related = self.is_related_to(
                source_type.clone(),
                target.clone(),
                Some(RecursionFlags::Source),
                report_errors,
                None,
                Some(intersection_state),
            );
            if related == Ternary::False {
                return Ternary::False;
            }
            result &= related;
        }
        result
    }

    fn recursive_type_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
        recursion_flags: RecursionFlags,
    ) -> Ternary {
        let result = if self.expanding_flags != ExpandingFlags::Both {
            self.structured_type_related_to(source, target, report_errors, intersection_state)
        } else {
            Ternary::Maybe
        };
        result
    }

    fn structured_type_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let result = self.structured_type_related_to_worker(
            source,
            target,
            report_errors,
            intersection_state,
        );
        result
    }

    fn structured_type_related_to_worker(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if intersection_state.intersects(IntersectionState::UnionIntersectionCheck) {
            if source.flags().intersects(TypeFlags::Union) {
                return if false {
                    unimplemented!()
                } else {
                    self.each_type_related_to_type(
                        match &*source {
                            Type::UnionOrIntersectionType(union_or_intersection_type) => {
                                union_or_intersection_type
                            }
                            _ => panic!("Expected UnionOrIntersectionType"),
                        },
                        target,
                        report_errors,
                        intersection_state & !IntersectionState::UnionIntersectionCheck,
                    )
                };
            }
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

        let result: Ternary;

        if false {
            unimplemented!()
        } else {
            if source
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
                && target.flags().intersects(TypeFlags::Object)
            {
                let report_structural_errors = report_errors && true;
                result = self.properties_related_to(
                    source,
                    target,
                    report_structural_errors,
                    None,
                    intersection_state,
                );
                if false && result != Ternary::False {
                    unimplemented!()
                } else if result != Ternary::False {
                    return result;
                }
            }
        }

        Ternary::False
    }

    fn exclude_properties(
        &self,
        properties: Vec<Rc<Symbol>>,
        excluded_properties: Option<HashSet<__String>>,
    ) -> Vec<Rc<Symbol>> {
        properties
    }

    fn is_property_symbol_type_related(
        &self,
        source_prop: Rc<Symbol>,
        target_prop: Rc<Symbol>,
        get_type_of_source_property: fn(&TypeChecker, Rc<Symbol>) -> Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let target_is_optional = false;
        let effective_target = self.type_checker.add_optionality(
            self.type_checker
                .get_non_missing_type_of_symbol(target_prop),
            Some(false),
            Some(target_is_optional),
        );
        let effective_source = get_type_of_source_property(self.type_checker, source_prop);
        self.is_related_to(
            effective_source,
            effective_target,
            Some(RecursionFlags::Both),
            report_errors,
            None,
            Some(intersection_state),
        )
    }

    fn property_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        source_prop: Rc<Symbol>,
        target_prop: Rc<Symbol>,
        get_type_of_source_property: fn(&TypeChecker, Rc<Symbol>) -> Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
        skip_optional: bool,
    ) -> Ternary {
        let related = self.is_property_symbol_type_related(
            source_prop,
            target_prop.clone(),
            get_type_of_source_property,
            report_errors,
            intersection_state,
        );
        if related == Ternary::False {
            if report_errors {
                self.report_incompatible_error(
                    Diagnostics::Types_of_property_0_are_incompatible,
                    Some(vec![self.type_checker.symbol_to_string(
                        target_prop,
                        None,
                        None,
                        None,
                        None,
                    )]),
                );
            }
            return Ternary::False;
        }
        related
    }

    fn properties_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        report_errors: bool,
        excluded_properties: Option<HashSet<__String>>,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::True;
        let require_optional_properties = false;
        // let unmatched_property =
        //     self.get_unmatched_property(source, target, require_optional_properties, false);
        // if let Some(unmatched_property) = unmatched_property {
        //     if report_errors {
        //         self.report_unmatched_property(
        //             source,
        //             target,
        //             unmatched_property,
        //             require_optional_properties,
        //         );
        //     }
        //     return Ternary::False;
        // }
        let properties = self.type_checker.get_properties_of_type(target.clone());
        for target_prop in self.exclude_properties(properties, excluded_properties) {
            let name = &target_prop.escaped_name;
            if true {
                let source_prop = self.type_checker.get_property_of_type(source.clone(), name);
                if let Some(source_prop) = source_prop {
                    if !Rc::ptr_eq(&source_prop, &target_prop) {
                        let related = self.property_related_to(
                            source.clone(),
                            target.clone(),
                            source_prop,
                            target_prop,
                            TypeChecker::get_non_missing_type_of_symbol,
                            report_errors,
                            intersection_state,
                            true,
                        );
                        if related == Ternary::False {
                            return Ternary::False;
                        }
                        result &= related;
                    }
                }
            }
        }
        result
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
