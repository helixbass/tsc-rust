#![allow(non_upper_case_globals)]

use std::cell::RefCell;
use std::rc::Rc;

use super::CheckMode;
use crate::{
    __String, create_symbol_table, get_effective_type_annotation_node, get_object_flags,
    has_initializer, is_object_literal_expression, Expression, HasExpressionInitializerInterface,
    Identifier, Node, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface,
    ObjectLiteralExpression, PropertyAssignment, Symbol, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_identifier(
        &self,
        node: &Identifier,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let symbol = self.get_resolved_symbol(node);

        let local_or_export_symbol = self
            .get_export_symbol_of_value_symbol_if_exported(Some(symbol))
            .unwrap();

        let type_ = self.get_type_of_symbol(&*local_or_export_symbol);

        type_
    }

    pub(super) fn get_contextual_type_for_variable_like_declaration(
        &self,
        declaration: &Node,
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(declaration);
        if let Some(type_node) = type_node {
            return Some(self.get_type_from_type_node(&*type_node));
        }
        match declaration.kind() {
            _ => None,
        }
    }

    pub(super) fn get_contextual_type_for_initializer_expression<TNode: NodeInterface>(
        &self,
        node: &TNode,
    ) -> Option<Rc<Type>> {
        let parent = node.parent();
        let declaration = match &*parent {
            Node::VariableDeclaration(variable_declaration) => variable_declaration,
            _ => panic!("Expected VariableDeclaration"),
        };
        if has_initializer(declaration)
            && Rc::ptr_eq(
                &node.node_wrapper(),
                &declaration.maybe_initializer().unwrap(),
            )
        {
            let result = self.get_contextual_type_for_variable_like_declaration(&*parent);
            if result.is_some() {
                return result;
            }
        }
        None
    }

    pub(super) fn get_type_of_property_of_contextual_type(
        &self,
        type_: Rc<Type>,
        name: &__String,
    ) -> Option<Rc<Type>> {
        self.map_type(
            type_,
            &mut |t| {
                if false {
                    unimplemented!()
                } else if t.flags().intersects(TypeFlags::StructuredType) {
                    let prop = self.get_property_of_type(t, name);
                    if let Some(prop) = prop {
                        return if false {
                            None
                        } else {
                            Some(self.get_type_of_symbol(&*prop))
                        };
                    }
                    return if let Some(found) = Option::<()>::None /*self.find_applicable_index_info(self.get_index_infos_of_structured_type(t), self.get_string_literal_type(unescape_leading_underscores(name)))*/ {
                        unimplemented!()
                    } else {
                        None
                    };
                }
                None
            },
            Some(true),
        )
    }

    pub(super) fn get_contextual_type_for_object_literal_element(
        &self,
        element: &PropertyAssignment,
    ) -> Option<Rc<Type>> {
        let parent = element.parent();
        let object_literal = match &*parent {
            Node::Expression(Expression::ObjectLiteralExpression(object_literal_expression)) => {
                object_literal_expression
            }
            _ => panic!("Expected ObjectLiteralExpression"),
        };
        // let property_assignment_type = if is_property_assignment(element) {
        // } else {
        //     None
        // };
        // if property_assignment_type.is_some() {
        //     return property_assignment_type;
        // }
        let type_ = self.get_apparent_type_of_contextual_type(object_literal);
        if let Some(type_) = type_ {
            if self.has_bindable_name(element) {
                return self.get_type_of_property_of_contextual_type(
                    type_,
                    self.get_symbol_of_node(element).unwrap().escaped_name(),
                );
            }
            unimplemented!()
        }
        None
    }

    pub(super) fn get_apparent_type_of_contextual_type<TNode: NodeInterface>(
        &self,
        node: &TNode, /*Expression | MethodDeclaration*/
    ) -> Option<Rc<Type>> {
        let contextual_type = if false {
            unimplemented!()
        } else {
            self.get_contextual_type(node)
        };
        let instantiated_type = self.instantiate_contextual_type(contextual_type, node);
        if let Some(instantiated_type) = instantiated_type {
            if true {
                let apparent_type = self
                    .map_type(
                        instantiated_type,
                        &mut |type_| Some(self.get_apparent_type(type_)),
                        Some(true),
                    )
                    .unwrap();
                return if apparent_type.flags().intersects(TypeFlags::Union)
                    && is_object_literal_expression(node)
                {
                    unimplemented!()
                } else if false {
                    unimplemented!()
                } else {
                    Some(apparent_type)
                };
            }
        }
        None
    }

    pub(super) fn instantiate_contextual_type<TNode: NodeInterface>(
        &self,
        contextual_type: Option<Rc<Type>>,
        node: &TNode,
    ) -> Option<Rc<Type>> {
        if false {
            unimplemented!()
        }
        contextual_type
    }

    pub(super) fn get_contextual_type<TNode: NodeInterface>(
        &self,
        node: &TNode, /*Expression*/
    ) -> Option<Rc<Type>> {
        let parent = node.parent();
        match &*parent {
            Node::VariableDeclaration(variable_declaration) => {
                self.get_contextual_type_for_initializer_expression(node)
            }
            Node::PropertyAssignment(property_assignment) => {
                self.get_contextual_type_for_object_literal_element(property_assignment)
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn check_object_literal(&self, node: &ObjectLiteralExpression) -> Rc<Type> {
        let mut properties_table = create_symbol_table();
        let mut properties_array: Vec<Rc<Symbol>> = vec![];

        let object_flags = self.fresh_object_literal_flag;

        for member_decl in &node.properties {
            let member = self.get_symbol_of_node(&**member_decl).unwrap();
            if member_decl.kind() == SyntaxKind::PropertyAssignment {
            } else {
                unimplemented!()
            }

            if false {
                unimplemented!()
            } else {
                properties_table.insert(member.escaped_name().clone(), member.clone());
            }
            properties_array.push(member);
        }

        let create_object_literal_type = || {
            let result =
                self.create_anonymous_type(node.symbol(), Rc::new(RefCell::new(properties_table)));
            result.set_object_flags(
                result.object_flags()
                    | object_flags
                    | ObjectFlags::ObjectLiteral
                    | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
            Rc::new(result.into())
        };

        create_object_literal_type()
    }

    pub(super) fn is_known_property(
        &self,
        target_type: Rc<Type>,
        name: &__String,
        is_comparing_jsx_attributes: bool,
    ) -> bool {
        if target_type.flags().intersects(TypeFlags::Object) {
            if self
                .get_property_of_object_type(target_type, name)
                .is_some()
                || false
            {
                return true;
            }
        } else if target_type
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
            && self.is_excess_property_check_target(target_type)
        {
            unimplemented!()
        }
        false
    }

    pub(super) fn is_excess_property_check_target(&self, type_: Rc<Type>) -> bool {
        (type_.flags().intersects(TypeFlags::Object)
            && !(get_object_flags(&*type_)
                .intersects(ObjectFlags::ObjectLiteralPatternWithComputedProperties)))
            || type_.flags().intersects(TypeFlags::NonPrimitive)
            || (type_.flags().intersects(TypeFlags::Union) && unimplemented!())
            || (type_.flags().intersects(TypeFlags::Intersection) && unimplemented!())
    }
}
