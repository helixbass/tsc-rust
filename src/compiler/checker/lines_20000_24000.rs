#![allow(non_upper_case_globals)]

use std::rc::Rc;

use crate::{
    every, for_each, get_object_flags, Debug_, DiagnosticMessage, Diagnostics, Expression, Node,
    ObjectFlags, Type, TypeChecker, TypeFlags, TypeInterface, UnionReduction,
};

impl TypeChecker {
    pub(super) fn type_could_have_top_level_singleton_types(&self, type_: &Type) -> bool {
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

    pub(super) fn is_unit_type(&self, type_: &Type) -> bool {
        type_.flags().intersects(TypeFlags::Unit)
    }

    pub(super) fn is_literal_type(&self, type_: &Type) -> bool {
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

    pub(super) fn get_base_type_of_literal_type(&self, type_: Rc<Type>) -> Rc<Type> {
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

    pub(super) fn get_widened_literal_type(&self, type_: Rc<Type>) -> Rc<Type> {
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

    pub(super) fn get_widened_unique_es_symbol_type(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    pub(super) fn get_widened_literal_like_type_for_contextual_type(
        &self,
        mut type_: Rc<Type>,
        contextual_type: Option<Rc<Type>>,
    ) -> Rc<Type> {
        if !self.is_literal_of_contextual_type(type_.clone(), contextual_type) {
            type_ = self.get_widened_unique_es_symbol_type(self.get_widened_literal_type(type_));
        }
        type_
    }

    pub(super) fn get_optional_type(&self, type_: Rc<Type>, is_property: Option<bool>) -> Rc<Type> {
        let is_property = is_property.unwrap_or(false);
        Debug_.assert(self.strict_null_checks, None);
        if type_.flags().intersects(TypeFlags::Undefined) {
            type_
        } else {
            unimplemented!()
        }
    }

    pub(super) fn remove_missing_type(&self, type_: Rc<Type>, is_optional: bool) -> Rc<Type> {
        if self.exact_optional_property_types && is_optional {
            unimplemented!()
        } else {
            type_
        }
    }

    pub(super) fn get_regular_type_of_object_literal(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    pub(super) fn get_widened_type(&self, type_: Rc<Type>) -> Rc<Type> {
        self.get_widened_type_with_context(type_)
    }

    pub(super) fn get_widened_type_with_context(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    pub(super) fn could_contain_type_variables(&self, type_: Rc<Type>) -> bool {
        unimplemented!()
    }

    pub(super) fn is_object_literal_type(&self, type_: Rc<Type>) -> bool {
        get_object_flags(&*type_).intersects(ObjectFlags::ObjectLiteral)
    }

    pub(super) fn get_cannot_find_name_diagnostic_for_name(
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

    pub(super) fn filter_type(
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

    pub(super) fn map_type<TMapper: FnMut(Rc<Type>) -> Option<Rc<Type>>>(
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

    pub(super) fn get_constituent_count(&self, type_: Rc<Type>) -> usize {
        if type_.flags().intersects(TypeFlags::Union) {
            type_.as_union_or_intersection_type().types().len()
        } else {
            1
        }
    }
}
