#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use super::{intrinsic_type_kinds, IntrinsicTypeKind};
use crate::{
    capitalize, uncapitalize, AccessFlags, Node, Symbol, SymbolInterface, TemplateLiteralType,
    Type, TypeChecker, __String, pseudo_big_int_to_string, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_template_string_for_type(&self, type_: &Type) -> Option<String> {
        if type_.flags().intersects(TypeFlags::StringLiteral) {
            Some(type_.as_string_literal_type().value.clone())
        } else if type_.flags().intersects(TypeFlags::NumberLiteral) {
            Some(type_.as_number_literal_type().value.to_string())
        } else if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            Some(pseudo_big_int_to_string(
                &type_.as_big_int_literal_type().value,
            ))
        } else if type_
            .flags()
            .intersects(TypeFlags::BooleanLiteral | TypeFlags::Nullable)
        {
            Some(type_.as_intrinsic_type().intrinsic_name().to_owned())
        } else {
            None
        }
    }

    pub(super) fn create_template_literal_type(
        &self,
        texts: Vec<String>,
        types: Vec<Rc<Type>>,
    ) -> Rc<Type> {
        let type_ = self.create_type(TypeFlags::TemplateLiteral);
        let type_: Rc<Type> = TemplateLiteralType::new(type_, texts, types).into();
        type_
    }

    pub(super) fn get_string_mapping_type(&self, symbol: &Symbol, type_: &Type) -> Rc<Type> {
        if type_
            .flags()
            .intersects(TypeFlags::Union | TypeFlags::Never)
        {
            self.map_type(
                type_,
                &mut |t| Some(self.get_string_mapping_type(symbol, t)),
                None,
            )
            .unwrap()
        } else if self.is_generic_index_type(type_) {
            self.get_string_mapping_type_for_generic_type(symbol, type_)
        } else if type_.flags().intersects(TypeFlags::StringLiteral) {
            self.get_string_literal_type(
                &self.apply_string_mapping(symbol, &type_.as_string_literal_type().value),
            )
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn apply_string_mapping(&self, symbol: &Symbol, str_: &str) -> String {
        match intrinsic_type_kinds.get(&**symbol.escaped_name()) {
            Some(IntrinsicTypeKind::Uppercase) => str_.to_uppercase(),
            Some(IntrinsicTypeKind::Lowercase) => str_.to_lowercase(),
            Some(IntrinsicTypeKind::Capitalize) => capitalize(str_),
            Some(IntrinsicTypeKind::Uncapitalize) => uncapitalize(str_),
            _ => str_.to_owned(),
        }
    }

    pub(super) fn get_string_mapping_type_for_generic_type(
        &self,
        symbol: &Symbol,
        type_: &Type,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_property_name_from_index(&self, index_type: &Type) -> Option<__String> {
        if self.is_type_usable_as_property_name(index_type) {
            Some(self.get_property_name_from_type(index_type))
        } else {
            unimplemented!()
        }
    }

    pub(super) fn get_property_type_for_index_type(
        &self,
        original_object_type: &Type,
        object_type: &Type,
        index_type: &Type,
        full_index_type: &Type,
    ) -> Option<Rc<Type>> {
        let prop_name = if false {
            unimplemented!()
        } else {
            self.get_property_name_from_index(index_type)
        };
        if let Some(prop_name) = prop_name {
            let prop = self.get_property_of_type_(object_type, &prop_name, None);
            if let Some(prop) = prop {
                let prop_type = self.get_type_of_symbol(&*prop);
                return if false {
                    unimplemented!()
                } else {
                    Some(prop_type)
                };
            }
        }
        None
    }

    pub(super) fn is_pattern_literal_placeholder_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_pattern_literal_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_generic_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_generic_object_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_generic_index_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_this_type_parameter(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_simplified_type(&self, type_: &Type, writing: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_indexed_access_type<
        TAccessNode: Borrow<Node>,
        TAliasSymbol: Borrow<Symbol>,
    >(
        &self,
        object_type: &Type,
        index_type: &Type,
        access_flags: Option<AccessFlags>,
        access_node: Option<
            TAccessNode, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let access_flags = access_flags.unwrap_or(AccessFlags::None);
        unimplemented!()
    }

    pub(super) fn get_indexed_access_type_or_undefined<
        TAccessNode: Borrow<Node>,
        TAliasSymbol: Borrow<Symbol>,
    >(
        &self,
        object_type: &Type,
        index_type: &Type,
        access_flags: Option<AccessFlags>,
        access_node: Option<
            TAccessNode, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Option<Rc<Type>> {
        let access_flags = access_flags.unwrap_or(AccessFlags::None);
        let apparent_object_type = self.get_reduced_apparent_type(object_type);
        self.get_property_type_for_index_type(
            object_type,
            &apparent_object_type,
            index_type,
            index_type,
        )
    }

    pub(super) fn get_type_from_mapped_type_node(
        &self,
        node: &Node, /*MappedTypeNode*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_actual_type_variable(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_true_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_false_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_inferred_true_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_infer_type_parameters(
        &self,
        node: &Node, /*ConditionalTypeNode*/
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        unimplemented!()
    }

    pub(super) fn get_alias_symbol_for_type_node(&self, node: &Node) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_arguments_for_alias_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }
}
