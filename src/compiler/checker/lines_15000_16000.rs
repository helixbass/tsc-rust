#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{get_symbol_id, intrinsic_type_kinds, IntrinsicTypeKind};
use crate::{
    capitalize, every, find_ancestor, get_combined_node_flags, get_object_flags,
    get_property_name_for_property_name_node, is_access_expression, is_call_like_expression,
    is_call_or_new_expression, is_function_like, is_identifier, is_property_name, maybe_every,
    some, uncapitalize, AccessFlags, IndexedAccessType, Node, NodeFlags, NodeInterface,
    ObjectFlags, StringMappingType, Symbol, SymbolFlags, SymbolInterface, TemplateLiteralType,
    Type, TypeChecker, UnionOrIntersectionTypeInterface, __String, pseudo_big_int_to_string,
    TypeFlags, TypeInterface,
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
        let id = format!("{},{}", get_symbol_id(symbol), self.get_type_id(type_));
        let mut result = self.string_mapping_types().get(&id).map(Clone::clone);
        if result.is_none() {
            result = Some(self.create_string_mapping_type(symbol, type_));
            self.string_mapping_types()
                .insert(id, result.clone().unwrap());
        }
        result.unwrap()
    }

    pub(super) fn create_string_mapping_type(&self, symbol: &Symbol, type_: &Type) -> Rc<Type> {
        let result = self.create_type(TypeFlags::StringMapping);
        let result: Rc<Type> =
            StringMappingType::new(result, symbol.symbol_wrapper(), type_.type_wrapper()).into();
        result
    }

    pub(super) fn create_indexed_access_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        object_type: &Type,
        index_type: &Type,
        access_flags: AccessFlags,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let type_ = self.create_type(TypeFlags::IndexedAccess);
        let type_: Rc<Type> = IndexedAccessType::new(
            type_,
            object_type.type_wrapper(),
            index_type.type_wrapper(),
            access_flags,
        )
        .into();
        *type_.maybe_alias_symbol() =
            alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
        *type_.maybe_alias_type_arguments() = alias_type_arguments.map(ToOwned::to_owned);
        type_
    }

    pub(super) fn is_js_literal_type(&self, type_: &Type) -> bool {
        if self.no_implicit_any {
            return false;
        }
        if get_object_flags(type_).intersects(ObjectFlags::JSLiteral) {
            return true;
        }
        if type_.flags().intersects(TypeFlags::Union) {
            return every(type_.as_union_type().types(), |type_: &Rc<Type>, _| {
                self.is_js_literal_type(type_)
            });
        }
        if type_.flags().intersects(TypeFlags::Intersection) {
            return some(
                Some(type_.as_intersection_type().types()),
                Some(|type_: &Rc<Type>| self.is_js_literal_type(type_)),
            );
        }
        if type_.flags().intersects(TypeFlags::Instantiable) {
            let constraint = self.get_resolved_base_constraint(type_);
            return !ptr::eq(&*constraint, type_) && self.is_js_literal_type(&constraint);
        }
        false
    }

    pub(super) fn get_property_name_from_index<TAccessNode: Borrow<Node>>(
        &self,
        index_type: &Type,
        access_node: Option<
            TAccessNode, /*StringLiteral | Identifier | PrivateIdentifier | ObjectBindingPattern | ArrayBindingPattern | ComputedPropertyName | NumericLiteral | IndexedAccessTypeNode | ElementAccessExpression | SyntheticExpression*/
        >,
    ) -> Option<__String> {
        if self.is_type_usable_as_property_name(index_type) {
            return Some(self.get_property_name_from_type(index_type));
        }
        let access_node = access_node.map(|access_node| access_node.borrow().node_wrapper());
        access_node
            .filter(|access_node| is_property_name(access_node))
            .and_then(|access_node| get_property_name_for_property_name_node(&access_node))
    }

    pub(super) fn is_uncalled_function_reference(&self, node: &Node, symbol: &Symbol) -> bool {
        if symbol
            .flags()
            .intersects(SymbolFlags::Function | SymbolFlags::Method)
        {
            let parent = find_ancestor(node.maybe_parent(), |n| !is_access_expression(n))
                .unwrap_or_else(|| node.parent());
            if is_call_like_expression(&parent) {
                return is_call_or_new_expression(&parent)
                    && is_identifier(node)
                    && self.has_matching_argument(&parent, node);
            }
            return maybe_every(symbol.maybe_declarations().as_deref(), |d: &Rc<Node>, _| {
                !is_function_like(Some(&**d))
                    || get_combined_node_flags(d).intersects(NodeFlags::Deprecated)
            });
        }
        true
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
            self.get_property_name_from_index(
                index_type,
                Option::<&Node>::None, /*TODO: this is wrong*/
            )
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
