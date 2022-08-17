#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, JsxNames, MinArgumentCountFlags, ResolveNameNameArg,
    TypeFacts, WideningKind,
};
use crate::{
    filter, is_function_expression_or_arrow_function, is_import_call, is_in_js_file,
    is_object_literal_method, length, unescape_leading_underscores, ContextFlags, Debug_,
    Diagnostics, FunctionFlags, InterfaceTypeInterface, JsxReferenceKind, NodeFlags, Signature,
    SignatureFlags, SignatureKind, StringOrRcNode, SymbolFlags, Ternary, UnionReduction, __String,
    create_symbol_table, get_function_flags, get_object_flags, has_initializer, InferenceContext,
    Node, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, Symbol, SymbolInterface,
    SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_jsx_managed_attributes_from_located_attributes(
        &self,
        context: &Node, /*JsxOpeningLikeElement*/
        ns: &Symbol,
        attributes_type: &Type,
    ) -> Rc<Type> {
        let managed_sym = self.get_jsx_library_managed_attributes(ns);
        if let Some(managed_sym) = managed_sym.as_ref() {
            let declared_managed_type = self.get_declared_type_of_symbol(managed_sym);
            let ctor_type = self.get_static_type_of_referenced_jsx_constructor(context);
            if managed_sym.flags().intersects(SymbolFlags::TypeAlias) {
                let params = (*self.get_symbol_links(managed_sym))
                    .borrow()
                    .type_parameters
                    .clone();
                if length(params.as_deref()) >= 2 {
                    let args = self.fill_missing_type_arguments(
                        Some(vec![ctor_type, attributes_type.type_wrapper()]),
                        params.as_deref(),
                        2,
                        is_in_js_file(Some(context)),
                    );
                    return self.get_type_alias_instantiation(
                        managed_sym,
                        args.as_deref(),
                        Option::<&Symbol>::None,
                        None,
                    );
                }
            }
            if length(
                declared_managed_type
                    .as_interface_type()
                    .maybe_type_parameters(),
            ) >= 2
            {
                let args = self.fill_missing_type_arguments(
                    Some(vec![ctor_type, attributes_type.type_wrapper()]),
                    declared_managed_type
                        .as_interface_type()
                        .maybe_type_parameters(),
                    2,
                    is_in_js_file(Some(context)),
                );
                return self.create_type_reference(&declared_managed_type, args);
            }
        }
        attributes_type.type_wrapper()
    }

    pub(super) fn get_jsx_props_type_from_class_type(
        &self,
        sig: Rc<Signature>,
        context: &Node, /*JsxOpeningLikeElement*/
    ) -> Rc<Type> {
        let ns = self.get_jsx_namespace_at(Some(context));
        let forced_lookup_location = self.get_jsx_element_properties_name(&ns);
        let attributes_type = match forced_lookup_location.as_ref() {
            None => Some(self.get_type_of_first_parameter_of_signature_with_fallback(
                &sig,
                &self.unknown_type(),
            )),
            Some(forced_lookup_location) => {
                if forced_lookup_location.is_empty() {
                    Some(self.get_return_type_of_signature(sig.clone()))
                } else {
                    self.get_jsx_props_type_for_signature_from_member(
                        sig.clone(),
                        forced_lookup_location,
                    )
                }
            }
        };

        if attributes_type.is_none() {
            if let Some(forced_lookup_location) = forced_lookup_location.as_ref() {
                if length(Some(
                    &*context
                        .as_jsx_opening_like_element()
                        .attributes()
                        .as_jsx_attributes()
                        .properties,
                )) > 0
                {
                    self.error(
                        Some(context),
                        &Diagnostics::JSX_element_class_does_not_support_attributes_because_it_does_not_have_a_0_property,
                        Some(vec![
                            unescape_leading_underscores(forced_lookup_location)
                        ])
                    );
                }
            }
            return self.unknown_type();
        }
        let mut attributes_type = attributes_type.unwrap();

        attributes_type =
            self.get_jsx_managed_attributes_from_located_attributes(context, &ns, &attributes_type);

        if self.is_type_any(Some(&*attributes_type)) {
            attributes_type
        } else {
            let mut apparent_attributes_type = attributes_type.clone();
            let intrinsic_class_attribs =
                self.get_jsx_type(&JsxNames::IntrinsicClassAttributes, Some(context));
            if !self.is_error_type(&intrinsic_class_attribs) {
                let type_params = self
                    .get_local_type_parameters_of_class_or_interface_or_type_alias(
                        &intrinsic_class_attribs.symbol(),
                    );
                let host_class_type = self.get_return_type_of_signature(sig.clone());
                apparent_attributes_type = self
                    .intersect_types(
                        Some(if let Some(type_params) = type_params.as_ref() {
                            self.create_type_reference(
                                &intrinsic_class_attribs,
                                self.fill_missing_type_arguments(
                                    Some(vec![host_class_type]),
                                    Some(type_params),
                                    self.get_min_type_argument_count(Some(type_params)),
                                    is_in_js_file(Some(context)),
                                ),
                            )
                        } else {
                            intrinsic_class_attribs
                        }),
                        Some(apparent_attributes_type),
                    )
                    .unwrap();
            }

            let intrinsic_attribs =
                self.get_jsx_type(&JsxNames::IntrinsicAttributes, Some(context));
            if !self.is_error_type(&intrinsic_attribs) {
                apparent_attributes_type = self
                    .intersect_types(Some(intrinsic_attribs), Some(apparent_attributes_type))
                    .unwrap();
            }

            apparent_attributes_type
        }
    }

    pub(super) fn get_intersected_signatures(
        &self,
        signatures: &[Rc<Signature>],
    ) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn get_contextual_call_signature(
        &self,
        type_: &Type,
        node: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Signature>> {
        let signatures = self.get_signatures_of_type(type_, SignatureKind::Call);
        let applicable_by_arity = filter(&signatures, |s| !self.is_arity_smaller(s, node));
        if applicable_by_arity.len() == 1 {
            Some(applicable_by_arity[0].clone())
        } else {
            self.get_intersected_signatures(&applicable_by_arity)
        }
    }

    pub(super) fn is_arity_smaller(
        &self,
        signature: &Signature,
        target: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_contextual_signature_for_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> Option<Rc<Signature>> {
        if is_function_expression_or_arrow_function(node) || is_object_literal_method(node) {
            self.get_contextual_signature(node)
        } else {
            None
        }
    }

    pub(super) fn get_contextual_signature(
        &self,
        node: &Node, /*FunctionExpression | ArrowFunction | MethodDeclaration*/
    ) -> Option<Rc<Signature>> {
        Debug_.assert(
            node.kind() != SyntaxKind::MethodDeclaration || is_object_literal_method(node),
            None,
        );
        let type_tag_signature = self.get_signature_of_type_tag(node);
        if type_tag_signature.is_some() {
            return type_tag_signature;
        }
        let type_ = self.get_apparent_type_of_contextual_type(node, Some(ContextFlags::Signature));
        if type_.is_none() {
            return None;
        }
        let type_ = type_.unwrap();
        if !type_.flags().intersects(TypeFlags::Union) {
            return self.get_contextual_call_signature(&type_, node);
        }
        let mut signature_list: Option<Vec<Rc<Signature>>> = None;
        let types = type_.as_union_or_intersection_type_interface().types();
        for current in types {
            let signature = self.get_contextual_call_signature(current, node);
            if let Some(signature) = signature {
                match signature_list.as_mut() {
                    None => {
                        signature_list = Some(vec![signature]);
                    }
                    Some(signature_list) => {
                        if self.compare_signatures_identical(
                            signature_list[0].clone(),
                            signature.clone(),
                            false,
                            true,
                            true,
                            |a, b| self.compare_types_identical(a, b),
                        ) == Ternary::False
                        {
                            return None;
                        } else {
                            signature_list.push(signature);
                        }
                    }
                }
            }
        }

        signature_list.map(|signature_list| {
            if signature_list.len() == 1 {
                signature_list[0].clone()
            } else {
                Rc::new(self.create_union_signature(&signature_list[0].clone(), signature_list))
            }
        })
    }

    pub(super) fn has_default_value(
        &self,
        node: &Node, /*BindingElement | Expression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_array_literal(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_numeric_literal_name(&self, name: &str) -> bool {
        unimplemented!()
    }

    pub(super) fn check_computed_property_name(
        &self,
        node: &Node, /*ComputedPropertyName*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_object_literal(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) -> Rc<Type> {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        let mut properties_table = create_symbol_table(None);
        let mut properties_array: Vec<Rc<Symbol>> = vec![];

        let object_flags = self.fresh_object_literal_flag;

        for member_decl in &node_as_object_literal_expression.properties {
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
            let result = self.create_anonymous_type(
                Some(node.symbol()),
                Rc::new(RefCell::new(properties_table)),
                vec![],
                vec![],
                vec![], // TODO: this is wrong
            );
            result.set_object_flags(
                result.object_flags()
                    | object_flags
                    | ObjectFlags::ObjectLiteral
                    | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
            result.into()
        };

        create_object_literal_type()
    }

    pub(super) fn is_valid_spread_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_hyphenated_jsx_name(&self, name: &str) -> bool {
        unimplemented!()
    }

    pub(super) fn is_jsx_intrinsic_identifier(
        &self,
        tag_name: &Node, /*JsxTagNameExpression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_jsx_attribute(
        &self,
        node: &Node, /*JsxAttribute*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_jsx_children(
        &self,
        node: &Node, /*JsxElement | JsxFragment*/
        check_mode: Option<CheckMode>,
    ) -> Vec<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_type<TLocation: Borrow<Node>>(
        &self,
        name: &__String,
        location: Option<TLocation>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_jsx_namespace_at<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_properties_name(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<__String> {
        unimplemented!()
    }

    pub(super) fn get_jsx_library_managed_attributes(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_children_property_name(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<__String> {
        unimplemented!()
    }

    pub(super) fn get_intrinsic_attributes_type_from_string_literal_type(
        &self,
        type_: &Type, /*StringLiteralType*/
        location: &Node,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_intrinsic_attributes_type_from_jsx_opening_like_element(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_known_property(
        &self,
        target_type: &Type,
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

    pub(super) fn is_excess_property_check_target(&self, type_: &Type) -> bool {
        (type_.flags().intersects(TypeFlags::Object)
            && !(get_object_flags(type_)
                .intersects(ObjectFlags::ObjectLiteralPatternWithComputedProperties)))
            || type_.flags().intersects(TypeFlags::NonPrimitive)
            || (type_.flags().intersects(TypeFlags::Union) && unimplemented!())
            || (type_.flags().intersects(TypeFlags::Intersection) && unimplemented!())
    }

    pub(super) fn get_declaration_node_flags_from_symbol(&self, s: &Symbol) -> NodeFlags {
        unimplemented!()
    }

    pub(super) fn is_prototype_property(&self, symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn check_non_null_expression(
        &self,
        node: &Node, /*Expression | QualifiedName*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_nullable_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_non_nullable_type_if_needed(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_non_null_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn lookup_symbol_for_private_identifier_declaration(
        &self,
        prop_name: &__String,
        location: &Node,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_symbol_for_private_identifier_expression(
        &self,
        priv_id: &Node, /*PrivateIdentifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_private_identifier_property_of_type_(
        &self,
        left_type: &Type,
        lexically_scoped_identifier: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_this_property_access_in_constructor(
        &self,
        node: &Node, /*ElementAccessExpression | PropertyAccessExpression | QualifiedName*/
        prop: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_unchecked_js_suggestion<TNode: Borrow<Node>, TSuggestion: Borrow<Symbol>>(
        &self,
        node: Option<TNode>,
        suggestion: Option<TSuggestion>,
        exclude_classes: bool,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_in_property_initializer_or_class_static_block(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn type_has_static_property(
        &self,
        prop_name: &__String,
        containing_type: &Type,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_suggested_lib_for_nonexistent_name(
        &self,
        name: ResolveNameNameArg,
    ) -> Option<String> {
        // unimplemented!()
        None
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_jsx_attribute<
        TName: Into<StringOrRcNode>,
    >(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_suggestion_for_nonexistent_property<TName: Into<StringOrRcNode>>(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        // unimplemented!()
        None
    }

    pub(super) fn get_suggestion_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_module(
        &self,
        name: &Node, /*Identifier*/
        target_module: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_suggestion_for_nonexistent_index_signature(
        &self,
        object_type: &Type,
        name: &Node, /*ElementAccessExpression*/
        keyed_type: &Type,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_type_for_nonexistent_string_literal_type(
        &self,
        source: &Type, /*StringLiteralType*/
        target: &Type, /*UnionType*/
    ) -> Option<Rc<Type /*StringLiteralType*/>> {
        unimplemented!()
    }

    pub(super) fn mark_property_as_referenced<TNodeForCheckWriteOnly: Borrow<Node>>(
        &self,
        prop: &Symbol,
        node_for_check_write_only: Option<TNodeForCheckWriteOnly>,
        is_self_type_access: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn is_self_type_access<TParent: Borrow<Symbol>>(
        &self,
        name: &Node, /*Expression | QualifiedName*/
        parent: Option<TParent>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_valid_property_access_(
        &self,
        node: &Node, /*PropertyAccessExpression | QualifiedName | ImportTypeNode*/
        property_name: &__String,
    ) -> bool {
        unimplemented!()
    }

    pub fn is_valid_property_access_for_completions_(
        &self,
        node_in: &Node, /*PropertyAccessExpression | ImportTypeNode | QualifiedName*/
        type_: &Type,
        property: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn accepts_void(&self, t: &Type) -> bool {
        t.flags().intersects(TypeFlags::Void)
    }

    pub(super) fn get_single_call_signature(&self, type_: &Type) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn get_single_call_or_construct_signature(
        &self,
        type_: &Type,
    ) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn instantiate_signature_in_context_of<
        TCompareTypes: FnMut(&Type, &Type, Option<bool>) -> Ternary,
    >(
        &self,
        signature: &Signature,
        contextual_signature: &Signature,
        inference_context: Option<&InferenceContext>,
        compare_types: Option<&mut TCompareTypes>,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn create_signature_for_jsx_intrinsic(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
        result: &Type,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn get_resolved_signature_(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates_out_array: Option<&[Rc<Signature>]>,
        check_mode: Option<CheckMode>,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn get_spread_argument_type<TContext: Borrow<InferenceContext>>(
        &self,
        args: &[Rc<Node /*Expression*/>],
        index: usize,
        arg_count: usize,
        rest_type: &Type,
        context: Option<TContext>,
        check_mode: CheckMode,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_jsx_reference_kind(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> JsxReferenceKind {
        unimplemented!()
    }

    pub(super) fn get_effective_call_arguments(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> Vec<Rc<Node /*Expression*/>> {
        unimplemented!()
    }

    pub(super) fn is_js_constructor<TNode: Borrow<Node>>(&self, node: Option<TNode>) -> bool {
        unimplemented!()
    }

    pub(super) fn merge_js_symbols<TSource: Borrow<Symbol>>(
        &self,
        target: &Symbol,
        source: Option<TSource>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_assigned_class_symbol(
        &self,
        decl: &Node, /*Declaration*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_symbol_of_expando(
        &self,
        node: &Node,
        allow_declaration: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_with_synthetic_default_only(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_with_synthetic_default_import_type(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_common_js_require(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_type_of_parameter(&self, symbol: &Symbol) -> Rc<Type> {
        let type_ = self.get_type_of_symbol(symbol);
        if self.strict_null_checks {
            let declaration = symbol.maybe_value_declaration();
            if matches!(declaration.as_ref(), Some(declaration) if has_initializer(&declaration)) {
                return self.get_optional_type_(&type_, None);
            }
        }
        type_
    }

    pub(super) fn get_tuple_element_label(
        &self,
        d: &Node, /*ParameterDeclaration | NamedTupleMember*/
    ) -> __String {
        unimplemented!()
    }

    pub(super) fn get_parameter_name_at_position<TOverrideRestType: Borrow<Type>>(
        &self,
        signature: &Signature,
        pos: usize,
        override_rest_type: Option<TOverrideRestType>,
    ) -> __String {
        unimplemented!()
    }

    pub(super) fn get_type_at_position(&self, signature: &Signature, pos: usize) -> Rc<Type> {
        self.try_get_type_at_position(signature, pos)
            .unwrap_or_else(|| self.any_type())
    }

    pub(super) fn try_get_type_at_position(
        &self,
        signature: &Signature,
        pos: usize,
    ) -> Option<Rc<Type>> {
        let param_count = signature.parameters().len()
            - if signature_has_rest_parameter(signature) {
                1
            } else {
                0
            };
        if pos < param_count {
            return Some(self.get_type_of_parameter(&signature.parameters()[pos]));
        }
        if signature_has_rest_parameter(signature) {
            let rest_type = self.get_type_of_symbol(&signature.parameters()[param_count]);
            let index = pos - param_count;
            unimplemented!()
        }
        None
    }

    pub(super) fn get_rest_type_at_position(&self, signature: &Signature, pos: usize) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_parameter_count(&self, signature: &Signature) -> usize {
        let length = signature.parameters().len();
        if signature_has_rest_parameter(signature) {
            let rest_type = self.get_type_of_symbol(&signature.parameters()[length - 1]);
            if self.is_tuple_type(&rest_type) {
                unimplemented!()
            }
        }
        length
    }

    pub(super) fn get_min_argument_count(
        &self,
        signature: &Signature,
        flags: Option<MinArgumentCountFlags>,
    ) -> usize {
        let strong_arity_for_untyped_js = match flags {
            None => false,
            Some(flags) => flags.intersects(MinArgumentCountFlags::StrongArityForUntypedJS),
        };
        let void_is_non_optional = match flags {
            None => false,
            Some(flags) => flags.intersects(MinArgumentCountFlags::VoidIsNonOptional),
        };
        if void_is_non_optional || signature.maybe_resolved_min_argument_count().is_none() {
            let mut min_argument_count = None;
            if signature_has_rest_parameter(signature) {
                let rest_type = self
                    .get_type_of_symbol(&signature.parameters()[signature.parameters().len() - 1]);
                if self.is_tuple_type(&rest_type) {
                    unimplemented!()
                }
            }
            if min_argument_count.is_none() {
                if !strong_arity_for_untyped_js
                    && signature
                        .flags
                        .intersects(SignatureFlags::IsUntypedSignatureInJSFile)
                {
                    return 0;
                }
                min_argument_count = Some(signature.min_argument_count());
            }
            let mut min_argument_count = min_argument_count.unwrap();
            if void_is_non_optional {
                return min_argument_count;
            }
            let mut i = min_argument_count - 1;
            while i >= 0 {
                let type_ = self.get_type_at_position(signature, i);
                if self
                    .filter_type(&type_, |type_| self.accepts_void(type_))
                    .flags()
                    .intersects(TypeFlags::Never)
                {
                    break;
                }
                min_argument_count = i;
                i -= 1;
            }
            signature.set_resolved_min_argument_count(min_argument_count);
        }
        signature.resolved_min_argument_count()
    }

    pub(super) fn has_effective_rest_parameter(&self, signature: &Signature) -> bool {
        if signature_has_rest_parameter(signature) {
            let rest_type =
                self.get_type_of_symbol(&signature.parameters()[signature.parameters().len() - 1]);
            return !self.is_tuple_type(&rest_type) || unimplemented!();
        }
        false
    }

    pub(super) fn get_effective_rest_type(&self, signature: &Signature) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_non_array_rest_type(&self, signature: &Signature) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_first_parameter_of_signature(
        &self,
        signature: &Signature,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_of_first_parameter_of_signature_with_fallback(
        &self,
        signature: &Signature,
        fallback_type: &Type,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_promise_type(&self, promised_type: &Type) -> Rc<Type> {
        let global_promise_type = self.get_global_promise_type(true);
        if !Rc::ptr_eq(&global_promise_type, &self.empty_generic_type()) {
            let promised_type = self
                .get_awaited_type_no_alias(
                    &self.unwrap_awaited_type(promised_type),
                    Option::<&Node>::None,
                    None,
                    None,
                )
                .unwrap_or_else(|| self.unknown_type());
            return self.create_type_reference(&global_promise_type, Some(vec![promised_type]));
        }

        self.unknown_type()
    }

    pub(super) fn create_promise_like_type(&self, promised_type: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_promise_return_type(
        &self,
        func: &Node, /*FunctionLikeDeclaration | ImportCall*/
        promised_type: &Type,
    ) -> Rc<Type> {
        let promise_type = self.create_promise_type(promised_type);
        if Rc::ptr_eq(&promise_type, &self.unknown_type()) {
            self.error(
                Some(func),
                if is_import_call(func) {
                    &Diagnostics::A_dynamic_import_call_returns_a_Promise_Make_sure_you_have_a_declaration_for_Promise_or_include_ES2015_in_your_lib_option
                } else {
                    &Diagnostics::An_async_function_or_method_must_return_a_Promise_Make_sure_you_have_a_declaration_for_Promise_or_include_ES2015_in_your_lib_option
                },
                None
            );
            return self.error_type();
        } else if self.get_global_promise_constructor_symbol(true).is_none() {
            self.error(
                Some(func),
                if is_import_call(func) {
                    &Diagnostics::A_dynamic_import_call_in_ES5_SlashES3_requires_the_Promise_constructor_Make_sure_you_have_a_declaration_for_the_Promise_constructor_or_include_ES2015_in_your_lib_option
                } else {
                    &Diagnostics::An_async_function_or_method_in_ES5_SlashES3_requires_the_Promise_constructor_Make_sure_you_have_a_declaration_for_the_Promise_constructor_or_include_ES2015_in_your_lib_option
                },
                None
            );
        }

        promise_type
    }

    pub(super) fn get_return_type_from_body(
        &self,
        func: &Node, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let func_as_function_like_declaration = func.as_function_like_declaration();
        if func_as_function_like_declaration.maybe_body().is_none() {
            return self.error_type();
        }
        let func_body = func_as_function_like_declaration.maybe_body().unwrap();

        let function_flags = get_function_flags(Some(func));
        let is_async = function_flags.intersects(FunctionFlags::Async);
        let is_generator = function_flags.intersects(FunctionFlags::Generator);

        let mut return_type: Option<Rc<Type>> = None;
        let mut yield_type: Option<Rc<Type>> = None;
        let mut next_type: Option<Rc<Type>> = None;
        let fallback_return_type = self.void_type();
        if func_body.kind() != SyntaxKind::Block {
            return_type = Some(self.check_expression_cached(
                &func_body,
                check_mode.map(|check_mode| check_mode & !CheckMode::SkipGenericFunctions),
            ));
            if is_async {
                unimplemented!()
            }
        } else if is_generator {
            unimplemented!()
        } else {
            let types = self.check_and_aggregate_return_expression_types(func, check_mode);
            if types.is_none() {
                return if function_flags.intersects(FunctionFlags::Async) {
                    self.create_promise_return_type(func, &self.never_type())
                } else {
                    self.never_type()
                };
            }
            let types = types.unwrap();
            if types.is_empty() {
                return if function_flags.intersects(FunctionFlags::Async) {
                    self.create_promise_return_type(func, &self.void_type())
                } else {
                    self.void_type()
                };
            }

            return_type = Some(self.get_union_type(
                types,
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            ));
        }

        if return_type.is_some() || yield_type.is_some() || next_type.is_some() {
            if let Some(yield_type) = yield_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    yield_type,
                    Some(WideningKind::GeneratorYield),
                );
            }
            if let Some(return_type) = return_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    return_type,
                    Some(WideningKind::FunctionReturn),
                );
            }
            if let Some(next_type) = next_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    next_type,
                    Some(WideningKind::GeneratorNext),
                );
            }

            if matches!(return_type.as_ref(), Some(return_type) if self.is_unit_type(return_type))
                || matches!(yield_type.as_ref(), Some(yield_type) if self.is_unit_type(yield_type))
                || matches!(next_type.as_ref(), Some(next_type) if self.is_unit_type(next_type))
            {
                unimplemented!()
            }

            if let Some(yield_type_present) = yield_type {
                yield_type = Some(self.get_widened_type(&yield_type_present));
            }
            if let Some(return_type_present) = return_type {
                return_type = Some(self.get_widened_type(&return_type_present));
            }
            if let Some(next_type_present) = next_type {
                next_type = Some(self.get_widened_type(&next_type_present));
            }
        }

        if is_generator {
            unimplemented!()
        } else {
            if is_async {
                self.create_promise_type(&return_type.unwrap_or(fallback_return_type))
            } else {
                return_type.unwrap_or(fallback_return_type)
            }
        }
    }

    pub(super) fn get_facts_from_typeof_switch(
        &self,
        start: usize,
        end: usize,
        witnesses: &[String],
        has_default: bool,
    ) -> TypeFacts {
        unimplemented!()
    }

    pub(super) fn is_exhaustive_switch_statement(
        &self,
        node: &Node, /*SwitchStatement*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_and_aggregate_return_expression_types(
        &self,
        func: &Node, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }
}
