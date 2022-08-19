#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, CheckTypeContainingMessageChain, JsxNames,
    MinArgumentCountFlags, ResolveNameNameArg, TypeFacts, WideningKind,
};
use crate::{
    chain_diagnostic_messages, escape_leading_underscores, get_text_of_node, is_import_call, map,
    unescape_leading_underscores, DiagnosticMessageChain, Diagnostics, FunctionFlags,
    JsxReferenceKind, NodeFlags, Signature, SignatureFlags, SignatureKind, StringOrRcNode,
    SymbolFlags, Ternary, UnionOrIntersectionTypeInterface, UnionReduction, __String,
    get_function_flags, get_object_flags, has_initializer, InferenceContext, Node, NodeInterface,
    ObjectFlags, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_name_from_jsx_element_attributes_container<TJsxNamespace: Borrow<Symbol>>(
        &self,
        name_of_attrib_prop_container: &__String,
        jsx_namespace: Option<TJsxNamespace>,
    ) -> Option<__String> {
        let jsx_namespace =
            jsx_namespace.map(|jsx_namespace| jsx_namespace.borrow().symbol_wrapper());
        let jsx_element_attrib_prop_interface_sym =
            jsx_namespace.as_ref().and_then(|jsx_namespace| {
                self.get_symbol(
                    &(**jsx_namespace.maybe_exports().as_ref().unwrap()).borrow(),
                    name_of_attrib_prop_container,
                    SymbolFlags::Type,
                )
            });
        let jsx_element_attrib_prop_interface_type = jsx_element_attrib_prop_interface_sym
            .as_ref()
            .map(|jsx_element_attrib_prop_interface_sym| {
                self.get_declared_type_of_symbol(jsx_element_attrib_prop_interface_sym)
            });
        let properties_of_jsx_element_attrib_prop_interface =
            jsx_element_attrib_prop_interface_type.as_ref().map(
                |jsx_element_attrib_prop_interface_type| {
                    self.get_properties_of_type(jsx_element_attrib_prop_interface_type)
                },
            );
        if let Some(properties_of_jsx_element_attrib_prop_interface) =
            properties_of_jsx_element_attrib_prop_interface.as_ref()
        {
            if properties_of_jsx_element_attrib_prop_interface.is_empty() {
                return Some(__String::new("".to_owned()));
            } else if properties_of_jsx_element_attrib_prop_interface.len() == 1 {
                return Some(
                    properties_of_jsx_element_attrib_prop_interface[0]
                        .escaped_name()
                        .clone(),
                );
            } else if properties_of_jsx_element_attrib_prop_interface.len() > 1 {
                let jsx_element_attrib_prop_interface_sym =
                    jsx_element_attrib_prop_interface_sym.as_ref().unwrap();
                if let Some(jsx_element_attrib_prop_interface_sym_declarations) =
                    jsx_element_attrib_prop_interface_sym
                        .maybe_declarations()
                        .as_ref()
                {
                    self.error(
                        jsx_element_attrib_prop_interface_sym_declarations
                            .get(0)
                            .cloned(),
                        &Diagnostics::The_global_type_JSX_0_may_not_have_more_than_one_property,
                        Some(vec![unescape_leading_underscores(
                            name_of_attrib_prop_container,
                        )]),
                    );
                }
            }
        }
        None
    }

    pub(super) fn get_jsx_library_managed_attributes<TJsxNamespace: Borrow<Symbol>>(
        &self,
        jsx_namespace: Option<TJsxNamespace>,
    ) -> Option<Rc<Symbol>> {
        let jsx_namespace = jsx_namespace?;
        let jsx_namespace = jsx_namespace.borrow();
        let ret = self.get_symbol(
            &(**jsx_namespace.maybe_exports().as_ref().unwrap()).borrow(),
            &JsxNames::LibraryManagedAttributes,
            SymbolFlags::Type,
        );
        ret
    }

    pub(super) fn get_jsx_element_properties_name<TJsxNamespace: Borrow<Symbol>>(
        &self,
        jsx_namespace: Option<TJsxNamespace>,
    ) -> Option<__String> {
        self.get_name_from_jsx_element_attributes_container(
            &JsxNames::ElementAttributesPropertyNameContainer,
            jsx_namespace,
        )
    }

    pub(super) fn get_jsx_element_children_property_name<TJsxNamespace: Borrow<Symbol>>(
        &self,
        jsx_namespace: Option<TJsxNamespace>,
    ) -> Option<__String> {
        self.get_name_from_jsx_element_attributes_container(
            &JsxNames::ElementChildrenAttributeNameContainer,
            jsx_namespace,
        )
    }

    pub(super) fn get_uninstantiated_jsx_signatures_of_type(
        &self,
        element_type: &Type,
        caller: &Node, /*JsxOpeningLikeElement*/
    ) -> Vec<Rc<Signature>> {
        if element_type.flags().intersects(TypeFlags::String) {
            return vec![self.any_signature()];
        } else if element_type.flags().intersects(TypeFlags::StringLiteral) {
            let intrinsic_type =
                self.get_intrinsic_attributes_type_from_string_literal_type(element_type, caller);
            match intrinsic_type.as_ref() {
                None => {
                    self.error(
                        Some(caller),
                        &Diagnostics::Property_0_does_not_exist_on_type_1,
                        Some(vec![
                            element_type.as_string_literal_type().value.clone(),
                            format!("JSX.{}", &**JsxNames::IntrinsicElements),
                        ]),
                    );
                    return vec![];
                }
                Some(intrinsic_type) => {
                    let fake_signature =
                        self.create_signature_for_jsx_intrinsic(caller, intrinsic_type);
                    return vec![fake_signature];
                }
            }
        }
        let apparent_elem_type = self.get_apparent_type(element_type);
        let mut signatures =
            self.get_signatures_of_type(&apparent_elem_type, SignatureKind::Construct);
        if signatures.is_empty() {
            signatures = self.get_signatures_of_type(&apparent_elem_type, SignatureKind::Call);
        }
        if signatures.is_empty() && apparent_elem_type.flags().intersects(TypeFlags::Union) {
            signatures = self.get_union_signatures(&map(
                apparent_elem_type.as_union_type().types(),
                |t: &Rc<Type>, _| self.get_uninstantiated_jsx_signatures_of_type(t, caller),
            ));
        }
        signatures
    }

    pub(super) fn get_intrinsic_attributes_type_from_string_literal_type(
        &self,
        type_: &Type, /*StringLiteralType*/
        location: &Node,
    ) -> Option<Rc<Type>> {
        let intrinsic_elements_type =
            self.get_jsx_type(&JsxNames::IntrinsicElements, Some(location));
        if !self.is_error_type(&intrinsic_elements_type) {
            let string_literal_type_name = &type_.as_string_literal_type().value;
            let intrinsic_prop = self.get_property_of_type_(
                &intrinsic_elements_type,
                &escape_leading_underscores(string_literal_type_name),
                None,
            );
            if let Some(intrinsic_prop) = intrinsic_prop.as_ref() {
                return Some(self.get_type_of_symbol(intrinsic_prop));
            }
            let index_signature_type =
                self.get_index_type_of_type_(&intrinsic_elements_type, &self.string_type());
            if index_signature_type.is_some() {
                return index_signature_type;
            }
            return None;
        }
        Some(self.any_type())
    }

    pub(super) fn check_jsx_return_assignable_to_appropriate_bound(
        &self,
        ref_kind: JsxReferenceKind,
        elem_instance_type: &Type,
        opening_like_element: &Node, /*JsxOpeningLikeElement*/
    ) {
        if ref_kind == JsxReferenceKind::Function {
            let sfc_return_constraint =
                self.get_jsx_stateless_element_type_at(opening_like_element);
            if let Some(sfc_return_constraint) = sfc_return_constraint.as_ref() {
                self.check_type_related_to(
                    elem_instance_type,
                    sfc_return_constraint,
                    self.assignable_relation.clone(),
                    Some(
                        opening_like_element
                            .as_jsx_opening_like_element()
                            .tag_name(),
                    ),
                    Some(Cow::Borrowed(
                        &Diagnostics::Its_return_type_0_is_not_a_valid_JSX_element,
                    )),
                    Some(Rc::new(GenerateInitialErrorChain::new(
                        opening_like_element.node_wrapper(),
                    ))),
                    None,
                );
            }
        } else if ref_kind == JsxReferenceKind::Component {
            let class_constraint = self.get_jsx_element_class_type_at(opening_like_element);
            if let Some(class_constraint) = class_constraint.as_ref() {
                self.check_type_related_to(
                    elem_instance_type,
                    class_constraint,
                    self.assignable_relation.clone(),
                    Some(
                        opening_like_element
                            .as_jsx_opening_like_element()
                            .tag_name(),
                    ),
                    Some(Cow::Borrowed(
                        &Diagnostics::Its_instance_type_0_is_not_a_valid_JSX_element,
                    )),
                    Some(Rc::new(GenerateInitialErrorChain::new(
                        opening_like_element.node_wrapper(),
                    ))),
                    None,
                );
            }
        } else {
            let sfc_return_constraint =
                self.get_jsx_stateless_element_type_at(opening_like_element);
            let class_constraint = self.get_jsx_element_class_type_at(opening_like_element);
            if sfc_return_constraint.is_none() || class_constraint.is_none() {
                return;
            }
            let sfc_return_constraint = sfc_return_constraint.unwrap();
            let class_constraint = class_constraint.unwrap();
            let combined = self.get_union_type(
                vec![sfc_return_constraint, class_constraint],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            );
            self.check_type_related_to(
                elem_instance_type,
                &combined,
                self.assignable_relation.clone(),
                Some(
                    opening_like_element
                        .as_jsx_opening_like_element()
                        .tag_name(),
                ),
                Some(Cow::Borrowed(
                    &Diagnostics::Its_element_type_0_is_not_a_valid_JSX_element,
                )),
                Some(Rc::new(GenerateInitialErrorChain::new(
                    opening_like_element.node_wrapper(),
                ))),
                None,
            );
        }
    }

    pub(super) fn get_intrinsic_attributes_type_from_jsx_opening_like_element(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_class_type_at(&self, location: &Node) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_type_at(&self, location: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_jsx_stateless_element_type_at(&self, location: &Node) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn check_jsx_opening_like_element_or_opening_fragment(
        &self,
        node: &Node, /*JsxOpeningLikeElement | JsxOpeningFragment*/
    ) {
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

pub(super) struct GenerateInitialErrorChain {
    opening_like_element: Rc<Node>,
}

impl GenerateInitialErrorChain {
    pub fn new(opening_like_element: Rc<Node>) -> Self {
        Self {
            opening_like_element,
        }
    }
}

impl CheckTypeContainingMessageChain for GenerateInitialErrorChain {
    fn get(&self) -> Option<Rc<RefCell<DiagnosticMessageChain>>> {
        let component_name = get_text_of_node(
            &self
                .opening_like_element
                .as_jsx_opening_like_element()
                .tag_name(),
            None,
        );
        Some(Rc::new(RefCell::new(chain_diagnostic_messages(
            None,
            &Diagnostics::_0_cannot_be_used_as_a_JSX_component,
            Some(vec![component_name.into_owned()]),
        ))))
    }
}
