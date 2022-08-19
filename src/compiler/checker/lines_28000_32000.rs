#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{
    anon, signature_has_rest_parameter, CheckMode, MinArgumentCountFlags, ResolveNameNameArg,
    TypeFacts, WideningKind,
};
use crate::{
    add_related_info, create_diagnostic_for_node, find_ancestor, for_each, get_containing_class,
    get_symbol_name_for_private_identifier, get_this_container, get_this_parameter, id_text,
    is_call_or_new_expression, is_expression_node, is_function_like, is_import_call,
    is_named_declaration, is_part_of_type_query, is_private_identifier, is_this_identifier, Debug_,
    Diagnostics, FunctionFlags, JsxReferenceKind, NodeFlags, Signature, SignatureFlags,
    StringOrRcNode, SymbolFlags, Ternary, UnionReduction, __String, get_function_flags,
    has_initializer, InferenceContext, Node, NodeInterface, Symbol, SymbolInterface, SyntaxKind,
    Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_this_parameter_from_node_context(&self, node: &Node) -> Option<Rc<Node>> {
        let this_container = get_this_container(node, false);
        /*thisContainer &&*/
        if is_function_like(Some(&*this_container)) {
            get_this_parameter(&this_container)
        } else {
            None
        }
    }

    pub(super) fn symbol_has_non_method_declaration(&self, symbol: &Symbol) -> bool {
        self.for_each_property_bool(symbol, &mut |prop: &Symbol| {
            !prop.flags().intersects(SymbolFlags::Method)
        })
    }

    pub(super) fn check_non_null_expression(
        &self,
        node: &Node, /*Expression | QualifiedName*/
    ) -> Rc<Type> {
        self.check_non_null_type(&self.check_expression(node, None, None), node)
    }

    pub(super) fn is_nullable_type(&self, type_: &Type) -> bool {
        if self.strict_null_checks {
            self.get_falsy_flags(type_)
        } else {
            type_.flags()
        }
        .intersects(TypeFlags::Nullable)
    }

    pub(super) fn get_non_nullable_type_if_needed(&self, type_: &Type) -> Rc<Type> {
        if self.is_nullable_type(type_) {
            self.get_non_nullable_type(&type_)
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn report_object_possibly_null_or_undefined_error(
        &self,
        node: &Node,
        flags: TypeFlags,
    ) {
        self.error(
            Some(node),
            if flags.intersects(TypeFlags::Undefined) {
                if flags.intersects(TypeFlags::Null) {
                    &*Diagnostics::Object_is_possibly_null_or_undefined
                } else {
                    &*Diagnostics::Object_is_possibly_undefined
                }
            } else {
                &*Diagnostics::Object_is_possibly_null
            },
            None,
        );
    }

    pub(super) fn report_cannot_invoke_possibly_null_or_undefined_error(
        &self,
        node: &Node,
        flags: TypeFlags,
    ) {
        self.error(
            Some(node),
            if flags.intersects(TypeFlags::Undefined) {
                if flags.intersects(TypeFlags::Null) {
                    &*Diagnostics::Cannot_invoke_an_object_which_is_possibly_null_or_undefined
                } else {
                    &*Diagnostics::Cannot_invoke_an_object_which_is_possibly_undefined
                }
            } else {
                &*Diagnostics::Cannot_invoke_an_object_which_is_possibly_null
            },
            None,
        );
    }

    pub(super) fn check_non_null_type_with_reporter<TReportError: FnMut(&Node, TypeFlags)>(
        &self,
        type_: &Type,
        node: &Node,
        mut report_error: TReportError,
    ) -> Rc<Type> {
        if self.strict_null_checks && type_.flags().intersects(TypeFlags::Unknown) {
            self.error(Some(node), &Diagnostics::Object_is_of_type_unknown, None);
            return self.error_type();
        }
        let kind = if self.strict_null_checks {
            self.get_falsy_flags(type_)
        } else {
            type_.flags()
        } & TypeFlags::Nullable;
        if kind != TypeFlags::None {
            report_error(node, kind);
            let t = self.get_non_nullable_type(type_);
            return if t.flags().intersects(TypeFlags::Nullable | TypeFlags::Never) {
                self.error_type()
            } else {
                t
            };
        }
        type_.type_wrapper()
    }

    pub(super) fn check_non_null_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        self.check_non_null_type_with_reporter(type_, node, |node: &Node, flags: TypeFlags| {
            self.report_object_possibly_null_or_undefined_error(node, flags)
        })
    }

    pub(super) fn check_non_null_non_void_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        let non_null_type = self.check_non_null_type(type_, node);
        if non_null_type.flags().intersects(TypeFlags::Void) {
            self.error(Some(node), &Diagnostics::Object_is_possibly_undefined, None);
        }
        non_null_type
    }

    pub(super) fn check_property_access_expression(
        &self,
        node: &Node, /*PropertyAccessExpression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let node_as_property_access_expression = node.as_property_access_expression();
        if node.flags().intersects(NodeFlags::OptionalChain) {
            self.check_property_access_chain(node, check_mode)
        } else {
            self.check_property_access_expression_or_qualified_name(
                node,
                &node_as_property_access_expression.expression,
                &self.check_non_null_expression(&node_as_property_access_expression.expression),
                &node_as_property_access_expression.name,
                check_mode,
            )
        }
    }

    pub(super) fn check_property_access_chain(
        &self,
        node: &Node, /*PropertyAccessChain*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let node_as_property_access_expression = node.as_property_access_expression();
        let left_type =
            self.check_expression(&node_as_property_access_expression.expression, None, None);
        let non_optional_type = self.get_optional_expression_type(
            &left_type,
            &node_as_property_access_expression.expression,
        );
        self.propagate_optional_type_marker(
            &self.check_property_access_expression_or_qualified_name(
                node,
                &node_as_property_access_expression.expression,
                &self.check_non_null_type(
                    &non_optional_type,
                    &node_as_property_access_expression.expression,
                ),
                &node_as_property_access_expression.name,
                check_mode,
            ),
            node,
            !Rc::ptr_eq(&non_optional_type, &left_type),
        )
    }

    pub(super) fn check_qualified_name(
        &self,
        node: &Node, /*QualifiedName*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let node_as_qualified_name = node.as_qualified_name();
        let left_type = if is_part_of_type_query(node)
            && is_this_identifier(Some(&*node_as_qualified_name.left))
        {
            self.check_non_null_type(
                &self.check_this_expression(&node_as_qualified_name.left),
                &node_as_qualified_name.left,
            )
        } else {
            self.check_non_null_expression(&node_as_qualified_name.left)
        };
        self.check_property_access_expression_or_qualified_name(
            node,
            &node_as_qualified_name.left,
            &left_type,
            &node_as_qualified_name.right,
            check_mode,
        )
    }

    pub(super) fn is_method_access_for_call(&self, node: &Node) -> bool {
        let mut node = node.node_wrapper();
        while node.parent().kind() == SyntaxKind::ParenthesizedExpression {
            node = node.parent();
        }
        is_call_or_new_expression(&node.parent())
            && Rc::ptr_eq(&node.parent().as_has_expression().expression(), &node)
    }

    pub(super) fn lookup_symbol_for_private_identifier_declaration(
        &self,
        prop_name: &__String,
        location: &Node,
    ) -> Option<Rc<Symbol>> {
        let mut containing_class = get_containing_class(location);
        while let Some(containing_class_present) = containing_class.as_ref() {
            let symbol = containing_class_present.symbol();
            let name = get_symbol_name_for_private_identifier(&symbol, prop_name);
            let prop = symbol
                .maybe_members()
                .as_ref()
                .and_then(|symbol_members| (**symbol_members).borrow().get(&name).cloned())
                .or_else(|| {
                    symbol
                        .maybe_exports()
                        .as_ref()
                        .and_then(|symbol_exports| (**symbol_exports).borrow().get(&name).cloned())
                });
            if prop.is_some() {
                return prop;
            }
            containing_class = get_containing_class(containing_class_present);
        }
        None
    }

    pub(super) fn check_grammar_private_identifier_expression(
        &self,
        priv_id: &Node, /*PrivateIdentifier*/
    ) -> bool {
        if get_containing_class(priv_id).is_none() {
            return self.grammar_error_on_node(
                priv_id,
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                None,
            );
        }
        if !is_expression_node(priv_id) {
            return self.grammar_error_on_node(
                priv_id,
                &Diagnostics::Private_identifiers_are_only_allowed_in_class_bodies_and_may_only_be_used_as_part_of_a_class_member_declaration_property_access_or_on_the_left_hand_side_of_an_in_expression,
                None,
            );
        }
        if self
            .get_symbol_for_private_identifier_expression(priv_id)
            .is_none()
        {
            return self.grammar_error_on_node(
                priv_id,
                &Diagnostics::Cannot_find_name_0,
                Some(vec![id_text(priv_id)]),
            );
        }
        false
    }

    pub(super) fn check_private_identifier_expression(
        &self,
        priv_id: &Node, /*PrivateIdentifier*/
    ) -> Rc<Type> {
        self.check_grammar_private_identifier_expression(priv_id);
        let symbol = self.get_symbol_for_private_identifier_expression(priv_id);
        if let Some(symbol) = symbol.as_ref() {
            self.mark_property_as_referenced(symbol, Option::<&Node>::None, false);
        }
        self.any_type()
    }

    pub(super) fn get_symbol_for_private_identifier_expression(
        &self,
        priv_id: &Node, /*PrivateIdentifier*/
    ) -> Option<Rc<Symbol>> {
        if !is_expression_node(priv_id) {
            return None;
        }

        let links = self.get_node_links(priv_id);
        if (*links).borrow().resolved_symbol.is_none() {
            links.borrow_mut().resolved_symbol = self
                .lookup_symbol_for_private_identifier_declaration(
                    &priv_id.as_private_identifier().escaped_text,
                    priv_id,
                );
        }
        let ret = (*links).borrow().resolved_symbol.clone();
        ret
    }

    pub(super) fn get_private_identifier_property_of_type_(
        &self,
        left_type: &Type,
        lexically_scoped_identifier: &Symbol,
    ) -> Option<Rc<Symbol>> {
        self.get_property_of_type_(left_type, lexically_scoped_identifier.escaped_name(), None)
    }

    pub(super) fn check_private_identifier_property_access<
        TLexicallyScopedIdentifier: Borrow<Symbol>,
    >(
        &self,
        left_type: &Type,
        right: &Node, /*PrivateIdentifier*/
        lexically_scoped_identifier: Option<TLexicallyScopedIdentifier>,
    ) -> bool {
        let mut property_on_type: Option<Rc<Symbol>> = None;
        let properties = self.get_properties_of_type(left_type);
        // if (properties) {
        let right_as_private_identifier = right.as_private_identifier();
        for_each(&properties, |symbol: &Rc<Symbol>, _| {
            let decl = symbol.maybe_value_declaration();
            if matches!(
                decl.as_ref(),
                Some(decl) if is_named_declaration(decl) &&
                    is_private_identifier(&decl.as_named_declaration().name()) &&
                    decl.as_named_declaration().name().as_identifier().escaped_text == right_as_private_identifier.escaped_text
            ) {
                property_on_type = Some(symbol.clone());
                return Some(());
            }
            None
        });
        // }
        let diag_name = self
            .diagnostic_name(right.node_wrapper().into())
            .into_owned();
        if let Some(property_on_type) = property_on_type.as_ref() {
            let type_value_decl =
                Debug_.check_defined(property_on_type.maybe_value_declaration(), None);
            let type_class = Debug_.check_defined(get_containing_class(&type_value_decl), None);
            if let Some(lexically_scoped_identifier_value_declaration) = lexically_scoped_identifier
                .and_then(|lexically_scoped_identifier| {
                    lexically_scoped_identifier
                        .borrow()
                        .maybe_value_declaration()
                })
            {
                let lexical_value_decl = lexically_scoped_identifier_value_declaration;
                let lexical_class = get_containing_class(&lexical_value_decl);
                Debug_.assert(lexical_class.is_some(), None);
                let lexical_class = lexical_class.unwrap();
                if find_ancestor(Some(&*lexical_class), |n: &Node| ptr::eq(&*type_class, n))
                    .is_some()
                {
                    let diagnostic = self.error(
                        Some(right),
                        &Diagnostics::The_property_0_cannot_be_accessed_on_type_1_within_this_class_because_it_is_shadowed_by_another_private_identifier_with_the_same_spelling,
                        Some(vec![
                            diag_name.clone(),
                            self.type_to_string_(
                                left_type,
                                Option::<&Node>::None,
                                None, None,
                            )
                        ])
                    );

                    add_related_info(
                        &diagnostic,
                        vec![
                            Rc::new(
                                create_diagnostic_for_node(
                                    &lexical_value_decl,
                                    &Diagnostics::The_shadowing_declaration_of_0_is_defined_here,
                                    Some(vec![
                                        diag_name.clone(),
                                    ])
                                ).into()
                            ),
                            Rc::new(
                                create_diagnostic_for_node(
                                    &type_value_decl,
                                    &Diagnostics::The_declaration_of_0_that_you_probably_intended_to_use_is_defined_here,
                                    Some(vec![
                                        diag_name.clone(),
                                    ])
                                ).into()
                            ),
                        ]
                    );
                    return true;
                }
            }
            self.error(
                Some(right),
                &Diagnostics::Property_0_is_not_accessible_outside_class_1_because_it_has_a_private_identifier,
                Some(vec![
                    diag_name,
                    self.diagnostic_name(
                        type_class.as_named_declaration().maybe_name().map_or_else(|| {
                            anon.clone().into()
                        }, Into::into)
                    ).into_owned()
                ])
            );
            return true;
        }
        false
    }

    pub(super) fn is_this_property_access_in_constructor(
        &self,
        node: &Node, /*ElementAccessExpression | PropertyAccessExpression | QualifiedName*/
        prop: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_property_access_expression_or_qualified_name(
        &self,
        node: &Node, /*PropertyAccessExpression | QualifiedName*/
        left: &Node, /*Expression | QualifiedName*/
        left_type: &Type,
        right: &Node, /*Identifier | PrivateIdentifier*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
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

    pub(super) fn check_deprecated_signature(
        &self,
        signature: &Signature,
        node: &Node, /*CallLikeExpression*/
    ) {
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
