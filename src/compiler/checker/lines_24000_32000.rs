#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use super::{signature_has_rest_parameter, CheckMode, MinArgumentCountFlags};
use crate::{
    is_function_expression_or_arrow_function, is_import_call, is_object_literal_method,
    ContextFlags, Debug_, Diagnostics, FunctionFlags, Signature, SignatureFlags, UnionReduction,
    __String, create_symbol_table, get_effective_type_annotation_node, get_function_flags,
    get_object_flags, has_initializer, is_object_literal_expression,
    HasExpressionInitializerInterface, Identifier, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, ObjectLiteralExpression, PropertyAssignment, Symbol, SymbolInterface,
    SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_identifier(
        &self,
        node: &Identifier,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let symbol = self.get_resolved_symbol(node);
        if Rc::ptr_eq(&symbol, &self.unknown_symbol()) {
            return self.error_type();
        }

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
        let declaration = parent.as_variable_declaration();
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
        type_: &Type,
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
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let parent = element.parent();
        let object_literal = parent.as_object_literal_expression();
        // let property_assignment_type = if is_property_assignment(element) {
        // } else {
        //     None
        // };
        // if property_assignment_type.is_some() {
        //     return property_assignment_type;
        // }
        let type_ = self.get_apparent_type_of_contextual_type(object_literal, context_flags);
        if let Some(type_) = type_ {
            if self.has_bindable_name(element) {
                return self.get_type_of_property_of_contextual_type(
                    &type_,
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
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let contextual_type = if false {
            unimplemented!()
        } else {
            self.get_contextual_type(node, context_flags)
        };
        let instantiated_type =
            self.instantiate_contextual_type(contextual_type, node, context_flags);
        if let Some(instantiated_type) = instantiated_type {
            if !(matches!(context_flags, Some(context_flags) if context_flags.intersects(ContextFlags::NoConstraints))
                && instantiated_type
                    .flags()
                    .intersects(TypeFlags::TypeVariable))
            {
                let apparent_type = self
                    .map_type(
                        &instantiated_type,
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

    pub(super) fn instantiate_contextual_type<TTypeRef: Borrow<Type>, TNode: NodeInterface>(
        &self,
        contextual_type: Option<TTypeRef>,
        node: &TNode,
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        if false {
            unimplemented!()
        }
        contextual_type.map(|contextual_type| contextual_type.borrow().type_wrapper())
    }

    pub(super) fn get_contextual_type<TNode: NodeInterface>(
        &self,
        node: &TNode, /*Expression*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let parent = node.parent();
        match &*parent {
            Node::VariableDeclaration(variable_declaration) => {
                self.get_contextual_type_for_initializer_expression(node)
            }
            Node::PropertyAssignment(property_assignment) => self
                .get_contextual_type_for_object_literal_element(property_assignment, context_flags),
            _ => unimplemented!(),
        }
    }

    pub(super) fn get_contextual_call_signature(
        &self,
        type_: &Type,
        node: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Signature>> {
        let signatures = self.get_signatures_of_type(type_, SignatureKind::Call);
        let applicable_by_arity = filter(signatures, |s| !is_arity_smaller(s, node));
        if applicable_by_arity.len() == 1 {
            applicable_by_arity[0]
        } else {
            self.get_intersected_signatures(applicable_by_arity)
        }
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
                match signature_list {
                    None => {
                        signature_list = Some(vec![signature]);
                    }
                    Some(signature_list) => {
                        if !self.compare_signature_identical(
                            &signature_list[0],
                            signature,
                            false,
                            true,
                            true,
                            compare_types_identical,
                        ) {
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
                signature_list[0]
            } else {
                self.create_union_signature(signature_list[0], signature_list)
            }
        })
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
            let result = self.create_anonymous_type(
                Some(node.symbol()),
                Rc::new(RefCell::new(properties_table)),
                vec![],
                vec![],
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

    pub(super) fn accepts_void(&self, t: &Type) -> bool {
        t.flags().intersects(TypeFlags::Void)
    }

    pub(super) fn get_type_of_parameter(&self, symbol: &Symbol) -> Rc<Type> {
        let type_ = self.get_type_of_symbol(symbol);
        if self.strict_null_checks {
            let declaration = symbol.maybe_value_declaration();
            if matches!(&*declaration, Some(declaration) if has_initializer(&*declaration.upgrade().unwrap()))
            {
                return self.get_optional_type(&type_, None);
            }
        }
        type_
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
                    .filter_type(&type_, TypeChecker::accepts_void)
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
            return self
                .create_type_reference(&global_promise_type, Some(vec![promised_type]))
                .into();
        }

        self.unknown_type()
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
                func_body.as_expression(),
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

            return_type = Some(self.get_union_type(types, Some(UnionReduction::Subtype)));
        }

        if return_type.is_some() || yield_type.is_some() || next_type.is_some() {
            if let Some(yield_type) = yield_type {
                self.report_errors_from_widening(func, yield_type, WideningKind::GeneratorYield);
            }
            if let Some(return_type) = return_type {
                self.report_errors_from_widening(func, return_type, WideningKind::FunctionReturn);
            }
            if let Some(next_type) = next_type {
                self.report_errors_from_widening(func, next_type, WideningKind::GeneratorNext);
            }

            if matches!(return_type, Some(return_type) if self.is_unit_type(&return_type))
                || matches!(yield_type, Some(yield_type) if self.is_unit_type(&yield_type))
                || matches!(next_type, Some(next_type) if self.is_unit_type(&next_type))
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
}
