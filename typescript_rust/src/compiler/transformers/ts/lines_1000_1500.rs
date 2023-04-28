use std::{borrow::Borrow, ptr};

use gc::Gc;
use itertools::Itertools;

use crate::{
    add_range, get_all_accessor_declarations, get_effective_return_type_node,
    get_first_constructor_with_body, get_original_node_id, get_rest_parameter_element_type,
    get_set_accessor_type_annotation_node, is_async_function, is_class_like, is_expression,
    is_function_like, is_identifier, maybe_map, move_range_past_decorators, node_is_present,
    visit_node, AllAccessorDeclarations, Debug_, EmitFlags, FunctionLikeDeclarationInterface,
    HasTypeInterface, Matches, NamedDeclarationInterface, Node, NodeArray, NodeExt, NodeInterface,
    ScriptTarget, SignatureDeclarationInterface, SyntaxKind, UnwrapOrEmpty,
};

use super::{AllDecorators, TransformTypeScript, USE_NEW_TYPE_METADATA_FORMAT};

impl TransformTypeScript {
    pub(super) fn get_all_decorators_of_class_element(
        &self,
        node: &Node,   /*ClassExpression | ClassDeclaration*/
        member: &Node, /*ClassElement*/
    ) -> Option<AllDecorators> {
        match member.kind() {
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                self.get_all_decorators_of_accessors(node, member)
            }
            SyntaxKind::MethodDeclaration => self.get_all_decorators_of_method(member),
            SyntaxKind::PropertyDeclaration => self.get_all_decorators_of_property(member),
            _ => None,
        }
    }

    pub(super) fn get_all_decorators_of_accessors(
        &self,
        node: &Node,     /*ClassExpression | ClassDeclaration*/
        accessor: &Node, /*AccessorDeclaration*/
    ) -> Option<AllDecorators> {
        if accessor
            .as_function_like_declaration()
            .maybe_body()
            .is_none()
        {
            return None;
        }

        let AllAccessorDeclarations {
            first_accessor,
            second_accessor,
            set_accessor,
            ..
        } = get_all_accessor_declarations(&node.as_class_like_declaration().members(), accessor);
        let first_accessor_with_decorators = if first_accessor.maybe_decorators().is_some() {
            Some(first_accessor.clone())
        } else if second_accessor
            .as_ref()
            .matches(|second_accessor| second_accessor.maybe_decorators().is_some())
        {
            second_accessor.clone()
        } else {
            None
        };
        let first_accessor_with_decorators = first_accessor_with_decorators?;
        if !ptr::eq(accessor, &*first_accessor_with_decorators) {
            return None;
        }

        let decorators = first_accessor_with_decorators.maybe_decorators();
        let parameters = self.get_decorators_of_parameters(set_accessor.as_deref());
        if decorators.is_none() && parameters.is_none() {
            return None;
        }

        Some(AllDecorators {
            decorators: decorators.map(Into::into),
            parameters,
        })
    }

    pub(super) fn get_all_decorators_of_method(
        &self,
        method: &Node, /*MethodDeclaration*/
    ) -> Option<AllDecorators> {
        let method_as_method_declaration = method.as_method_declaration();
        if method_as_method_declaration.maybe_body().is_none() {
            return None;
        }

        let decorators = method.maybe_decorators();
        let parameters = self.get_decorators_of_parameters(Some(method));
        if decorators.is_none() && parameters.is_none() {
            return None;
        }

        Some(AllDecorators {
            decorators: decorators.map(Into::into),
            parameters,
        })
    }

    pub(super) fn get_all_decorators_of_property(
        &self,
        property: &Node, /*PropertyDeclaration*/
    ) -> Option<AllDecorators> {
        let decorators = property.maybe_decorators();
        if decorators.is_none() {
            return None;
        }

        Some(AllDecorators {
            decorators: decorators.map(Into::into),
            parameters: None,
        })
    }

    pub(super) fn transform_all_decorators_of_declaration(
        &self,
        node: &Node,      /*Declaration*/
        container: &Node, /*ClassLikeDeclaration*/
        all_decorators: Option<&AllDecorators>,
    ) -> Option<Vec<Gc<Node>>> {
        let all_decorators = all_decorators?;

        let mut decorator_expressions: Vec<Gc<Node /*Expression*/>> = Default::default();
        add_range(
            &mut decorator_expressions,
            maybe_map(
                all_decorators.decorators.as_deref(),
                |decorator: &Gc<Node>, _| self.transform_decorator(decorator),
            )
            .as_deref(),
            None,
            None,
        );
        add_range(
            &mut decorator_expressions,
            all_decorators
                .parameters
                .as_ref()
                .map(|all_decorators_parameters| {
                    all_decorators_parameters
                        .iter()
                        .enumerate()
                        .flat_map(|(index, parameter)| {
                            self.transform_decorators_of_parameter(parameter.as_deref(), index)
                                .map(IntoIterator::into_iter)
                                .unwrap_or_empty()
                        })
                        .collect_vec()
                })
                .as_deref(),
            None,
            None,
        );
        self.add_type_metadata(node, container, &mut decorator_expressions);
        Some(decorator_expressions)
    }

    pub(super) fn add_class_element_decoration_statements(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ClassDeclaration*/
        is_static: bool,
    ) {
        add_range(
            statements,
            maybe_map(
                self.generate_class_element_decoration_expressions(node, is_static),
                |expression: Gc<Node>, _| self.expression_to_statement(expression),
            )
            .as_deref(),
            None,
            None,
        );
    }

    pub(super) fn generate_class_element_decoration_expressions(
        &self,
        node: &Node, /*ClassExpression | ClassDeclaration*/
        is_static: bool,
    ) -> Option<Vec<Gc<Node>>> {
        let members = self.get_decorated_class_elements(node, is_static);
        let mut expressions: Option<Vec<Gc<Node>>> = Default::default();
        for ref member in members {
            let expression = self.generate_class_element_decoration_expression(node, member);
            if let Some(expression) = expression {
                expressions
                    .get_or_insert_with(|| Default::default())
                    .push(expression);
            }
        }
        expressions
    }

    pub(super) fn generate_class_element_decoration_expression(
        &self,
        node: &Node,   /*ClassExpression | ClassDeclaration*/
        member: &Node, /*ClassElement*/
    ) -> Option<Gc<Node>> {
        let all_decorators = self.get_all_decorators_of_class_element(node, member);
        let decorator_expressions =
            self.transform_all_decorators_of_declaration(member, node, all_decorators.as_ref())?;

        let prefix = self.get_class_member_prefix(node, member);
        let member_name = self.get_expression_for_property_name(member, true);
        let descriptor = if self.language_version > ScriptTarget::ES3 {
            if member.kind() == SyntaxKind::PropertyDeclaration {
                Some(self.factory.create_void_zero())
            } else {
                Some(self.factory.create_null().wrap())
            }
        } else {
            None
        };

        Some(
            self.emit_helpers()
                .create_decorate_helper(
                    &decorator_expressions,
                    &prefix,
                    Some(&*member_name),
                    descriptor,
                )
                .set_text_range(Some(
                    &move_range_past_decorators(member).into_readonly_text_range(),
                ))
                .set_emit_flags(EmitFlags::NoComments),
        )
    }

    pub(super) fn add_constructor_decoration_statement(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ClassDeclaration*/
    ) {
        let expression = self.generate_constructor_decoration_expression(node);
        if let Some(expression) = expression {
            statements.push(
                self.factory
                    .create_expression_statement(expression)
                    .wrap()
                    .set_original_node(Some(node.node_wrapper())),
            );
        }
    }

    pub(super) fn generate_constructor_decoration_expression(
        &self,
        node: &Node, /*ClassExpression | ClassDeclaration*/
    ) -> Option<Gc<Node>> {
        let all_decorators = self.get_all_decorators_of_constructor(node);
        let decorator_expressions =
            self.transform_all_decorators_of_declaration(node, node, all_decorators.as_ref())?;

        let class_alias = self
            .maybe_class_aliases()
            .as_ref()
            .and_then(|class_aliases| class_aliases.get(&get_original_node_id(node)).cloned());

        let local_name = if self.language_version <= ScriptTarget::ES2015 {
            self.factory
                .get_internal_name(node, Some(false), Some(true))
        } else {
            self.factory.get_local_name(node, Some(false), Some(true))
        };
        let decorate = self.emit_helpers().create_decorate_helper(
            &decorator_expressions,
            &local_name,
            Option::<&Node>::None,
            Option::<&Node>::None,
        );
        Some(
            self.factory
                .create_assignment(
                    local_name,
                    class_alias.map_or_else(
                        || decorate.clone(),
                        |class_alias| {
                            self.factory
                                .create_assignment(class_alias, decorate.clone())
                                .wrap()
                        },
                    ),
                )
                .wrap()
                .set_emit_flags(EmitFlags::NoComments)
                .set_source_map_range(Some((&move_range_past_decorators(node)).into())),
        )
    }

    pub(super) fn transform_decorator(&self, decorator: &Node /*Decorator*/) -> Gc<Node> {
        visit_node(
            Some(&*decorator.as_decorator().expression),
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )
        .unwrap()
    }

    pub(super) fn transform_decorators_of_parameter(
        &self,
        decorators: Option<&[Gc<Node /*Decorator*/>]>,
        parameter_offset: usize,
    ) -> Option<Vec<Gc<Node>>> {
        let mut expressions: Option<Vec<Gc<Node /*Expression*/>>> = Default::default();
        if let Some(decorators) = decorators {
            let expressions = expressions.get_or_insert_with(|| Default::default());
            for decorator in decorators {
                let helper = self
                    .emit_helpers()
                    .create_param_helper(self.transform_decorator(decorator), parameter_offset)
                    .set_text_range(Some(&*decorator.as_decorator().expression))
                    .set_emit_flags(EmitFlags::NoComments);
                expressions.push(helper);
            }
        }

        expressions
    }

    pub(super) fn add_type_metadata(
        &self,
        node: &Node,      /*Declaration*/
        container: &Node, /*ClassLikeDeclaration*/
        decorator_expressions: &mut Vec<Gc<Node /*Expression*/>>,
    ) {
        if USE_NEW_TYPE_METADATA_FORMAT {
            self.add_new_type_metadata(node, container, decorator_expressions);
        } else {
            self.add_old_type_metadata(node, container, decorator_expressions);
        }
    }

    pub(super) fn add_old_type_metadata(
        &self,
        node: &Node,      /*Declaration*/
        container: &Node, /*ClassLikeDeclaration*/
        decorator_expressions: &mut Vec<Gc<Node /*Expression*/>>,
    ) {
        if self.compiler_options.emit_decorator_metadata == Some(true) {
            if self.should_add_type_metadata(node) {
                decorator_expressions.push(
                    self.emit_helpers()
                        .create_metadata_helper("design:type", self.serialize_type_of_node(node)),
                );
            }
            if self.should_add_param_types_metadata(node) {
                decorator_expressions.push(self.emit_helpers().create_metadata_helper(
                    "design:paramtypes",
                    self.serialize_parameter_types_of_node(node, container),
                ));
            }
            if self.should_add_return_type_metadata(node) {
                decorator_expressions.push(self.emit_helpers().create_metadata_helper(
                    "design:returntype",
                    self.serialize_return_type_of_node(node),
                ));
            }
        }
    }

    pub(super) fn add_new_type_metadata(
        &self,
        node: &Node,      /*Declaration*/
        container: &Node, /*ClassLikeDeclaration*/
        decorator_expressions: &mut Vec<Gc<Node /*Expression*/>>,
    ) {
        if self.compiler_options.emit_decorator_metadata == Some(true) {
            let mut properties: Option<Vec<Gc<Node /*ObjectLiteralElementLike*/>>> =
                Default::default();
            if self.should_add_type_metadata(node) {
                properties.get_or_insert_with(|| Default::default()).push(
                    self.factory
                        .create_property_assignment(
                            "type",
                            self.factory
                                .create_arrow_function(
                                    Option::<Gc<NodeArray>>::None,
                                    Option::<Gc<NodeArray>>::None,
                                    vec![],
                                    None,
                                    Some(
                                        self.factory
                                            .create_token(SyntaxKind::EqualsGreaterThanToken)
                                            .wrap(),
                                    ),
                                    self.serialize_type_of_node(node),
                                )
                                .wrap(),
                        )
                        .wrap(),
                );
            }
            if self.should_add_param_types_metadata(node) {
                properties.get_or_insert_with(|| Default::default()).push(
                    self.factory
                        .create_property_assignment(
                            "paramTypes",
                            self.factory
                                .create_arrow_function(
                                    Option::<Gc<NodeArray>>::None,
                                    Option::<Gc<NodeArray>>::None,
                                    vec![],
                                    None,
                                    Some(
                                        self.factory
                                            .create_token(SyntaxKind::EqualsGreaterThanToken)
                                            .wrap(),
                                    ),
                                    self.serialize_parameter_types_of_node(node, container),
                                )
                                .wrap(),
                        )
                        .wrap(),
                );
            }
            if self.should_add_return_type_metadata(node) {
                properties.get_or_insert_with(|| Default::default()).push(
                    self.factory
                        .create_property_assignment(
                            "returnType",
                            self.factory
                                .create_arrow_function(
                                    Option::<Gc<NodeArray>>::None,
                                    Option::<Gc<NodeArray>>::None,
                                    vec![],
                                    None,
                                    Some(
                                        self.factory
                                            .create_token(SyntaxKind::EqualsGreaterThanToken)
                                            .wrap(),
                                    ),
                                    self.serialize_return_type_of_node(node),
                                )
                                .wrap(),
                        )
                        .wrap(),
                );
            }
            if let Some(properties) = properties {
                decorator_expressions.push(
                    self.emit_helpers().create_metadata_helper(
                        "design:typeinfo",
                        self.factory
                            .create_object_literal_expression(Some(properties), Some(true))
                            .wrap(),
                    ),
                );
            }
        }
    }

    pub(super) fn should_add_type_metadata(&self, node: &Node /*Declaration*/) -> bool {
        let kind = node.kind();
        matches!(
            kind,
            SyntaxKind::MethodDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor
                | SyntaxKind::PropertyDeclaration
        )
    }

    pub(super) fn should_add_return_type_metadata(&self, node: &Node /*Declaration*/) -> bool {
        node.kind() == SyntaxKind::MethodDeclaration
    }

    pub(super) fn should_add_param_types_metadata(&self, node: &Node /*Declaration*/) -> bool {
        match node.kind() {
            SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => {
                get_first_constructor_with_body(node).is_some()
            }
            SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                true
            }
            _ => false,
        }
    }

    pub(super) fn get_accessor_type_node(
        &self,
        node: &Node, /*AccessorDeclaration*/
    ) -> Option<Gc<Node>> {
        let accessors = self.resolver.get_all_accessor_declarations(node);
        accessors
            .set_accessor
            .as_ref()
            .and_then(|accessors_set_accessor| {
                get_set_accessor_type_annotation_node(accessors_set_accessor)
            })
            .or_else(|| {
                accessors
                    .get_accessor
                    .as_ref()
                    .and_then(|accessors_get_accessor| {
                        get_effective_return_type_node(accessors_get_accessor)
                    })
            })
    }

    pub(super) fn serialize_type_of_node(&self, node: &Node) -> Gc<Node /*SerializedTypeNode*/> {
        match node.kind() {
            SyntaxKind::PropertyDeclaration | SyntaxKind::Parameter => {
                self.serialize_type_node(node.as_has_type().maybe_type())
            }
            SyntaxKind::SetAccessor | SyntaxKind::GetAccessor => {
                self.serialize_type_node(self.get_accessor_type_node(node))
            }
            SyntaxKind::ClassDeclaration
            | SyntaxKind::ClassExpression
            | SyntaxKind::MethodDeclaration => self
                .factory
                .create_identifier("Function", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            _ => self.factory.create_void_zero(),
        }
    }

    pub(super) fn serialize_parameter_types_of_node(
        &self,
        node: &Node,
        container: &Node, /*ClassLikeDeclaration*/
    ) -> Gc<Node /*ArrayLiteralExpression*/> {
        let value_declaration = if is_class_like(node) {
            get_first_constructor_with_body(node)
        } else if is_function_like(Some(node))
            && node_is_present(node.as_function_like_declaration().maybe_body())
        {
            Some(node.node_wrapper())
        } else {
            None
        };

        let mut expressions: Vec<Gc<Node /*SerializedTypeNode*/>> = Default::default();
        if let Some(value_declaration) = value_declaration {
            let parameters =
                self.get_parameters_of_decorated_declaration(&value_declaration, container);
            for (i, parameter) in parameters.iter().enumerate() {
                let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
                if i == 0
                    && is_identifier(&parameter_as_parameter_declaration.name())
                    && parameter_as_parameter_declaration
                        .name()
                        .as_identifier()
                        .escaped_text
                        == "this"
                {
                    continue;
                }
                if parameter_as_parameter_declaration
                    .dot_dot_dot_token
                    .is_some()
                {
                    expressions.push(self.serialize_type_node(get_rest_parameter_element_type(
                        parameter_as_parameter_declaration.maybe_type(),
                    )));
                } else {
                    expressions.push(self.serialize_type_of_node(parameter));
                }
            }
        }

        self.factory
            .create_array_literal_expression(Some(expressions), None)
            .wrap()
    }

    pub(super) fn get_parameters_of_decorated_declaration(
        &self,
        node: &Node,      /*SignatureDeclaration*/
        container: &Node, /*ClassLikeDeclaration*/
    ) -> Gc<NodeArray> {
        if
        /*container &&*/
        node.kind() == SyntaxKind::GetAccessor {
            let AllAccessorDeclarations { set_accessor, .. } = get_all_accessor_declarations(
                &container.as_class_like_declaration().members(),
                node,
            );
            if let Some(set_accessor) = set_accessor {
                return set_accessor.as_set_accessor_declaration().parameters();
            }
        }
        node.as_signature_declaration().parameters()
    }

    pub(super) fn serialize_return_type_of_node(
        &self,
        node: &Node,
    ) -> Gc<Node /*SerializedTypeNode*/> {
        if is_function_like(Some(node)) && node.as_has_type().maybe_type().is_some() {
            return self.serialize_type_node(node.as_has_type().maybe_type());
        } else if is_async_function(node) {
            return self
                .factory
                .create_identifier("Promise", Option::<Gc<NodeArray>>::None, None)
                .wrap();
        }

        self.factory.create_void_zero()
    }

    pub(super) fn serialize_type_node(
        &self,
        node: Option<impl Borrow<Node /*TypeNode*/>>,
    ) -> Gc<Node /*SerializedTypeNode*/> {
        if node.is_none() {
            return self
                .factory
                .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                .wrap();
        }
        let node = node.unwrap();
        let node: &Node = node.borrow();

        match node.kind() {
            SyntaxKind::VoidKeyword | SyntaxKind::UndefinedKeyword | SyntaxKind::NeverKeyword => {
                return self.factory.create_void_zero();
            }

            SyntaxKind::ParenthesizedType => {
                return self.serialize_type_node(Some(&*node.as_parenthesized_type_node().type_));
            }

            SyntaxKind::FunctionType | SyntaxKind::ConstructorType => {
                return self
                    .factory
                    .create_identifier("Function", Option::<Gc<NodeArray>>::None, None)
                    .wrap();
            }

            SyntaxKind::ArrayType | SyntaxKind::TupleType => {
                return self
                    .factory
                    .create_identifier("Array", Option::<Gc<NodeArray>>::None, None)
                    .wrap();
            }

            SyntaxKind::TypePredicate | SyntaxKind::BooleanKeyword => {
                return self
                    .factory
                    .create_identifier("Boolean", Option::<Gc<NodeArray>>::None, None)
                    .wrap();
            }

            SyntaxKind::StringKeyword => {
                return self
                    .factory
                    .create_identifier("String", Option::<Gc<NodeArray>>::None, None)
                    .wrap();
            }

            SyntaxKind::ObjectKeyword => {
                return self
                    .factory
                    .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                    .wrap();
            }

            SyntaxKind::LiteralType => {
                return match node.as_literal_type_node().literal.kind() {
                    SyntaxKind::StringLiteral | SyntaxKind::NoSubstitutionTemplateLiteral => self
                        .factory
                        .create_identifier("String", Option::<Gc<NodeArray>>::None, None)
                        .wrap(),

                    SyntaxKind::PrefixUnaryExpression | SyntaxKind::NumericLiteral => self
                        .factory
                        .create_identifier("Number", Option::<Gc<NodeArray>>::None, None)
                        .wrap(),

                    SyntaxKind::BigIntLiteral => self.get_global_big_int_name_with_fallback(),

                    SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword => self
                        .factory
                        .create_identifier("Boolean", Option::<Gc<NodeArray>>::None, None)
                        .wrap(),

                    SyntaxKind::NullKeyword => self.factory.create_void_zero(),

                    _ => Debug_.fail_bad_syntax_kind(&node.as_literal_type_node().literal, None),
                }
            }

            SyntaxKind::NumberKeyword => {
                return self
                    .factory
                    .create_identifier("Number", Option::<Gc<NodeArray>>::None, None)
                    .wrap();
            }

            SyntaxKind::BigIntKeyword => {
                return self.get_global_big_int_name_with_fallback();
            }

            SyntaxKind::SymbolKeyword => {
                return if self.language_version < ScriptTarget::ES2015 {
                    self.get_global_symbol_name_with_fallback()
                } else {
                    self.factory
                        .create_identifier("Symbol", Option::<Gc<NodeArray>>::None, None)
                        .wrap()
                };
            }

            SyntaxKind::TypeReference => {
                return self.serialize_type_reference_node(node);
            }

            SyntaxKind::IntersectionType | SyntaxKind::UnionType => {
                return self
                    .serialize_type_list(&node.as_union_or_intersection_type_node().types());
            }

            SyntaxKind::ConditionalType => {
                let node_as_conditional_type_node = node.as_conditional_type_node();
                return self.serialize_type_list(&[
                    node_as_conditional_type_node.true_type.clone(),
                    node_as_conditional_type_node.false_type.clone(),
                ]);
            }

            SyntaxKind::TypeOperator => {
                let node_as_type_operator_node = node.as_type_operator_node();
                if node_as_type_operator_node.operator == SyntaxKind::ReadonlyKeyword {
                    return self.serialize_type_node(Some(&*node_as_type_operator_node.type_));
                }
            }

            SyntaxKind::TypeQuery
            | SyntaxKind::IndexedAccessType
            | SyntaxKind::MappedType
            | SyntaxKind::TypeLiteral
            | SyntaxKind::AnyKeyword
            | SyntaxKind::UnknownKeyword
            | SyntaxKind::ThisType
            | SyntaxKind::ImportType => (),

            SyntaxKind::JSDocAllType
            | SyntaxKind::JSDocUnknownType
            | SyntaxKind::JSDocFunctionType
            | SyntaxKind::JSDocVariadicType
            | SyntaxKind::JSDocNamepathType => (),

            SyntaxKind::JSDocNullableType
            | SyntaxKind::JSDocNonNullableType
            | SyntaxKind::JSDocOptionalType => {
                return self.serialize_type_node(node.as_has_type().maybe_type());
            }
            _ => (),
        }

        self.factory
            .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
            .wrap()
    }
}
