

use crate::{Node, NodeInterface, SyntaxKind};

pub fn is_numeric_literal(node: &Node) -> bool {
    node.kind() == SyntaxKind::NumericLiteral
}

pub fn is_big_int_literal(node: &Node) -> bool {
    node.kind() == SyntaxKind::BigIntLiteral
}

pub fn is_string_literal(node: &Node) -> bool {
    node.kind() == SyntaxKind::StringLiteral
}

pub fn is_jsx_text(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxText
}

pub fn is_regular_expression_literal(node: &Node) -> bool {
    node.kind() == SyntaxKind::RegularExpressionLiteral
}

pub fn is_no_substitution_template_literal(node: &Node) -> bool {
    node.kind() == SyntaxKind::NoSubstitutionTemplateLiteral
}

pub fn is_template_head(node: &Node) -> bool {
    node.kind() == SyntaxKind::TemplateHead
}

pub fn is_template_middle(node: &Node) -> bool {
    node.kind() == SyntaxKind::TemplateMiddle
}

pub fn is_template_tail(node: &Node) -> bool {
    node.kind() == SyntaxKind::TemplateTail
}

pub fn is_dot_dot_dot_token(node: &Node) -> bool {
    node.kind() == SyntaxKind::DotDotDotToken
}

pub fn is_comma_token(node: &Node) -> bool {
    node.kind() == SyntaxKind::CommaToken
}

pub fn is_plus_token(node: &Node) -> bool {
    node.kind() == SyntaxKind::PlusToken
}

pub fn is_minus_token(node: &Node) -> bool {
    node.kind() == SyntaxKind::MinusToken
}

pub fn is_asterisk_token(node: &Node) -> bool {
    node.kind() == SyntaxKind::AsteriskToken
}

pub fn is_exclamation_token(node: &Node) -> bool {
    node.kind() == SyntaxKind::ExclamationToken
}

pub fn is_question_token(node: &Node) -> bool {
    node.kind() == SyntaxKind::QuestionToken
}

pub fn is_colon_token(node: &Node) -> bool {
    node.kind() == SyntaxKind::ColonToken
}

pub fn is_question_dot_token(node: &Node) -> bool {
    node.kind() == SyntaxKind::QuestionDotToken
}

pub fn is_equals_greater_than_token(node: &Node) -> bool {
    node.kind() == SyntaxKind::EqualsGreaterThanToken
}

pub fn is_identifier(node: &Node) -> bool {
    node.kind() == SyntaxKind::Identifier
}

pub fn is_private_identifier(node: &Node) -> bool {
    node.kind() == SyntaxKind::PrivateIdentifier
}

pub fn is_export_modifier(node: &Node) -> bool {
    node.kind() == SyntaxKind::ExportKeyword
}

pub fn is_async_modifier(node: &Node) -> bool {
    node.kind() == SyntaxKind::AsyncKeyword
}

pub fn is_asserts_keyword(node: &Node) -> bool {
    node.kind() == SyntaxKind::AssertsKeyword
}

pub fn is_await_keyword(node: &Node) -> bool {
    node.kind() == SyntaxKind::AwaitKeyword
}

pub fn is_readonly_keyword(node: &Node) -> bool {
    node.kind() == SyntaxKind::ReadonlyKeyword
}

pub fn is_static_modifier(node: &Node) -> bool {
    node.kind() == SyntaxKind::StaticKeyword
}

pub fn is_abstract_modifier(node: &Node) -> bool {
    node.kind() == SyntaxKind::AbstractKeyword
}

pub fn is_super_keyword(node: &Node) -> bool {
    node.kind() == SyntaxKind::SuperKeyword
}

pub fn is_import_keyword(node: &Node) -> bool {
    node.kind() == SyntaxKind::ImportKeyword
}

pub fn is_qualified_name(node: &Node) -> bool {
    node.kind() == SyntaxKind::QualifiedName
}

pub fn is_computed_property_name(node: &Node) -> bool {
    node.kind() == SyntaxKind::ComputedPropertyName
}

pub fn is_type_parameter_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::TypeParameter
}

pub fn is_parameter(node: &Node) -> bool {
    node.kind() == SyntaxKind::Parameter
}

pub fn is_decorator(node: &Node) -> bool {
    node.kind() == SyntaxKind::Decorator
}

pub fn is_property_signature(node: &Node) -> bool {
    node.kind() == SyntaxKind::PropertySignature
}

pub fn is_property_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::PropertyDeclaration
}

pub fn is_method_signature(node: &Node) -> bool {
    node.kind() == SyntaxKind::MethodSignature
}

pub fn is_method_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::MethodDeclaration
}

pub fn is_class_static_block_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::ClassStaticBlockDeclaration
}

pub fn is_constructor_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::Constructor
}

pub fn is_get_accessor_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::GetAccessor
}

pub fn is_set_accessor_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::SetAccessor
}

pub fn is_call_signature_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::CallSignature
}

pub fn is_construct_signature_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::ConstructSignature
}

pub fn is_index_signature_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::IndexSignature
}

pub fn is_type_predicate_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::TypePredicate
}

pub fn is_type_reference_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::TypeReference
}

pub fn is_function_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::FunctionType
}

pub fn is_constructor_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::ConstructorType
}

pub fn is_type_query_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::TypeQuery
}

pub fn is_type_literal_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::TypeLiteral
}

pub fn is_array_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::ArrayType
}

pub fn is_tuple_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::TupleType
}

pub fn is_named_tuple_member(node: &Node) -> bool {
    node.kind() == SyntaxKind::NamedTupleMember
}

pub fn is_optional_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::OptionalType
}

pub fn is_rest_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::RestType
}

pub fn is_union_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::UnionType
}

pub fn is_intersection_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::IntersectionType
}

pub fn is_conditional_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::ConditionalType
}

pub fn is_infer_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::InferType
}

pub fn is_parenthesized_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::ParenthesizedType
}

pub fn is_this_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::ThisType
}

pub fn is_type_operator_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::TypeOperator
}

pub fn is_indexed_access_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::IndexedAccessType
}

pub fn is_mapped_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::MappedType
}

pub fn is_literal_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::LiteralType
}

pub fn is_import_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::ImportType
}

pub fn is_template_literal_type_span(node: &Node) -> bool {
    node.kind() == SyntaxKind::TemplateLiteralTypeSpan
}

pub fn is_template_literal_type_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::TemplateLiteralType
}

pub fn is_object_binding_pattern(node: &Node) -> bool {
    node.kind() == SyntaxKind::ObjectBindingPattern
}

pub fn is_array_binding_pattern(node: &Node) -> bool {
    node.kind() == SyntaxKind::ArrayBindingPattern
}

pub fn is_binding_element(node: &Node) -> bool {
    node.kind() == SyntaxKind::BindingElement
}

pub fn is_array_literal_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::ArrayLiteralExpression
}

pub fn is_object_literal_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::ObjectLiteralExpression
}

pub fn is_property_access_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::PropertyAccessExpression
}

pub fn is_element_access_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::ElementAccessExpression
}

pub fn is_call_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::CallExpression
}

pub fn is_new_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::NewExpression
}

pub fn is_tagged_template_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::TaggedTemplateExpression
}

pub fn is_type_assertion_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::TypeAssertionExpression
}

pub fn is_parenthesized_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::ParenthesizedExpression
}

pub fn is_function_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::FunctionExpression
}

pub fn is_arrow_function(node: &Node) -> bool {
    node.kind() == SyntaxKind::ArrowFunction
}

pub fn is_delete_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::DeleteExpression
}

pub fn is_type_of_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::TypeOfExpression
}

pub fn is_void_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::VoidExpression
}

pub fn is_await_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::AwaitExpression
}

pub fn is_prefix_unary_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::PrefixUnaryExpression
}

pub fn is_postfix_unary_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::PostfixUnaryExpression
}

pub fn is_binary_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::BinaryExpression
}

pub fn is_conditional_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::ConditionalExpression
}

pub fn is_template_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::TemplateExpression
}

pub fn is_yield_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::YieldExpression
}

pub fn is_spread_element(node: &Node) -> bool {
    node.kind() == SyntaxKind::SpreadElement
}

pub fn is_class_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::ClassExpression
}

pub fn is_omitted_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::OmittedExpression
}

pub fn is_expression_with_type_arguments(node: &Node) -> bool {
    node.kind() == SyntaxKind::ExpressionWithTypeArguments
}

pub fn is_as_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::AsExpression
}

pub fn is_non_null_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::NonNullExpression
}

pub fn is_meta_property(node: &Node) -> bool {
    node.kind() == SyntaxKind::MetaProperty
}

pub fn is_synthetic_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::SyntheticExpression
}

pub fn is_partially_emitted_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::PartiallyEmittedExpression
}

pub fn is_comma_list_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::CommaListExpression
}

pub fn is_template_span(node: &Node) -> bool {
    node.kind() == SyntaxKind::TemplateSpan
}

pub fn is_semicolon_class_element(node: &Node) -> bool {
    node.kind() == SyntaxKind::SemicolonClassElement
}

pub fn is_block(node: &Node) -> bool {
    node.kind() == SyntaxKind::Block
}

pub fn is_variable_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::VariableStatement
}

pub fn is_empty_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::EmptyStatement
}

pub fn is_expression_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::ExpressionStatement
}

pub fn is_if_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::IfStatement
}

pub fn is_do_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::DoStatement
}

pub fn is_while_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::WhileStatement
}

pub fn is_for_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::ForStatement
}

pub fn is_for_in_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::ForInStatement
}

pub fn is_for_of_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::ForOfStatement
}

pub fn is_continue_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::ContinueStatement
}

pub fn is_break_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::BreakStatement
}

pub fn is_return_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::ReturnStatement
}

pub fn is_with_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::WithStatement
}

pub fn is_switch_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::SwitchStatement
}

pub fn is_labeled_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::LabeledStatement
}

pub fn is_throw_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::ThrowStatement
}

pub fn is_try_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::TryStatement
}

pub fn is_debugger_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::DebuggerStatement
}

pub fn is_variable_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::VariableDeclaration
}

pub fn is_variable_declaration_list(node: &Node) -> bool {
    node.kind() == SyntaxKind::VariableDeclarationList
}

pub fn is_function_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::FunctionDeclaration
}

pub fn is_class_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::ClassDeclaration
}

pub fn is_interface_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::InterfaceDeclaration
}

pub fn is_type_alias_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::TypeAliasDeclaration
}

pub fn is_enum_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::EnumDeclaration
}

pub fn is_module_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::ModuleDeclaration
}

pub fn is_module_block(node: &Node) -> bool {
    node.kind() == SyntaxKind::ModuleBlock
}

pub fn is_case_block(node: &Node) -> bool {
    node.kind() == SyntaxKind::CaseBlock
}

pub fn is_namespace_export_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::NamespaceExportDeclaration
}

pub fn is_import_equals_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::ImportEqualsDeclaration
}

pub fn is_import_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::ImportDeclaration
}

pub fn is_import_clause(node: &Node) -> bool {
    node.kind() == SyntaxKind::ImportClause
}

pub fn is_assert_clause(node: &Node) -> bool {
    node.kind() == SyntaxKind::AssertClause
}

pub fn is_assert_entry(node: &Node) -> bool {
    node.kind() == SyntaxKind::AssertEntry
}

pub fn is_namespace_import(node: &Node) -> bool {
    node.kind() == SyntaxKind::NamespaceImport
}

pub fn is_namespace_export(node: &Node) -> bool {
    node.kind() == SyntaxKind::NamespaceExport
}

pub fn is_named_imports(node: &Node) -> bool {
    node.kind() == SyntaxKind::NamedImports
}

pub fn is_import_specifier(node: &Node) -> bool {
    node.kind() == SyntaxKind::ImportSpecifier
}

pub fn is_export_assignment(node: &Node) -> bool {
    node.kind() == SyntaxKind::ExportAssignment
}

pub fn is_export_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::ExportDeclaration
}

pub fn is_named_exports(node: &Node) -> bool {
    node.kind() == SyntaxKind::NamedExports
}

pub fn is_export_specifier(node: &Node) -> bool {
    node.kind() == SyntaxKind::ExportSpecifier
}

pub fn is_missing_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::MissingDeclaration
}

pub fn is_not_emitted_statement(node: &Node) -> bool {
    node.kind() == SyntaxKind::NotEmittedStatement
}

pub fn is_synthetic_reference(node: &Node) -> bool {
    node.kind() == SyntaxKind::SyntheticReferenceExpression
}

pub fn is_merge_declaration_marker(node: &Node) -> bool {
    node.kind() == SyntaxKind::MergeDeclarationMarker
}

pub fn is_end_of_declaration_marker(node: &Node) -> bool {
    node.kind() == SyntaxKind::EndOfDeclarationMarker
}

pub fn is_external_module_reference(node: &Node) -> bool {
    node.kind() == SyntaxKind::ExternalModuleReference
}

pub fn is_jsx_element(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxElement
}

pub fn is_jsx_self_closing_element(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxSelfClosingElement
}

pub fn is_jsx_opening_element(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxOpeningElement
}

pub fn is_jsx_closing_element(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxClosingElement
}

pub fn is_jsx_fragment(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxFragment
}

pub fn is_jsx_opening_fragment(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxOpeningFragment
}

pub fn is_jsx_closing_fragment(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxClosingFragment
}

pub fn is_jsx_attribute(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxAttribute
}

pub fn is_jsx_attributes(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxAttributes
}

pub fn is_jsx_spread_attribute(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxSpreadAttribute
}

pub fn is_jsx_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::JsxExpression
}

pub fn is_case_clause(node: &Node) -> bool {
    node.kind() == SyntaxKind::CaseClause
}

pub fn is_default_clause(node: &Node) -> bool {
    node.kind() == SyntaxKind::DefaultClause
}

pub fn is_heritage_clause(node: &Node) -> bool {
    node.kind() == SyntaxKind::HeritageClause
}

pub fn is_catch_clause(node: &Node) -> bool {
    node.kind() == SyntaxKind::CatchClause
}

pub fn is_property_assignment(node: &Node) -> bool {
    node.kind() == SyntaxKind::PropertyAssignment
}

pub fn is_shorthand_property_assignment(node: &Node) -> bool {
    node.kind() == SyntaxKind::ShorthandPropertyAssignment
}

pub fn is_spread_assignment(node: &Node) -> bool {
    node.kind() == SyntaxKind::SpreadAssignment
}

pub fn is_enum_member(node: &Node) -> bool {
    node.kind() == SyntaxKind::EnumMember
}

pub fn is_unparsed_prepend(node: &Node) -> bool {
    node.kind() == SyntaxKind::UnparsedPrepend
}

pub fn is_source_file(node: &Node) -> bool {
    node.kind() == SyntaxKind::SourceFile
}

pub fn is_bundle(node: &Node) -> bool {
    node.kind() == SyntaxKind::Bundle
}

pub fn is_unparsed_source(node: &Node) -> bool {
    node.kind() == SyntaxKind::UnparsedSource
}

pub fn is_jsdoc_type_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocTypeExpression
}

pub fn is_jsdoc_name_reference(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocNameReference
}

pub fn is_jsdoc_member_name(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocMemberName
}

pub fn is_jsdoc_link(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocLink
}

pub fn is_jsdoc_link_code(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocLinkCode
}

pub fn is_jsdoc_link_plain(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocLinkPlain
}

pub fn is_jsdoc_all_type(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocAllType
}

pub fn is_jsdoc_unknown_type(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocUnknownType
}

pub fn is_jsdoc_nullable_type(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocNullableType
}

pub fn is_jsdoc_non_nullable_type(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocNonNullableType
}

pub fn is_jsdoc_optional_type(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocOptionalType
}

pub fn is_jsdoc_function_type(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocFunctionType
}

pub fn is_jsdoc_variadic_type(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocVariadicType
}

pub fn is_jsdoc_namepath_type(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocNamepathType
}

pub fn is_jsdoc(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocComment
}

pub fn is_jsdoc_type_literal(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocTypeLiteral
}

pub fn is_jsdoc_signature(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocSignature
}

pub fn is_jsdoc_augments_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocAugmentsTag
}

pub fn is_jsdoc_author_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocAuthorTag
}

pub fn is_jsdoc_class_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocClassTag
}

pub fn is_jsdoc_callback_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocCallbackTag
}

pub fn is_jsdoc_public_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocPublicTag
}

pub fn is_jsdoc_private_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocPrivateTag
}

pub fn is_jsdoc_protected_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocProtectedTag
}

pub fn is_jsdoc_readonly_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocReadonlyTag
}

pub fn is_jsdoc_override_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocOverrideTag
}

pub fn is_jsdoc_deprecated_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocDeprecatedTag
}

pub fn is_jsdoc_see_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocSeeTag
}

pub fn is_jsdoc_enum_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocEnumTag
}

pub fn is_jsdoc_parameter_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocParameterTag
}

pub fn is_jsdoc_return_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocReturnTag
}

pub fn is_jsdoc_this_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocThisTag
}

pub fn is_jsdoc_type_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocTypeTag
}

pub fn is_jsdoc_template_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocTemplateTag
}

pub fn is_jsdoc_typedef_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocTypedefTag
}

pub fn is_jsdoc_unknown_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocTag
}

pub fn is_jsdoc_property_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocPropertyTag
}

pub fn is_jsdoc_implements_tag(node: &Node) -> bool {
    node.kind() == SyntaxKind::JSDocImplementsTag
}

pub fn is_syntax_list(node: &Node) -> bool {
    node.kind() == SyntaxKind::SyntaxList
}
