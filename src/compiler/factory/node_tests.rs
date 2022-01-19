use crate::{NodeInterface, SyntaxKind};

pub fn is_numeric_literal<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::NumericLiteral
}

pub fn is_big_int_literal<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::BigIntLiteral
}

pub fn is_string_literal<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::StringLiteral
}

pub fn is_jsx_text<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxText
}

pub fn is_regular_expression_literal<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::RegularExpressionLiteral
}

pub fn is_no_substituion_template_literal<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::NoSubstitutionTemplateLiteral
}

pub fn is_template_head<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TemplateHead
}

pub fn is_template_middle<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TemplateMiddle
}

pub fn is_template_tail<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TemplateTail
}

pub fn is_dot_dot_dot_token<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::DotDotDotToken
}

pub fn is_comma_token<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::CommaToken
}

pub fn is_plus_token<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PlusToken
}

pub fn is_minus_token<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::MinusToken
}

pub fn is_asterisk_token<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::AsteriskToken
}

pub fn is_exclamation_token<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ExclamationToken
}

pub fn is_question_token<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::QuestionToken
}

pub fn is_colon_token<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ColonToken
}

pub fn is_question_dot_token<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::QuestionDotToken
}

pub fn is_equals_greater_than_token<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::EqualsGreaterThanToken
}

pub fn is_identifier<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Identifier
}

pub fn is_private_identifier<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PrivateIdentifier
}

pub fn is_export_modifier<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ExportKeyword
}

pub fn is_async_modifier<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::AsyncKeyword
}

pub fn is_asserts_keyword<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::AssertsKeyword
}

pub fn is_await_keyword<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::AwaitKeyword
}

pub fn is_readonly_keyword<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ReadonlyKeyword
}

pub fn is_static_modifier<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::StaticKeyword
}

pub fn is_abstract_modifier<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::AbstractKeyword
}

pub fn is_super_keyword<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::SuperKeyword
}

pub fn is_import_keyword<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ImportKeyword
}

pub fn is_qualified_name<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::QualifiedName
}

pub fn is_computed_property_name<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ComputedPropertyName
}

pub fn is_type_parameter_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TypeParameter
}

pub fn is_parameter<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Parameter
}

pub fn is_decorator<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Decorator
}

pub fn is_property_signature<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PropertySignature
}

pub fn is_property_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PropertyDeclaration
}

pub fn is_method_signature<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::MethodSignature
}

pub fn is_method_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::MethodDeclaration
}

pub fn is_class_static_block_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ClassStaticBlockDeclaration
}

pub fn is_constructor_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Constructor
}

pub fn is_get_accessor_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::GetAccessor
}

pub fn is_set_accessor_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::SetAccessor
}

pub fn is_call_signature_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::CallSignature
}

pub fn is_construct_signature_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ConstructSignature
}

pub fn is_index_signature_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::IndexSignature
}

pub fn is_type_predicate_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TypePredicate
}

pub fn is_type_reference_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TypeReference
}

pub fn is_function_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::FunctionType
}

pub fn is_constructor_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ConstructorType
}

pub fn is_type_query_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TypeQuery
}

pub fn is_type_literal_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TypeLiteral
}

pub fn is_array_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ArrayType
}

pub fn is_tuple_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TupleType
}

pub fn is_named_tuple_member<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::NamedTupleMember
}

pub fn is_optional_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::OptionalType
}

pub fn is_rest_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::RestType
}

pub fn is_union_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::UnionType
}

pub fn is_intersection_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::IntersectionType
}

pub fn is_conditional_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ConditionalType
}

pub fn is_infer_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::InferType
}

pub fn is_parenthesized_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ParenthesizedType
}

pub fn is_this_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ThisType
}

pub fn is_type_operator_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TypeOperator
}

pub fn is_indexed_access_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::IndexedAccessType
}

pub fn is_mapped_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::MappedType
}

pub fn is_literal_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::LiteralType
}

pub fn is_import_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ImportType
}

pub fn is_template_literal_type_span<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TemplateLiteralTypeSpan
}

pub fn is_template_literal_type_node<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TemplateLiteralType
}

pub fn is_object_binding_pattern<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ObjectBindingPattern
}

pub fn is_array_binding_pattern<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ArrayBindingPattern
}

pub fn is_binding_element<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::BindingElement
}

pub fn is_array_literal_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ArrayLiteralExpression
}

pub fn is_object_literal_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ObjectLiteralExpression
}

pub fn is_property_access_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PropertyAccessExpression
}

pub fn is_element_access_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ElementAccessExpression
}

pub fn is_call_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::CallExpression
}

pub fn is_new_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::NewExpression
}

pub fn is_tagged_template_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TaggedTemplateExpression
}

pub fn is_type_assertion_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TypeAssertionExpression
}

pub fn is_parenthesized_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ParenthesizedExpression
}

pub fn is_function_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::FunctionExpression
}

pub fn is_arrow_function<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ArrowFunction
}

pub fn is_delete_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::DeleteExpression
}

pub fn is_type_of_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TypeOfExpression
}

pub fn is_void_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::VoidExpression
}

pub fn is_await_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::AwaitExpression
}

pub fn is_prefix_unary_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PrefixUnaryExpression
}

pub fn is_postfix_unary_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PostfixUnaryExpression
}

pub fn is_binary_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::BinaryExpression
}

pub fn is_conditional_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ConditionalExpression
}

pub fn is_template_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TemplateExpression
}

pub fn is_yield_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::YieldExpression
}

pub fn is_spread_element<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::SpreadElement
}

pub fn is_class_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ClassExpression
}

pub fn is_omitted_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::OmittedExpression
}

pub fn is_expression_with_type_arguments<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ExpressionWithTypeArguments
}

pub fn is_as_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::AsExpression
}

pub fn is_non_null_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::NonNullExpression
}

pub fn is_meta_property<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::MetaProperty
}

pub fn is_synthetic_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::SyntheticExpression
}

pub fn is_partially_emitted_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PartiallyEmittedExpression
}

pub fn is_comma_list_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::CommaListExpression
}

pub fn is_template_span<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TemplateSpan
}

pub fn is_semicolon_class_element<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::SemicolonClassElement
}

pub fn is_block<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Block
}

pub fn is_variable_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::VariableStatement
}

pub fn is_empty_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::EmptyStatement
}

pub fn is_expression_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ExpressionStatement
}

pub fn is_if_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::IfStatement
}

pub fn is_do_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::DoStatement
}

pub fn is_while_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::WhileStatement
}

pub fn is_for_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ForStatement
}

pub fn is_for_in_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ForInStatement
}

pub fn is_for_of_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ForOfStatement
}

pub fn is_continue_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ContinueStatement
}

pub fn is_break_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::BreaStatement
}

pub fn is_return_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ReturnStatement
}

pub fn is_with_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::WithStatement
}

pub fn is_switch_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::SwitchStatement
}

pub fn is_labeled_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::LabeledStatement
}

pub fn is_throw_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ThrowStatement
}

pub fn is_try_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TryStatement
}

pub fn is_debugger_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::DebuggerStatement
}

pub fn is_variable_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::VariableDeclaration
}

pub fn is_variable_declaration_list<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::VariableDeclarationList
}

pub fn is_function_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::FunctionDeclaration
}

pub fn is_class_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ClassDeclaration
}

pub fn is_interface_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::InterfaceDeclaration
}

pub fn is_type_alias_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::TypeAliasDeclaration
}

pub fn is_enum_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::EnumDeclaration
}

pub fn is_module_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ModuleDeclaration
}

pub fn is_module_block<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ModuleBlock
}

pub fn is_case_block<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::CaseBlock
}

pub fn is_namespace_export_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::NamespaceExportDeclaration
}

pub fn is_import_equals_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ImportEqualsDeclaration
}

pub fn is_import_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ImportDeclaration
}

pub fn is_import_clause<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ImportClause
}

pub fn is_assert_clause<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::AssertClause
}

pub fn is_assert_entry<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::AssertEntry
}

pub fn is_namespace_import<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::NamespaceImport
}

pub fn is_namespace_export<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::NamespaceExport
}

pub fn is_named_imports<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::NamedImports
}

pub fn is_import_specifier<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ImportSpecifier
}

pub fn is_export_assignment<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ExportAssignment
}

pub fn is_export_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ExportDeclaration
}

pub fn is_named_exports<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::NamedExports
}

pub fn is_export_specifier<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ExportSpecifier
}

pub fn is_missing_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::MissingDeclaration
}

pub fn is_not_emitted_statement<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::NotEmittedStatement
}

pub fn is_synthetic_reference<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::SyntheticReferenceExpression
}

pub fn is_merge_declaration_marker<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::MergeDeclarationMarker
}

pub fn is_end_of_declaration_marker<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::EndOfDeclarationMarker
}

pub fn is_external_module_reference<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ExternalModuleReference
}

pub fn is_jsx_element<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxElement
}

pub fn is_jsx_self_closing_element<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxSelfClosingElement
}

pub fn is_jsx_opening_element<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxOpeningElement
}

pub fn is_jsx_closing_element<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxClosingElement
}

pub fn is_jsx_fragment<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxFragment
}

pub fn is_jsx_opening_fragment<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxOpeningFragment
}

pub fn is_jsx_closing_fragment<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxClosingFragment
}

pub fn is_jsx_attribute<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxAttribute
}

pub fn is_jsx_attributes<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxAttributes
}

pub fn is_jsx_spread_attribute<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxSpreadAttribute
}

pub fn is_jsx_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JsxExpression
}

pub fn is_case_clause<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::CaseClause
}

pub fn is_default_clause<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::DefaultClause
}

pub fn is_heritage_clause<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::HeritageClause
}

pub fn is_catch_clause<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::CatchClause
}

pub fn is_property_assignment<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PropertyAssignment
}

pub fn is_shorthand_property_assignment<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ShorthandPropertyAssignment
}

pub fn is_spread_assignment<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::SpreadAssignment
}

pub fn is_enum_member<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::EnumMember
}

pub fn is_unparsed_prepend<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::UnparsedPrepend
}

pub fn is_source_file<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::SourceFile
}

pub fn is_bundle<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Bundle
}

pub fn is_unparsed_source<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::UnparsedSource
}

pub fn is_jsdoc_type_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocTypeExpression
}

pub fn is_jsdoc_name_reference<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocNameReference
}

pub fn is_jsdoc_member_name<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocMemberName
}

pub fn is_jsdoc_link<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocLink
}

pub fn is_jsdoc_link_code<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocLinkCode
}

pub fn is_jsdoc_link_plain<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocLinkPlain
}

pub fn is_jsdoc_all_type<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocAllType
}

pub fn is_jsdoc_unknown_type<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocUnknownType
}

pub fn is_jsdoc_nullable_type<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocNullableType
}

pub fn is_jsdoc_non_nullable_type<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocNonNullableType
}

pub fn is_jsdoc_optional_type<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocOptionalType
}

pub fn is_jsdoc_function_type<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocFunctionType
}

pub fn is_jsdoc_variadic_type<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocVariadicType
}

pub fn is_jsdoc_namepath_type<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocNamepathType
}

pub fn is_jsdoc<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocComment
}

pub fn is_jsdoc_type_literal<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocTypeLiteral
}

pub fn is_jsdoc_signature<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocSignature
}

pub fn is_jsdoc_augments_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocAugmentsTag
}

pub fn is_jsdoc_author_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocAuthorTag
}

pub fn is_jsdoc_author_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocAuthorTag
}

pub fn is_jsdoc_class_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocClassTag
}

pub fn is_jsdoc_callback_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocCallbackTag
}

pub fn is_jsdoc_public_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocPublicTag
}

pub fn is_jsdoc_private_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocPrivateTag
}

pub fn is_jsdoc_protected_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocProtectedTag
}

pub fn is_jsdoc_readonly_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocReadonlyTag
}

pub fn is_jsdoc_override_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocOverrideTag
}

pub fn is_jsdoc_deprecated_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocDeprecatedTag
}

pub fn is_jsdoc_see_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocSeeTag
}

pub fn is_jsdoc_enum_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocEnumTag
}

pub fn is_jsdoc_parameter_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocParameterTag
}

pub fn is_jsdoc_return_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocReturnTag
}

pub fn is_jsdoc_this_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocThisTag
}

pub fn is_jsdoc_type_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocTypeTag
}

pub fn is_jsdoc_template_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocTemplateTag
}

pub fn is_jsdoc_typedef_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocTypedefTag
}

pub fn is_jsdoc_unknown_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocUnknownTag
}

pub fn is_jsdoc_property_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocPropertyTag
}

pub fn is_jsdoc_implements_tag<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::JSDocImplementsTag
}

pub fn is_syntax_list<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::SyntaxList
}
