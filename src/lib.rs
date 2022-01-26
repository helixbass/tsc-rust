#[macro_use]
extern crate lazy_static;

mod compiler;
mod execute_command_line;
mod rust_helpers;

pub use compiler::binder::bind_source_file;
pub use compiler::checker::{create_type_checker, NodeBuilder};
use compiler::command_line_parser::{
    module_resolution_option_declarations, options_affecting_program_structure,
};
pub use compiler::command_line_parser::{parse_command_line, OptionsNameMap};
pub use compiler::core::{
    add_range, append, append_if_unique, arrays_equal, binary_search, binary_search_copy_key,
    compare_strings_case_insensitive, compare_strings_case_sensitive,
    compare_strings_case_sensitive_maybe, compare_values, concatenate, ends_with,
    equate_strings_case_insensitive, equate_strings_case_sensitive, equate_values, every, filter,
    find, first_defined, first_or_undefined, flat_map, for_each, get_string_comparer,
    insert_sorted, last, last_or_undefined, length, map, maybe_for_each, range_equals, some,
    sort_and_deduplicate, starts_with, string_contains, trim_string_start, GetCanonicalFileName,
};
pub use compiler::core_public::{Comparer, Comparison, MapLike, SortedArray};
pub use compiler::debug::Debug_;
pub use compiler::diagnostic_information_map_generated::Diagnostics;
pub use compiler::emitter::create_printer;
pub use compiler::factory::base_node_factory::{
    create_base_node_factory, BaseNodeFactory, BaseNodeFactoryConcrete,
};
pub use compiler::factory::node_factory::{
    create_node_factory, factory, synthetic_factory, BaseNodeFactorySynthetic, NodeFactoryFlags,
};
pub use compiler::factory::node_tests::{
    is_abstract_modifier, is_array_binding_pattern, is_array_literal_expression,
    is_array_type_node, is_arrow_function, is_as_expression, is_assert_clause, is_assert_entry,
    is_asserts_keyword, is_asterisk_token, is_async_modifier, is_await_expression,
    is_await_keyword, is_big_int_literal, is_binary_expression, is_binding_element, is_block,
    is_break_statement, is_bundle, is_call_expression, is_call_signature_declaration,
    is_case_block, is_case_clause, is_catch_clause, is_class_declaration, is_class_expression,
    is_class_static_block_declaration, is_colon_token, is_comma_list_expression, is_comma_token,
    is_computed_property_name, is_conditional_expression, is_conditional_type_node,
    is_construct_signature_declaration, is_constructor_declaration, is_constructor_type_node,
    is_continue_statement, is_debugger_statement, is_decorator, is_default_clause,
    is_delete_expression, is_do_statement, is_dot_dot_dot_token, is_element_access_expression,
    is_empty_statement, is_end_of_declaration_marker, is_enum_declaration, is_enum_member,
    is_equals_greater_than_token, is_exclamation_token, is_export_assignment,
    is_export_declaration, is_export_modifier, is_export_specifier, is_expression_statement,
    is_expression_with_type_arguments, is_external_module_reference, is_for_in_statement,
    is_for_of_statement, is_for_statement, is_function_declaration, is_function_expression,
    is_function_type_node, is_get_accessor_declaration, is_heritage_clause, is_identifier,
    is_if_statement, is_import_clause, is_import_declaration, is_import_equals_declaration,
    is_import_keyword, is_import_specifier, is_import_type_node, is_index_signature_declaration,
    is_indexed_access_type_node, is_infer_type_node, is_interface_declaration,
    is_intersection_type_node, is_jsdoc, is_jsdoc_all_type, is_jsdoc_augments_tag,
    is_jsdoc_author_tag, is_jsdoc_callback_tag, is_jsdoc_class_tag, is_jsdoc_deprecated_tag,
    is_jsdoc_enum_tag, is_jsdoc_function_type, is_jsdoc_implements_tag, is_jsdoc_link,
    is_jsdoc_link_code, is_jsdoc_link_plain, is_jsdoc_member_name, is_jsdoc_name_reference,
    is_jsdoc_namepath_type, is_jsdoc_non_nullable_type, is_jsdoc_nullable_type,
    is_jsdoc_optional_type, is_jsdoc_override_tag, is_jsdoc_parameter_tag, is_jsdoc_private_tag,
    is_jsdoc_property_tag, is_jsdoc_protected_tag, is_jsdoc_public_tag, is_jsdoc_readonly_tag,
    is_jsdoc_return_tag, is_jsdoc_see_tag, is_jsdoc_signature, is_jsdoc_template_tag,
    is_jsdoc_this_tag, is_jsdoc_type_expression, is_jsdoc_type_literal, is_jsdoc_type_tag,
    is_jsdoc_typedef_tag, is_jsdoc_unknown_tag, is_jsdoc_unknown_type, is_jsdoc_variadic_type,
    is_jsx_attribute, is_jsx_attributes, is_jsx_closing_element, is_jsx_closing_fragment,
    is_jsx_element, is_jsx_expression, is_jsx_fragment, is_jsx_opening_element,
    is_jsx_opening_fragment, is_jsx_self_closing_element, is_jsx_spread_attribute, is_jsx_text,
    is_labeled_statement, is_literal_type_node, is_mapped_type_node, is_merge_declaration_marker,
    is_meta_property, is_method_declaration, is_method_signature, is_minus_token,
    is_missing_declaration, is_module_block, is_module_declaration, is_named_exports,
    is_named_imports, is_named_tuple_member, is_namespace_export, is_namespace_export_declaration,
    is_namespace_import, is_new_expression, is_no_substituion_template_literal,
    is_non_null_expression, is_not_emitted_statement, is_numeric_literal,
    is_object_binding_pattern, is_object_literal_expression, is_omitted_expression,
    is_optional_type_node, is_parameter, is_parenthesized_expression, is_parenthesized_type_node,
    is_partially_emitted_expression, is_plus_token, is_postfix_unary_expression,
    is_prefix_unary_expression, is_private_identifier, is_property_access_expression,
    is_property_assignment, is_property_declaration, is_property_signature, is_qualified_name,
    is_question_dot_token, is_question_token, is_readonly_keyword, is_regular_expression_literal,
    is_rest_type_node, is_return_statement, is_semicolon_class_element,
    is_set_accessor_declaration, is_shorthand_property_assignment, is_source_file,
    is_spread_assignment, is_spread_element, is_static_modifier, is_string_literal,
    is_super_keyword, is_switch_statement, is_syntax_list, is_synthetic_expression,
    is_synthetic_reference, is_tagged_template_expression, is_template_expression,
    is_template_head, is_template_literal_type_node, is_template_literal_type_span,
    is_template_middle, is_template_span, is_template_tail, is_this_type_node, is_throw_statement,
    is_try_statement, is_tuple_type_node, is_type_alias_declaration, is_type_assertion_expression,
    is_type_literal_node, is_type_of_expression, is_type_operator_node,
    is_type_parameter_declaration, is_type_predicate_node, is_type_query_node,
    is_type_reference_node, is_union_type_node, is_unparsed_prepend, is_unparsed_source,
    is_variable_declaration, is_variable_declaration_list, is_variable_statement,
    is_void_expression, is_while_statement, is_with_statement, is_yield_expression,
};
pub use compiler::factory::parenthesizer_rules::{
    create_parenthesizer_rules, null_parenthesizer_rules,
};
pub use compiler::factory::utilities::skip_outer_expressions;
pub use compiler::parser::{create_source_file, for_each_child, MissingNode};
pub use compiler::path::{is_rooted_disk_path, normalize_path, path_is_relative, to_path};
pub use compiler::program::create_program;
use compiler::scanner::{
    compute_line_and_character_of_position, compute_line_of_position, compute_line_starts,
    compute_position_of_line_and_character, get_line_starts, get_lines_between_positions,
    is_identifier_text, is_octal_digit, is_shebang_trivia, is_unicode_identifier_start,
    scan_shebang_trivia, skip_trivia, string_to_token, text_to_keyword_obj,
    token_is_identifier_or_keyword, token_is_identifier_or_keyword_or_greater_than,
    utf16_encode_as_string,
};
pub use compiler::scanner::{
    could_start_trivia, create_scanner, for_each_leading_comment_range,
    for_each_trailing_comment_range, get_leading_comment_ranges,
    get_line_and_character_of_position, get_position_of_line_and_character, get_shebang,
    get_trailing_comment_ranges, is_identifier_part, is_identifier_start, is_line_break,
    is_white_space_like, is_white_space_single_line, reduce_each_leading_comment_range,
    reduce_each_trailing_comment_range, token_to_string, ErrorCallback, Scanner,
};
pub use compiler::sys::{get_sys, System};
pub use compiler::types::{
    maybe_text_char_at_index, str_to_source_text_as_chars, text_char_at_index, text_len,
    text_str_num_chars, text_substring, ArrayLiteralExpression, ArrayTypeNode, AsExpression,
    AssignmentDeclarationKind, BaseBindingLikeDeclaration, BaseDiagnostic,
    BaseDiagnosticRelatedInformation, BaseFunctionLikeDeclaration, BaseGenericNamedDeclaration,
    BaseInterfaceOrClassLikeDeclaration, BaseInterfaceType, BaseIntrinsicType, BaseJSDocTag,
    BaseLiteralLikeNode, BaseLiteralType, BaseNamedDeclaration, BaseNode, BaseObjectType,
    BaseSignatureDeclaration, BaseSymbol, BaseTextRange, BaseTransientSymbol, BaseType,
    BaseUnionOrIntersectionType, BaseVariableLikeDeclaration, BigIntLiteral, BigIntLiteralType,
    BinaryExpression, BindingElement, BindingLikeDeclarationInterface, Block, CallExpression,
    CharacterCodes, CheckFlags, CommandLineOption, CommandLineOptionBase,
    CommandLineOptionInterface, CommandLineOptionMapTypeValue, CommandLineOptionOfBooleanType,
    CommandLineOptionOfCustomType, CommandLineOptionOfListType, CommandLineOptionOfNumberType,
    CommandLineOptionOfStringType, CommentDirective, CommentDirectiveType, CommentKind,
    CommentRange, CompilerHost, CompilerOptions, CompilerOptionsValue, CreateProgramOptions,
    Decorator, Diagnostic, DiagnosticCategory, DiagnosticCollection, DiagnosticInterface,
    DiagnosticMessage, DiagnosticMessageChain, DiagnosticMessageText, DiagnosticRelatedInformation,
    DiagnosticRelatedInformationInterface, DiagnosticWithDetachedLocation, DiagnosticWithLocation,
    ElementAccessExpression, EmitFlags, EmitHint, EmitTextWriter, EmptyStatement, EnumMember,
    ExitStatus, Expression, ExpressionStatement, FreshableIntrinsicType, FunctionDeclaration,
    FunctionLikeDeclarationBase, FunctionLikeDeclarationInterface,
    GenericNamedDeclarationInterface, HasExpressionInterface, HasInitializerInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, Identifier,
    IfStatement, ImportsNotUsedAsValues, InterfaceDeclaration, InterfaceType,
    InterfaceTypeWithDeclaredMembersInterface, InternalSymbolName, IntersectionTypeNode,
    IntrinsicType, IntrinsicTypeInterface, JSDoc, JSDocAugmentsTag, JSDocCallbackTag, JSDocEnumTag,
    JSDocImplementsTag, JSDocPropertyLikeTag, JSDocReturnTag, JSDocSeeTag, JSDocTag,
    JSDocTagInterface, JSDocTemplateTag, JSDocThisTag, JSDocTypeTag, JSDocTypedefTag, JsxAttribute,
    JsxEmit, KeywordTypeNode, LanguageVariant, LineAndCharacter, ListFormat, LiteralLikeNode,
    LiteralLikeNodeInterface, LiteralType, LiteralTypeInterface, LiteralTypeNode, ModifierFlags,
    ModuleDeclaration, ModuleKind, ModuleResolutionHost, ModuleResolutionKind,
    ModuleSpecifierResolutionHost, NamedDeclarationInterface, NewExpression, NewLineKind, Node,
    NodeArray, NodeArrayOrVec, NodeBuilderFlags, NodeCheckFlags, NodeFactory, NodeFlags, NodeId,
    NodeInterface, NodeLinks, NonNullExpression, NumberLiteralType, NumericLiteral, ObjectFlags,
    ObjectFlagsTypeInterface, ObjectLiteralExpression, ObjectType, ObjectTypeInterface,
    OuterExpressionKinds, ParameterDeclaration, ParenthesizedExpression, ParenthesizerRules,
    ParsedCommandLine, PartiallyEmittedExpression, Path, PostfixUnaryExpression,
    PrefixUnaryExpression, Printer, PrinterOptions, Program, PropertyAccessExpression,
    PropertyAssignment, PropertyDeclaration, PropertySignature, PseudoBigInt, QualifiedName,
    ReadonlyTextRange, RelationComparisonResult, ResolvableTypeInterface, ResolvedTypeInterface,
    ReturnStatement, ScriptTarget, ShorthandPropertyAssignment, SignatureDeclarationBase,
    SignatureDeclarationInterface, SourceFile, SourceFileLike, SourceTextAsChars, Statement,
    StringLiteral, StringLiteralType, StringOrNodeArray, StructureIsReused, Symbol, SymbolFlags,
    SymbolFormatFlags, SymbolId, SymbolInterface, SymbolLinks, SymbolTable, SymbolTracker,
    SymbolWriter, SyntaxKind, TemplateExpression, TemplateLiteralLikeNode,
    TemplateLiteralLikeNodeInterface, TemplateSpan, Ternary, TextRange, TextSpan, TokenFlags,
    TransformFlags, TransientSymbol, TransientSymbolInterface, TsConfigOnlyOption, Type,
    TypeAliasDeclaration, TypeAssertion, TypeChecker, TypeCheckerHost, TypeElement, TypeFlags,
    TypeFormatFlags, TypeId, TypeInterface, TypeLiteralNode, TypeMapper, TypeNode, TypeParameter,
    TypeParameterDeclaration, TypePredicateNode, TypeReference, TypeReferenceNode,
    UnderscoreEscapedMap, UnionOrIntersectionType, UnionOrIntersectionTypeInterface,
    UnionReduction, UnionType, UnionTypeNode, VariableDeclaration, VariableDeclarationList,
    VariableLikeDeclarationInterface, VariableStatement, VoidExpression, __String,
};
use compiler::types::{CommandLineOptionType, StringOrDiagnosticMessage};
pub use compiler::utilities::{
    attach_file_to_diagnostics, chain_diagnostic_messages, compare_diagnostics,
    create_detached_diagnostic, create_diagnostic_collection, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, create_symbol_table, create_text_writer,
    declaration_name_to_string, get_binary_operator_precedence, get_check_flags,
    get_declaration_of_kind, get_effective_initializer, get_effective_type_annotation_node,
    get_emit_script_target, get_escaped_text_of_identifier_or_literal,
    get_expression_associativity, get_expression_precedence, get_first_identifier, get_full_width,
    get_jsdoc_comments_and_tags, get_literal_text, get_object_flags, get_operator_associativity,
    get_operator_precedence, get_source_file_of_node, has_dynamic_name, is_block_or_catch_scoped,
    is_external_or_common_js_module, is_in_js_file, is_keyword, is_property_name_literal,
    is_type_alias, is_write_only_access, modifiers_to_flags, node_is_missing, object_allocator,
    parse_pseudo_big_int, position_is_synthesized, pseudo_big_int_to_string, set_parent,
    set_text_range_pos_end, set_value_declaration, using_single_line_string_writer, Associativity,
    GetLiteralTextFlags, OperatorPrecedence,
};
pub use compiler::utilities_public::{
    create_text_span_from_bounds, escape_leading_underscores, get_combined_node_flags,
    get_effective_type_parameter_declarations, get_jsdoc_parameter_tags,
    get_jsdoc_type_parameter_tags, get_jsdoc_type_tag, get_name_of_declaration, has_initializer,
    has_jsdoc_nodes, has_only_expression_initializer, id_text, is_binding_pattern, is_expression,
    is_function_like, is_function_or_module_block, is_literal_kind, is_member_name,
    is_modifier_kind, is_property_name, is_string_literal_like, is_template_literal_kind,
    sort_and_deduplicate_diagnostics, unescape_leading_underscores,
};
use compiler::utilities_public::{
    get_jsdoc_parameter_tags_no_cache, get_jsdoc_type_parameter_tags_no_cache,
    is_left_hand_side_expression, is_named_declaration, skip_partially_emitted_expressions,
};
pub use compiler::watch::emit_files_and_report_errors_and_get_exit_status;
pub use execute_command_line::execute_command_line::execute_command_line;
pub use rust_helpers::is_same_variant;
pub use rust_helpers::number::Number;
pub use rust_helpers::weak_self::WeakSelf;
