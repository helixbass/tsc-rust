#[macro_use]
extern crate lazy_static;

mod compiler;
mod execute_command_line;
mod rust_helpers;

pub use compiler::binder::{bind_source_file, get_module_instance_state, ModuleInstanceState};
pub use compiler::builder::ProgramBuildInfo;
pub use compiler::builder_public::{
    BuilderProgram, EmitAndSemanticDiagnosticsBuilderProgram, SemanticDiagnosticsBuilderProgram,
};
pub use compiler::checker::{create_type_checker, get_node_id, get_symbol_id, NodeBuilder};
use compiler::checker::{
    DuplicateInfoForFiles, DuplicateInfoForSymbol, IterationTypesResolver, TypeSystemEntity,
    TypeSystemPropertyName,
};
use compiler::command_line_parser::{
    build_opts, convert_to_object_worker, convert_to_options_with_absolute_paths,
    convert_to_tsconfig, get_diagnostic_text, module_resolution_option_declarations,
    option_declarations, options_affecting_program_structure, options_for_build, options_for_watch,
    parse_build_command, target_option_declaration, JsonConversionNotifier,
    JsonConversionNotifierDummy, ParsedBuildCommand, StringOrRcDiagnostic,
};
pub use compiler::command_line_parser::{
    get_parsed_command_line_of_config_file, hash_map_to_compiler_options, parse_command_line,
    ConfigFileDiagnosticsReporter, ConvertToTSConfigHost, DiagnosticReporter,
    ExtendedConfigCacheEntry, OptionsNameMap, ParseCommandLineWorkerDiagnostics,
    ParseConfigFileHost,
};
pub use compiler::core::{
    add_range, append, append_if_unique_rc, array_to_map, arrays_equal, binary_search,
    binary_search_copy_key, cast, compare_strings_case_insensitive, compare_strings_case_sensitive,
    compare_strings_case_sensitive_maybe, compare_values, concatenate, contains, contains_rc,
    count_where, create_get_canonical_file_name, create_multi_map, deduplicate_rc, ends_with,
    equate_strings_case_insensitive, equate_strings_case_sensitive, equate_values, every, filter,
    filter_mutate, filter_owning, find, find_best_pattern_match, find_index, find_last,
    find_last_index, first, first_defined, first_or_undefined, flat_map, flat_map_to_mutable,
    flatten, for_each, for_each_bool, get_or_update, get_ranges_where, get_spelling_suggestion,
    get_string_comparer, identity_str_to_cow, identity_str_to_owned, insert_sorted, last,
    last_or_undefined, length, map, map_defined, maybe_append_if_unique_rc, maybe_concatenate,
    maybe_every, maybe_first_defined, maybe_for_each, maybe_for_each_bool, not_implemented,
    pad_left, pad_right, push_if_unique_rc, range_equals_rc, reduce_left, remove_prefix, same_map,
    set_ui_locale, single_element_array, single_or_undefined, some, sort, sort_and_deduplicate,
    starts_with, string_contains, sum, to_file_name_lower_case, trim_string, trim_string_start,
    try_cast, try_to_add_to_set, AssertionLevel, GetCanonicalFileName, MultiMap, Pattern,
};
pub use compiler::core_public::{
    version, Comparer, Comparison, MapLike, Push, ReadonlyCollection, SortedArray,
};
pub use compiler::debug::{Debug_, LogLevel, LoggingHost};
pub use compiler::diagnostic_information_map_generated::Diagnostics;
pub use compiler::emitter::create_printer;
pub use compiler::factory::base_node_factory::{
    create_base_node_factory, BaseNodeFactory, BaseNodeFactoryConcrete,
};
pub use compiler::factory::emit_helpers::{create_emit_helper_factory, EmitHelperFactory};
pub use compiler::factory::emit_node::{
    add_synthetic_leading_comment, dispose_emit_nodes, set_emit_flags,
};
use compiler::factory::emit_node::{get_starts_on_new_line, set_starts_on_new_line};
pub use compiler::factory::node_converters::{create_node_converters, null_node_converters};
pub use compiler::factory::node_factory::{
    create_node_factory, factory, set_original_node, synthetic_factory, BaseNodeFactorySynthetic,
    NodeFactoryFlags, StringOrNumber, StringOrNumberOrBoolOrRcNode, StringOrRcNode,
    SyntaxKindOrRcNode,
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
    is_namespace_import, is_new_expression, is_no_substitution_template_literal,
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
use compiler::factory::utilities::get_jsdoc_type_alias_name;
pub use compiler::factory::utilities::{
    get_elements_of_binding_or_assignment_pattern, get_target_of_binding_or_assignment_element,
    is_comma_sequence, is_local_name, is_outer_expression, skip_outer_expressions,
    starts_with_use_strict,
};
pub use compiler::factory::utilities_public::set_text_range;
use compiler::module_name_resolver::{
    create_mode_aware_cache, get_types_package_name, mangle_scoped_package_name,
};
pub use compiler::module_name_resolver::{
    node_module_name_resolver, ModeAwareCache, ModuleResolutionCache,
};
pub use compiler::parser::{
    create_source_file, for_each_child, for_each_child_bool, for_each_child_recursively_bool,
    for_each_child_returns, is_external_module, parse_base_node_factory,
    parse_isolated_entity_name, parse_json_text, parse_node_factory, IncrementalParser,
    IncrementalParserSyntaxCursor, IncrementalParserSyntaxCursorInterface,
    IncrementalParserSyntaxCursorReparseTopLevelAwait, IncrementalParserType,
    ParsedIsolatedJSDocComment, ParsedJSDocTypeExpression, ParserType,
};
use compiler::parser::{
    is_declaration_file_name, is_jsdoc_like_text, process_comment_pragmas,
    process_pragmas_into_fields, tag_names_are_equivalent, PragmaContext,
};
pub use compiler::path::{
    alt_directory_separator, change_any_extension, combine_paths, compare_paths,
    compare_paths_case_insensitive, compare_paths_case_sensitive, contains_path,
    convert_to_relative_path, directory_separator, ensure_path_is_non_module_name,
    ensure_trailing_directory_separator, file_extension_is, file_extension_is_one_of,
    for_each_ancestor_directory, for_each_ancestor_directory_str, get_any_extension_from_path,
    get_base_file_name, get_directory_path, get_normalized_absolute_path,
    get_normalized_absolute_path_without_root, get_normalized_path_components, get_path_components,
    get_path_components_relative_to, get_path_from_path_components,
    get_relative_path_from_directory, get_relative_path_from_file,
    get_relative_path_to_directory_or_url, get_root_length, has_extension,
    has_trailing_directory_separator, is_any_directory_separator, is_disk_path_root,
    is_node_modules_directory, is_rooted_disk_path, is_url, normalize_path,
    normalize_path_and_parts, normalize_slashes, path_is_absolute, path_is_bare_specifier,
    path_is_relative, reduce_path_components, remove_trailing_directory_separator, resolve_path,
    starts_with_directory, to_path,
};
use compiler::program::{
    change_compiler_host_like_to_use_cache, create_compiler_host_worker, format_color_and_reset,
    get_mode_for_resolution_at_index, get_mode_for_usage_location, get_referenced_file_location,
    is_reference_file_location, is_referenced_file, ForegroundColorEscapeSequences,
    ReferenceFileLocation, ReferenceFileLocationOrSyntheticReferenceFileLocation,
    SyntheticReferenceFileLocation,
};
pub use compiler::program::{
    create_program, find_config_file, flatten_diagnostic_message_text, format_diagnostic,
    format_diagnostics_with_color_and_context, get_config_file_parsing_diagnostics,
    get_resolution_diagnostic, FormatDiagnosticsHost,
};
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
use compiler::sys::{generate_djb2_hash, missing_file_modified_time};
pub use compiler::sys::{get_sys, System};
pub use compiler::tracing::{dump_tracing_legend, start_tracing};
pub use compiler::transformer::null_transformation_context;
pub use compiler::transformers::{
    chain_bundle, transform_class_fields, transform_declarations, transform_ecmascript_module,
    transform_es2015, transform_es2016, transform_es2017, transform_es2018, transform_es2019,
    transform_es2020, transform_es2021, transform_es5, transform_esnext, transform_generators,
    transform_jsx, transform_module, transform_node_module, transform_system_module,
    transform_type_script,
};
pub use compiler::tsbuild_public::{
    create_builder_status_reporter, create_solution_builder, create_solution_builder_host,
    create_solution_builder_with_watch, create_solution_builder_with_watch_host, BuildOptions,
    ReportEmitErrorSummary, SolutionBuilderHostBase,
};
use compiler::types::{
    diagnostic_category_name, AccessFlags, CommandLineOptionType, CommentDirectivesMap, EmitNode,
    ExternalEmitHelpers, FileIncludeKind, FileIncludeReason, IterationTypes, PragmaArgumentType,
    ReadonlyPragmaMap, ReferencedFile, StringOrDiagnosticMessage,
};
pub use compiler::types::{
    extend_compiler_options, extend_watch_options, maybe_extend_compiler_options,
    maybe_text_char_at_index, str_to_source_text_as_chars, text_char_at_index, text_len,
    text_str_num_chars, text_substring, AllAccessorDeclarations, AlternateModeDiagnostics,
    ArrayBindingPattern, ArrayLiteralExpression, ArrayTypeNode, ArrowFunction, AsExpression,
    AssertClause, AssertEntry, AssignmentDeclarationKind, AwaitExpression,
    BaseBindingLikeDeclaration, BaseDiagnostic, BaseDiagnosticRelatedInformation,
    BaseFunctionLikeDeclaration, BaseGenericNamedDeclaration, BaseInterfaceOrClassLikeDeclaration,
    BaseInterfaceType, BaseIntrinsicType, BaseJSDocTag, BaseJSDocTypeLikeTag, BaseJSDocUnaryType,
    BaseLiteralLikeNode, BaseLiteralType, BaseNamedDeclaration, BaseNode, BaseObjectType,
    BaseSignatureDeclaration, BaseSymbol, BaseTextRange, BaseTransientSymbol, BaseType,
    BaseUnionOrIntersectionType, BaseUnparsedNode, BaseVariableLikeDeclaration, BigIntLiteral,
    BigIntLiteralType, BinaryExpression, BindingElement, BindingLikeDeclarationInterface, Block,
    BreakStatement, Bundle, CallExpression, CallSignatureDeclaration, CancellationToken,
    CancellationTokenDebuggable, CaseBlock, CaseClause, CatchClause, CharacterCodes, CheckFlags,
    ClassDeclaration, ClassExpression, ClassLikeDeclarationBase, ClassLikeDeclarationInterface,
    ClassStaticBlockDeclaration, CommaListExpression, CommandLineOption, CommandLineOptionBase,
    CommandLineOptionInterface, CommandLineOptionMapTypeValue, CommandLineOptionOfBooleanType,
    CommandLineOptionOfCustomType, CommandLineOptionOfListType, CommandLineOptionOfNumberType,
    CommandLineOptionOfStringType, CommentDirective, CommentDirectiveType, CommentKind,
    CommentRange, CompilerHost, CompilerOptions, CompilerOptionsBuilder, CompilerOptionsValue,
    ComputedPropertyName, ConditionalExpression, ConditionalRoot, ConditionalType,
    ConditionalTypeNode, ConfigFileSpecs, ConstructSignatureDeclaration, ConstructorDeclaration,
    ConstructorTypeNode, ContextFlags, ContinueStatement, CoreTransformationContext,
    CreateProgramOptions, CustomTransformer, CustomTransformerFactory, CustomTransformers,
    DebuggerStatement, Decorator, DefaultClause, DeleteExpression, Diagnostic, DiagnosticCategory,
    DiagnosticCollection, DiagnosticInterface, DiagnosticMessage, DiagnosticMessageChain,
    DiagnosticMessageText, DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface,
    DiagnosticWithDetachedLocation, DiagnosticWithLocation, DidYouMeanOptionsDiagnostics,
    DoStatement, ElementAccessExpression, ElementFlags, EmitFlags, EmitHelper, EmitHelperBase,
    EmitHint, EmitHost, EmitResolver, EmitResolverDebuggable, EmitResult, EmitTextWriter,
    EmitTransformers, EmptyStatement, EnumDeclaration, EnumKind, EnumMember, ExitStatus,
    ExportAssignment, ExportDeclaration, ExportSpecifier, ExpressionStatement,
    ExpressionWithTypeArguments, Extension, ExternalModuleReference, FileExtensionInfo,
    FileReference, FlowAssignment, FlowCall, FlowCondition, FlowFlags, FlowLabel, FlowNode,
    FlowNodeBase, FlowReduceLabel, FlowStart, FlowSwitchClause, FlowType, ForInStatement,
    ForOfStatement, ForStatement, FreshableIntrinsicType, FunctionDeclaration, FunctionExpression,
    FunctionLikeDeclarationBase, FunctionLikeDeclarationInterface, FunctionTypeNode,
    GeneratedIdentifierFlags, GenericNamedDeclarationInterface, GenericTypeInterface,
    GenericableTypeInterface, GetAccessorDeclaration, HasConditionInterface, HasElementsInterface,
    HasExpressionInterface, HasInitializerInterface, HasIsTypeOnlyInterface,
    HasJSDocDotPosInterface, HasLabelInterface, HasPropertiesInterface, HasPropertyNameInterface,
    HasQuestionDotTokenInterface, HasQuestionTokenInterface, HasStatementInterface,
    HasStatementsInterface, HasTypeArgumentsInterface, HasTypeInterface,
    HasTypeParametersInterface, HeritageClause, Identifier, IfStatement, ImportClause,
    ImportDeclaration, ImportEqualsDeclaration, ImportSpecifier, ImportTypeNode,
    ImportsNotUsedAsValues, IndexInfo, IndexKind, IndexSignatureDeclaration, IndexType,
    IndexedAccessType, IndexedAccessTypeNode, InferTypeNode, InputFiles, InterfaceDeclaration,
    InterfaceOrClassLikeDeclarationInterface, InterfaceType, InterfaceTypeInterface,
    InterfaceTypeWithDeclaredMembersInterface, InternalSymbolName, IntersectionType,
    IntersectionTypeNode, IntrinsicType, IntrinsicTypeInterface, JSDoc, JSDocAugmentsTag,
    JSDocCallbackTag, JSDocFunctionType, JSDocImplementsTag, JSDocLink, JSDocLinkCode,
    JSDocLinkLikeInterface, JSDocLinkPlain, JSDocMemberName, JSDocNameReference,
    JSDocNamespaceDeclaration, JSDocPropertyLikeTag, JSDocSeeTag, JSDocSignature,
    JSDocTagInterface, JSDocTemplateTag, JSDocText, JSDocTypeExpression, JSDocTypeLikeTagInterface,
    JSDocTypeLiteral, JSDocTypedefOrCallbackTagInterface, JSDocTypedefTag, JsxAttribute,
    JsxAttributes, JsxClosingElement, JsxClosingFragment, JsxElement, JsxEmit, JsxExpression,
    JsxFragment, JsxOpeningElement, JsxOpeningFragment, JsxSelfClosingElement, JsxSpreadAttribute,
    JsxText, KeywordTypeNode, LabeledStatement, LanguageVariant, LexicalEnvironmentFlags,
    LineAndCharacter, ListFormat, LiteralLikeNodeInterface, LiteralType, LiteralTypeInterface,
    LiteralTypeNode, MappedSymbol, MappedTypeNode, MetaProperty, MethodDeclaration,
    MethodSignature, MissingDeclaration, ModifierFlags, ModifiersArray, ModuleBlock,
    ModuleDeclaration, ModuleKind, ModuleResolutionHost, ModuleResolutionKind,
    ModuleSpecifierResolutionHost, NamedDeclarationInterface, NamedExports, NamedImports,
    NamedTupleMember, NamespaceExport, NamespaceExportDeclaration, NamespaceImport, NewExpression,
    NewLineKind, Node, NodeArray, NodeArrayOrVec, NodeBuilderFlags, NodeCheckFlags, NodeConverters,
    NodeFactory, NodeFlags, NodeId, NodeInterface, NodeLinks, NodeLinksSerializedType,
    NonNullExpression, NumberLiteralType, NumericLiteral, ObjectBindingPattern, ObjectFlags,
    ObjectFlagsTypeInterface, ObjectLiteralExpression, ObjectType, ObjectTypeInterface,
    OmittedExpression, OptionalTypeNode, OuterExpressionKinds, PackageId, ParameterDeclaration,
    ParenthesizedExpression, ParenthesizedTypeNode, ParenthesizerRules, ParseConfigHost,
    ParsedCommandLine, ParsedCommandLineWithBaseOptions, PartiallyEmittedExpression, Path,
    PatternAmbientModule, PluginImport, PollingWatchKind, PostfixUnaryExpression,
    PrefixUnaryExpression, PrintHandlers, Printer, PrinterOptions, PrinterOptionsBuilder,
    PrivateIdentifier, Program, ProjectReference, PropertyAccessExpression, PropertyAssignment,
    PropertyDeclaration, PropertySignature, PseudoBigInt, QualifiedName, RcNodeOrNodeArrayOrVec,
    ReadonlyTextRange, RedirectTargetsMap, RegularExpressionLiteral, RelationComparisonResult,
    RequireResult, ResolvableTypeInterface, ResolvedModuleFull,
    ResolvedModuleWithFailedLookupLocations, ResolvedProjectReference, ResolvedTypeInterface,
    ResolvedTypeReferenceDirective, RestTypeNode, ReturnStatement, ReverseMappedSymbol,
    ReverseMappedType, ScriptKind, ScriptReferenceHost, ScriptTarget, SemicolonClassElement,
    SetAccessorDeclaration, ShorthandPropertyAssignment, Signature, SignatureDeclarationBase,
    SignatureDeclarationInterface, SignatureFlags, SignatureKind,
    SignatureOptionalCallSignatureCache, SourceFile, SourceFileLike, SourceFileMayBeEmittedHost,
    SourceMapRange, SourceTextAsChars, SpreadAssignment, SpreadElement, StringLiteral,
    StringLiteralType, StringMappingType, StringOrNodeArray, StructureIsReused, SubstitutionType,
    SwitchStatement, Symbol, SymbolAccessibility, SymbolAccessibilityResult, SymbolFlags,
    SymbolFormatFlags, SymbolId, SymbolInterface, SymbolLinks, SymbolTable, SymbolTracker,
    SymbolVisibilityResult, SymbolWalker, SymbolWriter, SyntaxKind, SyntaxList, SynthesizedComment,
    TaggedTemplateExpression, TemplateExpression, TemplateLiteralLikeNode,
    TemplateLiteralLikeNodeInterface, TemplateLiteralType, TemplateLiteralTypeNode,
    TemplateLiteralTypeSpan, TemplateSpan, Ternary, TextChangeRange, TextRange, TextSpan,
    ThisTypeNode, ThrowStatement, ToHashMapOfCompilerOptionsValues, TokenFlags, TransformFlags,
    TransformationContext, TransformationResult, Transformer, TransformerFactory,
    TransformerFactoryOrCustomTransformerFactory, TransientSymbol, TransientSymbolInterface,
    TryStatement, TsConfigOnlyOption, TupleType, TupleTypeNode, Type, TypeAcquisition,
    TypeAliasDeclaration, TypeAssertion, TypeChecker, TypeCheckerHost, TypeCheckerHostDebuggable,
    TypeFlags, TypeFormatFlags, TypeId, TypeInterface, TypeLiteralNode, TypeMapper,
    TypeOfExpression, TypeOperatorNode, TypeParameter, TypeParameterDeclaration, TypePredicate,
    TypePredicateKind, TypePredicateNode, TypeQueryNode, TypeReference, TypeReferenceInterface,
    TypeReferenceNode, TypeReferenceSerializationKind, UnderscoreEscapedMap,
    UnionOrIntersectionType, UnionOrIntersectionTypeInterface, UnionReduction, UnionType,
    UnionTypeNode, UniqueESSymbolType, UnparsedPrepend, UnparsedPrologue, UnparsedSectionInterface,
    UnparsedSource, UnparsedTextLike, VariableDeclaration, VariableDeclarationList,
    VariableLikeDeclarationInterface, VariableStatement, VarianceFlags, VisitResult,
    VoidExpression, WatchDirectoryFlags, WatchDirectoryKind, WatchFileKind, WatchOptions,
    WhileStatement, WithStatement, WriteFileCallback, YieldExpression, __String,
};
pub use compiler::utilities::{
    add_related_info, attach_file_to_diagnostics, chain_diagnostic_messages, change_extension,
    compare_diagnostics, compare_diagnostics_skip_related_information, contains_parse_error,
    copy_entries, create_compiler_diagnostic, create_detached_diagnostic,
    create_diagnostic_collection, create_diagnostic_for_file_from_message_chain,
    create_diagnostic_for_node, create_diagnostic_for_node_from_message_chain,
    create_diagnostic_for_node_in_source_file, create_file_diagnostic, create_symbol_table,
    create_text_writer, declaration_name_to_string, default_maximum_truncation_length,
    ensure_script_kind, entity_name_to_string, escape_jsx_attribute_string,
    escape_non_ascii_string, escape_string, export_assignment_is_alias,
    external_helpers_module_name_text, for_each_entry, format_message, format_string_from_args,
    full_triple_slash_amd_reference_path_reg_ex, full_triple_slash_reference_path_reg_ex,
    get_alias_declaration_from_name, get_allow_js_compiler_option,
    get_allow_synthetic_default_imports, get_ancestor, get_assigned_expando_initializer,
    get_assignment_declaration_kind, get_assignment_declaration_property_access_kind,
    get_binary_operator_precedence, get_check_flags, get_class_like_declaration_of_symbol,
    get_compiler_option_value, get_containing_class, get_containing_function_or_class_static_block,
    get_declaration_modifier_flags_from_symbol, get_declaration_of_kind,
    get_declared_expando_initializer, get_effective_base_type_node,
    get_effective_container_for_jsdoc_template_tag, get_effective_implements_type_nodes,
    get_effective_initializer, get_effective_jsdoc_host, get_effective_modifier_flags,
    get_effective_modifier_flags_always_include_jsdoc, get_effective_return_type_node,
    get_effective_set_accessor_type_annotation_node, get_effective_type_annotation_node,
    get_emit_flags, get_emit_module_kind, get_emit_module_resolution_kind, get_emit_script_target,
    get_enclosing_block_scope_container, get_end_line_position, get_error_span_for_node,
    get_es_module_interop, get_escaped_text_of_identifier_or_literal, get_expando_initializer,
    get_expression_associativity, get_expression_precedence,
    get_external_module_import_equals_declaration_expression, get_external_module_name,
    get_external_module_require_argument, get_file_matcher_patterns,
    get_first_constructor_with_body, get_first_identifier, get_full_width, get_function_flags,
    get_host_signature_from_jsdoc, get_immediately_invoked_function_expression,
    get_interface_base_type_nodes, get_jsdoc_comment_ranges, get_jsdoc_comments_and_tags,
    get_jsdoc_host, get_jsdoc_root, get_jsdoc_type_parameter_declarations,
    get_jsx_transform_enabled, get_language_variant, get_leftmost_access_expression,
    get_leftmost_expression, get_literal_text, get_local_symbol_for_export_default,
    get_locale_specific_message, get_members_of_declaration, get_name_of_expando,
    get_name_or_argument, get_namespace_declaration_node, get_new_line_character, get_object_flags,
    get_operator_associativity, get_operator_precedence, get_parameter_symbol_from_jsdoc,
    get_pattern_from_spec, get_property_name_for_property_name_node, get_regex_from_pattern,
    get_regular_expression_for_wildcard, get_regular_expressions_for_wildcards,
    get_resolved_module, get_right_most_assigned_expression, get_root_declaration,
    get_source_file_of_node, get_source_text_of_node_from_source_file,
    get_span_of_token_at_position, get_strict_option_value, get_supported_extensions,
    get_supported_extensions_with_json_if_resolve_json_module,
    get_symbol_name_for_private_identifier, get_syntactic_modifier_flags,
    get_text_of_identifier_or_literal, get_text_of_node, get_text_of_node_from_source_text,
    get_text_of_property_name, get_this_container, get_token_pos_of_node,
    get_trailing_semicolon_deferring_writer, get_ts_config_prop_array,
    get_ts_config_prop_array_element_value, get_use_define_for_class_fields, has_dynamic_name,
    has_effective_modifier, has_effective_readonly_modifier, has_json_module_emit_enabled,
    has_question_token, has_rest_parameter, has_static_modifier, has_syntactic_modifier,
    is_access_expression, is_aliasable_expression, is_ambient_module, is_any_import_or_re_export,
    is_assignment_declaration, is_assignment_expression, is_assignment_operator,
    is_assignment_target, is_async_function, is_bindable_object_define_property_call,
    is_bindable_static_access_expression, is_bindable_static_element_access_expression,
    is_bindable_static_name_expression, is_block_or_catch_scoped,
    is_catch_clause_variable_declaration_or_binding_element, is_computed_non_literal_name,
    is_destructuring_assignment, is_dotted_name, is_dynamic_name, is_effective_module_declaration,
    is_empty_object_literal, is_entity_name_expression, is_enum_const, is_exports_identifier,
    is_expression_with_type_arguments_in_class_extends_clause, is_external_module_augmentation,
    is_external_module_import_equals_declaration, is_external_or_common_js_module,
    is_function_block, is_function_expression_or_arrow_function, is_function_symbol,
    is_global_scope_augmentation, is_identifier_name, is_implicit_glob, is_import_call,
    is_in_js_file, is_in_jsdoc, is_in_top_level_context, is_incremental_compilation,
    is_internal_module_import_equals_declaration, is_jsdoc_construct_signature,
    is_jsdoc_type_alias, is_json_source_file, is_keyword, is_late_visibility_painted_statement,
    is_literal_import_type_node, is_logical_or_coalescing_assignment_operator,
    is_module_augmentation_external, is_module_exports_access_expression,
    is_module_with_string_literal_name, is_namespace_reexport_declaration, is_node_descendant_of,
    is_object_literal_method, is_object_literal_or_class_expression_method_or_accessor,
    is_parameter_declaration, is_part_of_type_node, is_part_of_type_query, is_pinned_comment,
    is_prologue_directive, is_property_access_entity_name_expression, is_property_name_literal,
    is_prototype_access, is_prototype_property_assignment, is_push_or_unshift_identifier,
    is_require_call, is_require_variable_declaration, is_rest_parameter,
    is_right_side_of_qualified_name_or_property_access, is_shorthand_ambient_module_symbol,
    is_signed_numeric_literal, is_source_file_js, is_special_property_declaration, is_static,
    is_string_double_quoted, is_string_or_numeric_literal_like, is_super_property,
    is_this_identifier, is_this_initialized_declaration, is_this_property, is_type_alias,
    is_type_node_kind, is_umd_export_symbol, is_valid_type_only_alias_use_site,
    is_value_signature_declaration, is_variable_like, is_watch_set, is_write_only_access,
    maybe_set_parent, modifier_to_flag, modifiers_to_flags,
    no_truncation_maximum_truncation_length, node_is_missing, node_is_present, node_is_synthesized,
    node_starts_new_lexical_environment, object_allocator, out_file, package_id_to_string,
    parse_pseudo_big_int, position_is_synthesized, pseudo_big_int_to_string, remove_extension,
    remove_file_extension, resolution_extension_is_ts_or_json, resolving_empty_array, set_parent,
    set_parent_recursive, set_text_range_pos, set_text_range_pos_end, set_text_range_pos_width,
    set_value_declaration, should_preserve_const_enums, skip_parentheses, skip_type_checking,
    slice_after, supported_js_extensions_flat, supported_ts_extensions_flat,
    try_extract_ts_extension, try_get_import_from_module_specifier, try_parse_pattern,
    unreachable_code_is_error, unused_label_is_error, using_single_line_string_writer,
    walk_up_parenthesized_expressions, walk_up_parenthesized_types,
    walk_up_parenthesized_types_and_get_parent_and_child, write_file_ensuring_directories,
    Associativity, FileMatcherPatterns, FunctionFlags, GetLiteralTextFlags, OperatorPrecedence,
    StringOrPattern, SymlinkCache,
};
use compiler::utilities::{
    get_element_or_property_access_argument_expression_or_name,
    get_element_or_property_access_name, has_invalid_escape, set_localized_diagnostic_messages,
    set_type_acquisition_value, set_watch_option_value,
};
pub use compiler::utilities_public::{
    collapse_text_change_ranges_across_multiple_versions, create_text_change_range,
    create_text_span, create_text_span_from_bounds, decoded_text_span_intersects_with,
    escape_leading_underscores, find_ancestor, get_all_jsdoc_tags, get_all_jsdoc_tags_of_kind,
    get_combined_modifier_flags, get_combined_node_flags, get_default_lib_file_name,
    get_effective_constraint_of_type_parameter, get_effective_type_parameter_declarations,
    get_jsdoc_augments_tag, get_jsdoc_class_tag, get_jsdoc_deprecated_tag, get_jsdoc_enum_tag,
    get_jsdoc_implements_tags, get_jsdoc_override_tag_no_cache, get_jsdoc_parameter_tags,
    get_jsdoc_private_tag, get_jsdoc_protected_tag, get_jsdoc_public_tag, get_jsdoc_readonly_tag,
    get_jsdoc_return_tag, get_jsdoc_return_type, get_jsdoc_tags, get_jsdoc_template_tag,
    get_jsdoc_this_tag, get_jsdoc_type, get_jsdoc_type_parameter_tags, get_jsdoc_type_tag,
    get_name_of_declaration, get_name_of_jsdoc_typedef, get_original_node, get_parse_tree_node,
    get_text_of_jsdoc_comment, get_type_parameter_owner, has_jsdoc_parameter_tags,
    has_only_expression_initializer, id_text, is_accessor, is_assertion_expression,
    is_assertion_key, is_binding_name, is_binding_pattern, is_break_or_continue_statement,
    is_call_chain, is_call_like_expression, is_call_or_new_expression, is_case_or_default_clause,
    is_class_element, is_class_like, is_class_or_type_element, is_const_type_reference,
    is_element_access_chain, is_empty_binding_element, is_empty_binding_pattern, is_entity_name,
    is_external_module_name_relative, is_function_like, is_function_or_constructor_type_node,
    is_get_accessor, is_import_or_export_specifier, is_iteration_statement,
    is_jsdoc_comment_containing_node, is_jsdoc_link_like, is_jsdoc_property_like_tag,
    is_jsx_opening_like_element, is_literal_expression, is_member_name, is_modifier,
    is_named_export_bindings, is_non_null_chain, is_nullish_coalesce, is_object_literal_element,
    is_object_literal_element_like, is_optional_chain, is_parameter_property_declaration,
    is_parse_tree_node, is_property_access_chain, is_property_access_or_qualified_name,
    is_property_name, is_set_accessor, is_string_literal_like, is_string_text_containing_node,
    is_template_literal, is_template_literal_token, is_template_middle_or_template_tail, is_token,
    is_token_kind, is_type_element, is_type_node, is_type_only_import_or_export_declaration,
    is_unparsed_node, is_unparsed_text_like, skip_partially_emitted_expressions,
    sort_and_deduplicate_diagnostics, symbol_name, text_change_range_is_unchanged,
    text_change_range_new_span, text_span_contains_position, text_span_contains_text_span,
    text_span_end, text_span_intersection, text_span_intersects_with,
    text_span_intersects_with_position, text_span_intersects_with_text_span, text_span_is_empty,
    text_span_overlap, text_span_overlaps_with, unchanged_text_change_range,
    unescape_leading_underscores, validate_locale_and_set_language,
    walk_up_binding_elements_and_patterns, FindAncestorCallbackReturn,
};
use compiler::utilities_public::{
    get_assigned_name, get_combined_node_flags_always_include_jsdoc,
    get_jsdoc_deprecated_tag_no_cache, get_jsdoc_parameter_tags_no_cache,
    get_jsdoc_private_tag_no_cache, get_jsdoc_protected_tag_no_cache,
    get_jsdoc_public_tag_no_cache, get_jsdoc_readonly_tag_no_cache, get_jsdoc_tags_no_cache,
    get_jsdoc_type_parameter_tags_no_cache, get_non_assigned_name_of_declaration,
    guess_indentation, has_initializer, has_jsdoc_nodes, has_scope_marker, has_type,
    is_array_binding_element, is_array_binding_or_assigment_pattern, is_assignment_pattern,
    is_binding_or_assigment_pattern, is_boolean_literal, is_class_member_modifier, is_concise_body,
    is_declaration, is_declaration_binding_element, is_declaration_statement, is_expression,
    is_expression_of_optional_chain_root, is_external_module_indicator, is_for_in_or_of_statement,
    is_for_initializer, is_function_body, is_function_like_declaration, is_function_like_kind,
    is_function_like_or_class_static_block_declaration, is_function_or_module_block,
    is_generated_identifier, is_get_or_set_accessor_declaration, is_jsdoc_namespace_body,
    is_jsdoc_node, is_jsdoc_tag, is_jsx_attribute_like, is_jsx_child, is_jsx_tag_name_expression,
    is_left_hand_side_expression, is_literal_kind, is_method_or_accessor, is_modifier_kind,
    is_module_body, is_module_or_enum_declaration, is_module_reference, is_named_declaration,
    is_named_import_bindings, is_namespace_body, is_node, is_node_kind,
    is_not_emitted_or_partially_emitted_node, is_object_binding_or_assigment_pattern,
    is_object_binding_or_assignment_element, is_optional_chain_root, is_outermost_optional_chain,
    is_parameter_property_modifier, is_private_identifier_class_element_declaration,
    is_private_identifier_property_access_expression,
    is_property_access_or_qualified_name_or_import_type_node, is_scope_marker, is_statement,
    is_statement_but_not_declaration, is_statement_or_block, is_string_literal_or_jsx_expression,
    is_template_literal_kind, is_type_reference_type, is_unary_expression,
    is_unary_expression_with_write, needs_scope_marker, node_has_name,
    supported_locale_directories, text_range_contains_position_inclusive,
};
pub use compiler::visitor_public::{visit_each_child, visit_node};
pub use compiler::watch::{
    create_diagnostic_reporter, create_watch_compiler_host_of_config_file,
    create_watch_status_reporter, emit_files_and_report_errors_and_get_exit_status,
    get_error_summary_text, parse_config_file_with_system, perform_incremental_compilation,
    CreateWatchCompilerHostOfConfigFileInput, IncrementalCompilationOptions,
    ProgramOrBuilderProgram,
};
pub use compiler::watch_public::{
    create_incremental_compiler_host, create_watch_program, CreateProgram, ProgramHost,
    WatchCompilerHost, WatchCompilerHostOfConfigFile, WatchHost, WatchStatusReporter,
};
pub use execute_command_line::execute_command_line::execute_command_line;
pub use rust_helpers::number::Number;
pub use rust_helpers::weak_self::WeakSelf;
pub use rust_helpers::{
    are_option_rcs_equal, are_rc_slices_equal, index_of, index_of_rc, is_same_variant,
    last_index_of,
};
