#[macro_use]
extern crate lazy_static;

mod compiler;
mod execute_command_line;
mod rust_helpers;

pub use compiler::binder::bind_source_file;
pub use compiler::checker::{create_type_checker, NodeBuilder};
pub use compiler::command_line_parser::parse_command_line;
pub use compiler::core::{
    append_if_unique, binary_search, binary_search_copy_key, compare_strings_case_sensitive,
    compare_values, concatenate, every, first_defined, first_or_undefined, for_each, insert_sorted,
    last_or_undefined, map, maybe_for_each, range_equals, some,
};
pub use compiler::core_public::{Comparer, Comparison, SortedArray};
pub use compiler::debug::Debug_;
pub use compiler::diagnostic_information_map_generated::Diagnostics;
pub use compiler::emitter::create_printer;
pub use compiler::factory::base_node_factory::{
    create_base_node_factory, BaseNodeFactory, BaseNodeFactoryConcrete,
};
pub use compiler::factory::node_factory::{
    create_node_factory, factory, get_synthetic_factory, BaseNodeFactorySynthetic,
};
pub use compiler::factory::node_tests::{
    is_big_int_literal, is_binding_element, is_block, is_class_static_block_declaration,
    is_identifier, is_module_block, is_object_literal_expression, is_omitted_expression,
    is_private_identifier, is_property_assignment, is_property_declaration, is_property_signature,
    is_source_file, is_type_alias_declaration, is_variable_declaration,
};
pub use compiler::parser::{create_source_file, for_each_child, MissingNode};
pub use compiler::path::{normalize_path, to_path};
pub use compiler::program::create_program;
pub use compiler::scanner::{
    create_scanner, is_identifier_text, skip_trivia, token_is_identifier_or_keyword,
    token_to_string, Scanner,
};
pub use compiler::sys::{get_sys, System};
pub use compiler::types::{
    rc_source_file_into_rc_node, ArrayLiteralExpression, ArrayTypeNode, BaseBindingLikeDeclaration,
    BaseDiagnostic, BaseDiagnosticRelatedInformation, BaseGenericNamedDeclaration,
    BaseInterfaceOrClassLikeDeclaration, BaseInterfaceType, BaseIntrinsicType, BaseLiteralLikeNode,
    BaseLiteralType, BaseNamedDeclaration, BaseNode, BaseObjectType, BaseSymbol,
    BaseTransientSymbol, BaseType, BaseUnionOrIntersectionType, BaseVariableLikeDeclaration,
    BigIntLiteral, BigIntLiteralType, BinaryExpression, BindingLikeDeclarationInterface, Block,
    CharacterCodes, CheckFlags, CompilerHost, CreateProgramOptions, Diagnostic, DiagnosticCategory,
    DiagnosticCollection, DiagnosticInterface, DiagnosticMessage, DiagnosticMessageChain,
    DiagnosticMessageText, DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface,
    DiagnosticWithDetachedLocation, DiagnosticWithLocation, EmitFlags, EmitHint, EmitTextWriter,
    EmptyStatement, ExitStatus, Expression, ExpressionStatement, FreshableIntrinsicType,
    HasExpressionInitializerInterface, HasTypeArgumentsInterface, HasTypeInterface,
    HasTypeParametersInterface, Identifier, IfStatement, InterfaceDeclaration, InterfaceType,
    InterfaceTypeWithDeclaredMembersInterface, InternalSymbolName, IntersectionTypeNode,
    IntrinsicType, IntrinsicTypeInterface, KeywordTypeNode, ListFormat, LiteralLikeNode,
    LiteralLikeNodeInterface, LiteralType, LiteralTypeInterface, LiteralTypeNode,
    ModuleResolutionHost, ModuleSpecifierResolutionHost, NamedDeclarationInterface, Node,
    NodeArray, NodeArrayOrVec, NodeBuilderFlags, NodeCheckFlags, NodeFactory, NodeFlags, NodeId,
    NodeInterface, NodeLinks, NumberLiteralType, NumericLiteral, ObjectFlags,
    ObjectFlagsTypeInterface, ObjectLiteralExpression, ObjectType, ObjectTypeInterface,
    ParsedCommandLine, Path, PrefixUnaryExpression, Printer, PrinterOptions, Program,
    PropertyAssignment, PropertySignature, PseudoBigInt, ReadonlyTextRange,
    RelationComparisonResult, ResolvableTypeInterface, ResolvedTypeInterface, SourceFile,
    Statement, StringLiteral, StringLiteralType, StructureIsReused, Symbol, SymbolFlags,
    SymbolFormatFlags, SymbolId, SymbolInterface, SymbolLinks, SymbolTable, SymbolTracker,
    SymbolWriter, SyntaxKind, Ternary, TextSpan, TokenFlags, TransientSymbol,
    TransientSymbolInterface, Type, TypeAliasDeclaration, TypeChecker, TypeCheckerHost,
    TypeElement, TypeFlags, TypeFormatFlags, TypeId, TypeInterface, TypeLiteralNode, TypeMapper,
    TypeNode, TypeParameter, TypeParameterDeclaration, TypeReference, TypeReferenceNode,
    UnionOrIntersectionType, UnionOrIntersectionTypeInterface, UnionReduction, UnionType,
    UnionTypeNode, VariableDeclaration, VariableDeclarationList, VariableLikeDeclarationInterface,
    VariableStatement, __String,
};
pub use compiler::utilities::{
    attach_file_to_diagnostics, chain_diagnostic_messages, create_detached_diagnostic,
    create_diagnostic_collection, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, create_symbol_table, create_text_writer,
    declaration_name_to_string, get_binary_operator_precedence, get_check_flags,
    get_declaration_of_kind, get_effective_initializer, get_effective_type_annotation_node,
    get_escaped_text_of_identifier_or_literal, get_first_identifier, get_literal_text,
    get_object_flags, get_source_file_of_node, has_dynamic_name, is_block_or_catch_scoped,
    is_external_or_common_js_module, is_keyword, is_property_name_literal, is_type_alias,
    is_write_only_access, node_is_missing, object_allocator, parse_pseudo_big_int,
    position_is_synthesized, pseudo_big_int_to_string, set_parent, set_text_range_pos_end,
    set_value_declaration, using_single_line_string_writer, GetLiteralTextFlags,
    OperatorPrecedence,
};
pub use compiler::utilities_public::{
    create_text_span_from_bounds, escape_leading_underscores, get_combined_node_flags,
    get_effective_type_parameter_declarations, get_name_of_declaration, has_initializer,
    has_only_expression_initializer, id_text, is_binding_pattern, is_expression, is_function_like,
    is_function_or_module_block, is_literal_kind, is_member_name, unescape_leading_underscores,
};
pub use compiler::watch::emit_files_and_report_errors_and_get_exit_status;
pub use execute_command_line::execute_command_line::execute_command_line;
pub use rust_helpers::is_same_variant;
pub use rust_helpers::number::Number;
pub use rust_helpers::weak_self::WeakSelf;
